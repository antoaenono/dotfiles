#!/usr/bin/env python3
"""Restore config.org when a Codex tool call changes human-authored prose."""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import stat
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path
from typing import Any

TARGET_PATH = "config.org"
SNAPSHOT_VERSION = 1
STALE_AFTER_SECONDS = 24 * 60 * 60
BEGIN_SOURCE = re.compile(br"[ \t]*#\+begin_src(?:[ \t].*)?", re.IGNORECASE)
END_SOURCE = re.compile(br"[ \t]*#\+end_src[ \t]*", re.IGNORECASE)


class GuardError(RuntimeError):
    """The protected state could not be snapshotted or restored safely."""


def main() -> int:
    if sys.argv[1:] == ["--self-test"]:
        suite = unittest.defaultTestLoader.loadTestsFromTestCase(GuardTests)
        return 0 if unittest.TextTestRunner(verbosity=2).run(suite).wasSuccessful() else 1

    try:
        payload = json.load(sys.stdin)
        cwd = Path(str(payload.get("cwd") or ".")).resolve()
        repo = find_repository(cwd)
        if repo is None:
            return 0

        tool_use_id = str(payload.get("tool_use_id") or "")
        if not tool_use_id:
            raise GuardError("hook input has no tool_use_id")

        root = snapshot_root(repo)
        root.mkdir(parents=True, exist_ok=True, mode=0o700)
        cleanup_stale_snapshots(root)
        snapshot = root / safe_id(tool_use_id)
        event = payload.get("hook_event_name")

        if event == "PreToolUse":
            create_snapshot(repo, snapshot, payload)
            return 0
        if event == "PostToolUse":
            message = validate_and_restore(repo, snapshot, tool_use_id)
            if message:
                print(message, file=sys.stderr)
                return 2
            return 0
        raise GuardError(f"unsupported hook event: {event!r}")
    except (GuardError, json.JSONDecodeError, OSError, subprocess.CalledProcessError) as error:
        print(f"Literate config reflector failed: {error}", file=sys.stderr)
        return 2


def find_repository(cwd: Path) -> Path | None:
    try:
        result = run_git(cwd, "rev-parse", "--show-toplevel")
    except subprocess.CalledProcessError:
        return None
    return Path(result.stdout.decode().strip()).resolve()


def run_git(
    repo: Path, *args: str, input: bytes | None = None
) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        input=input,
        check=True,
        capture_output=True,
    )


def snapshot_root(repo: Path) -> Path:
    result = run_git(repo, "rev-parse", "--git-path", "codex-hooks/protect-literate-config")
    path = Path(result.stdout.decode().strip())
    return path if path.is_absolute() else (repo / path).resolve()


def safe_id(value: str) -> str:
    return hashlib.sha256(value.encode()).hexdigest()


def protected_projection(content: bytes) -> bytes:
    """Return exact ordered bytes outside complete Org source blocks."""

    protected = bytearray()
    inside_source = False
    source_started_at = 0

    for line_number, line in enumerate(content.splitlines(keepends=True), 1):
        logical_line = line.rstrip(b"\r\n")
        if inside_source:
            if END_SOURCE.fullmatch(logical_line):
                inside_source = False
            continue
        if END_SOURCE.fullmatch(logical_line):
            raise GuardError(f"unmatched #+end_src at line {line_number}")
        if BEGIN_SOURCE.fullmatch(logical_line):
            inside_source = True
            source_started_at = line_number
            continue
        protected.extend(line)

    if inside_source:
        raise GuardError(f"unclosed #+begin_src at line {source_started_at}")
    return bytes(protected)


def cleanup_stale_snapshots(root: Path) -> None:
    cutoff = time.time() - STALE_AFTER_SECONDS
    for child in root.iterdir():
        try:
            if child.is_dir() and child.stat().st_mtime < cutoff:
                shutil.rmtree(child)
        except OSError:
            pass


def create_snapshot(repo: Path, destination: Path, payload: dict[str, Any]) -> None:
    target = repo / TARGET_PATH
    info = target.lstat()
    if not stat.S_ISREG(info.st_mode):
        raise GuardError(f"{TARGET_PATH} is not a regular file")

    content = target.read_bytes()
    projection = protected_projection(content)
    if destination.exists():
        shutil.rmtree(destination)
    temporary = destination.with_name(f".{destination.name}.{os.getpid()}.tmp")
    if temporary.exists():
        shutil.rmtree(temporary)
    temporary.mkdir(parents=True, mode=0o700)

    metadata = {
        "version": SNAPSHOT_VERSION,
        "created_at": time.time(),
        "repo": str(repo),
        "tool_use_id": payload.get("tool_use_id"),
        "session_id": payload.get("session_id"),
        "turn_id": payload.get("turn_id"),
        "mode": stat.S_IMODE(info.st_mode),
        "content_sha256": digest(content),
        "projection_sha256": digest(projection),
    }
    write_private(temporary / "config.org", content)
    write_private(
        temporary / "manifest.json",
        json.dumps(metadata, sort_keys=True, separators=(",", ":")).encode(),
    )
    write_private(temporary / "index.entries", protected_index(repo))
    write_private(temporary / "integrity.sha256", snapshot_integrity(temporary).encode())
    os.replace(temporary, destination)


def validate_and_restore(repo: Path, snapshot: Path, tool_use_id: str) -> str | None:
    if not snapshot.is_dir():
        raise GuardError("matching pre-tool snapshot is missing")

    recorded_integrity = (snapshot / "integrity.sha256").read_text()
    if snapshot_integrity(snapshot) != recorded_integrity:
        raise GuardError("snapshot content or metadata was modified")

    metadata = json.loads((snapshot / "manifest.json").read_text())
    if (
        metadata.get("version") != SNAPSHOT_VERSION
        or metadata.get("repo") != str(repo)
        or metadata.get("tool_use_id") != tool_use_id
    ):
        raise GuardError("snapshot metadata does not match this tool call")

    before = (snapshot / "config.org").read_bytes()
    if digest(before) != metadata.get("content_sha256"):
        raise GuardError("snapshotted config.org content was modified")
    before_projection = protected_projection(before)
    if digest(before_projection) != metadata.get("projection_sha256"):
        raise GuardError("snapshotted prose projection was modified")

    target = repo / TARGET_PATH
    after_projection: bytes | None = None
    try:
        if target.is_file() and not target.is_symlink():
            after_projection = protected_projection(target.read_bytes())
    except GuardError:
        after_projection = None

    if after_projection == before_projection:
        shutil.rmtree(snapshot)
        return None

    restore_file(target, before, int(metadata["mode"]))
    restore_index(repo, (snapshot / "index.entries").read_bytes())
    shutil.rmtree(snapshot)
    return (
        "Restored config.org because this model tool call changed human-authored "
        "content outside Org source blocks. Retry with a source-block-only edit."
    )


def digest(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def write_private(path: Path, content: bytes) -> None:
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(descriptor, "wb") as stream:
        stream.write(content)


def snapshot_integrity(snapshot: Path) -> str:
    checksum = hashlib.sha256()
    for name in ("config.org", "manifest.json", "index.entries"):
        checksum.update(name.encode())
        checksum.update(b"\0")
        checksum.update((snapshot / name).read_bytes())
        checksum.update(b"\0")
    return checksum.hexdigest()


def protected_index(repo: Path) -> bytes:
    return run_git(repo, "ls-files", "--stage", "-z", "--", TARGET_PATH).stdout


def restore_file(target: Path, content: bytes, mode: int) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(prefix=f".{target.name}.", dir=target.parent)
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(content)
            stream.flush()
            os.fsync(stream.fileno())
        os.chmod(temporary, mode)
        os.replace(temporary, target)
    finally:
        if temporary.exists():
            temporary.unlink()


def restore_index(repo: Path, entries: bytes) -> None:
    run_git(repo, "update-index", "--force-remove", "--", TARGET_PATH)
    if entries:
        run_git(repo, "update-index", "-z", "--index-info", input=entries)


class GuardTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.repo = Path(self.temporary.name)
        run_git(self.repo, "init", "--quiet")
        run_git(self.repo, "config", "user.email", "lit@example.invalid")
        run_git(self.repo, "config", "user.name", "Lit Test")
        self.original = (
            b"* Human heading\nHuman prose.\n"
            b"#+begin_src shell :tangle ./example\n"
            b"echo before\n"
            b"#+end_src\n"
            b"Closing prose.\n"
        )
        (self.repo / TARGET_PATH).write_bytes(self.original)
        run_git(self.repo, "add", TARGET_PATH)
        run_git(self.repo, "commit", "--quiet", "-m", "initial")

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def snapshot(self, tool_use_id: str = "tool-1") -> Path:
        destination = snapshot_root(self.repo) / safe_id(tool_use_id)
        destination.parent.mkdir(parents=True, exist_ok=True)
        create_snapshot(
            self.repo,
            destination,
            {"tool_use_id": tool_use_id, "session_id": "session", "turn_id": "turn"},
        )
        return destination

    def run_hook(self, event: str, tool_use_id: str = "tool-1") -> subprocess.CompletedProcess[bytes]:
        payload = {
            "cwd": str(self.repo),
            "hook_event_name": event,
            "tool_use_id": tool_use_id,
            "session_id": "session",
            "turn_id": "turn",
        }
        return subprocess.run(
            [sys.executable, str(Path(__file__).resolve())],
            input=json.dumps(payload).encode(),
            capture_output=True,
        )

    def test_projection_ignores_source_blocks(self) -> None:
        changed = self.original.replace(b"echo before", b"echo after\necho another")
        self.assertEqual(protected_projection(self.original), protected_projection(changed))

    def test_projection_detects_prose_changes(self) -> None:
        changed = self.original.replace(b"Human prose.", b"Model prose.")
        self.assertNotEqual(protected_projection(self.original), protected_projection(changed))

    def test_projection_rejects_malformed_source_blocks(self) -> None:
        with self.assertRaisesRegex(GuardError, "unclosed"):
            protected_projection(b"Human prose.\n#+begin_src shell\necho nope\n")
        with self.assertRaisesRegex(GuardError, "unmatched"):
            protected_projection(b"Human prose.\n#+end_src\n")

    def test_source_only_edit_survives(self) -> None:
        snapshot = self.snapshot()
        changed = self.original.replace(b"echo before", b"echo after")
        (self.repo / TARGET_PATH).write_bytes(changed)

        self.assertIsNone(validate_and_restore(self.repo, snapshot, "tool-1"))
        self.assertEqual((self.repo / TARGET_PATH).read_bytes(), changed)

    def test_prose_edit_restores_file_and_index(self) -> None:
        snapshot = self.snapshot()
        changed = self.original.replace(b"Human prose.", b"Model prose.").replace(
            b"echo before", b"echo after"
        )
        (self.repo / TARGET_PATH).write_bytes(changed)
        run_git(self.repo, "add", TARGET_PATH)

        message = validate_and_restore(self.repo, snapshot, "tool-1")

        self.assertIn("Restored config.org", message or "")
        self.assertEqual((self.repo / TARGET_PATH).read_bytes(), self.original)
        self.assertEqual(run_git(self.repo, "diff", "--cached", "--", TARGET_PATH).stdout, b"")

    def test_malformed_edit_restores_file(self) -> None:
        snapshot = self.snapshot()
        (self.repo / TARGET_PATH).write_bytes(b"Human prose.\n#+begin_src shell\necho nope\n")

        self.assertIsNotNone(validate_and_restore(self.repo, snapshot, "tool-1"))
        self.assertEqual((self.repo / TARGET_PATH).read_bytes(), self.original)

    def test_staging_preexisting_human_prose_is_allowed(self) -> None:
        human = self.original.replace(b"Human prose.", b"Human revision.")
        (self.repo / TARGET_PATH).write_bytes(human)
        snapshot = self.snapshot()
        run_git(self.repo, "add", TARGET_PATH)

        self.assertIsNone(validate_and_restore(self.repo, snapshot, "tool-1"))
        self.assertEqual(run_git(self.repo, "show", f":{TARGET_PATH}").stdout, human)

    def test_deleted_file_is_restored(self) -> None:
        snapshot = self.snapshot()
        (self.repo / TARGET_PATH).unlink()
        run_git(self.repo, "update-index", "--force-remove", "--", TARGET_PATH)

        self.assertIsNotNone(validate_and_restore(self.repo, snapshot, "tool-1"))
        self.assertEqual((self.repo / TARGET_PATH).read_bytes(), self.original)
        self.assertEqual(run_git(self.repo, "show", f":{TARGET_PATH}").stdout, self.original)

    def test_hook_entrypoint_restores_deleted_file(self) -> None:
        self.assertEqual(self.run_hook("PreToolUse").returncode, 0)
        (self.repo / TARGET_PATH).unlink()

        result = self.run_hook("PostToolUse")

        self.assertEqual(result.returncode, 2)
        self.assertIn(b"Restored config.org", result.stderr)
        self.assertEqual((self.repo / TARGET_PATH).read_bytes(), self.original)


if __name__ == "__main__":
    raise SystemExit(main())
