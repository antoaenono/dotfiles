#!/usr/bin/env python3
"""Read-only audit of source attachments, folder placement, and inbox items."""
import argparse
import json
from pathlib import Path

from source_library import (BIB_PATH, SOURCE_ROOT, attachment_paths,
                            catalog_files, category_for, read_entries, source_files)


def audit(root, bib):
    root, bib = Path(root).expanduser().resolve(), Path(bib).expanduser().resolve()
    entries = read_entries(bib)
    indexed = catalog_files(entries, bib, root)
    issues = []
    files = source_files(root)
    for path in files:
        matches = indexed.get(path.resolve(), [])
        relative = str(path.relative_to(root))
        if not matches:
            issues.append({"kind": "uncataloged", "file": relative})
            continue
        if len(matches) > 1:
            issues.append({"kind": "shared-attachment", "file": relative,
                           "citekeys": [entry.key for entry in matches]})
        categories = {category_for(entry.etype) for entry in matches}
        if len(categories) != 1:
            issues.append({"kind": "conflicting-types", "file": relative})
            continue
        expected = categories.pop()
        if expected is None:
            issues.append({"kind": "needs-category", "file": relative,
                           "types": sorted({entry.etype for entry in matches}),
                           "citekeys": [entry.key for entry in matches]})
        elif path.relative_to(root).parts[0] != expected:
            issues.append({"kind": "unfiled" if path.parent == root else "misplaced",
                           "file": relative, "expected": expected,
                           "citekeys": [entry.key for entry in matches]})
    for entry in entries:
        for path in attachment_paths(entry, bib, root):
            if not path.is_file():
                issues.append({"kind": "missing-attachment", "file": str(path),
                               "citekeys": [entry.key]})
    return {"root": str(root), "files": len(files), "entries": len(entries), "issues": issues}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=SOURCE_ROOT)
    parser.add_argument("--bib", type=Path, default=BIB_PATH)
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()
    report = audit(args.root, args.bib)
    if args.json:
        print(json.dumps(report, ensure_ascii=False, indent=2))
    else:
        print(f"{report['files']} PDF/EPUB files; {report['entries']} bibliography entries")
        for issue in report["issues"]:
            context = ", ".join(issue.get("citekeys", []))
            expected = f" -> {issue['expected']}/" if "expected" in issue else ""
            print(f"{issue['kind']}: {issue['file']}{expected}" + (f" [{context}]" if context else ""))
    # The root is an inbox. Pending classifications and new files are reports,
    # while broken paths and incorrect established placement fail the audit.
    return int(any(issue["kind"] in {"missing-attachment", "misplaced", "conflicting-types"}
                   for issue in report["issues"]))


if __name__ == "__main__":
    raise SystemExit(main())
