"""Shared catalog and path rules for the local source library."""
from pathlib import Path
import os
import re

class Field:
    def __init__(self, name, value, vstart, vend, line_start, line_end, delim):
        self.name = name  # lowercased
        self.value = value  # raw text between delimiters
        self.vstart, self.vend = vstart, vend  # span of value (inside delimiters)
        self.line_start, self.line_end = line_start, line_end  # whole field incl. comma+newline
        self.delim = delim


class Entry:
    def __init__(self, etype, key, start, end):
        self.etype, self.key, self.start, self.end = etype, key, start, end
        self.fields = {}

    def get(self, name):
        f = self.fields.get(name)
        return f.value if f else None


def _match_brace(text, i):
    """text[i] == '{'; return index of the matching '}'."""
    depth = 0
    n = len(text)
    while i < n:
        c = text[i]
        if c == "\\":
            i += 2
            continue
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return i
        i += 1
    raise ValueError("unbalanced braces")


def _delete_span(text, name_pos, after):
    """Span to cut so the field disappears and the layout around it stays intact.

    A field alone on its line(s) loses the whole line; a field sharing a line
    with others loses only itself, its comma and the spaces next to it.
    """
    ls = text.rfind("\n", 0, name_pos) + 1
    alone_before = text[ls:name_pos].strip() == ""
    cm = re.compile(r"[ \t]*,[ \t]*").match(text, after)
    end = cm.end() if cm else after
    at_eol = text[end : end + 1] == "\n"
    if alone_before and at_eol:
        return ls, end + 1
    if alone_before:
        return name_pos, end
    start = name_pos
    if at_eol:
        while start > 0 and text[start - 1] in " \t":
            start -= 1
    return start, end


def parse_bib(text):
    entries = []
    for m in re.finditer(r"(?m)^@(\w+)\s*\{", text):
        etype = m.group(1).lower()
        if etype in ("comment", "string", "preamble"):
            continue
        open_i = m.end() - 1
        close_i = _match_brace(text, open_i)
        km = re.compile(r"\s*([^,\s]+)\s*,").match(text, m.end())
        e = Entry(etype, km.group(1), m.start(), close_i + 1)
        i = km.end()
        while True:
            fm = re.compile(r"\s*([A-Za-z][\w-]*)\s*=\s*").match(text, i)
            if not fm or fm.end() > close_i:
                break
            name = fm.group(1).lower()
            j = fm.end()
            if text[j] == "{":
                vend = _match_brace(text, j)
                vstart, after, delim = j + 1, vend + 1, "{"
            elif text[j] == '"':
                k = j + 1
                depth = 0
                while True:
                    c = text[k]
                    if c == "\\":
                        k += 2
                        continue
                    if c == "{":
                        depth += 1
                    elif c == "}":
                        depth -= 1
                    elif c == '"' and depth == 0:
                        break
                    k += 1
                vstart, vend, after, delim = j + 1, k, k + 1, '"'
            else:
                bm = re.compile(r"[^,}\s]+").match(text, j)
                vstart, vend, after, delim = j, bm.end(), bm.end(), ""
            line_start, line_end = _delete_span(text, fm.start(1), after)
            e.fields[name] = Field(name, text[vstart:vend], vstart, vend, line_start, line_end, delim)
            i = after
            sm = re.compile(r"\s*,").match(text, i)
            if sm:
                i = sm.end()
        entries.append(e)
    return entries

SOURCE_ROOT = Path.home() / "org/sources"
BIB_PATH = Path.home() / "org/bib/references.bib"
SOURCE_EXTENSIONS = {".pdf", ".epub"}
BOOK_TYPES = {"book", "mvbook", "collection", "mvcollection", "inbook",
              "incollection", "bookinbook", "suppbook", "suppcollection",
              "reference", "mvreference", "inreference", "booklet"}
PAPER_TYPES = {"article", "inproceedings", "conference"}


def category_for(entry_type):
    kind = entry_type.lower()
    if kind in BOOK_TYPES:
        return "books"
    if kind in PAPER_TYPES:
        return "papers"
    return None


def read_entries(bib=BIB_PATH):
    return parse_bib(Path(bib).expanduser().read_text(encoding="utf-8"))


def attachment_paths(entry, bib=BIB_PATH, root=SOURCE_ROOT):
    """Resolve simple and Zotero/Mendeley file fields without basename matching."""
    result = []
    for item in re.split(r"(?<!\\);", entry.get("file") or ""):
        item = item.strip()
        if not item:
            continue
        triplet = re.fullmatch(r"[^:]*:((?:/|~|\.{1,2}/).+):[A-Za-z0-9]+", item)
        if triplet:
            item = triplet.group(1)
        for value in re.split(r"(?<!\\):(?=(?:/|~|\.{1,2}/))", item):
            value = re.sub(r"\\([:;])", r"\1", value)
            path = Path(os.path.expandvars(value)).expanduser()
            if not path.is_absolute():
                candidates = [Path(root) / path, Path(bib).expanduser().parent / path]
                path = next((candidate for candidate in candidates if candidate.exists()), candidates[0])
            result.append(path.resolve())
    return result


def source_files(root=SOURCE_ROOT):
    return sorted(path for path in Path(root).expanduser().rglob("*")
                  if path.is_file() and path.suffix.lower() in SOURCE_EXTENSIONS)


def catalog_files(entries, bib=BIB_PATH, root=SOURCE_ROOT):
    indexed = {}
    for entry in entries:
        for path in attachment_paths(entry, bib, root):
            indexed.setdefault(path, []).append(entry)
    return indexed
