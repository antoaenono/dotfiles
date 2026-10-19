#!/usr/bin/env python3
"""Clean download suffixes on uncataloged PDF/EPUB filenames, recursively.

Known attachments are left alone so their bibliography and annotation paths
stay valid. Dry-run by default; --apply renames within each current directory.
"""
import argparse
from pathlib import Path
import re

from source_library import BIB_PATH, SOURCE_ROOT, catalog_files, read_entries, source_files

ANNAS = r"Anna\u2019s Archive|Anna's Archive"


def clean_stem(stem):
    stem = re.sub(r"\s*--\s*.+--\s*(?:" + ANNAS + r")\s*$", "", stem, flags=re.I)
    stem = re.sub(r"\s*--\s*(?:" + ANNAS + r")\s*$", "", stem, flags=re.I)
    stem = re.sub(r"\s*--\s*[0-9a-f]{32}\s*$", "", stem, flags=re.I)
    return stem.strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=SOURCE_ROOT)
    parser.add_argument("--bib", type=Path, default=BIB_PATH)
    parser.add_argument("--apply", action="store_true")
    args = parser.parse_args()
    indexed = catalog_files(read_entries(args.bib), args.bib, args.root)
    renamed = proposed = 0
    for path in source_files(args.root):
        if path.resolve() in indexed:
            continue
        name = clean_stem(path.stem)
        if not name or name == path.stem:
            continue
        destination = path.with_name(name + path.suffix)
        proposed += 1
        print(f"{path} -> {destination}")
        if destination.exists():
            print("SKIPPED: target already exists")
        elif args.apply:
            path.rename(destination)
            renamed += 1
    print(f"{renamed} file(s) renamed." if args.apply else f"{proposed} file(s) would be renamed. Pass --apply to rename.")


if __name__ == "__main__":
    main()
