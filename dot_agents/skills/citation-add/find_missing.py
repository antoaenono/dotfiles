#!/usr/bin/env python3
"""List uncataloged PDF and EPUB files, relative to the source-library root."""
import argparse
import csv
from io import StringIO
from pathlib import Path

from source_library import BIB_PATH, SOURCE_ROOT, catalog_files, read_entries, source_files


def csv_line(values):
    output = StringIO()
    csv.writer(output, lineterminator="").writerow(values)
    return output.getvalue()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=SOURCE_ROOT)
    parser.add_argument("--bib", type=Path, default=BIB_PATH)
    args = parser.parse_args()
    root = args.root.expanduser().resolve()
    entries = read_entries(args.bib)
    indexed = catalog_files(entries, args.bib, root)
    missing = [str(path.relative_to(root)) for path in source_files(root)
               if path.resolve() not in indexed]
    print("MISSING:" + csv_line(missing))
    if missing:
        print("CITEKEYS:" + csv_line(sorted(entry.key for entry in entries)))


if __name__ == "__main__":
    main()
