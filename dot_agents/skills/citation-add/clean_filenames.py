#!/usr/bin/env python3
"""
Strip Anna's Archive suffixes and trailing UUIDs from PDF filenames in ~/org/pdfs/.

Patterns removed (from the stem, working right-to-left):
  " -- <anything> -- Anna's Archive"   (greedy, catches doi slugs before the UUID)
  " -- Anna's Archive"                 (standalone label)
  " -- <hex32>"                        (bare 32-char hex UUID with no label)

Dry-run by default. Pass --apply to actually rename.
"""
import re
import os
import glob
import argparse

pdfs_dir = os.path.expanduser("~/org/pdfs/")

ANNAS = r"Anna\u2019s Archive|Anna's Archive"  # U+2019 right single quote or ASCII

def clean_stem(stem: str) -> str:
    # Remove " -- <anything> -- Anna's Archive" (handles doi-slug + uuid before label)
    stem = re.sub(r"\s*--\s*.+--\s*(?:" + ANNAS + r")\s*$", "", stem, flags=re.IGNORECASE)
    # Remove standalone " -- Anna's Archive"
    stem = re.sub(r"\s*--\s*(?:" + ANNAS + r")\s*$", "", stem, flags=re.IGNORECASE)
    # Remove trailing bare UUID " -- <hex32>"
    stem = re.sub(r"\s*--\s*[0-9a-f]{32}\s*$", "", stem, flags=re.IGNORECASE)
    return stem.strip()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--apply", action="store_true", help="Actually rename files (default: dry-run)")
    args = parser.parse_args()

    changes = []
    for path in sorted(glob.glob(os.path.join(pdfs_dir, "*.pdf"))):
        old_name = os.path.basename(path)
        stem, ext = os.path.splitext(old_name)
        new_stem = clean_stem(stem)
        new_name = new_stem + ext
        if new_name != old_name:
            changes.append((path, old_name, new_name))

    if not changes:
        print("Nothing to rename.")
    else:
        for path, old, new in changes:
            print(f"  {old}")
            print(f"→ {new}")
            print()
            if args.apply:
                new_path = os.path.join(pdfs_dir, new)
                if os.path.exists(new_path):
                    print(f"  SKIPPED (target already exists)")
                else:
                    os.rename(path, new_path)
        if not args.apply:
            print(f"{len(changes)} file(s) would be renamed. Pass --apply to rename.")
        else:
            print(f"{len(changes)} file(s) renamed.")
