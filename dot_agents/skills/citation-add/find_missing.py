#!/usr/bin/env python3
"""
Find PDF files in ~/org/pdfs/ that have no matching file= entry in ~/org/bib/references.bib.

Output:
  MISSING:<comma-separated filenames>   (empty after colon if none)
  CITEKEYS:<comma-separated citekeys>
"""
import re
import os
import glob

bib_path = os.path.expanduser("~/org/bib/references.bib")
pdfs_dir = os.path.expanduser("~/org/pdfs/")

with open(bib_path) as f:
    bib = f.read()

file_vals = re.findall(r'file\s*=\s*\{([^}]+)\}', bib)
bib_files = {os.path.basename(v.strip()) for v in file_vals}

citekeys = set(re.findall(r'@\w+\{(\S+),', bib))

disk_files = {os.path.basename(p) for p in glob.glob(os.path.join(pdfs_dir, "*.pdf"))}

missing = sorted(disk_files - bib_files)

print("MISSING:" + ",".join(missing))
if missing:
    print("CITEKEYS:" + ",".join(sorted(citekeys)))
