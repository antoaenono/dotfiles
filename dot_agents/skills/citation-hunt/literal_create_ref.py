#!/usr/bin/env python3
"""
Create a new org-roam ref file for a citekey, matching the citar-org-roam
capture template defined in doom config.

Usage:
    python3 create_ref.py --citekey KEY --title TITLE --pdf ~/org/pdfs/file.pdf

Output:
    CREATED:<path>   - file was created
    EXISTS:<path>    - file already exists, not overwritten
    ERROR:<reason>

The file is registered in the org-roam DB via emacsclient after creation.
"""

import sys
import os
import subprocess
import argparse
import uuid
from datetime import datetime

ROAM_REFS_DIR = os.path.expanduser("~/org/roam/refs")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--citekey", required=True)
    parser.add_argument("--title", required=True)
    parser.add_argument("--pdf", required=True)
    args = parser.parse_args()

    out_path = os.path.join(ROAM_REFS_DIR, f"{args.citekey}.org")

    if os.path.exists(out_path):
        print(f"EXISTS:{out_path}")
        return

    pdf_path = os.path.expanduser(args.pdf)
    date_str = datetime.now().strftime("%Y-%m-%d %a")

    content = f""":PROPERTIES:
:ID: {uuid.uuid4()}
:NOTER_DOCUMENT: {pdf_path}
:ROAM_REFS: @{args.citekey}
:END:
#+title: {args.title}
#+DATE: <{date_str}>
#+PUBLISH: garden
#+DRAFT: true

* Summary
-

* Key Points
-

* Annotations
"""

    with open(out_path, "w") as f:
        f.write(content)

    # Register in org-roam DB
    try:
        subprocess.run(
            ["emacsclient", "--eval",
             f'(org-roam-db-update-file "{out_path}")'],
            capture_output=True, timeout=10
        )
    except Exception:
        pass  # DB update failure is non-fatal - roam will catch it on next sync

    print(f"CREATED:{out_path}")


if __name__ == "__main__":
    main()
