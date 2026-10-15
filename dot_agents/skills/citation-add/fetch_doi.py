#!/usr/bin/env python3
"""
Fetch structured metadata from CrossRef API by DOI.

Usage:
    python3 fetch_doi.py <doi>

Output (stdout, one per line):
    FIELD:key=value   - metadata fields from CrossRef
    NOTFOUND:<doi>    - DOI not in CrossRef
    ERROR:<reason>    - network or parse error

Fields returned (when available):
    type        - CrossRef work type (journal-article, proceedings-article, etc.)
    title
    author      - "Lastname, Firstname and Lastname, Firstname" format
    year
    journal     - container title (for articles)
    booktitle   - container title (for proceedings)
    volume
    number
    pages
    publisher
    doi
    abstract    - present for many publishers; JATS markup converted to plain text

Inference reads these fields and formats the final BibTeX entry
following examples.md conventions. fetch_doi.py only fetches - it does
not format.

Exit codes:
    0 - success (fields printed)
    1 - not found or error
"""

import sys
import html
import json
import re
import urllib.request
import urllib.error


def fetch_crossref(doi):
    url = f"https://api.crossref.org/works/{urllib.request.quote(doi, safe='/')}"
    req = urllib.request.Request(
        url,
        headers={"User-Agent": "citation-add/1.0"}
    )
    try:
        with urllib.request.urlopen(req, timeout=10) as resp:
            return json.loads(resp.read())
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return None
        raise RuntimeError(f"HTTP {e.code}")
    except Exception as e:
        raise RuntimeError(str(e))


def strip_html(s):
    return re.sub(r"<[^>]+>", "", s or "").strip()


def jats_abstract(s):
    """CrossRef abstracts are JATS XML. Drop a leading "Abstract" heading, keep
    structured-abstract section titles as "Title:", keep paragraph breaks as
    spaces, and decode entities, so the text matches the printed abstract."""
    if not s:
        return ""
    s = re.sub(r"<jats:title>\s*(abstract|summary)\s*</jats:title>", " ", s, flags=re.I)
    s = re.sub(r"<jats:title>\s*([^<]*?)\s*:?\s*</jats:title>", r" \1: ", s)
    s = re.sub(r"</?jats:(p|sec)[^>]*>", " ", s)
    s = html.unescape(strip_html(s))
    s = re.sub(r"\s+", " ", s).strip()
    return re.sub(r"^(abstract|summary)\s*[:.\u2014-]?\s+", "", s, flags=re.I)


def parse_fields(data, doi):
    w = data.get("message", {})
    fields = {}

    fields["type"] = w.get("type", "")

    titles = w.get("title", [])
    if titles:
        fields["title"] = strip_html(titles[0])

    authors = w.get("author", [])
    if authors:
        parts = []
        for a in authors:
            if a.get("family"):
                given = a.get("given", "")
                parts.append(f"{a['family']}, {given}" if given else a["family"])
        if parts:
            fields["author"] = " and ".join(parts)

    for date_field in ("published-print", "published-online", "published", "issued"):
        dp = w.get(date_field, {}).get("date-parts", [[]])
        if dp and dp[0]:
            fields["year"] = str(dp[0][0])
            break

    containers = w.get("container-title", [])
    if containers:
        cr_type = fields.get("type", "")
        if cr_type == "proceedings-article":
            fields["booktitle"] = containers[0]
        else:
            fields["journal"] = containers[0]

    for f in ("volume", "issue", "page", "publisher"):
        v = w.get(f, "")
        if v:
            out_key = "number" if f == "issue" else ("pages" if f == "page" else f)
            fields[out_key] = str(v)

    fields["doi"] = doi

    abstract = jats_abstract(w.get("abstract", ""))
    if abstract:
        fields["abstract"] = abstract

    return fields


def main():
    if len(sys.argv) < 2:
        print("Usage: fetch_doi.py <doi>", file=sys.stderr)
        sys.exit(1)

    doi = sys.argv[1].strip()
    # Normalize - strip URL prefix if present
    doi = re.sub(r"^https?://doi\.org/", "", doi)
    doi = re.sub(r"^doi:\s*", "", doi, flags=re.IGNORECASE)

    try:
        data = fetch_crossref(doi)
    except RuntimeError as e:
        print(f"ERROR:{e}")
        sys.exit(1)

    if data is None:
        print(f"NOTFOUND:{doi}")
        sys.exit(1)

    fields = parse_fields(data, doi)
    for k, v in fields.items():
        # Escape newlines in multi-line values (abstract)
        v_clean = v.replace("\n", " ").replace("\r", "")
        print(f"FIELD:{k}={v_clean}")

    # Warn if CrossRef returned too few fields to build a complete entry
    # (excludes 'type' and 'doi' which are always present)
    substantive = {k for k in fields if k not in ('type', 'doi')}
    if len(substantive) < 4:
        missing = {'title', 'author', 'year', 'journal', 'booktitle'} - substantive
        print(f"SPARSE:CrossRef returned only {len(substantive)} substantive fields. "
              f"Missing: {', '.join(sorted(missing))}. Read PDF pages for fallback.")


if __name__ == "__main__":
    main()
