#!/usr/bin/env python3
"""
Duplicate detection for citation-add.

Given a new PDF's extracted DOI and/or title, checks references.bib for
existing entries that may be the same work. Outputs structured instructions
for the calling inference agent.

Usage:
    python3 check_duplicate.py --bib ~/org/bib/references.bib
                               [--doi <doi>]
                               [--title <title>]
                               [--author <author>]
                               [--year <year>]
                               [--new-pdf <filename>]

Output (stdout, one per line):
    NO_MATCH
        No existing entry looks like this paper. Proceed to add.

    KEEP_EXISTING: citekey=<key> existing_file=<path> reason=<text>
        Deterministic: delete new PDF, keep existing. Reason explains why.

    KEEP_NEW: citekey=<key> existing_file=<path> reason=<text>
        Deterministic: update bib file field to new PDF, delete old PDF.

    INFER: citekey=<key> existing_file=<path> new_pdf=<filename> hint=<text>
        Read pages 1-2 of both PDFs and reason about whether they are the
        same work. Resolve as KEEP_EXISTING, KEEP_NEW, or ASK based on
        content. If resolved silently, log the decision in the run report.

    ASK: citekey=<key> existing_file=<path> message=<text>
        Genuinely ambiguous. Show message to user and wait for input.

Exit codes:
    0 - output produced (check first token to interpret)
    1 - usage error
"""

import sys
import re
import os
import argparse


ARXIV_RE = re.compile(r'(\d{4}\.\d{4,5})(?:v(\d+))?(?:\.pdf)?$', re.IGNORECASE)


def extract_field(name, block):
    """Extract a bib field value, handling nested braces correctly."""
    m = re.search(r'\b' + name + r'\s*=\s*\{', block, re.IGNORECASE)
    if not m:
        return ''
    start = m.end()
    depth = 1
    i = start
    while i < len(block) and depth > 0:
        if block[i] == '{':
            depth += 1
        elif block[i] == '}':
            depth -= 1
        i += 1
    return block[start:i - 1].strip()


def load_bib(bib_path):
    """Parse bib into list of dicts with keys: citekey, doi, title, author, year, file."""
    entries = []
    try:
        with open(bib_path, encoding='utf-8') as stream:
            text = stream.read()
    except FileNotFoundError:
        return entries

    for block in re.split(r'(?=@\w+\s*\{)', text):
        block = block.strip()
        if not block.startswith('@'):
            continue
        m = re.match(r'@\w+\s*\{([^,]+),', block)
        if not m:
            continue
        citekey = m.group(1).strip()

        entries.append({
            'citekey': citekey,
            'doi': extract_field('doi', block).lower().strip(),
            'title': extract_field('title', block).lower().strip(),
            'author': extract_field('author', block),
            'year': extract_field('year', block).strip(),
            'file': extract_field('file', block),
        })
    return entries


def normalize_doi(doi):
    if not doi:
        return ''
    doi = re.sub(r'^https?://doi\.org/', '', doi, flags=re.IGNORECASE)
    doi = re.sub(r'^doi:\s*', '', doi, flags=re.IGNORECASE)
    return doi.lower().strip()


def title_words(title):
    """Return set of significant lowercase words from a title."""
    stop = {'a', 'an', 'the', 'of', 'in', 'on', 'for', 'and', 'or', 'to',
            'with', 'using', 'towards', 'combining', 'via'}
    words = re.findall(r'[a-z0-9]+', (title or '').lower())
    return {w for w in words if w not in stop and len(w) > 2}


def author_surnames(author_str):
    """Return set of lowercase surnames from a bib author string.

    Handles both 'Lastname, Firstname and ...' and 'Firstname Lastname and ...' forms.
    Also handles raw nav.py output like 'John Smith; Jane Doe'.
    """
    if not author_str:
        return set()
    # Split on ' and ' or ';'
    parts = re.split(r'\s+and\s+|;', author_str, flags=re.IGNORECASE)
    surnames = set()
    for part in parts:
        part = part.strip()
        if not part:
            continue
        # 'Lastname, Firstname' form
        if ',' in part:
            surname = part.split(',')[0].strip()
        else:
            # 'Firstname Lastname' - take last token
            tokens = part.split()
            surname = tokens[-1] if tokens else ''
        # Strip LaTeX accents like {\v{c}} -> just take alpha chars
        surname = re.sub(r'\{[^}]*\}', '', surname)
        surname = re.sub(r'[^a-zA-Z]', '', surname).lower()
        if surname:
            surnames.add(surname)
    return surnames


def arxiv_id_and_version(filename):
    """Return (arxiv_id, version_int) or (None, None)."""
    m = ARXIV_RE.search(filename or '')
    if m:
        arxiv_id = m.group(1)
        version = int(m.group(2)) if m.group(2) else 0
        return arxiv_id, version
    return None, None


def signals_match(new_title, new_author, new_year, entry, title_threshold):
    """Check whether title, author, and year all agree with an existing entry.

    Returns (matched: bool, overlap: float, missing_signals: list).
    missing_signals lists which signals were unavailable (not failed).
    All available signals must match; missing signals are noted in the hint.
    """
    missing = []

    # Title
    new_words = title_words(new_title)
    existing_words = title_words(entry['title'])
    if not new_words or not existing_words:
        return False, 0.0, missing
    overlap = len(new_words & existing_words) / max(len(new_words), len(existing_words))
    if overlap < title_threshold:
        return False, overlap, missing

    # Author
    new_surnames = author_surnames(new_author)
    existing_surnames = author_surnames(entry['author'])
    if new_surnames and existing_surnames:
        if not (new_surnames & existing_surnames):
            return False, overlap, missing
    else:
        missing.append('author')

    # Year
    new_yr = (new_year or '').strip()
    existing_yr = (entry['year'] or '').strip()
    if new_yr and existing_yr:
        if new_yr != existing_yr:
            return False, overlap, missing
    else:
        missing.append('year')

    return True, overlap, missing


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--bib', required=True)
    parser.add_argument('--doi', default='')
    parser.add_argument('--title', default='')
    parser.add_argument('--author', default='')
    parser.add_argument('--year', default='')
    parser.add_argument('--new-file', '--new-pdf', dest='new_pdf', default='')
    args = parser.parse_args()

    new_doi = normalize_doi(args.doi)
    new_title = args.title.lower().strip()
    new_author = args.author
    new_year = args.year.strip()
    new_pdf = args.new_pdf
    entries = load_bib(args.bib)

    new_arxiv_id, new_arxiv_ver = arxiv_id_and_version(new_pdf)

    for e in entries:
        citekey = e['citekey']
        existing_file = e['file']

        # --- Case 1: exact DOI match (deterministic) ---
        if new_doi and e['doi'] and new_doi == e['doi']:
            new_format = os.path.splitext(new_pdf)[1].lower()
            existing_formats = {'.' + suffix.lower() for suffix in re.findall(r'\.(pdf|epub)(?=[;:\s]|$)', existing_file, re.I)}
            if new_format in {'.pdf', '.epub'} and existing_formats and new_format not in existing_formats:
                print(f"ADD_FORMAT: citekey={citekey} existing_file={existing_file} new_file={new_pdf} reason=Same DOI, additional source format")
                return
            print(f"KEEP_EXISTING: citekey={citekey} existing_file={existing_file} "
                  f"reason=DOI match ({new_doi})")
            return

        # --- Case 2: arXiv ID match (deterministic version comparison) ---
        existing_arxiv_id, existing_arxiv_ver = arxiv_id_and_version(existing_file)
        if new_arxiv_id and existing_arxiv_id and new_arxiv_id == existing_arxiv_id:
            if new_arxiv_ver > existing_arxiv_ver:
                print(f"KEEP_NEW: citekey={citekey} existing_file={existing_file} "
                      f"reason=arXiv {new_arxiv_id} v{new_arxiv_ver} > v{existing_arxiv_ver}")
            else:
                print(f"KEEP_EXISTING: citekey={citekey} existing_file={existing_file} "
                      f"reason=arXiv {new_arxiv_id} existing v{existing_arxiv_ver} >= new v{new_arxiv_ver}")
            return

        # --- Case 3: new has DOI, existing is arXiv preprint (published beats preprint) ---
        if new_doi and existing_arxiv_id and not e['doi']:
            matched, overlap, missing = signals_match(
                new_title, new_author, new_year, e, title_threshold=0.6
            )
            if matched:
                caveat = f" (unconfirmed: {', '.join(missing)})" if missing else ""
                print(f"INFER: citekey={citekey} existing_file={existing_file} "
                      f"new_pdf={new_pdf} "
                      f"hint=New PDF has DOI {new_doi}, existing is arXiv preprint. "
                      f"Title overlap {overlap:.0%}, author+year match{caveat}. "
                      f"Read p1-2 of each: if same work, keep new (published) and delete arXiv file.")
                return

        # Different DOIs - definitely not a duplicate
        if new_doi and e['doi'] and new_doi != e['doi']:
            continue

        # --- Case 4: title + author + year similarity (all three required) ---
        if new_title and e['title']:
            matched, overlap, missing = signals_match(
                new_title, new_author, new_year, e, title_threshold=0.7
            )
            if matched:
                caveat = f" (unconfirmed: {', '.join(missing)})" if missing else ""
                print(f"INFER: citekey={citekey} existing_file={existing_file} "
                      f"new_pdf={new_pdf} "
                      f"hint=Title overlap {overlap:.0%}, author+year match{caveat}. "
                      f"No DOI confirmation - read p1-2 of each to verify same work.")
                return

    print("NO_MATCH")


if __name__ == '__main__':
    main()
