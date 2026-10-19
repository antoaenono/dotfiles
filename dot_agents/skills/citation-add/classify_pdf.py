#!/usr/bin/env python3
"""
Classify a PDF for citation-add: document class, DOI, pdfinfo metadata, and
which pages to read for the bib fields or the abstract.

Copied from the retired cite-nav skill; record classification mistakes and
fixes in classify.md next to this script.

Usage:
    python3 classify_pdf.py --pdf <path> --task <task> [--bib-type <type>] [--hint <hint>]

Tasks:
    classify          - classify document, return CLASS only
    extract:abstract  - return range/strategy for abstract extraction
    extract:bib       - return range/strategy and field list for bib entry extraction

Output (stdout):
    CLASS:<name>           - document class inferred
    RANGE:<start>-<end>    - page range to read (extract tasks only)
    STRATEGY:<name>        - strategy used (extract tasks only)
    FIELDS:<f1,f2,...>     - bib fields to read off the pages (extract:bib only);
                             never abstract, which citation-add copies verbatim
    NOTEXT:<reason>        - pdftotext returned empty (scanned/encrypted)

Exit codes:
    0 - success
    2 - NOTEXT or classification failed
"""

import sys
import os
import re
import subprocess
import argparse

def pdfinfo(pdf_path):
    """Return dict of pdfinfo fields."""
    try:
        result = subprocess.run(['pdfinfo', pdf_path], capture_output=True, text=True, timeout=10)
        info = {}
        for line in result.stdout.splitlines():
            if ':' in line:
                k, _, v = line.partition(':')
                info[k.strip()] = v.strip()
        return info
    except Exception:
        return {}

def page_count(pdf_path):
    info = pdfinfo(pdf_path)
    try:
        return int(info.get('Pages', 0))
    except ValueError:
        return 0

def meta_signals(info):
    """
    Extract classification signals and bib fields from pdfinfo metadata.
    Returns (signals_dict, fields_dict).
      signals_dict: keys that classify() can use (arxiv, thesis, etc.)
      fields_dict:  bib fields found (title, author, keywords)
    """
    title = info.get('Title', '').strip()
    author = info.get('Author', '').strip()
    subject = info.get('Subject', '').strip()
    keywords = info.get('Keywords', '').strip()
    creator = info.get('Creator', '').strip()

    title_lower = title.lower()
    creator_lower = creator.lower()

    signals = {}
    if 'arxiv genpdf' in creator_lower or 'arxiv' in creator_lower:
        signals['arxiv'] = True
    if re.search(r'\b(phd|ph\.d|doctoral|master.s thesis|dissertation)\b', title_lower):
        signals['thesis'] = True
    if subject:
        signals['subject'] = subject.lower()

    fields = {}
    if title:
        fields['title'] = title
    if author:
        fields['author'] = author
    if keywords:
        fields['keywords'] = keywords

    return signals, fields

def extract_doi(pdf_path, info=None, text=None):
    """
    Extract DOI from pdfinfo metadata or p.1 text.
    Only matches DOIs that appear near a doi: anchor to avoid picking up
    reference list DOIs.
    Returns DOI string or None.
    """
    bare_doi = re.compile(r'\b10\.\d{4,9}/[^\s,;>"\])\}]+')
    # Anchored: doi: or doi.org/ prefix - these are the paper's own DOI
    anchored_doi = re.compile(
        r'(?:doi\.org/|doi:\s*|DOI:\s*)(10\.\d{4,9}/[^\s,;>"\])\}]+)',
        re.IGNORECASE
    )

    # Check pdfinfo fields first (always reliable)
    if info:
        for field in ('doi', 'DOI', 'Subject', 'Keywords'):
            val = info.get(field, '')
            m = bare_doi.search(val)
            if m:
                return m.group(0).rstrip('.')

    # Scan p.1 text - anchored only to avoid reference DOIs
    if text is None:
        text = extract_text(pdf_path, "1-1") or ""
    m = anchored_doi.search(text)
    if m:
        doi = m.group(1).rstrip('.')
        # Filter placeholder DOIs (e.g. ACM template 10.1145/nnnnnnn)
        if not re.search(r'[nN]{3,}', doi):
            return doi

    return None


def extract_text(pdf_path, pages="1-2"):
    try:
        start, end = pages.split('-')
        result = subprocess.run(
            ['pdftotext', '-f', start, '-l', end, pdf_path, '-'],
            capture_output=True, text=True, timeout=30
        )
        return result.stdout if result.returncode == 0 else None
    except Exception:
        return None

def classify(pdf_path, bib_type=None, hint=None, info=None):
    """
    Classify document into one of:
      journal-article, conference-paper, arxiv-preprint,
      book, book-chapter, thesis, tech-report, unknown

    Priority: bib-type arg > pdfinfo metadata signals > filename > p.1 text > page count
    """
    # Bib type mapping - resolve unambiguous types immediately
    if bib_type:
        t = bib_type.lower()
        if t == 'book':
            return 'book'
        if t in ('incollection', 'inbook', 'bookinbook'):
            return 'book-chapter'
        if t in ('mastersthesis', 'phdthesis'):
            return 'thesis'
        if t == 'techreport':
            return 'tech-report'
        # inproceedings, article, misc: refine below (may be arxiv-preprint)

    if info is None:
        info = pdfinfo(pdf_path)
    meta, _ = meta_signals(info)

    # pdfinfo metadata signals - cheap, before reading any page text
    if meta.get('arxiv'):
        return 'arxiv-preprint'
    if meta.get('thesis'):
        return 'thesis'

    pages = page_count(pdf_path) if not info else int(info.get('Pages', 0) or 0)
    text = extract_text(pdf_path, "1-1") or ""
    text_lower = text.lower()
    filename = os.path.basename(pdf_path).lower()

    # arXiv signals - check early, before all other heuristics.
    # arXiv papers mention university affiliations which falsely trigger thesis,
    # and "Published as a conference paper at ICLR" which triggers conference.
    if (re.search(r'arxiv:', text_lower)
            or re.search(r'\d{4}\.\d{4,5}v\d', filename)
            or 'arxiv' in (hint or '').lower()):
        return 'arxiv-preprint'

    # SSRN signals
    if (re.search(r'ssrn-\d+', filename)
            or re.search(r'ssrn|social science research network', text_lower)):
        return 'ssrn-preprint'

    # inproceedings early return - safe now that arXiv has been checked
    if bib_type and bib_type.lower() == 'inproceedings':
        return 'conference-paper'

    # Extracted chapters from this edited ebook carry its cover and editor list.
    if 'in their own words' in text_lower and re.search(r'\beditors\b', text_lower):
        return 'book-chapter'

    # Book signals - page count is the primary signal; text signals alone are
    # too noisy (ACM DL downloads embed "all rights reserved" in metadata)
    if pages > 150:
        return 'book'

    # Thesis signals - require strong explicit markers, not just substrings
    # ('master' alone matches 'masterclass', 'masterful', university dept names)
    if re.search(r'\b(phd|ph\.d\.?|doctoral|master\'?s thesis|dissertation)\b', text_lower):
        return 'thesis'

    # Tech report signals
    if re.search(r'technical report|cmu/sei|sei/cmu|nist |rand corporation', text_lower):
        return 'tech-report'

    # Conference vs journal
    if re.search(r'proceedings|conference|workshop|symposium|icse|esec|fse|ecsa|icsa', text_lower):
        return 'conference-paper'

    if re.search(r'journal|volume|issue|elsevier|springer|wiley|ieee trans|nature\.com|mdpi\.com|tandfonline|sage pub', text_lower):
        return 'journal-article'

    # Fallback by page count
    if pages <= 12:
        return 'conference-paper'
    if pages <= 30:
        return 'journal-article'

    return 'unknown'


def strategy_for(doc_class, task, pages):
    """
    Return (strategy_name, page_range_string) for a doc class + task.

    Strategies:
      full-read    - read the whole document
      front-read   - read first N pages (abstract always here)
      toc-first    - read TOC pages, then navigate
      term-narrow  - read second half of document
    """
    # Normalize task aliases
    if task == 'abstract':
        task = 'extract:abstract'

    if task == 'extract:abstract':
        if doc_class in ('conference-paper', 'arxiv-preprint', 'ssrn-preprint'):
            return 'front-read', '1-2'
        if doc_class == 'journal-article':
            return 'front-read', '1-3'
        if doc_class == 'tech-report':
            return 'front-read', '1-15'   # SEI reports abstract on p9-11
        if doc_class in ('book', 'thesis'):
            return 'toc-first', f'1-{min(15, pages)}'
        return 'front-read', '1-3'

    if task == 'extract:bib':
        # Bib extraction needs the same range as abstract plus p.1 for metadata
        # For short docs, full-read is cheap enough
        if pages <= 30:
            return 'full-read', f'1-{pages}'
        if doc_class in ('book', 'thesis'):
            return 'toc-first', f'1-{min(15, pages)}'
        return 'front-read', '1-5'

    return 'full-read', f'1-{min(pages, 20)}'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--pdf', required=True)
    parser.add_argument('--task', required=True)
    parser.add_argument('--bib-type', default=None)
    parser.add_argument('--hint', default=None)
    args = parser.parse_args()

    pdf_path = os.path.expanduser(args.pdf)
    if not os.path.exists(pdf_path):
        print(f"NOTEXT:file not found: {pdf_path}")
        sys.exit(2)

    # Quick text check
    text = extract_text(pdf_path, "1-1")
    if not text or len(text.strip()) < 20:
        print("NOTEXT:pdftotext returned empty (scanned or encrypted)")
        sys.exit(2)

    info = pdfinfo(pdf_path)
    pages = int(info.get('Pages', 0) or 0) or 1
    doc_class = classify(pdf_path, bib_type=args.bib_type, hint=args.hint, info=info)

    print(f"CLASS:{doc_class}")

    if args.task == 'classify':
        return

    strategy, page_range = strategy_for(doc_class, args.task, pages)
    print(f"RANGE:{page_range}")
    print(f"STRATEGY:{strategy}")

    if args.task == 'extract:bib':
        # Emit metadata fields found directly from pdfinfo
        _, meta_fields = meta_signals(info)
        for k, v in meta_fields.items():
            print(f"META:{k}={v}")
        # DOI - try pdfinfo then p.1 text
        doi = extract_doi(pdf_path, info=info)
        if doi:
            print(f"DOI:{doi}")
        # Fields still to read off the pages (those not in metadata or doi).
        # The abstract is not one of them: citation-add copies it verbatim from
        # CrossRef, arXiv or the PDF, or omits it.
        needed = [f for f in ('author', 'title', 'year', 'venue')
                  if f not in meta_fields]
        if not doi and doc_class not in ('book', 'thesis'):
            needed.append('doi')
        print(f"FIELDS:{','.join(needed)}")


if __name__ == '__main__':
    main()
