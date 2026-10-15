#!/usr/bin/env python3
"""
Deterministically extract abstracts from PDFs using pdftotext + heuristics.

Usage:
    python3 extract_abstract.py <pdf_path>

Output (to stdout):
    ABSTRACT:<text>        - abstract found verbatim
    NOABSTRACT:<reason>    - no abstract expected (book, magazine, etc.)
    FAIL:<reason>          - extraction failed, fall back to inference

Exit codes:
    0 - ABSTRACT found
    1 - NOABSTRACT (not an error, just not applicable)
    2 - FAIL (extraction failed, fall back to inference)

Patterns file:
    patterns.md - documents what patterns work and what failed, for self-healing.
    When a new pattern is added to this script, append a note to patterns.md.
"""

import sys
import os
import re
import subprocess

PDF_PATH = os.path.expanduser(sys.argv[1]) if len(sys.argv) > 1 else None

# --- Helpers ---

def extract_text(pdf_path, pages="1-2"):
    """Run pdftotext on given page range, return text or None."""
    try:
        result = subprocess.run(
            ["pdftotext", "-f", pages.split("-")[0], "-l", pages.split("-")[-1], pdf_path, "-"],
            capture_output=True, text=True, timeout=30
        )
        return result.stdout if result.returncode == 0 else None
    except Exception:
        return None


def normalize(text):
    """Collapse whitespace."""
    return re.sub(r'\s+', ' ', text).strip()


# --- Pattern library ---
# Each pattern is tried in order. First match wins.
# Returns (abstract_text, pattern_name) or None.

def try_patterns(text):
    patterns = [
        # Pattern 1: IEEE/ACM two-column format
        # "Abstract—" at start; terminates at Index Terms, Keywords, Introduction heading,
        # or a blank line (two-column layout where body text follows after blank line).
        # Note: pdftotext sometimes splits "INTRODUCTION" as "I NTRODUCTION" due to column layout.
        ("ieee_abstract_dash",
         r'Abstract[—\-–]\s*(.+?)(?=\n\n|\n\s*(?:Index Terms|Keywords|I\.\s+I.?NTRODUCTION|\d+\.\s+INTRODUCTION))',
         re.DOTALL | re.IGNORECASE),

        # Pattern 2: ICLR/NeurIPS spaced-cap heading "A BSTRACT"
        # pdftotext splits the heading due to column layout: "A BSTRACT\n<text>"
        # Terminates at "I NTRODUCTION" (same split artifact) or blank line.
        ("iclr_a_bstract",
         r'A BSTRACT\s*\n(.+?)(?=\n\n|\n\s*I\s+NTRODUCTION)',
         re.DOTALL),

        # Pattern 2b: MDPI style - "Abstract" heading followed by article-info sidebar,
        # then blank line, then actual abstract text.
        # e.g. "Abstract\n\nAcademic Editors: ...\nReceived: ...\n\n<actual abstract>"
        ("mdpi_abstract_after_info",
         r'Abstract\s*\n\n(?:Academic Editors?:.*?\n|Received:.*?\n|Revised:.*?\n|Accepted:.*?\n)+\n(.+?)(?=\n\n|\nKeywords?:)',
         re.DOTALL | re.IGNORECASE),

        # Pattern 3: "Abstract" as heading, body follows on next line(s)
        # Terminates at Keywords, Index Terms, numbered section, or end of text block.
        # Skip if immediately followed by article metadata (handled by mdpi_abstract_after_info).
        ("heading_abstract",
         r'(?<!\w)Abstract\s*\n\s*(.+?)(?=\n\s*(?:Keywords|Index Terms|1\s+Introduction|\d\.\s+[A-Z])|\n\n)',
         re.DOTALL | re.IGNORECASE),

        # Pattern 4: "ABSTRACT" all caps inline - text follows on same line
        # e.g. "ABSTRACT As software systems..." (ACM/IEEE single-column)
        # Terminates at CCS Concepts, Keywords, or double newline.
        ("caps_abstract_inline",
         r'\bABSTRACT\s+(.+?)(?=\n\s*(?:CCS Concepts|Keywords|Additional Key Words|\d+\s+INTRODUCTION)|\n\n)',
         re.DOTALL),

        # Pattern 4b: "Abstract" title-case inline - text follows on same line
        # e.g. "Abstract Mining Software Repositories..." (Springer/arXiv preprints)
        # Terminates at Keywords, double newline, numbered section, or author email block.
        ("abstract_titlecase_inline",
         r'\bAbstract\s+(.+?)(?=\nKeywords|\n\n|\n\d+\s+Introduction|\n\S+\s+·\s+\S+|\nE-mail:)',
         re.DOTALL),

        # Pattern 5: "ABSTRACT" all caps as heading, text on next line
        ("caps_abstract",
         r'\bABSTRACT\b\s*\n\s*(.+?)(?=\n\s*(?:KEYWORDS|KEY WORDS|INTRODUCTION|Keywords|1\s+\w)|\n\n)',
         re.DOTALL | re.IGNORECASE),

        # Pattern 6: "Abstract:" with colon
        ("abstract_colon",
         r'\bAbstract:\s*(.+?)(?=\n\s*(?:Keywords|Index Terms|1\.\s|\d\.\s+\w)|\n\n)',
         re.DOTALL | re.IGNORECASE),

        # Pattern 7: "Abstract." with period (ECSA/Springer LNCS format)
        # e.g. "Abstract. To establish good architectural practice..."
        # Terminates at Keywords: or next blank line after section ends.
        ("abstract_dot",
         r'\bAbstract\.\s+(.+?)(?=\n\s*(?:Keywords|Index Terms|1\s+Introduction)|\n\n)',
         re.DOTALL | re.IGNORECASE),

        # Pattern 8: "Abstract - " with space-dash (conference proceedings variant)
        # e.g. "Abstract - The maturation of agentic AI..."
        # Terminates at "Keywords - " or double newline.
        ("abstract_space_dash",
         r'\bAbstract\s+-\s+(.+?)(?=\nKeywords\s+-|\n\n)',
         re.DOTALL | re.IGNORECASE),

        # Pattern 9: Elsevier "A R T I C L E I N F O / A B S T R A C T" sidebar format
        # pdftotext renders as "A B S T R A C T\n\nKeywords:\n<kw>\n...\n\n<abstract text>"
        # The keywords sidebar appears first, then a blank line, then the abstract.
        ("elsevier_spaced_abstract",
         r'A B S T R A C T\s*\n\n(?:Keywords:[^\n]*\n(?:[^\n]+\n)*)\n(.+?)(?=\n\n|\Z)',
         re.DOTALL),

        # Pattern 9b: Elsevier sidebar where Keywords appear BEFORE "a b s t r a c t"
        # pdftotext renders article-info + keywords block, then "a b s t r a c t\n<text>"
        # e.g. "Keywords:\n...\n\na b s t r a c t\n<abstract text>\n\n1. Introduction"
        ("elsevier_spaced_abstract_kw_before",
         r'a b s t r a c t\s*\n(.+?)(?=\n\n|\n\s*\d+\.\s+Introduction|\Z)',
         re.DOTALL | re.IGNORECASE),

        # Pattern 10: ACM preprints with no explicit Abstract heading
        # Abstract text follows author affiliations and copyright boilerplate,
        # terminated by "CCS Concepts:" or "Additional Key Words".
        ("acm_no_heading_ccs",
         r'(?:permission[^\n]+\n.{0,500}?)\n\n(.+?)(?=\nCCS Concepts|\nAdditional Key Words)',
         re.DOTALL | re.IGNORECASE),

        # Pattern 10b: ACM/arXiv papers where abstract is inline before "CCS Concepts:"
        # with no explicit Abstract heading and no permission block anchor.
        # The abstract is the last paragraph-length block before "CCS Concepts:".
        # Strategy: find the last \n\n<text>\nCCS Concepts: where text >= 150 chars and
        # contains no \n\n (single paragraph).
        ("acm_ccs_inline",
         r'\n\n([^\n]{20}[^\n]*(?:\n[^\n]+){2,})\nCCS Concepts:',
         re.DOTALL),

        # Pattern 12: Nature Scientific Data / similar journals
        # Abstract is inline text directly after author line, terminated by "Background & Summary"
        # or next section heading. No "Abstract" keyword present.
        ("nature_background_summary",
         r'\n\n(.+?)\n\nBackground\s*&\s*Summary',
         re.DOTALL),

        # Pattern 11: arXiv style - abstract block bounded by blank lines after "Abstract"
        ("arxiv_blank_lines",
         r'\bAbstract\b\s*\n\n(.+?)\n\n',
         re.DOTALL | re.IGNORECASE),

    ]

    for name, pattern, flags in patterns:
        for m in re.finditer(pattern, text, flags):
            candidate = normalize(m.group(1))
            # Sanity checks: must be at least 150 chars, not more than 6000
            # (150 excludes titles and graphical abstract captions)
            if 150 <= len(candidate) <= 6000:
                return candidate, name

    return None


# --- Book/non-abstract detection ---

BOOK_SIGNALS = [
    r'all rights reserved',
    r'isbn[:\s]',
    r'library of congress',
    r'printed in',
    r'addison.?wesley',
    r"o'reilly",
    r'mit press',
    r'springer',
    r'draft copy',
    r'copyright.*\d{4}.*by',
    r'table of contents',
]

MAG_SIGNALS = [
    r'scientific american',
    r'ieee spectrum',
    r'communications of the acm',
    r'acm queue',
]


def is_book(text_lower):
    hits = sum(1 for sig in BOOK_SIGNALS if re.search(sig, text_lower))
    return hits >= 2


def is_magazine(text_lower):
    hits = sum(1 for sig in MAG_SIGNALS if re.search(sig, text_lower))
    return hits >= 1 and not re.search(r'\babstract\b', text_lower)


# --- Entry point ---

def main():
    if not PDF_PATH:
        print("FAIL:no pdf path given")
        sys.exit(2)

    if not os.path.exists(PDF_PATH):
        print(f"FAIL:file not found: {PDF_PATH}")
        sys.exit(2)

    # First pass: pages 1-2
    text = extract_text(PDF_PATH, "1-2")
    if not text or len(text.strip()) < 20:
        print("FAIL:pdftotext returned empty text")
        sys.exit(2)

    # Try patterns before book/magazine detection - patterns take priority
    result = try_patterns(text)
    if result:
        abstract, pattern_name = result
        print(f"ABSTRACT:{abstract}")
        print(f"PATTERN:{pattern_name}", file=sys.stderr)
        sys.exit(0)

    # Second pass: pages 1-15 (catches tech reports, PhD theses with late abstract pages)
    text_wide = extract_text(PDF_PATH, "1-15")
    if text_wide and len(text_wide.strip()) > len(text.strip()):
        result = try_patterns(text_wide)
        if result:
            abstract, pattern_name = result
            print(f"ABSTRACT:{abstract}")
            print(f"PATTERN:{pattern_name}(wide)", file=sys.stderr)
            sys.exit(0)

    # No patterns matched - check if this is a book or magazine (no abstract expected)
    text_lower = text.lower()
    if is_book(text_lower):
        print("NOABSTRACT:appears to be a book (no abstract expected)")
        sys.exit(1)

    if is_magazine(text_lower):
        print("NOABSTRACT:appears to be a magazine article (no formal abstract)")
        sys.exit(1)

    print("FAIL:abstract section not found by any pattern")
    sys.exit(2)


if __name__ == "__main__":
    main()
