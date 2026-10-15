#!/usr/bin/env python3
"""
Find the bounding box of a quote on a PDF page and write a highlight annotation.

Usage:
    python3 bbox.py --pdf <path> --page <N> --quote "<text>" [--color green]

Output (on success, to stdout):
    NOTER_PAGE: (N x1 . y1)
    HIGHLIGHT: #s(pdf-highlight N ((y1 x1 y2 x2)))

Also writes a highlight annotation directly into the PDF at the matched word spans.
Color is an RGB tuple name: green (citation-hunt), yellow (default manual).

On MATCH_FAILED: exits with code 1 and prints reason to stderr.

Extraction fallback ladder:
  1. pdftotext -bbox-layout  -> precise word bboxes via XHTML
  2. XML sanitize + retry     -> handles control chars / unescaped & in text layer
  3. pypdf visitor API        -> char-level positions, zero extra deps
  4. pdfminer.six             -> best encoding coverage, handles more PDF variants
  5. pdftoppm visual render   -> page rendered to PNG, coords estimated from image read
  6. MATCH_FAILED             -> all extraction methods exhausted
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET

COLORS = {
    'green':  (0, 1, 0),
    'yellow': (1, 1, 0),
    'cyan':   (0, 1, 1),
}


# ---------------------------------------------------------------------------
# Normalization (shared by all tiers)
# ---------------------------------------------------------------------------

def normalize(s):
    s = re.sub(r'-', ' ', s.lower())
    return re.sub(r'[^\w\s]', '', s).split()


# ---------------------------------------------------------------------------
# Span matching (shared by all tiers)
# words: list of (text, x1, y1_top, x2, y2_top) in top-left origin coords
# Returns matched word sublist or raises ValueError.
# ---------------------------------------------------------------------------

def match_span(words, quote):
    q_words = normalize(quote)
    if len(q_words) < 4:
        raise ValueError("Quote too short for reliable matching (< 4 words)")

    anchor_start = q_words[:4]
    anchor_end   = q_words[-4:]

    flat = []  # (norm_word, word_idx)
    for wi, w in enumerate(words):
        for nw in normalize(w[0]):
            flat.append((nw, wi))

    if len(flat) < 8:
        raise ValueError("Too few words on page for matching")

    start_flat = None
    for i in range(len(flat) - 4):
        span = [flat[i + j][0] for j in range(4)]
        if sum(a == b for a, b in zip(span, anchor_start)) >= 3:
            start_flat = i
            break
    if start_flat is None:
        raise ValueError("Start anchor not found")

    end_flat = None
    search_limit = min(start_flat + len(q_words) + 80, len(flat) - 4)
    for i in range(start_flat, search_limit):
        span = [flat[i + j][0] for j in range(4)]
        if sum(a == b for a, b in zip(span, anchor_end)) >= 3:
            end_flat = i + 3
            break
    if end_flat is None:
        raise ValueError("End anchor not found")

    start_wi = flat[start_flat][1]
    end_wi   = flat[end_flat][1]
    return words[start_wi:end_wi + 1]


# ---------------------------------------------------------------------------
# Tier 1: pdftotext -bbox-layout
# ---------------------------------------------------------------------------

def _sanitize_xml(raw):
    """Strip control characters and fix bare ampersands that break XML parsing."""
    cleaned = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]', '', raw)
    cleaned = re.sub(r'&(?!(?:[a-zA-Z][a-zA-Z0-9]*|#[0-9]+|#x[0-9a-fA-F]+);)', '&amp;', cleaned)
    return cleaned


def tier1_pdftotext(pdf_path, page):
    """Returns (words, W, H) with top-left origin coords, or raises RuntimeError."""
    result = subprocess.run(
        ['pdftotext', '-f', str(page), '-l', str(page), '-bbox-layout', pdf_path, '-'],
        capture_output=True, text=True
    )
    if result.returncode != 0 and not result.stdout.strip():
        raise RuntimeError(f"pdftotext failed: {result.stderr.strip()}")

    raw = result.stdout

    # Try parse as-is, then with sanitization
    root = None
    for attempt, xml_str in enumerate([raw, _sanitize_xml(raw)], 1):
        try:
            root = ET.fromstring(xml_str)
            break
        except ET.ParseError as e:
            if attempt == 2:
                raise RuntimeError(f"XML parse failed after sanitization: {e}")

    NS = 'http://www.w3.org/1999/xhtml'
    page_el = root.find(f'.//{{{NS}}}page')
    if page_el is None:
        raise RuntimeError("No <page> element in pdftotext bbox output")

    W = float(page_el.get('width'))
    H = float(page_el.get('height'))
    words = [
        (w.text or '', float(w.get('xMin')), float(w.get('yMin')),
         float(w.get('xMax')), float(w.get('yMax')))
        for w in root.iter(f'{{{NS}}}word')
    ]
    if not words:
        raise RuntimeError("pdftotext returned zero words")
    return words, W, H


# ---------------------------------------------------------------------------
# Tier 3: pypdf visitor API
# coords from pypdf are PDF-native (bottom-left origin); we flip to top-left.
# ---------------------------------------------------------------------------

def tier3_pypdf(pdf_path, page):
    """Returns (words, W, H) with top-left origin coords, or raises RuntimeError."""
    from pypdf import PdfReader

    reader = PdfReader(pdf_path)
    if page < 1 or page > len(reader.pages):
        raise RuntimeError(f"Page {page} out of range (doc has {len(reader.pages)} pages)")

    page_obj = reader.pages[page - 1]
    mb = page_obj.mediabox
    W = float(mb.width)
    H = float(mb.height)

    # Visitor yields (text, cm, tm, fontname, fontsize)
    # tm[4]=x, tm[5]=y in PDF coords (bottom-left origin)
    chunks = []  # (text, x_bl, y_bl, fontsize)

    def visitor(text, cm, tm, font, size):
        if text and text.strip():
            x, y = float(tm[4]), float(tm[5])
            sz = float(size) if size else 10.0
            chunks.append((text, x, y, sz))

    page_obj.extract_text(visitor_text=visitor)

    if not chunks:
        raise RuntimeError("pypdf visitor yielded no text chunks")

    # Split chunks into individual words; estimate bbox from font size
    words = []
    for (text, x_bl, y_bl, sz) in chunks:
        # Split on whitespace, estimate each word's x offset
        # pypdf gives us the start-of-chunk x; advance crudely by char_width ~ 0.5*sz
        char_w = sz * 0.5
        token_x = x_bl
        for token in re.split(r'(\s+)', text):
            if not token.strip():
                token_x += len(token) * char_w * 0.4
                continue
            w_width = len(token) * char_w
            x1 = token_x
            x2 = token_x + w_width
            # PDF y_bl is baseline (bottom-left); top of glyph ~ y_bl + sz*0.8
            y_top_pdf = y_bl + sz * 0.8
            y_bot_pdf = y_bl
            # Flip to top-left origin for match_span / write_highlight consistency
            y1_top = H - y_top_pdf
            y2_top = H - y_bot_pdf
            words.append((token, x1, y1_top, x2, y2_top))
            token_x += w_width

    if not words:
        raise RuntimeError("pypdf visitor: no word tokens after splitting")
    return words, W, H


# ---------------------------------------------------------------------------
# Tier 4: pdfminer.six
# ---------------------------------------------------------------------------

def tier4_pdfminer(pdf_path, page):
    """Returns (words, W, H) with top-left origin coords, or raises RuntimeError."""
    from pdfminer.high_level import extract_pages
    from pdfminer.layout import LTTextBox, LTTextLine, LTChar, LTAnno, LTPage

    words = []
    W, H = None, None

    for page_num, page_layout in enumerate(extract_pages(pdf_path), 1):
        if page_num != page:
            continue
        W = page_layout.width
        H = page_layout.height

        for element in page_layout:
            if not isinstance(element, LTTextBox):
                continue
            for line in element:
                if not isinstance(line, LTTextLine):
                    continue
                # Accumulate characters into word tokens
                current_word = []
                current_chars = []  # (x0, y0, x1, y1) per char in pdfminer coords (bottom-left)

                def flush_word():
                    if not current_word:
                        return
                    text = ''.join(current_word)
                    if text.strip():
                        x1 = min(c[0] for c in current_chars)
                        y1_pdf = min(c[1] for c in current_chars)
                        x2 = max(c[2] for c in current_chars)
                        y2_pdf = max(c[3] for c in current_chars)
                        # pdfminer uses bottom-left origin; flip to top-left
                        y1_top = H - y2_pdf
                        y2_top = H - y1_pdf
                        words.append((text, x1, y1_top, x2, y2_top))
                    current_word.clear()
                    current_chars.clear()

                for char in line:
                    if isinstance(char, LTChar):
                        if char.get_text() in (' ', '\t', '\n'):
                            flush_word()
                        else:
                            current_word.append(char.get_text())
                            current_chars.append((char.x0, char.y0, char.x1, char.y1))
                    elif isinstance(char, LTAnno):
                        if char.get_text() in (' ', '\t', '\n'):
                            flush_word()
                flush_word()
        break

    if W is None:
        raise RuntimeError(f"pdfminer: page {page} not found in document")
    if not words:
        raise RuntimeError("pdfminer: no words extracted")
    return words, W, H


# ---------------------------------------------------------------------------
# Tier 5: visual render via pdftoppm
# Renders page to PNG, reads it with the Read tool (caller handles this).
# Returns estimated coords from visual inspection - we do our best here.
# Since this runs as a script, we emit a special VISUAL_FALLBACK line and
# let the caller (citation-hunt skill) handle the image read step.
# ---------------------------------------------------------------------------

def tier5_visual_render(pdf_path, page):
    """
    Render page to PNG and print VISUAL_FALLBACK with the image path.
    The caller (citation-hunt) reads the image, visually locates the passage,
    and estimates fractional coords manually.
    Raises RuntimeError if pdftoppm is unavailable.
    """
    pdftoppm = subprocess.run(['which', 'pdftoppm'], capture_output=True, text=True)
    if pdftoppm.returncode != 0:
        raise RuntimeError("pdftoppm not available for visual fallback")

    tmp_dir = tempfile.mkdtemp(prefix='bbox_visual_')
    out_prefix = os.path.join(tmp_dir, 'page')
    result = subprocess.run(
        ['pdftoppm', '-f', str(page), '-l', str(page), '-r', '150', '-png', pdf_path, out_prefix],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        raise RuntimeError(f"pdftoppm failed: {result.stderr.strip()}")

    # Find the generated PNG
    pngs = sorted(f for f in os.listdir(tmp_dir) if f.endswith('.png'))
    if not pngs:
        raise RuntimeError("pdftoppm produced no PNG output")

    img_path = os.path.join(tmp_dir, pngs[0])
    # Signal to caller: they should read this image and estimate coords
    print(f"VISUAL_FALLBACK: {img_path}", file=sys.stderr)
    raise RuntimeError(f"VISUAL_FALLBACK:{img_path}")


# ---------------------------------------------------------------------------
# Write highlight (unchanged from original; coords are top-left origin)
# ---------------------------------------------------------------------------

def write_highlight(pdf_path, page, word_span, W, H, color_rgb):
    """Write highlight annotation into PDF using per-word QuadPoints."""
    from pypdf import PdfReader, PdfWriter
    from pypdf.generic import (
        ArrayObject, DictionaryObject, FloatObject, NameObject,
        NumberObject, ByteStringObject,
    )
    import random, string

    reader = PdfReader(pdf_path)
    writer = PdfWriter()
    writer.append(reader)

    page_obj = writer.pages[page - 1]
    media_box = page_obj.mediabox
    page_h = float(media_box.height)

    quad_points = []
    all_x1, all_y1_pdf, all_x2, all_y2_pdf = [], [], [], []

    for (_, x1, y1_top, x2, y2_top) in word_span:
        # Flip from top-left to PDF bottom-left
        y1_pdf = page_h - y2_top
        y2_pdf = page_h - y1_top
        quad_points += [x1, y1_pdf, x2, y1_pdf, x1, y2_pdf, x2, y2_pdf]
        all_x1.append(x1); all_x2.append(x2)
        all_y1_pdf.append(y1_pdf); all_y2_pdf.append(y2_pdf)

    rect = [min(all_x1), min(all_y1_pdf), max(all_x2), max(all_y2_pdf)]

    r, g, b = color_rgb
    annot = DictionaryObject({
        NameObject('/Type'):       NameObject('/Annot'),
        NameObject('/Subtype'):    NameObject('/Highlight'),
        NameObject('/Rect'):       ArrayObject([FloatObject(v) for v in rect]),
        NameObject('/QuadPoints'): ArrayObject([FloatObject(v) for v in quad_points]),
        NameObject('/C'):          ArrayObject([FloatObject(r), FloatObject(g), FloatObject(b)]),
        NameObject('/CA'):         FloatObject(0.5),
        NameObject('/F'):          NumberObject(4),
        NameObject('/NM'):         ByteStringObject(
            ''.join(random.choices(string.ascii_letters + string.digits, k=8)).encode()
        ),
    })

    annot_ref = writer._add_object(annot)
    if '/Annots' in page_obj:
        page_obj['/Annots'].append(annot_ref)
    else:
        page_obj[NameObject('/Annots')] = ArrayObject([annot_ref])

    with open(pdf_path, 'wb') as f:
        writer.write(f)


# ---------------------------------------------------------------------------
# Main: run the fallback ladder
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--pdf',   required=True)
    parser.add_argument('--page',  type=int, required=True)
    parser.add_argument('--quote', required=True)
    parser.add_argument('--color', default='green', choices=COLORS.keys())
    args = parser.parse_args()

    pdf_path = os.path.expanduser(args.pdf)

    tiers = [
        ('pdftotext+xml',  tier1_pdftotext),
        ('pypdf-visitor',  tier3_pypdf),
        ('pdfminer',       tier4_pdfminer),
    ]

    words, W, H = None, None, None
    tier_used = None
    last_errors = []

    for name, fn in tiers:
        try:
            words, W, H = fn(pdf_path, args.page)
            tier_used = name
            print(f"TIER: {name}", file=sys.stderr)
            break
        except Exception as e:
            last_errors.append(f"{name}: {e}")
            print(f"TIER_FAIL: {name}: {e}", file=sys.stderr)

    # Tier 5: visual render - only reached if all text-layer tiers failed
    if words is None:
        try:
            tier5_visual_render(pdf_path, args.page)
        except RuntimeError as e:
            err = str(e)
            if err.startswith('VISUAL_FALLBACK:'):
                img_path = err.split(':', 1)[1]
                # Emit the special signal - citation-hunt will read the image
                # and estimate coords visually, then call write_highlight directly
                print(f"VISUAL_FALLBACK: {img_path}")
                sys.exit(2)  # exit code 2 = visual fallback needed
            last_errors.append(f"visual: {e}")

        print("MATCH_FAILED: all extraction tiers exhausted", file=sys.stderr)
        for e in last_errors:
            print(f"  {e}", file=sys.stderr)
        sys.exit(1)

    # Match the quote
    try:
        span = match_span(words, args.quote)
    except ValueError as e:
        print(f"MATCH_FAILED: {e}", file=sys.stderr)
        print(f"TIER_USED: {tier_used}", file=sys.stderr)
        sys.exit(1)

    # Fractional coords (top-left origin for org-noter)
    x1 = min(w[1] for w in span) / W
    y1 = min(w[2] for w in span) / H
    x2 = max(w[3] for w in span) / W
    y2 = max(w[4] for w in span) / H

    print(f"NOTER_PAGE: ({args.page} {x1:.10f} . {y1:.10f})")
    print(f"HIGHLIGHT: #s(pdf-highlight {args.page} (({y1:.10f} {x1:.10f} {y2:.10f} {x2:.10f})))")

    write_highlight(pdf_path, args.page, span, W, H, COLORS[args.color])
    print(f"WROTE: highlight on page {args.page} of {pdf_path}", file=sys.stderr)


if __name__ == '__main__':
    main()
