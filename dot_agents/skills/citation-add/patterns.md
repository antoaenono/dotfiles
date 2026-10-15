# Abstract Extraction Patterns

This file documents which patterns work for which PDF types, and records failures.
When `extract_abstract.py` fails on a new PDF layout, diagnose the failure,
add a new pattern to `extract_abstract.py`, and append a record here.

---

## How to self-heal a FAIL

1. Run `pdftotext -f 1 -l 2 <pdf> -` and inspect the raw text around the abstract
2. Identify why no pattern matched:
   - Different delimiter? (e.g. "Abstract." instead of "Abstract—")
   - Abstract starts on page 3?
   - No "Abstract" keyword at all (structured differently)?
3. Add a new pattern to the `patterns` list in `extract_abstract.py`
   - Give it a descriptive name
   - Keep it before the fallback patterns
   - Test it: `python3 extract_abstract.py <pdf>` should print `ABSTRACT:...`
4. Append a record below: pattern name, what it fixes, example PDF

---

## Pattern records

### ieee_abstract_dash
- **Fixes**: IEEE/ACM two-column papers with "Abstract—" delimiter
- **Terminator**: Index Terms, Keywords, or "I. INTRODUCTION"
- **Added**: 2026-04-09
- **Tested on**: Detecting_Encryption_Vulnerabilities_..., Exploratory_Study_Architectural_Smell_..., Graph_Convolutional_Networks_...

### heading_abstract
- **Fixes**: Papers where "Abstract" is a standalone heading followed by body text
- **Terminator**: Keywords, numbered section heading
- **Added**: 2026-04-09

### caps_abstract
- **Fixes**: Papers using all-caps "ABSTRACT" as heading
- **Terminator**: KEYWORDS, INTRODUCTION
- **Added**: 2026-04-09

### abstract_colon
- **Fixes**: Papers using "Abstract:" with colon
- **Terminator**: Keywords, numbered section
- **Added**: 2026-04-09

### arxiv_blank_lines
- **Fixes**: arXiv preprints where abstract is a blank-line-delimited block
- **Terminator**: double newline after abstract body
- **Added**: 2026-04-09

---

## Known NOABSTRACT types

- **Books**: 2+ signals from: "all rights reserved", "ISBN:", "Library of Congress",
  "Printed in", publisher name, "Draft copy", "Copyright YYYY by"
- **Magazine articles**: Scientific American, IEEE Spectrum, ACM Queue, CACM magazine
  section - no "Abstract" keyword present

---

### iclr_a_bstract
- **Fixes**: ICLR/NeurIPS papers where pdftotext column-split renders "ABSTRACT" as "A BSTRACT"
- **Terminator**: `I NTRODUCTION` (same split artifact) or double newline
- **Added**: 2026-04-09
- **Tested on**: llm-internal-instruction-following-2410.14516v5.pdf, 2310.13548v4.pdf, ssrn-6372119.pdf

### mdpi_abstract_after_info
- **Fixes**: MDPI papers where "Abstract" heading is followed by article-info sidebar (Academic Editors, dates) before the actual abstract text
- **Terminator**: double newline or `Keywords:`
- **Added**: 2026-04-09
- **Tested on**: architecture-05-00094.pdf (yiannoudes2025genai)

### abstract_dot
- **Fixes**: ECSA/Springer LNCS papers using "Abstract." (period) delimiter
- **Terminator**: `Keywords:` or double newline
- **Added**: 2026-04-09
- **Tested on**: adr-in-practice-2024_ECSA.pdf (ahmeti2024adr)

### abstract_space_dash
- **Fixes**: Conference proceedings using "Abstract - " (space-dash) format
- **Terminator**: `Keywords - ` or double newline
- **Added**: 2026-04-09
- **Tested on**: AECTIC-118.pdf (konda2025agentic)

### caps_abstract_inline
- **Fixes**: Papers where "ABSTRACT" appears on same line as abstract text (ACM/IEEE single-column)
- **Terminator**: `CCS Concepts:`, `Keywords:`, or double newline
- **Added**: 2026-04-09
- **Tested on**: Explainability_in_Software_Architectural_Decisions_The_ADR-E_Framework_and_Empirical_Evaluation.pdf

### elsevier_spaced_abstract
- **Fixes**: Elsevier sidebar format rendering as "A B S T R A C T\n\nKeywords:\n<kw>\n...\n\n<abstract>"
- **Terminator**: double newline or end of string
- **Added**: 2026-04-09
- **Tested on**: 1-s2.0-S0090261625000749-main.pdf (shinkle2026scenario-analysis-ai)

### acm_no_heading_ccs
- **Fixes**: ACM preprints with no explicit Abstract heading; abstract precedes CCS Concepts block
- **Terminator**: `CCS Concepts:` or `Additional Key Words`
- **Added**: 2026-04-09
- **Note**: Requires copyright boilerplate (`permission`) before the abstract to anchor the pattern

### Wide page range fallback (pages 1-15)
- **Fixes**: SEI technical reports (ATAM series) and PhD theses where abstract appears on pages 9-11
- **Added**: 2026-04-09
- **Tested on**: 1998_005_001_16646.pdf (kazman1998architecture), 2000_005_001_13706.pdf, LakshithadeSilvaPhDThesis.pdf

### finditer instead of search
- **Fixes**: Documents where first Abstract match is a TOC entry or graphical abstract caption
- **How**: Try all matches for each pattern, return first with length >= 150 chars
- **Added**: 2026-04-09
- **Tested on**: 2000_005_001_13706.pdf (TOC Abstract at pos 2474 → real abstract at pos 5339)

---

## Known NOABSTRACT types (updated)

- **Books**: 2+ signals from: "all rights reserved", "ISBN:", "Library of Congress", "Printed in", publisher name, "Draft copy", "Copyright YYYY by", "Table of Contents"
- **Magazine articles**: Scientific American, IEEE Spectrum, ACM Queue, CACM magazine section
- **Note**: Book/magazine detection now runs AFTER pattern matching, not before. This prevents false positives when publisher names appear in article metadata (e.g., Emerald articles mentioning Springer in related-articles sections).

### elsevier_spaced_abstract_kw_before
- **Fixes**: Elsevier papers where Keywords appear BEFORE "a b s t r a c t" in the sidebar (vs. after in the original elsevier_spaced_abstract pattern)
- **Terminator**: double newline or `1. Introduction`
- **Added**: 2026-04-09
- **Tested on**: 1-s2.0-S0164121222002369-main.pdf (razavian2022odaks)

### acm_ccs_inline
- **Fixes**: ACM/arXiv preprints where abstract is inline before "CCS Concepts:" with no permission block anchor
- **Note**: May capture author affiliation lines before abstract; abstract paragraph is normalized in
- **Added**: 2026-04-09
- **Tested on**: llm-gen-design-rationale-2504.20781v3.pdf (zhou2025llm-design-rationale), 2404.04834v4.pdf (he2025llm-mas-swe)

### nature_background_summary
- **Fixes**: Nature Scientific Data / similar journals where abstract is inline text before "Background & Summary" with no Abstract heading
- **Note**: May capture sidebar/metadata text before the abstract paragraph
- **Added**: 2026-04-09
- **Tested on**: s41597-023-02202-2.pdf (rosenman2023race-ethnicity-names)

---

## Failure log

2026-04-09 | Graph_Convolutional_Networks_Mapping_Source_Code_Architectural_Modules.pdf | ieee_abstract_dash terminator regex `I\.\s+INTRODUCTION` didn't match pdftotext output `I. I NTRODUCTION` (column-layout word split) | Fixed: changed to `I\.\s+I.?NTRODUCTION` to tolerate the split

2026-04-09 | ai-math-research-assistant-2602.22842v1.pdf | ieee_abstract_dash didn't terminate - no Index Terms/Keywords/Introduction heading; abstract ends with blank line before body text | Fixed: added `\n\n` as additional terminator to ieee_abstract_dash

2026-04-09 | adr-drafting-llm-2504.08207v1.pdf | heading_abstract matched graphical abstract caption (50 chars exactly) instead of real abstract | Fixed: increased minimum sanity-check length from 50 to 150 chars

2026-04-09 | grey-systems-Abriefintroduction.pdf | book detection triggered (Emerald article mentions "Springer" in related-articles section) before pattern matching | Fixed: moved book/magazine detection after pattern matching

2026-04-09 | architecture-05-00094.pdf (MDPI) | heading_abstract captured article-info sidebar (Academic Editors, dates) instead of abstract text | Fixed: added mdpi_abstract_after_info pattern before heading_abstract

2026-04-09 | 2001_004_001_13808.pdf | Font-encoded TOC - heading "Abstract" renders as garbled `$EVWUDFW\x03` in pdftotext | Status: genuine FAIL - requires visual read fallback

2026-04-09 | parnas1994software-aging.pdf | pdftotext returns empty text (encrypted/image-only PDF) | Status: genuine FAIL - requires visual read fallback

2026-04-10 | 2212.13179v5.pdf | Springer two-column preprint where author affiliations are interleaved into the abstract block by pdftotext linearization; heading_abstract(wide) matched a TOC "Study title, abstract, keywords" entry instead | Status: genuine FAIL (layout artifact) - abstract extracted manually by filtering affiliation lines from raw text

### abstract_titlecase_inline
- **Fixes**: Springer/arXiv preprints where "Abstract" is title-case and immediately followed by abstract text on the same line (no newline between heading and body)
- **Terminator**: `\nKeywords`, double newline, `\n\d+ Introduction`, or author email block (`\nE-mail:`, `\n\S+ · \S+`)
- **Added**: 2026-04-10
- **Tested on**: 2212.13179v5.pdf (musengamana2024mining) - note: still fails due to interleaved affiliations; visual fallback used for that PDF

---

## Where the abstract is, by document class

Page ranges `classify_pdf.py --task extract:abstract` returns, and what was
found in them. Carried over from the retired cite-nav skill.

#### conference-paper
- Range 1-2 is sufficient for all IEEE/ACM/ECSA conference papers tested (8 entries, all PAGE:1)
- Abstract always on p.1 regardless of delimiter style (Abstract—, Abstract., Abstract -, ABSTRACT inline)
- Notable: ECSA/Springer LNCS uses "Abstract." (period); IEEE two-column splits "INTRODUCTION" as "I NTRODUCTION"

Entries: ivers2025genai-architecture, weidinger2022taxonomy, reiche2025detecting, pandini2025exploratory,
jabeen2025graph, ahmeti2024adr, konda2025agentic, gacitua2026adr

#### journal-article
- Range 1-3 is sufficient for all Elsevier/MDPI/Nature journal articles tested (5 entries, all PAGE:1)
- Elsevier sidebar format renders keywords before or after "a b s t r a c t" - both handled by patterns
- MDPI inserts article-info sidebar (Academic Editors, dates) between "Abstract" heading and text
- Nature Scientific Data has no "Abstract" heading; uses "Background & Summary" instead

Entries: babaei2025nasa-tlx, razavian2022odaks, shinkle2026scenario-analysis-ai,
yiannoudes2025genai, rosenman2023race-ethnicity-names

#### arxiv-preprint
- Range 1-2 is sufficient for all arXiv papers tested (5 entries, all PAGE:1)
- ICLR/NeurIPS column layout splits "ABSTRACT" as "A BSTRACT" in pdftotext - handled by iclr_a_bstract pattern
- ACM/arXiv preprints: abstract inline before "CCS Concepts:" with no explicit heading

Entries: batista2026sycophantic-ai, sharma2024sycophancy, heo2025llm-instruction-following,
zhou2025llm-design-rationale, he2025llm-mas-swe

#### ssrn-preprint
- Range 1-2 sufficient; abstract on p.1 in all cases tested

Entries: shaw2026cognitive-surrender, kim2026harness-engineering

#### tech-report
- Range 1-15 required; SEI/CMU reports have front matter (cover, ToC, executive summary) before abstract
- Abstract appears on pp.9-11 for ATAM series reports
- First "Abstract" match in pdftotext may be a TOC entry - finditer needed to skip short matches

Entries: kazman1998architecture (PAGE:9), kazman2000atam (PAGE:9)

#### thesis
- Range 1-15 sufficient via toc-first strategy; abstract typically pp.3-5

Entries: desilva2012phd-arch-erosion (PAGE:3)

#### book
- NOTEXT for all books tested (scanned/encrypted); extract:abstract not applicable to books anyway

---

## Known page-range failures

#### Springer two-column preprint with interleaved affiliations
- **Trigger**: `2212.13179v5.pdf` - author affiliations interleaved into abstract block by pdftotext linearization;
  heading_abstract(wide) matched a TOC "Study title, abstract, keywords" entry instead
- **Status**: genuine layout artifact; abstract_titlecase_inline pattern added but still fails for this PDF;
  visual fallback required
- **Date**: 2026-04-10
