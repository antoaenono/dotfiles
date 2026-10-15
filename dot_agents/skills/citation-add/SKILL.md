---
name: citation-add
description: Add new PDFs in ~/org/pdfs/ to references.bib. Cleans filenames, classifies, hits CrossRef for authoritative metadata, checks for duplicates, writes a validated bib entry, renames the PDF to the citekey, and runs biber. Zero verification debt - every entry is correct on arrival.
user-invocable: true
allowed-tools: Bash, Read, Edit
---

# citation-add

Adds every untracked PDF in `~/org/pdfs/` to `~/org/bib/references.bib`.
Process one PDF completely before moving to the next.
Entry quality is baked in at add time - no separate verify pass needed.

---

## Step 1: Clean and inventory

Strip Anna's Archive suffixes:

```bash
python3 ~/.agents/skills/citation-add/clean_filenames.py --apply
```

Find untracked PDFs:

```bash
python3 ~/.agents/skills/citation-add/find_missing.py
```

- `MISSING:<csv>` - filenames to process (stop here if empty)
- `CITEKEYS:<csv>` - existing citekeys (keep for collision checks)

---

## Step 2: For each missing PDF - one at a time

### 2a. Classify and extract

```bash
python3 ~/.agents/skills/citation-add/classify_pdf.py \
  --pdf ~/org/pdfs/<filename> \
  --task extract:bib
```

Parse output:
- `CLASS:<class>` - suggested document class; treat as a hint only. Use content
  and venue to determine the correct BibTeX entry type (`@article`, `@inproceedings`,
  `@incollection`, `@book`, `@phdthesis`, etc.) - CLASS can be wrong. If it is,
  fix the rule in `classify_pdf.py` and note the case in `classify.md`.
- `DOI:<doi>` - go to Step 3a if present
- `META:title=...` / `META:author=...` - PDF metadata; use it when it is a real
  value (it is sometimes a file name like `main-all.dvi`)
- `FIELDS:<csv>` - fields still needed from page content
- `RANGE:<range>` - pages to read for fallback
- `NOTEXT:...` - scanned/encrypted; do NOT skip. Instead:
  1. Check the filename for embedded metadata (Anna's Archive names include author, title, year, publisher, ISBN)
  2. Use world knowledge for well-known works (classic books, famous papers)
  3. Only escalate to the user if neither source yields enough to build a complete entry

### 2b. Duplicate check

Once DOI or title is known, run:

```bash
python3 ~/.agents/skills/citation-add/check_duplicate.py \
  --bib ~/org/bib/references.bib \
  --doi "<doi-or-empty>" \
  --title "<title-or-empty>" \
  --author "<author-or-empty>" \
  --year "<year-or-empty>" \
  --new-pdf "<filename>"
```

Interpret the output token:

- `NO_MATCH` - no duplicate found, proceed to Step 3
- `KEEP_EXISTING: citekey=... existing_file=... reason=...`
  - Delete the new PDF silently, log the decision in the Step 8 report
  - Do NOT add a new bib entry
- `KEEP_NEW: citekey=... existing_file=... reason=...`
  - Update the existing bib entry's `file` field to point to the new PDF
  - Delete the old PDF file, log the decision
  - Do NOT add a new bib entry
- `INFER: citekey=... existing_file=... new_pdf=... hint=...`
  - Read pages 1-2 of both PDFs using nav.py or Read
  - Reason about whether they are the same work, using the `hint` as guidance
  - Resolve as KEEP_EXISTING, KEEP_NEW, or escalate to ASK if still unclear
  - If resolved, log the decision; if escalating, show the user the evidence
- `ASK: citekey=... existing_file=... message=...`
  - Show the user the message and wait for their choice before proceeding

**Never silently overwrite a `file` field** without going through this flow.

---

## Step 3a: DOI path (preferred)

```bash
python3 ~/.agents/skills/citation-add/fetch_doi.py <doi>
```

Parse `FIELD:key=value` lines. Also watch for:
- `SPARSE:...` - CrossRef returned fewer than 4 substantive fields; read PDF pages
  to fill gaps before building the entry.

Then:

**CrossRef year is authoritative.** Use it for both the `year` field and the
citekey year. If CrossRef returns `published-print` and `published-online` in
different years, use `published-print`. If only `published-online` is present,
use that.

Check citekey year matches CrossRef year before writing. If they differ, use
CrossRef year in both the citekey and the `year` field.

Format following `~/.agents/skills/citation-add/examples.md`:
- `journal` for articles (not `journaltitle`)
- `publisher` in `note = {Publisher: X}` for `@article`
- `publisher = {IEEE}` / `publisher = {ACM}` valid for `@inproceedings`
- `abstract`: follow **Abstract** below

Add `file = {~/org/pdfs/<filename>}` (temporary - will be updated after rename).

---

## Step 3b: Fallback path (no DOI)

1. Read the `RANGE:` pages from the PDF
2. Use `META:` fields as starting point; fill `FIELDS:` from page content
3. Get the `abstract` as described in **Abstract** below, never by inference
4. Read `~/.agents/skills/citation-add/examples.md` for formatting conventions
5. Build entry manually

**Citekey convention:** `authoryear-slug`
- `year` = publication year (not submission/acceptance year)
- `slug` = 1-4 hyphenated words that answer "what would I search for?"
- Drop filler words: "towards", "combining", "using", "a", "an", "the", "on"
- Example: "Towards Responsible Software Engineering: combining value-based
  processes..." → `kapferer2024responsible-se`
- Check `CITEKEYS` for collisions; append `b`, `c` if needed

---

## Abstract (both paths)

The site shows `abstract` under an "Abstract" label as the paper's own text, so
it is either the work's own abstract, verbatim and complete, or omitted. Never
write it yourself: no paraphrase, summary, condensing, or truncation, and never
end it with "...". Summaries go in `annote` (Step 5).

Take the first source that has it:

1. CrossRef: the `FIELD:abstract=` line from `fetch_doi.py`.
2. arXiv, for `10.48550/arXiv.*` DOIs: the `<summary>` of
   `https://export.arxiv.org/api/query?id_list=<arxiv-id>`.
3. The PDF:

   ```bash
   python3 ~/.agents/skills/citation-add/extract_abstract.py ~/org/pdfs/<filename>
   ```

   - `ABSTRACT:<text>` - use it, after the check below
   - `NOABSTRACT:<reason>` - omit the field
   - `FAIL:<reason>` - get the pages to read from
     `classify_pdf.py --pdf <pdf> --task extract:abstract` (`patterns.md` notes
     where abstracts sit by document type) and copy the abstract exactly as
     printed. If a new pattern would have caught it, add the pattern to
     `extract_abstract.py` and record it in `patterns.md`.

Before writing it, check the text against the PDF page:

- It is the whole abstract. Nothing between it and the next heading
  (Keywords, Index Terms, CCS Concepts, 1 Introduction) is missing.
- No text-layer damage: dropped ligatures (`dene` for `define`), run-together
  or split words, lost line-end hyphens, OCR errors. Fix them to match the page.
- No publisher boilerplate: drop trailing copyright lines
  (`© 2015 Elsevier B.V. All rights reserved.`), received/accepted dates,
  funding or supplemental-material notes, and keyword lists.

Omit `abstract` when the work has none: books, theses whose only summary runs
over several pages, and columns, essays or magazine articles without one. For a
magazine article with no printed abstract, the publisher's registered abstract
(CrossRef, IEEE Xplore) may be used if it reads coherently; otherwise omit it.

---

## Step 4: Write entry and rename PDF

Append the completed entry to the bib.

Rename the PDF to `{citekey}.pdf`:

```python
# Always use Python for renames - shell mv fails on non-ASCII and special chars
import os
pdfs = os.path.expanduser('~/org/pdfs')
files = os.listdir(pdfs)
src = [f for f in files if f == '<original-filename>'][0]
os.rename(os.path.join(pdfs, src), os.path.join(pdfs, '<citekey>.pdf'))
```

Update the `file` field in the just-written entry to `{~/org/pdfs/<citekey>.pdf}`.

---

## Step 5: Annote

Read the abstract (from the entry, or the PDF's opening pages when the work has none).
Write a 2-3 sentence `annote`: plain-language summary written for your own future
reference. The `annote` is the only place for a summary; the site shows it as
"Brief", separate from the verbatim "Abstract".

A good annote answers: **why would I cite this?** It names the specific finding,
claim, or contribution - not just the topic. Prefer concrete over vague:

- Bad: "Discusses the role of automation in decision-making."
- Good: "Shows that automation bias persists even in expert users and cannot be
  eliminated by training - only by redesigning the automation itself."

If the paper is a foundational reference (Simon 1956, Bainbridge 1983), note what
it established and why it remains cited. If it's empirical, name the key result.
If it's a framework or model, say what it lets you do.

Append `annote = {<text>}` to the entry before validating.

---

## Step 6: Audit the abstract

Skip this step if the entry has no `abstract`. Otherwise check it against the
work's own text:

```bash
python3 ~/.agents/skills/citation-add/audit_abstracts.py fetch ~/org/bib/references.bib --keys <citekey>
python3 ~/.agents/skills/citation-add/audit_abstracts.py score ~/org/bib/references.bib --keys <citekey>
```

Act on the result line for the citekey:

- `VERBATIM` - done.
- `REPLACE` - the abstract is not the work's text or is incomplete, and the
  line names a verified source. Write that source's text, then compare it with
  the page: text taken from a PDF (`-> pdf_cut`) can still carry text-layer
  damage such as a mis-scanned quote, so apply the checks under **Abstract**:

  ```bash
  python3 ~/.agents/skills/citation-add/audit_abstracts.py apply ~/org/bib/references.bib --keys <citekey>
  ```

- `DELETE` - the script found no abstract for the work, and the bib text is not
  in the PDF. Check the first page; if the work really has none, run `apply` as
  above to remove the field.
- `REVIEW` - the script cannot settle it: a scanned or column-scrambled PDF, an
  abstract with no heading after it, or sources that disagree. Run
  `audit_abstracts.py show ~/org/bib/references.bib <citekey>` to see the bib
  text next to every source. Compare with the PDF page itself (if the text
  layer is damaged, render it with `pdftoppm -r 110 -f 1 -l 1 -png <pdf> <dir>/page`
  and read the image). Make the bib text match the page exactly, or remove the
  field, then record the check:

  ```bash
  python3 ~/.agents/skills/citation-add/audit_abstracts.py reviewed ~/org/bib/references.bib <citekey> "<what you checked>"
  ```

The audit is a guard, not an oracle. It reliably catches paraphrases, condensed
copies and missing sentences, and it says REVIEW rather than guessing when its
sources are weak. It sends no personal data: a generic User-Agent and no email.

---

## Step 7: Validate after each entry

Run biber after every single entry is added - catch errors immediately, not in bulk:

```bash
biber --tool --validate-datamodel ~/org/bib/references.bib 2>&1 | grep -E "WARN|ERROR"
rm -f ~/org/bib/references_bibertool.bib ~/org/bib/references.bib.blg
```

Fix all ERRORs before moving to the next PDF.
Fix WARNs if possible; note if not.

Target: 0 errors, 0 warnings.

---

## Step 8: Report

After all PDFs are processed:

- Each added citekey: DOI path vs fallback, CrossRef year correction if any
- Abstract source per entry (CrossRef, arXiv, extractor, page read) or why it was omitted
- Audit result per entry, and what was done for REPLACE, DELETE and REVIEW
- Duplicates found and how resolved
- PDFs skipped (NOTEXT, confirmed duplicates)
- Final biber validation status

---

## Re-auditing the whole bib

To check every existing abstract (for example after editing entries by hand),
run `fetch` and `score` without `--keys`. The first full fetch takes a few
minutes; later runs reuse the cache in `~/.cache/citation-add/abstract-audit/`.
`score` prints every entry that is not VERBATIM and writes `report.tsv` there.

- Settle REVIEW rows as in Step 6 and record each with `reviewed`.
- `apply` without `--keys` writes all REPLACE and DELETE results. It refuses to
  run while REVIEW rows remain unless they are covered by an overrides file.
- When the right text needs a source other than the one the script picked,
  put it in an overrides JSON (format in the script's `source_text` docstring:
  `from` is `crossref`, `openalex`, `arxiv`, `cut:N` or `span` with `start`/`end`
  anchors, plus optional `edits` and `drop_from`). Check each with
  `preview <file> <citekey>`, then run `apply --overrides <file>`.

Validate with biber afterwards (Step 7).
