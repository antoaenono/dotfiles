---
name: citation-hunt
description: Given a section of text, find citations for uncited claims by scanning the full bib, tracing upstream references of matched papers, then searching with world knowledge and the web. Pauses for confirmation before fetching anything.
user-invocable: true
allowed-tools: Bash, Read, Edit, WebSearch, WebFetch
---

# citation-hunt

Find citations for all claims in a section of text - both uncited and already-cited ones. Works in three chapters:

- **Chapter 1 - Bib scan:** Decompose the section into paragraph-level claims, then scan every entry in `references.bib` using titles, abstracts, and `annote` fields to find the best match for each claim.
- **Chapter 2 - Upstream references:** For every paper matched in Chapter 1, read its reference list and trace one level upstream to find additional supporting sources not already in the bib.
- **Chapter 3 - World knowledge + web:** For claims still uncovered or only weakly covered, use world knowledge and targeted web searches to find papers that should exist.

## Invocation

```
/citation-hunt [section] [--file path/to/file.org]
```

- `section`: a heading name (partial match OK) or line range like `100-120`
- `--file`: path to the org/tex file to search. If omitted, defaults to `~/org/thesis/thesis.org`

The `--seeds` argument has been removed. Chapter 1 scans the entire bib. Chapter 2 traces upstream from whatever Chapter 1 finds.

---

## Phase 0: Write scratch file

Before doing anything else, establish a scratch directory:

```bash
mkdir -p ~/.cache/citation-hunt/scratch
```

After extracting the section (Phase 1b), immediately write the scratch file. The file persists across all phases and is the single source of truth for section text and claim state - read it rather than relying on in-context repetition when context grows long.

File path: `~/.cache/citation-hunt/scratch/<section-slug>.org`

The slug is the heading text lowercased, spaces replaced with hyphens, non-alphanumeric stripped (e.g., "Problem Statement" -> `problem-statement`).

File structure:

```org
* SECTION: <heading> (lines <start>-<end>, <filename>)
** Para 1 (lines <A>-<B>)
<verbatim paragraph text>

** Para 2 (lines <C>-<D>)
<verbatim paragraph text>

...

* CLAIMS
(written after Phase 2 - leave blank until then)

* TO-DO
(written after Phase 3 examiner judgment - one checkbox per paper per claim)
Format:
** TODO Para N - ClaimX description
   - [ ] @citekey (conf NN) - one line reason
   - [ ] @citekey2 (conf NN) - one line reason

Mark [ ] -> [X] as each paper is fully processed (roam + thesis.org both done).
**This list is the authoritative work queue for Phase 3.5.** Read it at the start
of each paper's processing cycle rather than reconstructing from context.

* MATCH STATE
(written incrementally during Chapter 1 - leave blank until then)
```

Delete this file at the end of Phase 6.

---

## Phase 1: Extract and confirm target text

### 1a. Locate the file

If `--file` was given, use that path. Otherwise use `~/org/thesis/thesis.org`.

### 1b. Extract the section

Read the file. Find the section matching the given heading (case-insensitive partial match) or line range.

Extract:
- The heading text
- All body text under that heading, stopping at the next same-level or higher heading
- Any existing cite keys already present (org `[[cite:@key]]` or LaTeX `\cite{key}` format)

**Segment by paragraph:** Split the extracted body text at blank lines. Each non-empty block is a paragraph. Record the line range for each paragraph.

Write the scratch file now (Phase 0 format above, CLAIMS and MATCH STATE sections left blank).

### 1c. Confirm with user

Print the extracted text verbatim in a code block, with paragraph boundaries marked. Then print:

```
--- citation-hunt: Phase 1 confirmation ---
File:    <resolved file path>
Section: <heading>
Lines:   <start>-<end>
Paragraphs found: <N>
Existing citations found: <list or "none">
Scratch file: ~/.cache/citation-hunt/scratch/<slug>.org

Proceed? (If this is wrong, stop and re-invoke with correct arguments.)
```

**STOP HERE.** Do not proceed to Phase 2 until the user confirms. If the text is wrong, abort and delete the scratch file.

---

## Phase 2: Decompose into claims

Read the scratch file. For each paragraph, break it into individual claims. A claim is any sentence or clause that:
- States a fact about the world ("software systems degrade over time")
- Makes a motivation or justification ("this is problematic because...")
- States a prevalence or frequency ("decisions are rarely documented")
- Attributes a property to a concept ("architecture erosion causes...")

Skip: purely definitional sentences that introduce your own framework/terminology (those don't need external citations), and transition sentences.

**Do NOT skip already-cited sentences.** Process every claim regardless of whether it already has a citation - the goal is the *best* citation, not just any citation.

For each claim, record:
- **Para**: which paragraph it came from (number and line range)
- **Type**: one of:
  - `count/prevalence` - asserts how often, how many, or how common something is. Needs a measured quantity, survey count, or documented frequency. Example: "architects make many decisions", "documentation is rarely maintained"
  - `causal/consequence` - asserts that X causes Y or that a phenomenon produces a specific outcome. Needs a mechanism, longitudinal study, or controlled evidence. Example: "knowledge loss increases maintenance costs"
  - `canonical/definitional` - introduces an established concept or framework. Needs the paper that coined or canonically established it. Example: "bounded rationality", "architecture erosion", "residual causality"
  - `cognitive/behavioral` - claims about how people think, decide, or behave. Needs psychology, HCI, or cognitive science study. Example: "experts rely on pattern recognition", "decision fatigue impairs judgment"
  - `observation-in-practice` - asserts that something happens in real software projects. Needs a case study, interview study, or empirical observation in actual projects. Example: "ADRs are rarely written", "architects face time pressure"
  - `design-rationale` - justifies a specific approach or tool. Needs prior work proposing or evaluating that approach. Example: "ADRs reduce knowledge vaporization"

Also record for each claim: the **minimum evidence bar** - one sentence describing what a PhD examiner would require to accept a citation for this claim. Be specific. Example: "a study that counted or surveyed the number of architectural decisions made per project" - not just "a paper about architectural decisions."

Write the CLAIMS section of the scratch file:

```org
* CLAIMS
** Para 1 (lines <A>-<B>)
- [count/prevalence | bar: study measuring decision frequency or volume per project] architects make many decisions
- [causal/consequence | bar: empirical evidence linking undocumented decisions to maintenance cost increase] knowledge loss increases costs

** Para 2 (lines <C>-<D>)
- [canonical/definitional | bar: paper that coined or canonically defined the concept] architecture erosion is divergence between intended and actual structure
...
```

Print the decomposed claim list for visibility. Continue automatically (no pause needed here).

---

# CHAPTER 1: Full bib scan

## Phase 3: Match all claims against the full bibliography

### 3a. Load bib

Read `~/org/bib/references.bib`. For each entry, collect:
- citekey
- title
- abstract (if present)
- `annote` field (concise summary - use this as the primary matching signal when present)
- keywords (if present)
- `file` field (whether a local PDF exists)

### 3b. Ideal source reflection

Before scanning the bib, for each claim write a one-paragraph ideal source description. Answer:
- What methodology would this paper use? (e.g. controlled study, survey of N practitioners, systematic literature review, longitudinal case study, RCT, theoretical framework with empirical validation)
- What would its key finding or contribution look like? (e.g. "found that architects make on average X decisions per project", "defined and operationalized the concept of Y", "showed through interviews that Z is common in practice")
- What would disqualify a paper from satisfying this claim? (e.g. "a paper that only argues for X without measuring it", "a position paper", "a paper about a related but distinct phenomenon")

This description is your matching target for the bib scan - not the claim text itself.

Example for claim "architects make many decisions" (type: count/prevalence):
> Ideal: an empirical study - observational, survey, or case study - that measured or documented the volume of architectural decisions made during a software project. Key finding would be a count, frequency, or documented pattern of decision-making activity. Disqualifies: any paper that asserts decision volume without measuring it, position papers arguing for decision-centric architecture, conceptual frameworks.

### 3c. Match every claim against every bib entry

Process claims **one paragraph at a time**. After finishing all claims in a paragraph, immediately append those rows to the `* MATCH STATE` section of the scratch file before moving to the next paragraph. This ensures compaction cannot wipe in-progress work.

**Resuming after compaction:** If context compaction occurs mid-scan, re-read the scratch file at the start of the next message. Find the last paragraph recorded in `* MATCH STATE`. Resume scoring from the *next* paragraph - do not re-score paragraphs already written to MATCH STATE. Any paragraph whose rows are not yet in MATCH STATE must be re-scored from scratch (its in-progress work was lost).

For each claim, scan all bib entries and score them against the **ideal source description** from 3b - not the claim text. Use the `annote` as the primary signal. Fall back to abstract and title if `annote` is absent.

Scoring guidance:
- **direct (80-100):** The `annote` or abstract explicitly states the same phenomenon the claim asserts. The paper is clearly about this.
- **indirect (40-79):** The `annote`/abstract covers a related concept that supports the claim but is not the central point.
- **weak (10-39):** Tangential overlap - the paper is in the area but the claim is peripheral to what it studies.
- **none (0-9):** No meaningful overlap.

Hard disqualification rules by claim type - apply these before scoring. A paper that fails a disqualification rule is capped at score 39 regardless of topical overlap:
- `count/prevalence`: disqualify papers whose annote/abstract contains none of: "found that", "measured", "survey of", "interviewed", "N projects", "N architects", "empirical", "case study", "% of", "on average". Position papers, opinion pieces, and conceptual frameworks are disqualified.
- `causal/consequence`: disqualify papers that only assert the causal relationship without providing a mechanism, longitudinal evidence, or controlled study.
- `canonical/definitional`: survey and foundational papers are explicitly preferred. Do NOT disqualify.
- `cognitive/behavioral`: disqualify papers from SE venues that only discuss the phenomenon without grounding in psychology, cognitive science, or HCI study methodology.
- `observation-in-practice`: disqualify papers that are purely theoretical or that study student/toy projects rather than real-world software development.
- `design-rationale`: prefer papers that evaluate or propose the approach empirically; deprioritize papers that merely mention it.

For each claim, select the best candidate(s). A claim may have multiple good matches - list up to 3, ranked by score. Only list a candidate if score >= 40. Weak matches (10-39) are noted but not recommended.

### 3d. Examiner judgment

For each claim-candidate pair with score >= 40, apply an explicit PhD examiner judgment. Ask: *"If a PhD examiner saw this citation for this claim, would they accept it as genuine evidence, or would they push back?"*

Classify each pair as one of:

- **`DIRECT_EVIDENCE`**: the paper directly provides the kind of evidence the claim requires (matches the minimum evidence bar from Phase 2). A passage in this paper will satisfy a PhD examiner. Proceed to Phase 3.5.
- **`UPSTREAM_NEEDED`**: the paper is topically relevant and likely cites the actual primary evidence, but is itself a position paper, framing paper, survey, or opinion piece that doesn't directly evidence the claim. The real value is in its reference list. **Skip Phase 3.5 for this paper. Add to Chapter 2 seed list immediately.**
- **`REJECTED`**: on reflection, this paper doesn't actually support this claim even indirectly. Score downgraded to < 40. Move to uncovered.

The key distinction between `DIRECT_EVIDENCE` and `UPSTREAM_NEEDED`: ask "if I find a passage in this paper, will it contain a measurement, a documented observation, or an established definition - or will it just restate the same claim without grounding?" If the latter, it is `UPSTREAM_NEEDED`, not `DIRECT_EVIDENCE`.

**After completing each paragraph's claims:** append the new rows to the `* MATCH STATE` section of the scratch file before moving to the next paragraph. Do not wait until all paragraphs are done.

**Then** print the match table:

```
| Para | Claim (truncated 60 chars) | Type | Existing | Candidate | Conf | Judgment | Basis |
```

- **Judgment**: `DIRECT_EVIDENCE`, `UPSTREAM_NEEDED`, or `REJECTED`
- **Basis**: one sentence explaining the judgment

Claims flagged `UPSTREAM_NEEDED` or `REJECTED` with no `DIRECT_EVIDENCE` alternative: `[→ Chapter 2 seed: @key]`
Claims with no candidate at all (conf < 40): `[UNCOVERED - Chapter 2/3]`

Continue automatically.

---

## Phase 3.5: Locate supporting passages and page numbers

**Before starting:** write the `* TO-DO` section of the scratch file - one checkbox per `DIRECT_EVIDENCE` paper per claim. This is the authoritative work queue. At the start of each paper's processing cycle, re-read the scratch file's TO-DO list to find the next unchecked item. Mark `[ ]` -> `[X]` only after both roam annotation and thesis.org insertion are complete.

For each claim-citekey pair that is `DIRECT_EVIDENCE` AND `conf >= 60` AND has a local PDF, find the specific supporting passage. Skip any pair marked `UPSTREAM_NEEDED` or `REJECTED` - those go to Chapter 2.

**Process one paper at a time.** Complete the full cycle for a single paper (passage search -> feedback loop evaluation -> roam update -> scratch file update -> thesis.org insertion) before moving to the next. Do not batch multiple PDFs in parallel. This preserves the feedback loop and prevents context from being flooded with raw PDF text.

**CRITICAL - compaction safety:** After reading PDF text and identifying a passage, immediately write the roam file annotation AND insert the cite+transclusion block into thesis.org in the same response, before doing anything else. Never defer these writes to a later message. Context compaction can occur at any time between messages, wiping PDF text from context. If the writes are deferred, the work is lost. The sequence within a single message must be: (1) bbox, (2) create/update roam file, (3) insert cite+transclude into thesis.org. All three steps must complete before moving on to the next paper.

### 3.5a. Read roam file and check for a local source

Source attachments live recursively under `~/org/sources/`; the root is an
inbox. Resolve each entry's actual `file` field rather than assuming a flat
filename. For EPUBs, use `citation-add/inspect_epub.py <file> --text` to read
metadata and sections. Record verified quotations with chapter/section
locations, without fabricated PDF page numbers or HIGHLIGHT coordinates.
The PDF page, OCR, and bbox steps below apply only to PDFs. A source may have
both PDF and EPUB attachments; preserve both formats.

**First:** check if `~/org/roam/refs/<citekey>.org` exists and read it. Scan all existing `citation-hunt annotations` subheadings for passages that already match the current claim. If a match exists:
- Reuse the existing annotation (cite key + heading) immediately - emit the `[cite:]` and `#+transclude:` block into thesis.org right away
- Then continue searching the PDF for *additional* passages that the existing annotation does not cover

**Then:** read the `file` field from the bib entry. If no `file` field, skip - report `p. ?` and continue.

### 3.5b. Choose navigation strategy by page count

```bash
pdfinfo <file> | grep Pages
```

- **<= 20 pages:** Path A (full-read) - extract all text at once
- **21-60 pages:** Path A (search-first) - search by term across all pages, then read ±2 pages around each hit for context
- **> 60 pages (books/theses):** Path B (toc-first) - read first 15 pages for TOC, identify relevant sections, read only those

### 3.5c. Extract search terms from the abstract/annote

Do NOT use words from the claim - the claim is a paraphrase, not a quote from the paper. Instead:

1. Read the bib entry's `annote` field (preferred) or `abstract` field
2. Identify 2-3 distinctive noun phrases that are specific to the point supporting the claim
   - Good: "knowledge vaporization", "design decisions not captured", "documentation entropy"
   - Bad: "software", "system", "architecture" (too common)
3. These are your search terms - words the paper itself uses when making its point

### 3.5d. Find all supporting passages

For each claim, find **all** passages in the PDF that support it - not just the first match.

**Path A - short docs (full-read):**

```bash
python3 -c "
import subprocess
text = subprocess.run(['pdftotext', 'FILEPATH', '-'], capture_output=True, text=True).stdout
pages = text.split('\f')
query = 'SEARCH TERM'
for i, page in enumerate(pages, 1):
    hits = [l.strip() for l in page.splitlines() if query.lower() in l.lower() and len(l.strip()) > 50]
    if hits:
        print(f'p.{i}:', hits[0][:120])
"
```

Collect all hits across all terms. For each hit page, read a few lines of context to extract the full supporting sentence(s). Discard hits that are: reference list entries, section headings only, figure captions, or repetitions of the same argument already captured from another page.

**Path B - long docs (toc-first):**

1. Read the first 15 pages for the TOC
2. Identify **all** candidate sections whose titles relate to the claim's topic
3. Read each candidate section range and collect all substantive passages
4. Do not stop at the first hit - work through all relevant sections

Aim for 2-4 distinct passages per claim-citekey pair. One passage is acceptable for short papers or when the argument is made in only one place.

**Feedback loop:** After reading the passage, apply the PhD examiner test: does this passage contain a specific piece of evidence - a measurement, a documented observation, a named study result, or a canonical definition? A passage that merely restates the claim or asserts the same thing without grounding is NOT evidence and must be rejected.

Concretely: if the passage could have been written by the thesis author themselves without any empirical work, it is not evidence. Reject it.

If the retrieved passage fails:
- Search for other passages in the same paper that do contain grounding
- If found, use that instead
- If no passage in this paper provides grounding, downgrade to `UPSTREAM_NEEDED` and add to Chapter 2 seed list - do not emit a citation for this paper
- Update MATCH STATE in the scratch file

### 3.5e. Update roam ref file

For each citekey, create the roam ref file if needed, then add one subheading per passage found:

1. Check if `~/org/roam/refs/<citekey>.org` exists
2. If not, create it:
   ```bash
   python3 ~/.agents/skills/citation-hunt/create_ref.py \
     --citekey <key> \
     --title "<title from bib>" \
     --file "<one resolved PDF or EPUB attachment path>"
   ```
3. For each passage, run bbox.py to write the green highlight and get coordinates:
   ```bash
   python3 ~/.agents/skills/citation-hunt/bbox.py \
     --pdf "<file from bib>" \
     --page <N> \
     --color green \
     --quote "<verbatim passage>"
   ```
   bbox.py runs a four-tier extraction ladder automatically:
   - Tier 1: `pdftotext -bbox-layout` + XML parse (precise word bboxes)
   - Tier 2: XML sanitization + retry (handles control chars / bare ampersands)
   - Tier 3: pypdf visitor API (char-level positions, zero extra deps)
   - Tier 4: pdfminer.six (best encoding coverage)
   - Tier 5: `pdftoppm` visual render - exits with code 2 and prints `VISUAL_FALLBACK: /tmp/.../page.png`

   Capture stdout: `NOTER_PAGE: (N x . y)` and `HIGHLIGHT: #s(pdf-highlight ...)`.

   If exit code is 2 (VISUAL_FALLBACK): read the PNG path from stderr, use the Read
   tool to view the image, visually locate the passage, estimate fractional x/y coords
   (0.0=left/top, 1.0=right/bottom), then use `(N x . y)` as the NOTER_PAGE manually.
   No highlight is written to the PDF in this case - note it as `p.N (visual approx)`.

   If exit code is 1 (MATCH_FAILED): fall back to `(N 0.5 . 0.5)` with no HIGHLIGHT.

4. Append a `* citation-hunt annotations` section if not already present. Add one subheading per passage.
   Keep the heading text exactly `citation-hunt annotations`: the site build (`lisp/preamble.el`,
   `lisp/project.el`) looks for it.

```org
* citation-hunt annotations
:PROPERTIES:
:GENERATED: yes
:LAST_UPDATED: <date>
:END:

** <inferred heading summarizing the passage>
:PROPERTIES:
:NOTER_PAGE: (N x . y)
:HIGHLIGHT: #s(pdf-highlight N ((y1 x1 y2 x2)))
:CITE: [cite:@<key> p. <N>]
:END:
#+BEGIN_QUOTE
<verbatim passage>
#+END_QUOTE
```

Do not duplicate passages already present in the roam file from a previous run - check existing subheadings before appending.

### 3.5f. Format the citations in thesis.org

One cite+transclusion block per passage. Each passage in the roam file gets its own transclusion line.

```org
[cite:@key p. N]
#+transclude: [[file:~/org/roam/refs/<citekey>.org::*<heading for passage at p.N>]]
[cite:@key p. M]
#+transclude: [[file:~/org/roam/refs/<citekey>.org::*<heading for passage at p.M>]]
```

Rules:
- If a page is found: use `[cite:@key p. N]` form
- If no match found (scanned PDF, encrypted): use `[cite:@key]`, note `p. ? (text search failed)` in report
- For entries with no `file` field: use `[cite:@key]` and skip roam annotation

### 3.5g. Report

Update the match table from Phase 3c with a "Pages" column (listing all passages found):

```
| Para | Claim (truncated) | Type | Existing | Candidate | Conf | Pages | Basis |
```

Continue automatically.

---

# CHAPTER 2: Upstream reference tracing

Run this chapter for:
- Claims still uncovered (conf < 40) after Chapter 1
- Claims with only weak/indirect matches (conf 40-59) that warrant a stronger source
- Claims where the best Chapter 1 match was judged `UPSTREAM_NEEDED` - regardless of its confidence score. A high-confidence topical match that can't directly evidence the claim is a seed, not a citation.

## Phase 4: Trace upstream references from Chapter 1 matches

### 4a. Identify papers to trace

Collect all citekeys that were matched in Chapter 1 with conf >= 40. These are the seed papers for upstream tracing. Also include any citekeys already cited in the section.

Do not limit to a curated seed list - trace upstream from everything Chapter 1 found.

### 4b. Read seed paper reference lists

For each seed paper that has a local PDF (check `file` field in the bib entry):
1. Read the last 2-4 pages of the PDF to get the reference list
2. For each reference, note: authors, title, year, venue
3. Flag references that are clearly relevant to the *uncovered or weakly-covered claims* based on title alone

Depth is **1 level**: read the reference lists of the matched papers only, not the reference lists of those references.

Skip papers already in `~/org/bib/references.bib` (they were already scanned in Chapter 1).

### 4c. Search for open-access PDFs

Read `~/.agents/skills/citation-hunt/sources.md`. Try each source in the listed order.

For each flagged upstream reference not already in `~/org/sources/`:
1. Work through the sources in `sources.md` in order until a PDF is found or all are exhausted
2. Do NOT attempt paywalled downloads (IEEE Xplore, ACM DL, Springer without open access)
3. If found via a source not in `sources.md`, append it to the "Additional sources found during hunting" section of `sources.md`
4. Note the URL but do NOT download yet

### 4d. Confirm upstream candidates with user

Print a structured report:

```
--- citation-hunt: Chapter 2 confirmation ---

Claims targeted (uncovered or weak from Chapter 1):
  1. [Para N] "<claim text>" (current best: @key conf=NN, or "none")
  2. ...

Upstream candidates found:

  For claim 1:
    - [TITLE] Author et al. YEAR (VENUE)
      Why: <one sentence>
      PDF: <URL or "paywalled - manual download needed">

  For claim 2:
    - ...

Actions if approved:
  - Download N PDFs to ~/org/sources/
  - Run /citation-add to generate bib entries
  - Re-run passage search on new papers

Approve downloads? List any to skip.
```

**STOP HERE.** Do not download anything until the user approves.

### 4e. Fetch approved PDFs and process

For each approved candidate with a found PDF URL:
1. Download to `~/org/sources/` using curl:
   ```bash
   curl -L "<url>" -o "$HOME/org/sources/<authorYEARkeyword>.pdf"
   ```
2. Verify the file is a valid PDF:
   ```bash
   pdfinfo ~/org/sources/<filename>.pdf 2>/dev/null | head -3
   ```
3. If verification fails, delete the file and report as failed.

For paywalled papers, print the title and DOI so the user can download manually.

After fetching, run Phase 3.5 passage search on each new PDF immediately to check whether it actually supports the claim before proceeding to Chapter 3.

Update the MATCH STATE section of the scratch file with any new matches found.

---

# CHAPTER 3: World knowledge and web search

Run this chapter for claims still uncovered or weakly covered after Chapters 1 and 2.

## Phase 5: World knowledge pass

### 5a. Identify remaining gaps

Re-read the MATCH STATE section of the scratch file. List all claims with conf < 60 after Chapters 1 and 2.

### 5b. Apply world knowledge

For each remaining uncovered or weak claim, reason from your training knowledge about what papers are likely to exist that support it. Consider:
- Classic papers in the field that are commonly cited for this type of claim
- Foundational works the claim implicitly relies on
- Empirical studies you know of that measured this phenomenon

For each suggestion, produce:
- Full title (as accurate as possible)
- Author(s) and year (if known with reasonable confidence)
- Venue (if known)
- Why it supports the claim
- Confidence in the suggestion: `high` (paper is well-known and you are sure it exists), `medium` (you believe it exists but may have detail errors), `low` (this type of paper should exist; treat as a search lead)

Flag `medium` and `low` confidence suggestions explicitly - the user should verify before citing.

### 5c. Web search pass

For each remaining gap, perform targeted web searches to find papers not in your training or with uncertain details.

Use queries such as:
- `"<key phrase from claim>" software engineering empirical study site:semanticscholar.org`
- `"<phenomenon>" architecture decisions survey filetype:pdf`
- Semantic Scholar API: `https://api.semanticscholar.org/graph/v1/paper/search?query=<terms>&fields=title,authors,year,openAccessPdf,abstract`

For each web result:
- Verify the paper actually exists (check title + authors on Semantic Scholar or the publisher page)
- Do NOT fabricate or guess DOIs
- Note whether an open-access PDF is available

### 5d. Confirm Chapter 3 candidates with user

```
--- citation-hunt: Chapter 3 confirmation ---

Remaining uncovered claims after Chapters 1-2:
  1. [Para N] "<claim text>"

World knowledge suggestions:
  For claim 1:
    - [TITLE] Author et al. YEAR - confidence: high/medium/low
      Why: <one sentence>
      Note: <any caveats about title/year accuracy>

Web search finds:
  For claim 1:
    - [TITLE] Author et al. YEAR (VENUE)
      PDF: <URL or "paywalled">
      Verified: yes/no

Actions if approved:
  - Download N PDFs
  - Run /citation-add

Approve? List any to skip.
```

**STOP HERE.** Do not download until approved.

### 5e. Fetch approved Chapter 3 PDFs

Same procedure as Phase 4e. Run Phase 3.5 passage search on each new PDF.

Update the MATCH STATE section of the scratch file.

---

## Phase 6: Final report and cite placement

### 6a. Paragraph-grouped placement report

Group all recommendations by paragraph. This is the primary output:

```
--- citation-hunt: Results ---

Para 1 (lines <A>-<B>):
  "<sentence>" -> @key p.N (conf: 90, direct)
    Passage: "<verbatim supporting sentence from paper>"

  "<sentence>" -> @key2 (conf: 72, indirect) | no local PDF
    Basis: <annote/abstract sentence>

Para 2 (lines <C>-<D>):
  "<sentence>" -> @key3 p.4 (conf: 85, direct)
    Passage: "..."

  "<sentence>" -> UNCOVERED
    Best found: @key4 (conf: 35, weak) - not recommended
    Suggestion: look for empirical study on X

...

BETTER ALTERNATIVES FOUND:
  Line <N>: currently cited @old - recommend @new instead (conf 55 -> 88)
  Basis: <why new is stronger>

PDFS DOWNLOADED (pending /citation-add):
  <filename>

PDFS NEEDING MANUAL DOWNLOAD:
  <title> | DOI: <doi>

NEXT STEPS:
  1. Run /citation-add to generate bib entries for new PDFs
  2. Review paragraph-level placements above
  3. For uncovered claims, consider manual library search
```

### 6b. Emit thesis.org cite blocks

For each confirmed match, emit the cite+transclusion block as specified in Phase 3.5f, grouped by paragraph and in reading order.

### 6c. Cleanup

Delete the scratch file:

```bash
rm ~/.cache/citation-hunt/scratch/<slug>.org
```

---

## Important rules

- Never cite a survey paper for a claim that the survey itself attributes to a primary source - flag it and suggest the primary source instead.
- Never invent DOIs, URLs, or quotes. If you can't find a direct quote from the abstract supporting the claim, say so.
- Never download from paywalled sources.
- The verbatim abstract requirement is enforced by `/citation-add`, not this skill - just get the PDFs into `~/org/sources/`.
- Process every claim regardless of whether it already has a citation. The goal is the best possible citation, not just any citation.
- The scratch file is the source of truth across phases. When in doubt, re-read it rather than reconstructing from context.
- A candidate from world knowledge with `medium` or `low` confidence must be flagged as unverified in the final report. Never emit a `[cite:]` for an unverified paper.
- Chapter 3 web searches are a last resort for genuinely uncovered claims. Do not run them for claims already covered at conf >= 60.
