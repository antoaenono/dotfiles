# classify_pdf.py: classification log

Read this when `classify_pdf.py` puts a document in the wrong class.
Records edge cases where its deterministic rules were wrong or surprising.
Routine correct classifications are not logged here - only failures, fixes, and boundary cases.

New entries go at the bottom of the relevant doc-class section.

---

## Known failure modes and fixes

### arXiv papers with descriptive filenames misclassified as thesis
- **Trigger**: arXiv paper saved with descriptive filename (e.g. `sycophantic-ai-effects-2602.14270v1.pdf`);
  university dept names in affiliations (e.g. "Department of Psychology") matched `master` substring in thesis regex
- **Fix**: moved arXiv check (`arxiv:` in text + `\d{4}\.\d{4,5}v\d` in filename) before thesis check;
  tightened thesis regex to `\b(phd|ph\.d\.?|doctoral|master\'?s thesis|dissertation)\b`
- **Date**: 2026-04-10

### SSRN preprints classified as unknown
- **Trigger**: `bib-type=misc` not mapped; no strong venue signals in SSRN page-1 text
- **Fix**: added `ssrn-` filename prefix check and `social science research network` text signal
- **Date**: 2026-04-10

### inproceedings early-return masked arXiv classification
- **Trigger**: ICLR papers submitted as `bib-type=inproceedings` returned conference-paper before arXiv check ran
- **Fix**: moved inproceedings early-return to after arXiv check in classify()
- **Date**: 2026-04-10

### ACM papers regressed to book after inproceedings fix
- **Trigger**: removing inproceedings early-return let ACM DL download metadata ("all rights reserved", publisher names)
  trigger book text signal check
- **Fix**: changed book detection to use page count only (>150pp), removed text signals for book class
- **Date**: 2026-04-10

### Nature/MDPI journals misclassified as conference-paper
- **Trigger**: Nature Scientific Data lacks "journal"/"volume"/"issue" on p.1; ~15pp falls into conference fallback
- **Fix**: added `nature\.com|mdpi\.com|tandfonline|sage pub` to journal-article regex
- **Date**: 2026-04-10

---

## Boundary cases by doc-class

### arxiv-preprint
- `batista2026sycophantic-ai`: descriptive filename triggered thesis before fix; arXiv: in text is reliable signal
- `heo2025llm-instruction-following`: bib-type=inproceedings but arxiv: signal takes priority (correct)
- `sharma2024sycophancy`: ICLR via arXiv; classified arxiv-preprint despite inproceedings bib-type

### ssrn-preprint
- `shaw2026cognitive-surrender`: ssrn-XXXXXXX.pdf filename is reliable; was unknown before fix
- `kim2026harness-engineering`: same pattern confirmed

### journal-article
- `gacitua2026adr`: bib-type=article but no strong journal signals on p.1; falls through to conference-paper
  (acceptable - range is still correct for abstract extraction)
- `rosenman2023race-ethnicity-names`: Nature Scientific Data; fixed by nature\.com signal
