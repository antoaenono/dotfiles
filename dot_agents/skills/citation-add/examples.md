# Bibtex Entry Examples

Real cases from this bib. When generating a new entry, find the matching case
and follow it exactly - field names, field order, formatting conventions.

Abstracts are shown as a placeholder to keep the examples short. In a real
entry the `abstract` is the work's own abstract, copied verbatim and complete
(see "Abstract" in SKILL.md), or the field is omitted. Never shorten, reword or
summarize it: the site shows it under an "Abstract" label as the paper's own
text. Summaries belong in `annote`, which the site shows as "Brief".

---

## 1. arXiv preprint

**Situation**: Paper only on arXiv. Filename like `2602.14270v1.pdf` or
`descriptive-name-2602.14270v1.pdf`. DOI is always `10.48550/arXiv.XXXX.XXXXX`.

**Rule**: Use `@article` (our convention). Set `journal` to
`arXiv preprint arXiv:XXXX.XXXXX`. Always include the arXiv DOI.

```bibtex
@article{batista2026sycophantic-ai,
  author    = {Batista, Rafael M. and Griffiths, Thomas L.},
  title     = {A Rational Analysis of the Effects of Sycophantic AI},
  journal   = {arXiv preprint arXiv:2602.14270},
  year      = {2026},
  keywords  = {sycophancy, llm, belief-updating, confirmation-bias, epistemic-risk},
  abstract  = {<the work's own abstract, copied verbatim and complete>},
  file      = {~/org/sources/papers/sycophantic-ai-effects-2602.14270v1.pdf},
  doi       = {10.48550/arXiv.2602.14270}
}
```

---

## 2. Journal article (named publisher)

**Situation**: Paper in a named journal published by Elsevier, Nature, Springer,
ACM, etc. The `publisher` field is invalid on `@article` per biber's datamodel.

**Rule**: Use `@article`. Put the publisher in `note = {Publisher: X}` to
preserve the information without triggering a validation warning.

```bibtex
@article{babaei2025nasa-tlx,
  author    = {Babaei, Ebrahim and Dingler, Tilman and Tag, Benjamin and Velloso, Eduardo},
  title     = {Should we use the NASA-TLX in HCI? A review of theoretical and
               methodological issues around Mental Workload Measurement},
  journal   = {International Journal of Human-Computer Studies},
  volume    = {201},
  pages     = {103515},
  year      = {2025},
  note      = {Publisher: Elsevier},
  keywords  = {mental-workload, nasa-tlx, hci, cognitive-load, measurement},
  abstract  = {<the work's own abstract, copied verbatim and complete>},
  file      = {~/org/sources/papers/1-s2.0-S1071581925000722-main.pdf},
  doi       = {10.1016/j.ijhcs.2025.103515}
}
```

Filename heuristics: `1-s2.0-S...` = Elsevier ScienceDirect,
`s41597-...` = Nature/Springer.

---

## 3. IEEE conference paper

**Situation**: Paper in IEEE proceedings (ICSA, ICSE, ICSME, etc.). DOI
typically in the footer of page 1. Filename often descriptive.

**Rule**: Use `@inproceedings`. `publisher = {IEEE}` is valid here. Include
full conference name in `booktitle`.

```bibtex
@inproceedings{ivers2025genai-architecture,
  author    = {Ivers, James and Ozkaya, Ipek},
  title     = {Will Generative AI Fill the Automation Gap in Software Architecting?},
  booktitle = {2025 IEEE 22nd International Conference on Software Architecture
               Companion (ICSA-C)},
  year      = {2025},
  pages     = {41--48},
  publisher = {IEEE},
  keywords  = {software-architecture, genai, llm, automation, architecture-decisions},
  abstract  = {<the work's own abstract, copied verbatim and complete>},
  file      = {~/org/sources/papers/Will_Generative_AI_Fill_the_Automation_Gap_in_Software_Architecting.pdf},
  doi       = {10.1109/ICSA-C65153.2025.00014}
}
```

---

## 4. ACM conference paper

**Situation**: Paper in ACM proceedings (FAccT, CHI, PLDI, etc.). Filename may
be an ACM DL identifier like `3531146.3533088.pdf`.

**Rule**: Use `@inproceedings`. `publisher = {ACM}` is valid. Use full
proceedings title in `booktitle`.

```bibtex
@inproceedings{weidinger2022taxonomy,
  author    = {Weidinger, Laura and Uesato, Jonathan and Rauh, Maribeth and
               Griffin, Conor and Huang, Po-Sen and Mellor, John and ...},
  title     = {Taxonomy of Risks posed by Language Models},
  booktitle = {Proceedings of the 2022 ACM Conference on Fairness,
               Accountability, and Transparency},
  year      = {2022},
  publisher = {ACM},
  keywords  = {language-models, responsible-ai, risk-taxonomy, safety, ethics},
  abstract  = {<the work's own abstract, copied verbatim and complete>},
  file      = {~/org/sources/papers/3531146.3533088.pdf},
  doi       = {10.1145/3531146.3533088}
}
```

---

## 5. OpenReview / ICLR / NeurIPS workshop paper

**Situation**: Paper at ICLR, NeurIPS, ICML, or similar ML venue that
publishes via OpenReview. No traditional publisher. DOI may be an arXiv DOI
if only an arXiv version is available.

**Rule**: Use `@inproceedings`. Omit `publisher` entirely - these venues have
no publisher in the traditional sense. Use the conference short name (e.g.
`ICLR 2025`) in `booktitle` if the full proceedings title is not on the PDF.

```bibtex
@inproceedings{sharma2024sycophancy,
  author    = {Sharma, Mrinank and Tong, Meg and Korbak, Tomasz and ...},
  title     = {Towards Understanding Sycophancy in Language Models},
  booktitle = {ICLR 2024},
  year      = {2024},
  keywords  = {sycophancy, llm, rlhf, human-feedback, alignment},
  abstract  = {<the work's own abstract, copied verbatim and complete>},
  file      = {~/org/sources/papers/2310.13548v4.pdf},
  doi       = {10.48550/arXiv.2310.13548}
}
```

---

## 6. SSRN preprint

**Situation**: Paper on SSRN. Filename like `ssrn-XXXXXXX.pdf`. Version
string like `v20260111` may appear on the title page. No DOI on first page.

**Rule**: Use `@misc`. No `publisher`. Omit `doi` if not found on any page
read - do not guess it.

```bibtex
@misc{shaw2026cognitive-surrender,
  author    = {Shaw, Steven D. and Nave, Gideon},
  title     = {Thinking---Fast, Slow, and Artificial: How AI is Reshaping
               Human Reasoning and the Rise of Cognitive Surrender},
  year      = {2026},
  keywords  = {cognitive-surrender, dual-process, human-ai-interaction,
               reasoning, tri-system-theory},
  abstract  = {<the work's own abstract, copied verbatim and complete>},
  file      = {~/org/sources/ssrn-6097646.pdf}
}
```

---

## 7. Never invent a DOI

**Situation**: DOI field not visible on page 1 or last page.

**Rule**: Omit the `doi` field entirely. A missing DOI is harmless. A
hallucinated DOI (e.g. `10.1109/XXX.0000.0000000`) silently breaks citation
linking and is worse than no DOI.

Wrong:
```bibtex
  doi = {10.1109/XXX.0000.0000000}   % placeholder - DO NOT DO THIS
  doi = {10.1145/unknown}             % guessed - DO NOT DO THIS
```

Correct: omit the field if unsure.

---

## 8. Book (commercial, no abstract)

**Situation**: A commercially published book (Addison-Wesley, O'Reilly, MIT Press, etc.).
No abstract field - books don't have abstracts. ISBN goes in `note`.

**Rule**: Use `@book`. Include `publisher`, `year`, `keywords`, `note` (for ISBN), `file`.
Omit `abstract` entirely. Omit `doi` unless a DOI is printed in the book itself.

```bibtex
@book{humble2010continuous-delivery,
  author    = {Humble, Jez and Farley, David},
  title     = {Continuous Delivery: Reliable Software Releases Through Build, Test, and Deployment Automation},
  publisher = {Addison-Wesley},
  year      = {2010},
  keywords  = {continuous-delivery, continuous-integration, deployment, build-automation, devops},
  note      = {ISBN: 978-0-321-60191-9},
  file      = {~/org/sources/books/Continuous Delivery_ Reliable Software Releases through -- Humble, Jez & Farley, David.pdf}
}
```

---

## 9. Magazine article (no abstract)

**Situation**: Article in a popular-science or trade magazine (Scientific American,
IEEE Spectrum, Communications of the ACM magazine section, etc.). No formal abstract
printed - the article just begins. No DOI visible on pages read.

**Rule**: Use `@article`. Set `journal` to the magazine name as printed. Include
`volume`, `number`, `pages`, `year`. Omit `abstract` entirely - do not summarize
or paraphrase. Omit `doi` if not found on pages read.

```bibtex
@article{schwartz2004tyranny-choice,
  author    = {Schwartz, Barry},
  title     = {The Tyranny of Choice},
  journal   = {Scientific American},
  volume    = {92},
  number    = {4},
  pages     = {70--75},
  year      = {2004},
  keywords  = {choice, decision-making, cognitive-load, paradox-of-choice, well-being},
  file      = {~/org/sources/papers/schwartz2004tyranny-of-choice.pdf}
}
```

---

## 10. Non-journal in `journal` field

**Situation**: The `journal` field contains something that is clearly not a
journal - a company name, a personal website, an institution name, etc. (e.g.
`journal = {Lingenic LLC}`).

**Rule**: Reconsider the `@type`. This is likely `@techreport` (if from a
named institution) or `@misc`. Move the institution to the `institution` field
for `@techreport`, or to `note` for `@misc`.

```bibtex
@misc{slavenskoj2026knuth-pseudocode,
  author      = {Slavenskoj, Danslav},
  title       = {Towards a Generalization of Knuth's Pseudocode Architecture:
                 From Algorithms to Knowledge},
  year        = {2026},
  note        = {Lingenic LLC},
  keywords    = {pseudocode, knowledge-representation, formal-systems, notation, llm},
  abstract    = {<the work's own abstract, copied verbatim and complete>},
  file        = {~/org/sources/knuth-pseudocode-generalization.pdf}
}
```
