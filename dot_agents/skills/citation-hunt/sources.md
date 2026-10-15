# citation-hunt: PDF Search Sources

When searching for open-access PDFs in Phase 4c, try these sources in order.
If a source not listed here yields a successful hit, append it to this file.

## Primary sources (try first)

1. **arXiv** - `https://arxiv.org/abs/<id>` or search `https://arxiv.org/search/?query=<title>&searchtype=all`
   - Best for: CS/SE preprints, recent papers
   - PDF URL pattern: `https://arxiv.org/pdf/<id>`

2. **Semantic Scholar** - `https://api.semanticscholar.org/graph/v1/paper/search?query=<title>&fields=title,authors,year,openAccessPdf`
   - Best for: finding open-access PDF links by title search; returns `openAccessPdf.url` if available
   - No auth required for basic search

3. **Unpaywall** - `https://api.unpaywall.org/v2/<doi>?email=citation-hunt@thesis`
   - Best for: finding legal open-access versions of paywalled papers when you have a DOI
   - Returns `best_oa_location.url_for_pdf` if an OA version exists

4. **OpenAlex** - `https://api.openalex.org/works?search=<title>&select=title,open_access,primary_location`
   - Best for: broad coverage, checking if a paper has any OA version
   - Returns `open_access.oa_url` if available

5. **SEI CMU library** - `https://resources.sei.cmu.edu/library/`
   - Best for: SEI technical reports, ATAM, architecture evaluation papers

6. **Author institutional pages** - search `<author name> <paper title> site:<university>.edu filetype:pdf`
   - Best for: papers where author self-hosts a preprint

## Fallback (try if all above fail)

- WebSearch with query: `"<exact title>" filetype:pdf`
- WebSearch with query: `"<first author lastname>" "<year>" "<first significant title word>" PDF`

## How to add a new source

If a source not listed here yields a PDF that downloads and validates successfully,
append a new entry under "## Additional sources found during hunting" below,
following the same format: name, URL pattern, best-for note.

## Additional sources found during hunting

<!-- append new entries here -->
