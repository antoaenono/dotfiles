#!/usr/bin/env python3
"""Audit `abstract` fields in references.bib against the works' own abstracts.

Every abstract must be the work's own abstract, verbatim and complete, or be
absent. This script gathers reference texts, scores each bib abstract, and
proposes an action per entry: VERBATIM (keep), REPLACE (with a verified source
abstract), DELETE (the work has no abstract), or REVIEW (it cannot tell; check
the page by hand).

Usage (BIB is normally ~/org/bib/references.bib):
    audit_abstracts.py fetch    BIB [--keys k1,k2]   # fill the cache
    audit_abstracts.py score    BIB [--keys k1,k2]   # write the report, print results
    audit_abstracts.py show     BIB KEY              # bib text next to every source, for REVIEW
    audit_abstracts.py apply    BIB [--keys k1,k2] [--overrides FILE] [--dry-run]
    audit_abstracts.py preview  BIB FILE KEY         # text an override would write
    audit_abstracts.py reviewed BIB KEY NOTE         # record a hand-checked abstract

--keys limits fetch, score and apply to those citekeys (new entries); without
it the whole bib is audited.

Sources, cached under ~/.cache/citation-add/abstract-audit/:
    crossref  https://api.crossref.org/works/<doi>          (JATS converted)
    openalex  https://api.openalex.org/works/doi:<doi>      (inverted index rebuilt)
    arxiv     https://export.arxiv.org/api/query?id_list=   (for arXiv ids)
    search    CrossRef title lookup for entries without a DOI (exact title only;
              the bib's doi field is never changed)
    pdf       pdftotext on the `file` field (tesseract OCR for scans), plus
              regex cuts of the abstract

No personal data is sent: requests carry a generic User-Agent and no mailto.
Needs poppler (pdftotext, pdfinfo, pdftoppm) and tesseract.
"""

import difflib
import html
import json
import os
import re
import subprocess
import sys
import threading
import time
import unicodedata
import urllib.error
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
STATE = os.path.expanduser("~/.cache/citation-add/abstract-audit")
CACHE = os.path.join(STATE, "cache")
REPORT = os.path.join(STATE, "report.json")
REVIEWED = os.path.join(STATE, "reviewed.json")
UA = "citation-add-abstract-audit/1.0 (python-urllib)"

# --------------------------------------------------------------------------
# BibTeX parsing with exact spans
# --------------------------------------------------------------------------


from source_library import parse_bib


# --------------------------------------------------------------------------
# Normalization and n-gram scoring
# --------------------------------------------------------------------------

LIGATURES = {"\ufb00": "ff", "\ufb01": "fi", "\ufb02": "fl", "\ufb03": "ffi", "\ufb04": "ffl", "\ufb05": "st", "\ufb06": "st"}
LATEX_ACCENT = re.compile(r"\\[`'^\"~=.uvHcdbtrk]\s*(?:\{\s*([A-Za-z])\s*\}|([A-Za-z]))")
LATEX_LETTERS = {r"\o": "o", r"\O": "O", r"\ss": "ss", r"\ae": "ae", r"\AE": "AE", r"\l": "l", r"\L": "L", r"\i": "i"}


def strip_latex(s):
    s = LATEX_ACCENT.sub(lambda m: m.group(1) or m.group(2), s)
    for k, v in LATEX_LETTERS.items():
        s = re.sub(re.escape(k) + r"(?![A-Za-z])", v, s)
    s = re.sub(r"\\(?:textit|textbf|emph|textsc|texttt|textrm|mathrm|url)\s*\{", "{", s)
    s = re.sub(r"\\([&%#$_{}])", r"\1", s)
    s = s.replace("{", "").replace("}", "")
    s = s.replace("~", " ").replace("--", "-")
    return s


def normalize(s):
    """Lowercased word tokens, robust to quoting, dashes, hyphenation, accents, LaTeX."""
    s = strip_latex(s)
    for k, v in LIGATURES.items():
        s = s.replace(k, v)
    s = s.replace("\u00ad", "")  # soft hyphen
    s = re.sub(r"[\u2018\u2019\u201a\u201b\u2032`]", "'", s)
    s = re.sub(r"[\u201c\u201d\u201e\u201f\u2033]", '"', s)
    s = re.sub(r"[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]", "-", s)
    # rejoin words broken across lines, and treat in-word hyphens as joins,
    # so "exam-\nple", "data-\ndriven" and "data-driven" all compare equal
    s = re.sub(r"(\w)-\s*\n\s*(\w)", r"\1\2", s)
    s = re.sub(r"(\w)-(\w)", r"\1\2", s)
    s = re.sub(r"(\w)'(\w)", r"\1\2", s)
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = s.lower()
    s = re.sub(r"\s+", " ", s)
    return re.findall(r"[a-z0-9]+", s)


def grams(tokens, n=5):
    if not tokens:
        return set()
    if len(tokens) < n:
        return {tuple(tokens)}
    return {tuple(tokens[i : i + n]) for i in range(len(tokens) - n + 1)}


def containment(a_grams, b_grams):
    """Fraction of a's n-grams found in b."""
    if not a_grams:
        return 0.0
    return len(a_grams & b_grams) / len(a_grams)


def align_ratio(a, b, n=20):
    """Similarity of a to the stretch of b it most plausibly came from.

    Both sides are reduced to normalized letters with spacing removed, so PDF
    artifacts ("whic hthe", a dropped "fi" ligature) cost a few letters, while
    words or sentences dropped from the middle make the window longer than a
    and pull the ratio down. Returns 0.0 when no window is found.
    """
    A, B = "".join(normalize(a)), "".join(normalize(b))
    if len(A) < n or len(B) < n:
        return 0.0
    first = last = None
    for i in range(len(A) - n + 1):
        k = B.find(A[i : i + n])
        if k >= 0:
            first = max(0, k - i)
            break
    if first is None:
        return 0.0
    for j in range(len(A) - n, -1, -1):
        k = B.find(A[j : j + n], first)
        if k >= 0:
            last = k + n + (len(A) - j - n)
            break
    if last is None or last <= first or last - first > 3 * len(A):
        return 0.0
    W = B[first:last]
    sm = difflib.SequenceMatcher(None, A, W, autojunk=False)
    matched = sum(b.size for b in sm.get_matching_blocks() if b.size >= 4)
    return matched / max(len(A), len(W))


def title_sim(a, b):
    ta, tb = set(normalize(a or "")), set(normalize(b or ""))
    if not ta or not tb:
        return 0.0
    return len(ta & tb) / min(len(ta), len(tb))


# --------------------------------------------------------------------------
# Fetching with cache and per-host rate limits
# --------------------------------------------------------------------------


class RateLimiter:
    def __init__(self, interval):
        self.interval, self.lock, self.last = interval, threading.Lock(), 0.0

    def wait(self):
        with self.lock:
            delay = self.last + self.interval - time.monotonic()
            if delay > 0:
                time.sleep(delay)
            self.last = time.monotonic()


LIMITS = {"crossref": RateLimiter(0.5), "openalex": RateLimiter(0.25), "arxiv": RateLimiter(3.5)}


def http_get(url, limiter, retries=3):
    for attempt in range(retries):
        limiter.wait()
        req = urllib.request.Request(url, headers={"User-Agent": UA})
        try:
            with urllib.request.urlopen(req, timeout=30) as r:
                return r.status, r.read().decode("utf-8", "replace")
        except urllib.error.HTTPError as e:
            if e.code in (404, 410):
                return e.code, ""
            if e.code in (429, 500, 502, 503, 504) and attempt < retries - 1:
                time.sleep(5 * (attempt + 1))
                continue
            return e.code, ""
        except Exception as e:  # network errors
            if attempt < retries - 1:
                time.sleep(5 * (attempt + 1))
                continue
            return -1, str(e)
    return -1, ""


def cache_path(kind, key):
    safe = re.sub(r"[^A-Za-z0-9._-]+", "_", key)
    d = os.path.join(CACHE, kind)
    os.makedirs(d, exist_ok=True)
    return os.path.join(d, safe + ".json")


def cached(kind, key, fn):
    p = cache_path(kind, key)
    if os.path.exists(p):
        with open(p, encoding="utf-8") as f:
            return json.load(f)
    val = fn()
    if val.get("status", 0) in (-1, 429) or (isinstance(val.get("status"), int) and val["status"] >= 500):
        return val  # do not cache transient failures
    with open(p, "w", encoding="utf-8") as f:
        json.dump(val, f, ensure_ascii=False, indent=1)
    return val


def jats_to_text(s):
    if not s:
        return ""
    # drop a leading "Abstract"/"Summary" heading; keep other section titles as words
    s = re.sub(r"<jats:title>\s*(abstract|summary|a b s t r a c t)\s*</jats:title>", " ", s, flags=re.I)
    s = re.sub(r"<jats:title>\s*([^<]*?)\s*</jats:title>", lambda m: " " + m.group(1).rstrip(":") + ": ", s)
    s = re.sub(r"</?jats:(p|sec)[^>]*>", " ", s)
    s = re.sub(r"<[^>]+>", "", s)
    s = html.unescape(s)
    s = re.sub(r"\s+", " ", s).strip()
    s = re.sub(r"^(abstract|summary)\s*[:.\-\u2014]?\s+", "", s, flags=re.I)
    return s


def fetch_crossref(doi):
    def go():
        url = "https://api.crossref.org/works/" + urllib.parse.quote(doi, safe="/:;()")
        status, body = http_get(url, LIMITS["crossref"])
        out = {"status": status}
        if status == 200:
            w = json.loads(body).get("message", {})
            out.update(
                title=(w.get("title") or [""])[0],
                type=w.get("type", ""),
                abstract_raw=w.get("abstract", ""),
            )
        return out

    r = cached("crossref", doi.lower(), go)
    if r.get("abstract_raw"):
        r["abstract"] = strip_boilerplate(jats_to_text(r["abstract_raw"]))
    return r


def fetch_openalex(doi):
    def go():
        url = (
            "https://api.openalex.org/works/doi:"
            + urllib.parse.quote(doi, safe="/:;()")
            + "?select=id,title,type,publication_year,abstract_inverted_index"
        )
        status, body = http_get(url, LIMITS["openalex"])
        out = {"status": status}
        if status == 200:
            w = json.loads(body)
            out.update(id=w.get("id"), title=w.get("title") or "", type=w.get("type", ""), inv=w.get("abstract_inverted_index"))
        return out

    r = cached("openalex", doi.lower(), go)
    inv = r.get("inv")
    if inv:
        pos = {}
        for word, idxs in inv.items():
            for i in idxs:
                pos[i] = word
        r["abstract"] = strip_boilerplate(re.sub(r"\s+", " ", " ".join(pos[i] for i in sorted(pos))).strip())
    return r


def fetch_arxiv(ids):
    """Fetch many arXiv ids; returns {id: {...}} and caches each id."""
    todo = [i for i in ids if not os.path.exists(cache_path("arxiv", i))]
    for k in range(0, len(todo), 50):
        batch = todo[k : k + 50]
        url = "https://export.arxiv.org/api/query?max_results=100&id_list=" + ",".join(batch)
        status, body = http_get(url, LIMITS["arxiv"])
        if status != 200:
            print(f"arxiv batch failed: {status}", file=sys.stderr)
            continue
        ns = {"a": "http://www.w3.org/2005/Atom"}
        root = ET.fromstring(body)
        found = {}
        for ent in root.findall("a:entry", ns):
            eid = ent.findtext("a:id", "", ns)
            m = re.search(r"abs/([^v\s]+?)(v\d+)?$", eid)
            if not m:
                continue
            found[m.group(1)] = {
                "status": 200,
                "title": re.sub(r"\s+", " ", ent.findtext("a:title", "", ns)).strip(),
                "abstract": re.sub(r"\s+", " ", ent.findtext("a:summary", "", ns)).strip(),
            }
        for i in batch:
            val = found.get(i, {"status": 404})
            with open(cache_path("arxiv", i), "w", encoding="utf-8") as f:
                json.dump(val, f, ensure_ascii=False, indent=1)
    out = {}
    for i in ids:
        with open(cache_path("arxiv", i), encoding="utf-8") as f:
            out[i] = json.load(f)
    return out


def pdftotext(path, first=None, last=None):
    cmd = ["pdftotext", "-enc", "UTF-8"]
    if first:
        cmd += ["-f", str(first)]
    if last:
        cmd += ["-l", str(last)]
    cmd += [path, "-"]
    try:
        r = subprocess.run(cmd, capture_output=True, timeout=120)
        return r.stdout.decode("utf-8", "replace")
    except Exception:
        return ""


def pdf_info(path):
    try:
        r = subprocess.run(["pdfinfo", path], capture_output=True, timeout=30)
        m = re.search(rb"Pages:\s+(\d+)", r.stdout)
        return int(m.group(1)) if m else 0
    except Exception:
        return 0


sys.path.insert(0, HERE)
try:
    from extract_abstract import try_patterns as _skill_patterns  # noqa: E402
except Exception:  # pragma: no cover
    _skill_patterns = None

CUT_START = re.compile(
    r"(?:^|\n)[ \t\f]*(?:a\s?b\s?s\s?t\s?r\s?a\s?c\s?t|summary)\b[ \t]*[:.\u2014\u2013-]*[ \t]*", re.I
)
CUT_END = re.compile(
    r"\n\s*(?:key\s?words?|index\s+terms|ccs\s+concepts|categories\s+and\s+subject|general\s+terms|"
    r"additional\s+key\s+words|acm\s+reference|(?:1|i)\.?\s+introduction|introduction\s*\n|1\s*\n|1\.?\s+[A-Z])",
    re.I,
)


def pdf_cuts(text):
    """Candidate regex cuts of the abstract, best first: the text after an
    "Abstract" heading, ending at the next heading (Keywords, Index Terms,
    1 Introduction) when that is the same block, else at the first blank line or
    page break, else at a blank line across a page break, else at the heading.
    The add-bibs skill's pattern library comes last. Returns [(text, how)]."""
    out = []
    words = lambda t: len(re.findall(r"\w+", t or ""))
    for m in CUT_START.finditer(text):
        rest = text[m.end() :].lstrip()
        he = CUT_END.search(rest)
        head_cut = rest[: he.start()] if he else None
        pb = re.search(r"\n[ \t\f]*\n|\f", rest)
        para_cut = rest[: pb.start()] if pb else None
        pb2 = re.search(r"\n[ \t]*\n", rest)
        para2_cut = rest[: pb2.start()] if pb2 else None
        if head_cut and para_cut and len(head_cut) <= 1.15 * len(para_cut) and words(head_cut) >= 40:
            out.append((head_cut, "heading"))
        if para_cut and words(para_cut) >= 40:
            after = rest[pb.end() :].lstrip()
            nb = re.search(r"\n[ \t\f]*\n|\f", after)
            nxt = after[: nb.start()] if nb else after
            prose = words(nxt) >= 30 and not CUT_END.match("\n" + nxt) and re.search(r"[.?!]\s*$", nxt.strip())
            out.append((para_cut, "paragraph+prose" if prose else "paragraph"))
        if para2_cut and para2_cut != para_cut and words(para2_cut) >= 40:
            out.append((para2_cut, "paragraph-across-page"))
        if head_cut and 40 <= words(head_cut) <= 700:
            out.append((head_cut, "heading-long"))
    if _skill_patterns:
        r = _skill_patterns(text)
        if r:
            out.append((r[0], "skill:" + r[1]))
    seen, uniq = set(), []
    for t, how in out:
        t = t.strip()
        if t not in seen:
            seen.add(t)
            uniq.append((t, how))
    return uniq


def ocr_pdf(path, first=None, last=None):
    """OCR a scanned PDF with pdftoppm + tesseract; pages joined with form feeds."""
    import tempfile

    out = []
    with tempfile.TemporaryDirectory() as d:
        cmd = ["pdftoppm", "-r", "300", "-gray", "-png"]
        if first:
            cmd += ["-f", str(first)]
        if last:
            cmd += ["-l", str(last)]
        subprocess.run(cmd + [path, os.path.join(d, "p")], capture_output=True, timeout=900)
        for img in sorted(os.listdir(d)):
            r = subprocess.run(["tesseract", os.path.join(d, img), "-", "--psm", "1"], capture_output=True, timeout=300)
            out.append(r.stdout.decode("utf-8", "replace"))
    return "\f".join(out)


def fetch_pdf(key, path):
    # An EPUB attachment must never be sent through the PDF/OCR pipeline.
    from source_library import attachment_paths
    pdfs = [str(p) for p in attachment_paths({"file": path}) if p.suffix.lower() == ".pdf"]
    if not pdfs:
        return {"status": 415, "path": path, "reason": "No PDF attachment"}
    path = pdfs[0]
    def go():
        p = os.path.expanduser(path)
        if not os.path.exists(p):
            return {"status": 404, "path": path}
        full = pdftotext(p)
        head, wide, ocr = pdftotext(p, 1, 3), pdftotext(p, 1, 15), False
        if not usable_text(full):
            full, ocr = ocr_pdf(p), True
            pages = full.split("\f")
            head, wide = "\f".join(pages[:3]), "\f".join(pages[:15])
        return {"status": 200, "path": path, "pages": pdf_info(p), "ocr": ocr, "full": full, "head": head, "wide": wide}

    return cached("pdf", key, go)


def search_doi(e):
    """Find a DOI for an entry that has none: CrossRef bibliographic query, accepted
    only on an exact normalized title match (or main title before a colon) and a
    year within one. The bib's own fields are never changed by this."""
    title = strip_latex(e.get("title") or "")
    author = strip_latex((e.get("author") or e.get("editor") or "").split(" and ")[0].split(",")[0])
    year = re.sub(r"\D", "", e.get("year") or "")

    def go():
        q = {"query.bibliographic": title, "rows": "5", "select": "DOI,title,issued,type"}
        if author:
            q["query.author"] = author
        url = "https://api.crossref.org/works?" + urllib.parse.urlencode(q)
        status, body = http_get(url, LIMITS["crossref"])
        out = {"status": status, "items": []}
        if status == 200:
            for it in json.loads(body).get("message", {}).get("items", []):
                y = ((it.get("issued") or {}).get("date-parts") or [[None]])[0][0]
                out["items"].append({"doi": it.get("DOI"), "title": (it.get("title") or [""])[0], "year": y, "type": it.get("type")})
        return out

    r = cached("search", e.key, go)
    want = normalize(title)
    want_main = normalize(title.split(":")[0])
    for it in r.get("items", []):
        got = normalize(it["title"])
        if got != want and got != want_main and normalize(it["title"].split(":")[0]) != want:
            continue
        if year and it.get("year") and abs(int(it["year"]) - int(year[:4])) > 1:
            continue
        return it["doi"]
    return None


# --------------------------------------------------------------------------
# Per-entry source gathering
# --------------------------------------------------------------------------

ARXIV_RE = re.compile(r"(?:arxiv[.:/ ]+(?:abs/|pdf/)?|10\.48550/arxiv\.)(\d{4}\.\d{4,5}|[a-z-]+(?:\.[A-Z]{2})?/\d{7})", re.I)


def arxiv_id(e):
    doi = (e.get("doi") or "").strip()
    m = re.match(r"10\.48550/arxiv\.(.+)$", doi, re.I)
    if m:
        return re.sub(r"v\d+$", "", m.group(1))
    if (e.get("eprinttype") or "").lower() == "arxiv" and e.get("eprint"):
        return re.sub(r"v\d+$", "", e.get("eprint").strip())
    for f in ("url", "journal", "note", "howpublished"):
        v = e.get(f) or ""
        m = ARXIV_RE.search(v)
        if m:
            return re.sub(r"v\d+$", "", m.group(1))
    return None


def load(bib):
    with open(bib, encoding="utf-8") as f:
        text = f.read()
    return text, parse_bib(text)


def entry_doi(e, search=False):
    """(doi, how): the bib's own DOI, else one found by title search (cached),
    else None. arXiv DOIs are handled by the arXiv source instead."""
    doi = (e.get("doi") or "").strip()
    if doi:
        return (None, None) if re.match(r"10\.48550/", doi, re.I) else (doi, "bib")
    if arxiv_id(e):
        return None, None
    if search or os.path.exists(cache_path("search", e.key)):
        found = search_doi(e)
        if found:
            return found, "title-search"
    return None, None


def cmd_fetch(bib, keys=None):
    text, entries = load(bib)
    with_abs = [e for e in entries if "abstract" in e.fields and (not keys or e.key in keys)]
    dois = sorted({d for e in with_abs if (d := entry_doi(e, search=True)[0])})
    aids = sorted({a for e in with_abs if (a := arxiv_id(e))})
    print(f"{len(with_abs)} entries with abstracts, {len(dois)} DOIs, {len(aids)} arXiv ids", file=sys.stderr)

    def pdf_job(e):
        if e.get("file"):
            fetch_pdf(e.key, e.get("file"))

    with ThreadPoolExecutor(6) as ex:
        list(ex.map(pdf_job, with_abs))
    print("pdfs done", file=sys.stderr)

    def api_job(kind):
        fn = fetch_crossref if kind == "crossref" else fetch_openalex
        for i, d in enumerate(dois):
            r = fn(d)
            if r.get("status") not in (200, 404):
                print(f"{kind} {d}: {r.get('status')}", file=sys.stderr)
        print(f"{kind} done", file=sys.stderr)

    threads = [threading.Thread(target=api_job, args=(k,)) for k in ("crossref", "openalex")]
    threads.append(threading.Thread(target=fetch_arxiv, args=(aids,)))
    for t in threads:
        t.start()
    for t in threads:
        t.join()


# --------------------------------------------------------------------------
# Scoring and classification
# --------------------------------------------------------------------------

T_HI = 0.90  # bib abstract found in a reference text: verbatim
T_LO = 0.30  # below this against every source: clear miss
T_REV = 0.85  # reference abstract covered by bib abstract: complete
T_TITLE = 0.6  # API title vs bib title
T_AGREE = 0.80  # API abstract found in PDF full text (when PDF text is usable)
T_GAP = 6  # a verified abstract may not have a run this long missing from a "verbatim" bib
T_ALIGN = 0.97  # letter alignment with the PDF window: verbatim despite extraction damage

END_WORDS = {
    "keywords", "keyword", "key", "index", "ccs", "categories", "general", "additional", "acm",
    "introduction", "1", "i", "permission", "copyright", "c", "author", "authors", "received",
    "citation", "doi", "http", "https", "www", "corresponding", "email", "e", "references",
    "subject", "jel", "contents", "table", "preprint", "submitted", "published", "proceedings",
    "background", "motivation", "overview", "a", "1introduction", "th", "rd", "nd", "st",
}


def usable_text(t):
    if not t:
        return False
    toks = re.findall(r"[A-Za-z]{2,}", t)
    return len(toks) > 300


_DICT = None


def dict_word(w):
    """True if w (or w minus a common suffix) is in the system word list."""
    global _DICT
    if _DICT is None:
        try:
            with open("/usr/share/dict/words") as f:
                _DICT = {x.strip().lower() for x in f}
        except OSError:
            _DICT = set()
    w = w.lower()
    if w in _DICT:
        return True
    for suf in ("s", "es", "ed", "d", "ing", "ly", "er", "ers", "ies", "al", "ity", "ment", "ments", "ness"):
        if w.endswith(suf) and len(w) - len(suf) >= 3:
            stem = w[: -len(suf)]
            if stem in _DICT or stem + "e" in _DICT or stem + "y" in _DICT:
                return True
    return False


def clean_pdf_cut(cut, full):
    """Turn a pdftotext abstract cut into a plain paragraph.

    Line-broken hyphens are rejoined only when the joined word occurs elsewhere
    in the PDF text and the hyphenated form does not; otherwise the hyphen stays.
    """
    if not cut:
        return None
    for k, v in LIGATURES.items():
        cut = cut.replace(k, v)
    words_full = full.lower()

    def join(m):
        a, b = m.group(1), m.group(2)
        joined, hyph = (a + b).lower(), (a + "-" + b).lower()
        jw = len(re.findall(r"\b" + re.escape(joined) + r"\b", words_full))
        hw = len(re.findall(r"\b" + re.escape(hyph) + r"\b", words_full))
        if hw > 0 and jw == 0:
            return a + "-" + b
        if jw > 0:
            return a + b
        if dict_word(joined):
            return a + b
        if dict_word(a) and dict_word(b):
            return a + "-" + b
        return a + b if b[:1].islower() else a + "-" + b

    cut = re.sub(r"(\w+)-\s*\n\s*(\w+)", join, cut)
    cut = cut.replace("\u00ad", "")
    cut = re.sub(r"[-\u2010\u2011]{2,}", "-", cut)
    cut = cut.replace("\u2018\u2018", "\u201c").replace("\u2019\u2019", "\u201d")
    cut = re.sub(r"\s+", " ", cut).strip()

    def split_joined(m):
        # "contextaware" -> "context-aware" when the PDF itself spells it that way elsewhere
        w = m.group(0)
        if dict_word(w):
            return w
        for i in range(2, len(w) - 1):
            a, b = w[:i], w[i:]
            if re.search(r"\b" + re.escape(a + "-" + b) + r"\b", full, re.I):
                return a + "-" + b
        return w

    cut = re.sub(r"\b[a-z]{5,}\b", split_joined, cut)
    return strip_boilerplate(cut)


BOILER_TAIL = re.compile(
    r"\s*(?:\u00a9|\(c\)\s*\d{4}|\u00d3\s*\d{4}|[\x02\x03]\s*\d{4}|Copyright\b|Received:?\s+(?:\d|[A-Z][a-z]+ \d)"
    r"|Article history|All rights reserved).*$",
    re.S,
)


def strip_boilerplate(t):
    """Drop a trailing publisher copyright or received/accepted block."""
    m = BOILER_TAIL.search(t)
    if m and m.start() > 0.6 * len(t) and len(re.findall(r"\w+", t[: m.start()])) >= 40:
        return t[: m.start()].rstrip()
    return t


JUNK = re.compile(
    r"requests? for reprints|this (?:research|work|study) was (?:supported|funded)|correspondence (?:concerning|should)"
    r"|\u00a9|\(c\) \d{4}|copyright|all rights reserved|published by elsevier|key\s?words?\s*[:\u2014-]|https?://doi\.org"
    r"|e-?mail\s*:|received \w+ \d{1,2},? \d{4}|graphical abstract|\[\d+(?:\s*[,\u2013-]\s*\d+)*\]",
    re.I,
)


def hyphen_artifacts(t, full):
    """Hyphenated words in t that the PDF text shows only as a line-break split."""
    out = []
    for m in re.finditer(r"\b([a-z]+)-([a-z]+)\b", t):
        a, b = m.group(1), m.group(2)
        if re.search(re.escape(a) + r"-\s*\n\s*" + re.escape(b) + r"\b", full) and not re.search(
            r"\b" + re.escape(a) + "-" + re.escape(b) + r"\b", full
        ):
            out.append(m.group(0))
    return out


def ends_cleanly(t):
    t = t.rstrip()
    return bool(re.search(r"[.?!)\]\"'\u201d\u2019]$", t)) and not re.search(r"(\.\.\.|\u2026)$", t)


def trustworthy_api(abs_text):
    if not abs_text:
        return False
    n = len(normalize(abs_text))
    return 40 <= n <= 1200 and not JUNK.search(abs_text)


def trustworthy_pdf_cut(cut):
    if not cut:
        return False
    toks = normalize(cut)
    if not (60 <= len(toks) <= 700):
        return False
    if not re.match(r"^[A-Z0-9\"'\u201c(]", cut):
        return False
    if not re.search(r"(?:[.?!)\]\"\u201d\u2019]|https?://\S+|\w\.\w+/\S+)$", cut):
        return False
    if re.match(r"(?i)key\s?words?\b|index terms\b", cut):
        return False
    bad = (
        r"@|\u00a9|copyright|permission to make|all rights reserved|e-?mail\b"
        r"|\b(?:department|dept\.?|school|faculty|institute) of [A-Z]"
        r"|\[\d+(?:\s*[,\u2013-]\s*\d+)*\]|\bhistory:|supplemental material:|funding:|licensed under"
    )
    if re.search(bad, cut, re.I):
        return False
    return True


def gaps(a, ref, n=5):
    """Runs of ref's tokens not covered by any n-gram shared with a.
    Returns [(start, length, 'start'|'mid'|'end', preview)]."""
    rt, ag = normalize(ref), grams(normalize(a), n)
    cov = [False] * len(rt)
    for i in range(len(rt) - n + 1):
        if tuple(rt[i : i + n]) in ag:
            for j in range(i, i + n):
                cov[j] = True
    runs, i = [], 0
    while i < len(rt):
        if cov[i]:
            i += 1
            continue
        j = i
        while j < len(rt) and not cov[j]:
            j += 1
        where = "start" if i == 0 else ("end" if j == len(rt) else "mid")
        runs.append((i, j - i, where, " ".join(rt[i:j][:10])))
        i = j
    return runs


def continuation_ok(bib_toks, full_toks):
    """True if, in the PDF text, the bib abstract is followed by a heading-like token
    (no truncation), or if we cannot locate it."""
    if len(bib_toks) < 10 or not full_toks:
        return None
    tail = tuple(bib_toks[-5:])
    n = len(full_toks)
    hits = [i for i in range(n - 4) if tuple(full_toks[i : i + 5]) == tail]
    if not hits:
        return None
    head = tuple(bib_toks[:5])
    starts = [i for i in range(n - 4) if tuple(full_toks[i : i + 5]) == head]
    if starts:
        hits = [h for h in hits if h >= starts[0]] or hits
    i = hits[0] + 5
    nxt = full_toks[i : i + 3]
    return bool(nxt) and nxt[0] in END_WORDS


def score_entry(e):
    bib_abs = e.get("abstract")
    bib_toks = normalize(bib_abs)
    bg = grams(bib_toks)
    rec = {
        "key": e.key,
        "type": e.etype,
        "doi": e.get("doi") or "",
        "file": e.get("file") or "",
        "bib_words": len(bib_toks),
        "bib_head": re.sub(r"\s+", " ", bib_abs)[:120],
        "sources": {},
        "flags": [],
    }
    refs = {}  # name -> abstract text
    full_text = None

    doi, doi_how = entry_doi(e)
    if doi_how == "title-search":
        rec["doi_found"] = doi
    if doi:
        for kind, fn in (("crossref", fetch_crossref), ("openalex", fetch_openalex)):
            p = cache_path(kind, doi.lower())
            if not os.path.exists(p):
                rec["sources"][kind] = {"status": "uncached"}
                continue
            r = fn(doi)
            s = {"status": r.get("status")}
            if r.get("title"):
                s["title_sim"] = round(title_sim(r["title"], e.get("title")), 2)
            if r.get("abstract"):
                s["words"] = len(normalize(r["abstract"]))
                refs[kind] = (r["abstract"], s)
            rec["sources"][kind] = s
    aid = arxiv_id(e)
    if aid:
        p = cache_path("arxiv", aid)
        if os.path.exists(p):
            r = json.load(open(p, encoding="utf-8"))
            s = {"status": r.get("status"), "id": aid}
            if r.get("title"):
                s["title_sim"] = round(title_sim(r["title"], e.get("title")), 2)
            if r.get("abstract"):
                s["words"] = len(normalize(r["abstract"]))
                refs["arxiv"] = (r["abstract"], s)
            rec["sources"]["arxiv"] = s
    if e.get("file"):
        p = cache_path("pdf", e.key)
        if os.path.exists(p):
            r = json.load(open(p, encoding="utf-8"))
            s = {"status": r.get("status"), "pages": r.get("pages")}
            if r.get("status") == 200:
                full_text = r.get("full") or ""
                s["usable"] = usable_text(full_text)
                s["chars"] = len(full_text)
                s["title_in_head"] = round(
                    containment(set(normalize(e.get("title") or "")), set(normalize((r.get("head") or "")[:4000]))), 2
                )
                s["ocr"] = r.get("ocr", False)
                cands = [(t, h) for t, h in pdf_cuts(r.get("head") or "")]
                cands += [(t, h + "(wide)") for t, h in pdf_cuts(r.get("wide") or "")]
                cands = [(clean_pdf_cut(t, full_text), h) for t, h in cands]
                good = [c for c in cands if trustworthy_pdf_cut(c[0]) and "+prose" not in c[1]]
                if good or cands:
                    cut, how = (good or cands)[0]
                    s["cut_how"] = how
                    s["cut_words"] = len(normalize(cut))
                    s["cut_trust"] = trustworthy_pdf_cut(cut)
                    refs["pdf_cut"] = (cut, s)
                s["has_abstract_word"] = bool(
                    re.search(r"(?im)^[ \t\f]*(?:a\s?b\s?s\s?t\s?r\s?a\s?c\s?t|summary)\b", (r.get("wide") or ""))
                )
            rec["sources"]["pdf"] = s

    full_toks = normalize(full_text) if full_text else []
    full_g = grams(full_toks)
    pdf_usable = bool(full_text) and usable_text(full_text)

    # forward scores: bib abstract inside each reference
    fwd = {}
    for name, (txt, s) in refs.items():
        if name == "pdf_cut":
            continue
        fwd[name] = containment(bg, grams(normalize(txt)))
    if full_text:
        fwd["pdf_full"] = containment(bg, full_g)
    if "pdf_cut" in refs:
        fwd["pdf_cut"] = containment(bg, grams(normalize(refs["pdf_cut"][0])))
    rec["fwd"] = {k: round(v, 3) for k, v in fwd.items()}

    # reverse scores: each reference abstract covered by the bib abstract (completeness)
    rev = {}
    agree = {}
    for name, (txt, s) in refs.items():
        rg = grams(normalize(txt))
        rev[name] = containment(rg, bg)
        if pdf_usable and name != "pdf_cut":
            agree[name] = containment(rg, full_g)
    rec["rev"] = {k: round(v, 3) for k, v in rev.items()}
    rec["api_in_pdf"] = {k: round(v, 3) for k, v in agree.items()}

    # alignment against the PDF text tolerates extraction damage but not edits
    align = align_ratio(bib_abs, full_text) if pdf_usable else 0.0
    rec["align_pdf"] = round(align, 3)

    best = max(fwd.values()) if fwd else 0.0
    rec["best"] = round(best, 3)
    rec["best_src"] = max(fwd, key=fwd.get) if fwd else ""

    pdf_s = rec["sources"].get("pdf", {})
    cut_trusted = "pdf_cut" in refs and pdf_s.get("cut_trust") and pdf_s.get("title_in_head", 0) >= 0.8

    def api_ok(name):
        """An API abstract is used only if it is the right work, clean, and complete."""
        txt, s = refs[name]
        problems = []
        if s.get("title_sim", 1.0) < T_TITLE:
            problems.append("title")
        if not trustworthy_api(txt):
            problems.append("junk-or-length")
        if full_text:
            arts = hyphen_artifacts(txt, full_text)
            if arts:
                problems.append("hyphenation:" + ",".join(arts[:3]))
        if pdf_usable:
            if agree.get(name, 0) < T_AGREE:
                problems.append(f"not-in-pdf:{agree.get(name, 0):.2f}")
            xg = grams(normalize(txt))
            if cut_trusted:
                cg = grams(normalize(refs["pdf_cut"][0]))
                x_in_cut, cut_in_x = containment(xg, cg), containment(cg, xg)
                if x_in_cut < T_REV:
                    problems.append(f"beyond-pdf-abstract:{x_in_cut:.2f}")
                complete = cut_in_x >= T_REV or continuation_ok(normalize(txt), full_toks)
            else:
                complete = ends_cleanly(txt)
            if not complete:
                problems.append("maybe-truncated")
        elif not ends_cleanly(txt):
            problems.append("maybe-truncated")
        s["problems"] = problems
        return not problems

    api_pref = [n for n in ("crossref", "arxiv", "openalex") if n in refs]
    good_api = [n for n in api_pref if api_ok(n)]
    for n in api_pref:
        if n not in good_api:
            rec["flags"].append(f"untrusted:{n}")

    # the replacement we would use: trusted API abstract first, then a clean PDF cut
    cand = None
    if good_api:
        cand = good_api[0]
    elif cut_trusted and pdf_usable:
        cand = "pdf_cut"
    if cand:
        rec["candidate_src"], rec["candidate"] = cand, refs[cand][0]

    verbatim_api = [n for n in api_pref if fwd[n] >= T_HI]
    verbatim_pdf = pdf_usable and (fwd.get("pdf_full", 0) >= T_HI or align >= T_ALIGN)

    action, why, use = None, "", None
    if verbatim_api:
        comp = max(rev[n] for n in verbatim_api)
        trusted = [n for n in good_api if n in verbatim_api]
        if trusted:
            # complete means no run of T_GAP+ words of a verified abstract is missing
            missing = {n: [g for g in gaps(bib_abs, refs[n][0]) if g[1] >= T_GAP] for n in trusted}
            rec["missing"] = {n: [f"{g[2]}:{g[3]}" for g in v] for n, v in missing.items() if v}
            whole = [n for n in trusted if not missing[n]]
            cut_whole = (
                cut_trusted
                and fwd.get("pdf_cut", 0) >= T_HI
                and not [g for g in gaps(bib_abs, refs["pdf_cut"][0]) if g[1] >= T_GAP and g[2] != "start"]
            )
            if whole:
                action, why = "VERBATIM", f"in {','.join(verbatim_api)}; nothing missing from {whole[0]}"
            elif cut_whole:
                action, why = "VERBATIM", f"nothing missing from the PDF abstract; {trusted[0]} has another version"
            else:
                n = trusted[0]
                where = ",".join(sorted({g[2] for g in missing[n]}))
                action, why, use = "REPLACE", f"incomplete: {where} of {n} missing", n
        elif comp >= T_REV:
            action, why = "VERBATIM", f"in untrusted {','.join(verbatim_api)}; covers {comp:.2f} of it"
        else:
            action, why = "REVIEW", f"in untrusted {','.join(verbatim_api)}; covers {comp:.2f} of it"
    elif verbatim_pdf:
        cut_ok = (
            "pdf_cut" in refs
            and fwd.get("pdf_cut", 0) >= T_HI
            and not [g for g in gaps(bib_abs, refs["pdf_cut"][0]) if g[1] >= T_GAP and g[2] != "start"]
        )
        cont = continuation_ok(bib_toks, full_toks)
        rec["continues_to_heading"] = cont
        differs = f"; {good_api[0]} has another version" if good_api else ""
        # a cut-short copy: the bib text sits inside a clean PDF abstract that goes on
        cut_missing = []
        if cut_trusted and fwd.get("pdf_cut", 0) >= T_HI:
            cut_missing = [g for g in gaps(bib_abs, refs["pdf_cut"][0]) if g[1] >= T_GAP and g[2] != "start"]
        if cut_ok:
            action, why = "VERBATIM", "in PDF; covers the PDF abstract cut" + differs
        elif cont:
            action, why = "VERBATIM", "in PDF; followed by a heading" + differs
        elif cut_missing:
            where = ",".join(sorted({g[2] for g in cut_missing}))
            action, why, use = "REPLACE", f"incomplete: {where} of the PDF abstract missing", "pdf_cut"
        else:
            action, why = "REVIEW", "in PDF; completeness unclear" + differs
    elif best < T_LO:
        if cand:
            action, why, use = "REPLACE", f"paraphrase; {cand} abstract", cand
        elif api_pref:
            action, why = "REVIEW", "paraphrase; API abstract untrusted"
        elif pdf_usable and not pdf_s.get("has_abstract_word"):
            if pdf_s.get("title_in_head", 0) >= 0.5:
                action, why = "DELETE", "paraphrase; source has no abstract"
            else:
                action, why = "REVIEW", "paraphrase; PDF title mismatch"
        elif pdf_usable:
            action, why = "REVIEW", "paraphrase; PDF has an Abstract heading but no clean cut"
        else:
            action, why = "REVIEW", "paraphrase?; no usable reference text"
    elif good_api:
        # a clean, verified abstract exists and the bib fails both T_HI against it and
        # T_ALIGN against the PDF: an edited or condensed copy, not extraction noise
        action, why, use = "REPLACE", f"edited (best {best:.2f}); {good_api[0]} abstract", good_api[0]
    else:
        action, why = "REVIEW", f"middle score {best:.2f}"
    replacement = (refs[use][0], use) if use else None

    rec["action"], rec["why"] = action, why
    if replacement:
        rec["replacement"], rec["replacement_src"] = replacement
    return rec


def abstract_sha(value):
    import hashlib

    return hashlib.sha1(re.sub(r"\s+", " ", value).strip().encode("utf-8")).hexdigest()


def load_reviewed():
    if os.path.exists(REVIEWED):
        with open(REVIEWED, encoding="utf-8") as f:
            return json.load(f)
    return {}


def cmd_score(bib, keys=None):
    text, entries = load(bib)
    reviewed = load_reviewed()
    recs = []
    for e in entries:
        if "abstract" not in e.fields or (keys and e.key not in keys):
            continue
        r = score_entry(e)
        hand = reviewed.get(e.key)
        if hand and r["action"] != "VERBATIM" and hand["sha1"] == abstract_sha(e.get("abstract")):
            r["auto_action"], r["auto_why"] = r["action"], r["why"]
            r["action"], r["why"] = "VERBATIM", "hand-checked: " + hand["note"]
            r.pop("replacement", None)
        recs.append(r)
    if keys and len(recs) < len(keys):
        found = {r["key"] for r in recs}
        print("no abstract in bib (nothing to audit): " + ", ".join(sorted(set(keys) - found)))
    os.makedirs(STATE, exist_ok=True)
    with open(REPORT, "w", encoding="utf-8") as f:
        json.dump(recs, f, ensure_ascii=False, indent=1)
    cols = ["key", "action", "best", "best_src", "align_pdf", "fwd", "rev", "api_in_pdf", "why", "flags", "candidate_src", "sources", "bib_head"]
    with open(REPORT[:-5] + ".tsv", "w", encoding="utf-8") as f:
        f.write("\t".join(cols) + "\n")
        for r in recs:
            row = []
            for c in cols:
                v = r.get(c, "")
                if isinstance(v, (dict, list)):
                    v = json.dumps(v, ensure_ascii=False, separators=(",", ":"))
                row.append(str(v).replace("\t", " ").replace("\n", " "))
            f.write("\t".join(row) + "\n")
    counts = {}
    for r in recs:
        counts[r["action"]] = counts.get(r["action"], 0) + 1
    print(json.dumps(counts))
    for r in recs:
        if keys or r["action"] != "VERBATIM":
            src = f" -> {r['replacement_src']}" if r.get("replacement_src") else ""
            print(f"{r['action']:8s} {r['key']}: {r['why']}{src}")
    print(f"report: {REPORT[:-5]}.tsv")


# --------------------------------------------------------------------------
# Applying changes
# --------------------------------------------------------------------------


def bib_escape(s):
    """Make plain text safe inside a braced BibTeX value without changing its words."""
    s = re.sub(r"\s+", " ", s).strip()
    # keep braces balanced: drop any unmatched ones
    out, depth = [], 0
    for c in s:
        if c == "{":
            depth += 1
        elif c == "}":
            if depth == 0:
                continue
            depth -= 1
        out.append(c)
    s = "".join(out)
    while depth > 0:
        i = s.rfind("{")
        s = s[:i] + s[i + 1 :]
        depth -= 1
    return s


def source_text(e, spec):
    """Text named by an override: {"from": "crossref"|"openalex"|"arxiv"|"cut:N"|"span",
    "start": ..., "end": ..., "edits": [[old, new], ...]}. "span" takes the PDF text
    from the first occurrence of `start` through the following `end`, inclusive,
    cleaned like a cut. Each edit must match exactly once."""
    src = spec["from"]
    doi = entry_doi(e)[0]
    if src == "crossref":
        t = fetch_crossref(doi)["abstract"]
    elif src == "openalex":
        t = fetch_openalex(doi)["abstract"]
    elif src == "arxiv":
        with open(cache_path("arxiv", arxiv_id(e)), encoding="utf-8") as f:
            t = json.load(f)["abstract"]
    else:
        with open(cache_path("pdf", e.key), encoding="utf-8") as f:
            d = json.load(f)
        if src.startswith("cut:"):
            cands = pdf_cuts(d.get("head") or "") + pdf_cuts(d.get("wide") or "")
            t = clean_pdf_cut(cands[int(src[4:])][0], d["full"])
        elif src == "span":
            full = d["full"]
            for k, v in LIGATURES.items():
                full = full.replace(k, v)
            loose = lambda x: re.compile(r"\s+".join(map(re.escape, x.split())))
            ms = loose(spec["start"]).search(full)
            me = ms and loose(spec["end"]).search(full, ms.start())
            if not (ms and me) or me.end() - ms.start() > 6000:
                raise SystemExit(f"span anchors not found (or span too long) in {e.key}")
            t = clean_pdf_cut(full[ms.start() : me.end()], full)
        else:
            raise SystemExit(f"unknown source {src} for {e.key}")
    if spec.get("drop_from"):
        # drop a trailing non-abstract block (funding, supplement, license lines)
        if t.count(spec["drop_from"]) != 1:
            raise SystemExit(f"drop_from marker matches {t.count(spec['drop_from'])} times in {e.key}")
        t = t[: t.index(spec["drop_from"])].rstrip()
    for old, new in spec.get("edits", []):
        if t.count(old) != 1:
            raise SystemExit(f"edit {old!r} matches {t.count(old)} times in {e.key}")
        t = t.replace(old, new)
    return t


def cmd_apply(bib, overrides=None, dry=False, keys=None):
    text, entries = load(bib)
    with open(REPORT, encoding="utf-8") as f:
        recs = {r["key"]: r for r in json.load(f)}
    manual = {}
    if overrides:
        with open(overrides, encoding="utf-8") as f:
            manual = json.load(f)  # key -> {"action": "KEEP"|"DELETE"|"REPLACE", "from": ..., "note": ...}
    edits = []  # (start, end, new)
    summary, unresolved = {}, []
    for e in entries:
        f = e.fields.get("abstract")
        if not f:
            continue
        if keys and e.key not in keys:
            continue
        r = recs.get(e.key)
        if r is None:
            if keys:
                raise SystemExit(f"{e.key} is not in the report; rerun score with the same --keys")
            continue  # the report covers only some entries (score --keys)
        if r["bib_head"] != re.sub(r"\s+", " ", f.value)[:120]:
            raise SystemExit(f"report is stale for {e.key}; rerun score")
        if e.key in manual:
            m = manual[e.key]
            act = m["action"]
            new = source_text(e, m) if act == "REPLACE" else None
            tag = act + "(manual)"
        elif r["action"] in ("REPLACE", "DELETE"):
            act, new, tag = r["action"], r.get("replacement"), r["action"]
        elif r["action"] == "REVIEW":
            unresolved.append(e.key)
            continue
        else:
            continue
        if act == "DELETE":
            edits.append((f.line_start, f.line_end, ""))
        elif act == "REPLACE":
            edits.append((f.vstart, f.vend, bib_escape(new)))
        elif act != "KEEP":
            raise SystemExit(f"bad action {act} for {e.key}")
        summary[tag] = summary.get(tag, 0) + 1
    if unresolved:
        raise SystemExit(f"{len(unresolved)} REVIEW entries without an override: {unresolved}")
    for s_, t_, new in sorted(edits, reverse=True):
        text = text[:s_] + new + text[t_:]
    print(json.dumps(summary, indent=1))
    if not dry:
        with open(bib, "w", encoding="utf-8") as fh:
            fh.write(text)


def cmd_preview(bib, overrides, key):
    """Print the text an override would write."""
    text, entries = load(bib)
    with open(overrides, encoding="utf-8") as f:
        m = json.load(f)[key]
    e = [x for x in entries if x.key == key][0]
    print(m["action"], m.get("note", ""))
    if m["action"] == "REPLACE":
        print(bib_escape(source_text(e, m)))


def cmd_show(bib, key, n=4000):
    """Print the bib abstract next to every source and PDF cut, for REVIEW."""
    text, entries = load(bib)
    e = [x for x in entries if x.key == key][0]
    recs = {r["key"]: r for r in json.load(open(REPORT, encoding="utf-8"))} if os.path.exists(REPORT) else {}
    r = recs.get(key, {})
    print(f"== {key} [{e.etype}] {r.get('action', '?')}: {r.get('why', 'not scored')}")
    print("title:", e.get("title"), "|", e.get("journal") or e.get("booktitle") or e.get("publisher"), e.get("year"))
    print("file:", e.get("file"), " doi:", e.get("doi"), (" doi by title: " + r["doi_found"]) if r.get("doi_found") else "")
    print("\nBIB:", re.sub(r"\s+", " ", e.get("abstract") or "(none)"))
    doi = entry_doi(e)[0]
    if doi:
        for k, fn in (("crossref", fetch_crossref), ("openalex", fetch_openalex)):
            t = fn(doi).get("abstract")
            if t:
                print(f"\n{k.upper()}:", t[:n])
    aid = arxiv_id(e)
    if aid and os.path.exists(cache_path("arxiv", aid)):
        print("\nARXIV:", json.load(open(cache_path("arxiv", aid), encoding="utf-8")).get("abstract", "")[:n])
    p = cache_path("pdf", key)
    if os.path.exists(p):
        d = json.load(open(p, encoding="utf-8"))
        cands = pdf_cuts(d.get("head") or "") + pdf_cuts(d.get("wide") or "")
        for i, (t, h) in enumerate(cands):
            print(f"\nCUT {i} [{h}]:", clean_pdf_cut(t, d["full"])[:n])
        if not cands:
            print("\nPDF: no cut found; read the first pages of", e.get("file"))


def cmd_reviewed(bib, key, note):
    """Record that KEY's current abstract was checked against the page by hand,
    so later full audits report it as VERBATIM until the text changes."""
    text, entries = load(bib)
    e = [x for x in entries if x.key == key][0]
    d = load_reviewed()
    d[key] = {"sha1": abstract_sha(e.get("abstract")), "note": note, "date": time.strftime("%Y-%m-%d")}
    os.makedirs(STATE, exist_ok=True)
    with open(REVIEWED, "w", encoding="utf-8") as f:
        json.dump(d, f, ensure_ascii=False, indent=1)
    print(f"recorded {key} as hand-checked")


def _opt(args, name):
    if name in args:
        i = args.index(name)
        v = args[i + 1]
        del args[i : i + 2]
        return v
    return None


if __name__ == "__main__":
    args = sys.argv[1:]
    if len(args) < 2:
        raise SystemExit(__doc__)
    keys = _opt(args, "--keys")
    keys = {k.strip() for k in keys.split(",") if k.strip()} if keys else None
    ov = _opt(args, "--overrides")
    dry = "--dry-run" in args
    args = [a for a in args if a != "--dry-run"]
    cmd, bib = args[0], os.path.expanduser(args[1])
    if cmd == "fetch":
        cmd_fetch(bib, keys)
    elif cmd == "score":
        cmd_score(bib, keys)
    elif cmd == "apply":
        cmd_apply(bib, ov, dry, keys)
    elif cmd == "preview":
        cmd_preview(bib, os.path.expanduser(args[2]), args[3])
    elif cmd == "show":
        cmd_show(bib, args[2])
    elif cmd == "reviewed":
        cmd_reviewed(bib, args[2], " ".join(args[3:]))
    else:
        raise SystemExit(__doc__)
