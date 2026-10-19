"""Behavior checks for source-library discovery, classification, and EPUBs."""
from contextlib import redirect_stdout
from io import StringIO
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
from zipfile import ZipFile

from audit_sources import audit
from clean_filenames import main as clean
from find_missing import main as missing
from inspect_epub import inspect
from source_library import attachment_paths, catalog_files, category_for, read_entries
from check_duplicate import main as duplicate


class SourcesTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.sources = self.root / "sources"
        self.sources.mkdir()
        self.bib = self.root / "references.bib"

    def call(self, function, *args):
        with patch("sys.argv", ["helper", "--root", str(self.sources), "--bib", str(self.bib), *args]), redirect_stdout(StringIO()) as output:
            function()
        return output.getvalue()

    def test_catalog_resolves_nested_fields_and_multiple_formats(self):
        (self.sources / "books").mkdir()
        pdf, epub = self.sources / "books/a.pdf", self.sources / "books/a.epub"
        pdf.touch()
        epub.touch()
        self.bib.write_text('@book{a, title={A {nested} title}, file={books/a.pdf;books/a.epub}}\n')
        entries = read_entries(self.bib)
        self.assertEqual(entries[0].get("title"), "A {nested} title")
        self.assertEqual(set(attachment_paths(entries[0], self.bib, self.sources)), {pdf.resolve(), epub.resolve()})
        self.assertEqual(len(catalog_files(entries, self.bib, self.sources)), 2)
        self.assertEqual(self.call(missing), "MISSING:\n")

    def test_discovery_compares_paths_instead_of_basenames(self):
        for folder in ("books", "papers"):
            (self.sources / folder).mkdir()
            (self.sources / folder / "same.pdf").touch()
        self.bib.write_text('@book{a, file={books/same.pdf}}\n')
        self.assertIn("MISSING:papers/same.pdf", self.call(missing))

    def test_audit_reports_broken_and_misplaced_but_keeps_inbox(self):
        (self.sources / "papers").mkdir()
        (self.sources / "papers/book.epub").touch()
        (self.sources / "incoming.epub").touch()
        (self.sources / "thesis.pdf").touch()
        self.bib.write_text('@book{b, file={papers/book.epub}}\n@article{missing, file={papers/absent.pdf}}\n@phdthesis{t, file={thesis.pdf}}\n')
        report = audit(self.sources, self.bib)
        kinds = {issue["kind"] for issue in report["issues"]}
        self.assertEqual(kinds, {"misplaced", "uncataloged", "missing-attachment", "needs-category"})
        self.assertEqual(category_for("incollection"), "books")
        self.assertEqual(category_for("inproceedings"), "papers")
        self.assertIsNone(category_for("misc"))

    def test_cleanup_preserves_known_paths_and_does_not_overwrite(self):
        (self.sources / "books").mkdir()
        known = self.sources / "books/Known -- Anna's Archive.epub"
        known.write_bytes(b"known")
        self.bib.write_text('@book{k, file={' + str(known) + '}}\n')
        incoming = self.sources / "New -- Anna's Archive.epub"
        incoming.write_bytes(b"incoming")
        existing = self.sources / "New.epub"
        existing.write_bytes(b"existing")
        self.call(clean, "--apply")
        self.assertEqual(known.read_bytes(), b"known")
        self.assertEqual(incoming.read_bytes(), b"incoming")
        self.assertEqual(existing.read_bytes(), b"existing")
        existing.unlink()
        self.call(clean, "--apply")
        self.assertFalse(incoming.exists())
        self.assertEqual(existing.read_bytes(), b"incoming")

    def test_epub_metadata_and_spine_order(self):
        epub = self.root / "book.epub"
        with ZipFile(epub, "w") as archive:
            archive.writestr("mimetype", "application/epub+zip")
            archive.writestr("META-INF/container.xml", '<container xmlns="urn:oasis:names:tc:opendocument:xmlns:container"><rootfiles><rootfile full-path="OEBPS/content.opf"/></rootfiles></container>')
            archive.writestr("OEBPS/content.opf", '<package xmlns="http://www.idpf.org/2007/opf"><metadata xmlns:dc="http://purl.org/dc/elements/1.1/"><dc:title>A Book</dc:title><dc:creator>A Writer</dc:creator><dc:identifier>urn:isbn:1234</dc:identifier><dc:description>A publisher description, not an abstract.</dc:description></metadata><manifest><item id="two" href="two.xhtml" media-type="application/xhtml+xml"/><item id="one" href="one.xhtml" media-type="application/xhtml+xml"/></manifest><spine><itemref idref="one"/><itemref idref="two"/></spine></package>')
            archive.writestr("OEBPS/one.xhtml", '<html><head><style>hidden style</style></head><body><h1>First</h1><p>A &amp; B.</p><script>hidden script</script></body></html>')
            archive.writestr("OEBPS/two.xhtml", '<html><body><p>Second section.</p></body></html>')
        metadata = inspect(epub)
        self.assertEqual(metadata["metadata"]["creator"], ["A Writer"])
        self.assertNotIn("abstract", metadata["metadata"])
        self.assertNotIn("sections", metadata)
        result = inspect(epub, True)
        self.assertEqual([section["file"] for section in result["sections"]], ["OEBPS/one.xhtml", "OEBPS/two.xhtml"])
        self.assertIn("A & B.", result["sections"][0]["text"])
        self.assertNotIn("hidden", result["sections"][0]["text"])

    def test_same_work_in_another_format_is_preserved(self):
        self.bib.write_text('@book{book, title={A Book}, doi={10.1234/example}, file={book.pdf}}\n')
        for filename, token in [('book.epub', 'ADD_FORMAT:'), ('book.pdf', 'KEEP_EXISTING:')]:
            with patch('sys.argv', ['duplicate', '--bib', str(self.bib), '--doi', '10.1234/example', '--new-file', filename]), redirect_stdout(StringIO()) as output:
                duplicate()
            self.assertTrue(output.getvalue().startswith(token))

    def test_epub_is_not_sent_to_pdf_abstract_extraction(self):
        from audit_abstracts import fetch_pdf
        epub = self.sources / 'book.epub'
        epub.touch()
        with patch('audit_abstracts.subprocess.run', side_effect=AssertionError('PDF tools must not run on EPUBs')):
            self.assertEqual(fetch_pdf('book', str(epub))['status'], 415)


if __name__ == "__main__":
    unittest.main()
