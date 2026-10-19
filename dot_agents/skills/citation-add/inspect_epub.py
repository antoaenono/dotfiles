#!/usr/bin/env python3
"""Read EPUB package metadata and, optionally, its spine text using the standard library."""
import argparse
from html.parser import HTMLParser
import json
from pathlib import Path
import posixpath
from urllib.parse import unquote
import xml.etree.ElementTree as ET
from zipfile import ZipFile


class Text(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.parts = []
        self.hidden = 0

    def handle_starttag(self, tag, attrs):
        if tag in {"script", "style"}:
            self.hidden += 1

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.hidden = max(0, self.hidden - 1)
        if tag in {"p", "div", "li", "h1", "h2", "h3", "section", "br"}:
            self.parts.append("\n")

    def handle_data(self, data):
        if not self.hidden:
            self.parts.append(data)


def inspect(path, include_text=False):
    with ZipFile(Path(path).expanduser()) as archive:
        container = ET.fromstring(archive.read("META-INF/container.xml"))
        rootfile = container.find(".//{*}rootfile")
        if rootfile is None or not rootfile.get("full-path"):
            raise ValueError("EPUB has no package document")
        package_path = rootfile.get("full-path")
        package = ET.fromstring(archive.read(package_path))
        metadata = package.find("{*}metadata")
        result = {"format": "epub", "metadata": {}}
        if metadata is not None:
            for item in metadata:
                if item.tag.startswith("{http://purl.org/dc/elements/1.1/}"):
                    result["metadata"].setdefault(item.tag.rsplit("}", 1)[1], []).append("".join(item.itertext()).strip())
        if include_text:
            manifest = {item.get("id"): item for item in package.findall("{*}manifest/{*}item")}
            result["sections"] = []
            for reference in package.findall("{*}spine/{*}itemref"):
                item = manifest.get(reference.get("idref"))
                if item is None or item.get("media-type") not in {"application/xhtml+xml", "text/html"}:
                    continue
                filename = posixpath.normpath(posixpath.join(posixpath.dirname(package_path), unquote(item.get("href").split("#", 1)[0])))
                reader = Text()
                reader.feed(archive.read(filename).decode("utf-8-sig"))
                result["sections"].append({"file": filename, "text": "".join(reader.parts).strip()})
        return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", type=Path)
    parser.add_argument("--text", action="store_true", help="Include XHTML content in spine order")
    args = parser.parse_args()
    print(json.dumps(inspect(args.file, args.text), ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
