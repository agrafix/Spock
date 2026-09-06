#!/usr/bin/env python3
"""Check local HTML links and representative request-action anchors in Haddocks."""

import argparse
import json
from html.parser import HTMLParser
from pathlib import Path
import re
from urllib.parse import unquote, urlsplit


class Links(HTMLParser):
    def __init__(self, source):
        super().__init__()
        self.links = []
        self.anchors = set()
        self.feed(source)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.anchors.add(attrs["id"])
        if tag == "a":
            if "name" in attrs:
                self.anchors.add(attrs["name"])
            if "href" in attrs:
                self.links.append(attrs["href"])


def check(root, current=False):
    root = root.resolve()
    if current:
        versions = json.loads((root / "versions.json").read_text())
        repository = Path(__file__).resolve().parents[1]
        for package, version in versions.items():
            cabal = repository / package / (package + ".cabal")
            expected = re.search(r"^version:\s*(\S+)", cabal.read_text(), re.M)[1]
            if version != expected:
                raise AssertionError(f"Refresh {package} reference: published {version}, source {expected}")
    documents = {path: Links(path.read_text()) for path in root.rglob("*.html")}
    if not documents:
        raise AssertionError(f"No HTML documentation under {root}")
    failures = []
    versions = json.loads((root / "versions.json").read_text())
    client_version = versions.get("Spock-api-ghcjs")
    if not client_version:
        failures.append("Missing Spock-api-ghcjs from the published reference")
    else:
        client = root / ("Spock-api-ghcjs-" + client_version)
        expected = {"Web-Spock-Api-Client.html": ["callEndpoint", "callDocumentedEndpoint", "newClient"],
                    "Web-Spock-Api-Client-Browser.html": ["browserClient"]}
        for name, symbols in expected.items():
            page = documents.get(client / name)
            if page is None or any("v:" + symbol not in page.anchors for symbol in symbols):
                failures.append(f"Missing browser API page or anchors: {name}")
        info = client / "build-info.json"
        if not info.is_file() or json.loads(info.read_text()).get("target") != "javascript":
            failures.append("Client reference must be built from JavaScript interfaces")
        page = documents.get(client / "Web-Spock-Api-Client.html")
        if page and not any("Spock-api-" + versions["Spock-api"] + "/Web-Spock-Api.html#t:Endpoint" in href
                            and not urlsplit(href).scheme for href in page.links):
            failures.append("Missing local browser-client link to the shared Endpoint type")
    for source, document in documents.items():
        for href in document.links:
            url = urlsplit(href)
            if url.scheme == "file":
                failures.append(f"{source.name}: local filesystem URL {href}")
                continue
            if url.scheme or url.netloc or url.path.startswith("/"):
                continue
            target = (source.parent / unquote(url.path)).resolve() if url.path else source
            if target.is_dir():
                target /= "index.html"
            if not target.exists():
                failures.append(f"{source.relative_to(root)}: missing {href}")
            elif url.fragment and target in documents and unquote(url.fragment) not in documents[target].anchors:
                failures.append(f"{source.relative_to(root)}: missing anchor {href}")
    actions = [p for p in documents if p.name == "Web-Spock-Action.html"]
    if not actions:
        failures.append("Missing Web.Spock.Action page")
    for path in actions:
        for symbol in ["param", "params", "jsonBody", "body", "header", "setHeader", "text", "filesMulti"]:
            if "v:" + symbol not in documents[path].anchors:
                failures.append(f"{path}: missing public action {symbol}")
    for path, document in documents.items():
        if path.name == "Web-Spock.html" and not any(
                "Web-Spock-Action.html" in href and not urlsplit(href).scheme for href in document.links):
            failures.append(f"{path}: missing local cross-package action link")
    if failures:
        raise AssertionError("\n".join(failures))
    print(f"Checked {len(documents)} HTML pages, local links, and public request-action anchors")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--current", action="store_true", help="Require versions matching the checkout")
    args = parser.parse_args()
    check(args.directory, args.current)
