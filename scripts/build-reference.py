#!/usr/bin/env python3
"""Build linked Haddocks for native libraries, session backends and the JavaScript client."""

import argparse
import html
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
from urllib.parse import unquote, urlsplit

ROOT = Path(__file__).resolve().parents[1]
NATIVE_PACKAGES = ["reroute", "Spock-core", "Spock", "Spock-api", "Spock-api-server", "Spock-session-postgresql", "Spock-session-cookie"]
PACKAGES = NATIVE_PACKAGES + ["Spock-api-ghcjs"]


def javascript_library():
    haddock = shutil.which("javascript-unknown-ghcjs-haddock")
    if not haddock:
        raise SystemExit("Run scripts/setup-javascript.py --prefix DIRECTORY --haddock, then source DIRECTORY/env.sh.")
    haddock = Path(haddock).resolve()
    env = os.environ.copy()
    # Cabal's --with-haddock flag alone does not select the tool for dependencies.
    env["PATH"] = str(haddock.parent) + os.pathsep + env["PATH"]
    builddir = ROOT / "dist-newstyle-docs-javascript"
    common = ["--project-file=cabal.project.javascript", f"--builddir={builddir}",
              "--disable-documentation", "--ghc-options=-haddock -fwrite-ide-info"]
    names = ["reroute", "Spock-api", "Spock-api-ghcjs"]
    subprocess.run(["cabal", "build", *[name + ":lib:" + name for name in names], *common, "-j4"],
                   cwd=ROOT, env=env, check=True)
    plan = json.loads((builddir / "cache/plan.json").read_text())
    assert plan["arch"] == "javascript" and plan["compiler-id"] == "ghc-9.12.2", "Expected the pinned JavaScript compiler"
    libraries = {p["pkg-name"]: p for p in plan["install-plan"] if p.get("style") == "local" and p.get("component-name") == "lib"}
    # Read fresh interfaces produced by the real JS compiler. Letting host
    # Haddock recompile would assume dynamic native libraries absent in the
    # cross bindist. Build dependency interfaces first for shared-type links.
    for name in names:
        directory = Path(libraries[name]["dist-dir"]) / "build"
        options = ["--no-compilation", "--optghc=-hidir=" + str(directory),
                   "--optghc=-hiedir=" + str(directory / "extra-compilation-artifacts/hie")]
        subprocess.run(["cabal", "haddock", name, *common, f"--with-haddock={haddock}",
                        "--haddock-html", "--haddock-hyperlink-source",
                        "--haddock-html-location=https://hackage.haskell.org/package/$pkg-$version/docs",
                        *["--haddock-option=" + option for option in options]], cwd=ROOT, env=env, check=True)
    library = libraries["Spock-api-ghcjs"]
    source = Path(library["dist-dir"]) / "doc/html/Spock-api-ghcjs"
    assert (source / "Web-Spock-Api-Client-Browser.html").is_file(), "Missing JavaScript-only Browser module"
    (source / "build-info.json").write_text(json.dumps({"compiler": plan["compiler-id"], "target": plan["arch"],
        "Spock-api": libraries["Spock-api"]["pkg-version"], "source": "GHC JavaScript .hi/.hie interfaces"}, indent=2) + "\n")
    return library


def repair_fragments(text):
    # Haddock 2.32's interface mode emits an empty line number for synthesized
    # instances. Their source page exists; link to it without a bogus fragment.
    text = text.replace('.html#line-"', '.html"')
    # Haddock's source renderer percent-encodes IDs as well as URLs. Browsers
    # decode URL fragments, so keep literal IDs and encoded hrefs.
    text = re.sub(r'\b(id|name)="([^"]*)"',
                  lambda m: f'{m[1]}="{html.escape(unquote(html.unescape(m[2])), quote=True)}"', text)
    anchors = {html.unescape(value) for value in re.findall(r'\b(?:id|name)="([^"]*)"', text)}

    # Haddock 2.33 also emits local links for inherited instance methods that
    # have no local declaration. Keep the signature text and the working class
    # link above it; omit these redundant, broken self-links.
    def local_link(match):
        if unquote(html.unescape(match[2])) in anchors:
            return match[0]
        if 'class="selflink"' in match[1] + match[3]:
            return ""
        return match[4]

    return re.sub(r'<a\b([^>]*)href="#([^"]+)"([^>]*)>(.*?)</a>', local_link, text, flags=re.S)


def build(output):
    browser = javascript_library()
    # Keep optional dependencies and documentation build flags isolated from
    # the native build. Use Cabal's plan to find each local library's output.
    project = ROOT / "dist-newstyle-docs/project"
    project.mkdir(parents=True, exist_ok=True)
    (project / "cabal.project").write_text(
        "packages:\n" + "".join(f'  "{ROOT / package / (package + ".cabal")}"\n' for package in NATIVE_PACKAGES)
        + "tests: False\nbenchmarks: False\n"
        + "allow-newer: postgresql-simple:base, postgresql-simple:template-haskell\n")
    builddir = ROOT / "dist-newstyle-docs/build"
    subprocess.run(["cabal", "haddock", "all:libs", "--enable-documentation", "--haddock-html",
                    "--haddock-hyperlink-source",
                    "--haddock-html-location=https://hackage.haskell.org/package/$pkg-$version/docs",
                    f"--builddir={builddir}", "-j4"],
                   cwd=project, check=True)
    plan = json.loads((builddir / "cache/plan.json").read_text())
    libraries = {p["pkg-name"]: p for p in plan["install-plan"]
                 if p.get("style") == "local" and p.get("component-name") == "lib"}
    assert set(libraries) == set(NATIVE_PACKAGES), "Missing a local library from the documentation plan"
    libraries["Spock-api-ghcjs"] = browser
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    package_ids = {name: name + "-" + library["pkg-version"] for name, library in libraries.items()}
    for name, library in libraries.items():
        source = Path(library["dist-dir"]) / "doc/html" / name
        assert (source / "index.html").is_file(), f"Missing Haddocks for {name}"
        destination = output / package_ids[name]
        # Retain other versions already published. Replace only this package's
        # current documentation; .haddock interfaces are build artifacts.
        if destination.exists():
            shutil.rmtree(destination)
        shutil.copytree(source, destination, ignore=shutil.ignore_patterns("*.haddock"))
        shutil.copyfile(ROOT / name / "LICENSE", destination / "LICENSE")
        for path in destination.rglob("*"):
            if path.suffix not in [".html", ".json", ".js"]:
                continue
            text = path.read_text()
            for package_id in package_ids.values():
                relative = Path(os.path.relpath(output / package_id, path.parent)).as_posix()
                text = text.replace(f"https://hackage.haskell.org/package/{package_id}/docs/", relative + "/")
            if path.suffix == ".html":
                text = repair_fragments(text)
            path.write_text(text)
    # Haddock's "Defined in" links can name a hidden internal module. Its
    # hyperlinked source is available even when it has no public API page.
    for package_id in package_ids.values():
        for path in (output / package_id).rglob("*.html"):
            def definition_link(match):
                url = urlsplit(html.unescape(match[1]))
                if url.scheme or url.netloc or url.fragment or not url.path.endswith(".html"):
                    return match[0]
                target = (path.parent / unquote(url.path)).resolve()
                source = target.parent / "src" / (target.stem.replace("-", ".") + ".html")
                if not target.exists() and source.is_file():
                    return 'href="' + Path(os.path.relpath(source, path.parent)).as_posix() + '"'
                return match[0]

            path.write_text(re.sub(r'href="([^"]+)"', definition_link, path.read_text()))
    core = package_ids["Spock-core"] + "/Web-Spock-Action.html"
    entry_modules = {"Spock": "Web-Spock.html", "Spock-core": "Web-Spock-Core.html",
                     "Spock-api": "Web-Spock-Api.html", "Spock-api-server": "Web-Spock-Api-Server.html",
                     "Spock-session-postgresql": "Web-Spock-Session-Postgresql.html",
                     "Spock-session-cookie": "Web-Spock-Session-Cookie.html",
                     "Spock-api-ghcjs": "Web-Spock-Api-Client.html",
                     "reroute": "Web-Routing-Combinators.html"}
    rows = "".join(f'<tr><td><a href="{package_ids[name]}/{entry_modules[name]}">{name}</a></td>'
                   f'<td>{libraries[name]["pkg-version"]}</td>'
                   f'<td><a href="{package_ids[name]}/index.html">All modules</a></td></tr>' for name in PACKAGES)
    actions = "".join(f'<li><a href="{core}#v:{symbol}"><code>{symbol}</code></a>: {description}</li>'
                      for symbol, description in [("param", "typed query and form parameters"),
                      ("params", "all request parameters"), ("jsonBody", "JSON request bodies"),
                      ("body", "raw request bytes"), ("header", "request headers"),
                      ("filesMulti", "file uploads"), ("setHeader", "response headers"),
                      ("text", "plain-text responses")])
    (output / "index.html").write_text(f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Spock API reference</title><link rel="stylesheet" href="/css/bootstrap.min.css"></head>
<body><main class="container"><p><a href="/">Spock</a> · <a href="/tutorials/">Tutorials</a></p>
<h1>Spock API reference</h1><p>These package versions are built together from the Spock repository.
Links between them stay within this reference; available dependency documentation links to Hackage.</p>
<table class="table"><thead><tr><th>Package</th><th>Version</th><th>Modules</th></tr></thead><tbody>{rows}</tbody></table>
<h2>Haskell in the browser</h2><p>Spock-api-ghcjs {libraries['Spock-api-ghcjs']['pkg-version']} pairs with
Spock-api {libraries['Spock-api']['pkg-version']}. The client reference is generated from GHC JavaScript 9.12.2 interfaces;
the native libraries use GHC 9.14.1. Start with
<a href="{package_ids['Spock-api-ghcjs']}/Web-Spock-Api-Client.html#v:callEndpoint">callEndpoint</a> and
<a href="{package_ids['Spock-api-ghcjs']}/Web-Spock-Api-Client-Browser.html#v:browserClient">browserClient</a>,
or follow the <a href="/tutorials/browser-client">shared browser/server tutorial</a>.</p>
<h2>Handling requests and responses</h2><p><code>Web.Spock</code> reexports the
<a href="{core}">Web.Spock.Action</a> module from Spock-core. Start there for request parsing and response helpers.</p>
<ul>{actions}</ul><p>Typed path captures are arguments to your route handler.
Set status and headers before sending a response; response helpers finish the action.</p>
<p><a href="https://github.com/agrafix/Spock">Source repository</a> · Generated with Cabal and Haddock.</p>
</main></body></html>''')
    (output / "versions.json").write_text(json.dumps({name: libraries[name]["pkg-version"] for name in PACKAGES}, indent=2) + "\n")
    subprocess.run(["python3", str(ROOT / "scripts/check-reference.py"), str(output), "--current"], check=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=ROOT / "docs/reference")
    build(parser.parse_args().output)
