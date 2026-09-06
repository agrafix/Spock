#!/usr/bin/env python3
"""Build the native server and GHC JavaScript client into a deployable directory."""

import argparse
import json
from pathlib import Path
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[1]


def cabal(project, builddir, *args, capture=False):
    result = subprocess.run(["cabal", *args, f"--project-file={project}", f"--builddir={builddir}"],
                            cwd=ROOT, check=True, text=True, stdout=subprocess.PIPE if capture else None)
    return result.stdout.strip() if capture else None


def build(output, test=False):
    native_version = subprocess.check_output(["ghc", "--numeric-version"], text=True).strip()
    javascript_version = subprocess.check_output(["javascript-unknown-ghcjs-ghc", "--numeric-version"], text=True).strip()
    if (native_version, javascript_version) != ("9.14.1", "9.12.2"):
        raise SystemExit("Use native GHC 9.14.1 and the pinned JavaScript GHC 9.12.2.")
    js_project, js_build = "cabal.project.javascript", "dist-newstyle-javascript"
    server_project, server_build = "cabal.project.browser-server", "dist-newstyle-browser-server"
    cabal(js_project, js_build, "build", "spock-browser-client", "-j4")
    cabal(server_project, server_build, "build", "spock-browser-server:exe:spock-browser-server", "-j4")
    if test:
        # GHC 9.12's Node runtime lacks Hspec's filesystem config lookup and
        # SplitMix's OS seed initialization. Run all assertions with a fixed seed.
        cabal(js_project, js_build, "test", "all", "--test-show-details=direct",
              "--test-options=--ignore-dot-hspec --seed=105", "-j4")
        cabal(server_project, server_build, "test", "spock-browser-server", "--test-show-details=direct", "-j4")
    plan = json.loads((ROOT / js_build / "cache/plan.json").read_text())
    client = next(p for p in plan["install-plan"] if p["pkg-name"] == "spock-browser-client"
                  and p.get("component-name") == "exe:spock-browser-client")
    source = Path(client["dist-dir"]) / "build/spock-browser-client/spock-browser-client.jsexe"
    assert (source / "all.js").is_file(), "JavaScript link output is missing"
    binary = Path(cabal(server_project, server_build, "list-bin", "spock-browser-server:exe:spock-browser-server", capture=True))
    output = output.resolve()
    public = output / "public"
    public.mkdir(parents=True, exist_ok=True)
    for name in ["all.js", "clibs.js", "clibs.wasm"]:
        if (source / name).is_file():
            shutil.copy2(source / name, public / name)
    shutil.copy2(ROOT / "examples/browser/client/index.html", public / "index.html")
    shutil.copy2(binary, output / "spock-browser-server")
    (output / "manifest.json").write_text(json.dumps({"native": native_version, "javascript": javascript_version,
                                                     "client": client["pkg-version"]}, indent=2) + "\n")
    print(f"Built {output}; run its spock-browser-server with --local-http PORT {public}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=ROOT / "dist-browser")
    parser.add_argument("--test", action="store_true")
    args = parser.parse_args()
    build(args.output, args.test)
