#!/usr/bin/env python3
"""Install the pinned JavaScript cross compiler and Emscripten into an isolated prefix."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shlex
import shutil
import subprocess
import tarfile
import tempfile
import urllib.request

CONFIG = json.loads(Path(__file__).with_name("javascript-toolchain.json").read_text())


def run(args, **kwargs):
    return subprocess.run([str(a) for a in args], check=True, **kwargs)


def output(args, **kwargs):
    return run(args, text=True, stdout=subprocess.PIPE, **kwargs).stdout.strip()


def download(url, target, checksum):
    with urllib.request.urlopen(url) as response, target.open("wb") as output_file:
        shutil.copyfileobj(response, output_file)
    with target.open("rb") as source:
        hasher = hashlib.sha256()
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            hasher.update(chunk)
    if hasher.hexdigest() != checksum:
        raise SystemExit("GHC archive checksum mismatch.")


def setup_haddock(prefix, compiler, env):
    # Cross bindists omit Haddock. The host executable of the SAME GHC version
    # can read the cross compiler's settings and interfaces. Keep only Haddock,
    # its shared libraries and HTML resources from the verified native bindist.
    destination = prefix / "haddock-native"
    if not destination.exists():
        name, checksum = CONFIG["haddock_archives"][f"{platform.system()}-{platform.machine()}"]
        with tempfile.TemporaryDirectory(prefix="haddock-download-", dir=prefix) as directory:
            archive = Path(directory) / name
            download(CONFIG["haddock_base_url"] + name, archive, checksum)
            extracted = Path(directory) / "extracted"
            with tarfile.open(archive) as source:
                members = [m for m in source if "/bin/haddock" in m.name or "/lib/html/" in m.name
                           or m.name.endswith(".dylib") or ".so" in Path(m.name).name
                           or len(Path(m.name).parts) == 2 and Path(m.name).name == "LICENSE"]
                # The exact archive was authenticated above, including symlinks.
                source.extractall(extracted, members=members)
            roots = list(extracted.iterdir())
            assert len(roots) == 1, "Unexpected native bindist layout"
            shutil.move(str(roots[0]), destination)
    native = destination / "bin/haddock"
    if output([native, "--ghc-version"], env=env) != CONFIG["ghc"]:
        raise SystemExit("Haddock must match the JavaScript compiler version.")
    libdir = output([compiler / "bin/javascript-unknown-ghcjs-ghc", "--print-libdir"], env=env)
    wrappers = prefix / "haddock-bin"
    wrappers.mkdir(exist_ok=True)
    wrapper = wrappers / "haddock"
    wrapper.write_text("#!/bin/sh\nexec " + " ".join(map(shlex.quote,
        [str(native), "-B" + libdir, "--lib=" + str(destination / "lib"),
         "--optghc=-static", "--optghc=-fexternal-interpreter"])) + ' "$@"\n')
    wrapper.chmod(0o755)
    link = compiler / "bin/javascript-unknown-ghcjs-haddock"
    if link.is_symlink():
        link.unlink()
    link.symlink_to(wrapper)


def setup(prefix, haddock=False):
    archive_name, checksum = CONFIG["archives"][f"{platform.system()}-{platform.machine()}"]
    for command in ["git", "make", "tar", "xz", "node"]:
        if not shutil.which(command):
            raise SystemExit(f"Install {command} first (use Node 22).")
    prefix = prefix.expanduser().resolve()
    prefix.mkdir(parents=True, exist_ok=True)
    emsdk = prefix / "emsdk"
    compiler = prefix / f'javascript-{CONFIG["ghc"]}'
    if not emsdk.exists():
        run(["git", "clone", "--depth=1", "--branch", CONFIG["emscripten"],
             "https://github.com/emscripten-core/emsdk.git", emsdk])
    if output(["git", "rev-parse", "HEAD"], cwd=emsdk) != CONFIG["emsdk_commit"]:
        raise SystemExit(f"{emsdk} has a different revision; choose a fresh --prefix.")
    run([emsdk / "emsdk", "install", CONFIG["emscripten"]], cwd=emsdk)
    run([emsdk / "emsdk", "activate", CONFIG["emscripten"]], cwd=emsdk)
    env = os.environ.copy()
    env.update(EMSDK=str(emsdk), EM_CONFIG=str(emsdk / ".emscripten"))
    env["PATH"] = os.pathsep.join([str(compiler / "bin"), str(emsdk),
                                  str(emsdk / "upstream/emscripten"), env["PATH"]])
    ghc = compiler / "bin/javascript-unknown-ghcjs-ghc"
    if ghc.exists():
        if output([ghc, "--numeric-version"], env=env) != CONFIG["ghc"]:
            raise SystemExit(f"{compiler} contains another compiler; choose a fresh --prefix.")
    else:
        # Verify the GHCup-published hash before extracting or executing anything.
        with tempfile.TemporaryDirectory(prefix="download-", dir=prefix) as directory:
            archive = Path(directory) / archive_name
            download(CONFIG["base_url"] + archive_name, archive, checksum)
            run(["tar", "-xf", archive, "-C", directory])
            source = Path(directory) / f'ghc-{CONFIG["ghc"]}-javascript-unknown-ghcjs'
            run(["emconfigure", "./configure", f"--prefix={compiler}"], cwd=source, env=env)
            run(["make", "install"], cwd=source, env=env)
    if haddock:
        setup_haddock(prefix, compiler, env)
    activation = prefix / "env.sh"
    activation.write_text(
        "# Generated by Spock's scripts/setup-javascript.py; source this file.\n"
        f"export EMSDK={shlex.quote(str(emsdk))}\n"
        f"export EM_CONFIG={shlex.quote(str(emsdk / '.emscripten'))}\n"
        f"export PATH={shlex.quote(os.pathsep.join([str(compiler / 'bin'), str(emsdk), str(emsdk / 'upstream/emscripten')]))}:\"$PATH\"\n")
    run([ghc, "--version"], env=env)
    run(["emcc", "--version"], env=env)
    print(f"Ready. Run: source {shlex.quote(str(activation))}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--prefix", type=Path, required=True)
    parser.add_argument("--haddock", action="store_true", help="Also install matching Haddock for browser API reference builds")
    args = parser.parse_args()
    setup(args.prefix, args.haddock)
