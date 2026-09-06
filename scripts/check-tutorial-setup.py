#!/usr/bin/env python3
"""Build the two documented Stack setups in fresh directories and exercise HTTP."""

from contextlib import contextmanager
import json
from pathlib import Path
import re
import socket
import subprocess
import tempfile
import time
import urllib.error
import urllib.request

ROOT = Path(__file__).resolve().parents[1]


def blocks(document):
    return [b.replace("{% raw %}", "").replace("{% endraw %}", "")
            for b in re.findall(r"{% highlight haskell %}(.*?){% endhighlight %}", document, re.S)]


@contextmanager
def server(directory, executable, port):
    process = subprocess.Popen(["stack", "exec", "--system-ghc", "--no-install-ghc", executable], cwd=directory)
    try:
        for _ in range(100):
            if process.poll() is not None:
                raise RuntimeError(f"{executable} exited before accepting requests")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                time.sleep(0.1)
        else:
            raise RuntimeError(f"{executable} did not start")
        yield
    finally:
        process.terminate()
        try:
            process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()


def check(parent, chapter, name):
    document = (ROOT / "docs/tutorials" / f"{chapter}.md").read_text()
    subprocess.run(["stack", "new", name, "simple", "--no-init"], cwd=parent, check=True)
    directory = parent / name
    assert not (directory / "package.yaml").exists(), "Tutorial requires a Cabal-only template"
    dependencies = re.search(r"<!-- setup:dependencies -->\s*```cabal\n(.*?)```", document, re.S)[1]
    cabal = directory / f"{name}.cabal"
    cabal.write_text(re.sub(r"  build-depends:.*\n", dependencies, cabal.read_text())
                     .replace("  ghc-options:", "  ghc-options: -threaded"))
    (directory / "stack.yaml").write_text((ROOT / "docs/_includes/tutorial-stack.yaml").read_text())
    snippets = blocks(document)
    if chapter == "getting-started":
        source = next(b for b in snippets if "main :: IO ()" in b)
    else:
        source = "\n".join(next(b for b in snippets if marker in b)
                           for marker in ["module Main", "data Person =", "main :: IO ()", "thePerson <- jsonBody'"])
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        port = sock.getsockname()[1]
    (directory / "src/Main.hs").write_text(source.replace("runSpock 8080", f"runSpock {port}"))
    expected_cabal = cabal.read_bytes()
    for _ in range(2):
        subprocess.run(["stack", "build", "--system-ghc", "--no-install-ghc", "--fast", "--pedantic", "-j4"],
                       cwd=directory, check=True)
        assert cabal.read_bytes() == expected_cabal, "Build changed the documented dependency file"
    with server(directory, name, port):
        base = f"http://127.0.0.1:{port}"
        if chapter == "getting-started":
            with urllib.request.urlopen(base) as response:
                assert response.read() == b"Hello World!"
            for count in [1, 2]:
                with urllib.request.urlopen(base + "/hello/Alex") as response:
                    assert response.read().decode() == f"Hello Alex, you are visitor number {count}"
        else:
            with urllib.request.urlopen(base + "/people") as response:
                assert response.headers.get_content_type() == "application/json"
                assert json.load(response) == {"name": "Fry", "age": 25}
            request = urllib.request.Request(base + "/people", b'{"name":"Bart","age":10}',
                                             {"Content-Type": "application/json"})
            with urllib.request.urlopen(request) as response:
                assert response.read() == b'Parsed: Person {name = "Bart", age = 10}'
            request.data = b"invalid JSON"
            try:
                urllib.request.urlopen(request)
            except urllib.error.HTTPError as error:
                assert error.code == 400
            else:
                raise AssertionError("Invalid JSON was accepted")
    print(f"{chapter}: fresh setup, repeated build, and HTTP checks passed", flush=True)


if __name__ == "__main__":
    with tempfile.TemporaryDirectory(prefix="spock-tutorials-") as temporary:
        for chapter, name in [("getting-started", "spock-example"), ("rest-api", "spock-rest")]:
            check(Path(temporary), chapter, name)
