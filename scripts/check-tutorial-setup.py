#!/usr/bin/env python3
"""Build the documented Stack setups, database continuation, and HTTP tests."""

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
def server(directory, executable, port, arguments=()):
    command = ["stack", "exec", "--system-ghc", "--no-install-ghc", executable]
    if arguments:
        command += ["--", *arguments]
    process = subprocess.Popen(command, cwd=directory)
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
    if chapter == "rest-api":
        check_database(directory, document, name, port)


def fenced(document, marker, language):
    return re.search(re.escape(f"<!-- {marker} -->") + rf"\s*```{language}\n(.*?)```", document, re.S)[1]


def check_database(directory, document, name, port):
    cabal = directory / f"{name}.cabal"
    config = re.sub(r"  build-depends:.*\n", fenced(document, "database:dependencies", "cabal"), cabal.read_text())
    config = re.sub(r"^[ \t]+other-modules:.*\n", "", config, flags=re.M)
    config = re.sub(r"(^[ \t]+main-is:[^\n]*\n)", r"\1  other-modules: People\n", config, flags=re.M)
    cabal.write_text(config)
    for target, source in [("People.hs", "People.hs"), ("Main.hs", "PeopleMain.hs")]:
        (directory / "src" / target).write_text((ROOT / "docs/_includes/examples" / source).read_text())
    for _ in range(2):
        subprocess.run(["stack", "build", "--system-ghc", "--no-install-ghc", "--fast", "--pedantic", "-j4"],
                       cwd=directory, check=True)
        assert cabal.read_text() == config, "Database build changed the documented Cabal file"
    database = str(directory / "people.sqlite")
    base = f"http://127.0.0.1:{port}"

    def request(path, method="GET", value=None, status=200):
        payload = None if value is None else json.dumps(value).encode()
        req = urllib.request.Request(base + path, payload, {"Content-Type": "application/json"}, method=method)
        try:
            response = urllib.request.urlopen(req)
        except urllib.error.HTTPError as error:
            response = error
        with response:
            assert response.status == status, (method, path, response.status, status)
            content = response.read()
            if content:
                assert response.headers.get_content_type() == "application/json"
                return json.loads(content), response.headers
            return None, response.headers

    with server(directory, name, port, [database, str(port)]):
        created, headers = request("/people", "POST", {"name": "Alex", "age": 25}, 201)
        assert created == {"result": "success", "id": 1}
        assert headers["Location"] == "/people/1"
        people, _ = request("/people")
        assert people == [{"id": 1, "name": "Alex", "age": 25}]
        request("/people", "POST", {"name": "Alex", "age": -1}, 400)
        request("/people/404", status=404)
    with server(directory, name, port, [database, str(port)]):
        person, _ = request("/people/1")
        assert person == {"id": 1, "name": "Alex", "age": 25}
        person, _ = request("/people/1", "PUT", {"name": "Ada", "age": 30})
        assert person == {"id": 1, "name": "Ada", "age": 30}
        request("/people/1", "DELETE", status=204)
        request("/people/1", status=404)
    print("rest-api database: fresh build, HTTP statuses, CRUD, and restart persistence passed", flush=True)


def check_testing(parent):
    directory = parent / "spock-testing-example"
    directory.mkdir()
    document = (ROOT / "docs/tutorials/testing.md").read_text()
    cabal = directory / "spock-testing-example.cabal"
    expected = fenced(document, "testing:cabal", "cabal")
    cabal.write_text(expected)
    (directory / "stack.yaml").write_text((ROOT / "docs/_includes/tutorial-stack.yaml").read_text())
    for path, contents in {
        "src/Hello.hs": (ROOT / "docs/_includes/examples/Hello.hs").read_text(),
        "test/HelloSpec.hs": (ROOT / "docs/_includes/examples/HelloSpec.hs").read_text(),
        "app/Main.hs": fenced(document, "testing:main", "haskell"),
        "test/Spec.hs": fenced(document, "testing:driver", "haskell"),
    }.items():
        target = directory / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(contents)
    subprocess.run(["stack", "test", "--system-ghc", "--no-install-ghc", "--fast", "--pedantic", "-j4"],
                   cwd=directory, check=True)
    assert cabal.read_text() == expected, "Test setup changed the documented Cabal file"
    print("testing: fresh project, executable build, and all documented HTTP tests passed", flush=True)


if __name__ == "__main__":
    subprocess.run(["python3", str(ROOT / "scripts/sync-tutorial-examples.py"), "--check"], check=True)
    with tempfile.TemporaryDirectory(prefix="spock-tutorials-") as temporary:
        for chapter, name in [("getting-started", "spock-example"), ("rest-api", "spock-rest")]:
            check(Path(temporary), chapter, name)
        check_testing(Path(temporary))
