#!/usr/bin/env python3
"""Copy compiled tutorial sources/regions into Jekyll includes; --check detects drift."""

import argparse
from pathlib import Path
import re
import textwrap

ROOT = Path(__file__).resolve().parents[1]
SOURCES = {
    "Hello.hs": ("examples/cookbook/src/Hello.hs", None),
    "HelloSpec.hs": ("examples/cookbook/test/HelloSpec.hs", None),
    "People.hs": ("examples/rest-api/src/People.hs", None),
    "PeopleMain.hs": ("examples/rest-api/app/Main.hs", None),
    **{f"cookbook-{section}.hs": ("examples/cookbook/src/Cookbook.hs", section)
       for section in ["configuration", "middleware", "headers", "json", "form", "upload", "errors"]},
}


def sync(check=False):
    failures = []
    for name, (source, section) in SOURCES.items():
        contents = (ROOT / source).read_text()
        if section:
            matches = re.findall(rf"^ *-- example: {section}\n(.*?)^ *-- end-example: {section}$",
                                 contents, re.M | re.S)
            if len(matches) != 1:
                raise AssertionError(f"Expected one {section} region in {source}")
            contents = textwrap.dedent(matches[0])
        target = ROOT / "docs/_includes/examples" / name
        if check:
            if not target.exists() or target.read_text() != contents:
                failures.append(str(target.relative_to(ROOT)))
        else:
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(contents)
    if failures:
        raise AssertionError("Refresh with scripts/sync-tutorial-examples.py: " + ", ".join(failures))
    print(f"{'Checked' if check else 'Updated'} {len(SOURCES)} compiled tutorial includes")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    sync(parser.parse_args().check)
