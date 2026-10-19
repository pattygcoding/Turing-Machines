#!/usr/bin/env python3
"""Generate the static showcase dashboard data.

This script reads the *real* repository contents and writes
``public/dashboard-data.js`` - the file the dashboard loads with a ``<script>``
tag, so ``npm run build`` copies it into the published site untouched:

  * every folder under ``langs/`` becomes a sidebar entry (source code
    included),
  * the verified console captures in ``tests/expected/`` become the selectable
    "Console Output" views, and
  * the project URL (the git remote, else ``GITHUB_REPOSITORY``, else a
    default) and branch are recorded so the dashboard can link each
    implementation to its folder on GitHub.

Because the data is emitted as a plain JavaScript object it can be loaded
with a ``<script>`` tag, so the dashboard works both on a static host and
straight from ``file://`` (no fetch/CORS problems).

Usage::

    python tools/generate_dashboard.py
"""

from __future__ import annotations

import datetime
import json
import os
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
# Every implementation lives in its own folder under langs/ (langs/PHP, ...).
LANG_DIR = ROOT / "langs"
# The Vue app loads this with a classic <script> tag, so it has to sit beside the
# built ``index.html``.  Vite copies ``public/`` verbatim into ``dist/``, which is
# also what the dev server serves at the site root - hence ``public/`` and not the
# repository root.  It is generated, never hand-edited, and not tracked in git.
OUTPUT_FILE = ROOT / "public" / "dashboard-data.js"

# Fallbacks for the project link the dashboard shows above each implementation.
DEFAULT_REPOSITORY = "https://github.com/pattygcoding/Turing-Machines"
DEFAULT_BRANCH = "main"

# The verified console captures the master test suite writes (and checks).  They
# become the dashboard's "Console Output" tab, exactly as in the Connect Four
# showcase.
EXPECTED_DIR = ROOT / "tests" / "expected"

# Friendly labels for those scenarios.
SCENARIO_LABELS = {
    "basic": "Increment 1011",
    "zero": "Increment 0",
    "one": "Increment 1",
    "carry_chain": "Carry chain (1111)",
    "leading_zero": "Leading zero (00101)",
    "empty": "Empty tape",
    "long": "Long tape (0101010101)",
    "invalid": "Rejected invalid input",
    "eof": "End of input",
}

# ---------------------------------------------------------------------------
# Display metadata per implementation.  The repository stores each language in
# a folder named after it (``C#``, ``Objective-C``, ``VB.NET``, ...), so every
# entry names its folder and source file explicitly rather than deriving them.
#
#   id         the URL slug (``/python``, ``/csharp``, ...)
#   name       display name shown in the sidebar and header
#   folder     the directory relative to the repo root (also the GitHub link)
#   source     the file inside that directory
#   prism      the Prism grammar class for syntax highlighting
#   category   the badge and the sidebar's category filter
#   extension  the short file-extension tag shown beside the name
#   family     optional grouping key when one language has several builds
#   note       optional sentence shown above the code
# ---------------------------------------------------------------------------
LANGUAGE_INFO = [
    {
        "id": "ada",
        "name": "Ada",
        "folder": "Ada",
        "source": "TuringMachine.ada",
        "prism": "language-ada",
        "category": "Systems",
        "extension": ".ada",
    },
    {
        "id": "assembly32",
        "name": "Assembly (32-bit)",
        "folder": "Assembly/32bit",
        "source": "TuringMachine.asm",
        "prism": "language-nasm",
        "category": "Systems",
        "extension": ".asm",
        "family": "assembly",
        "note": "This is the 32-bit NASM build. The 64-bit build is the other Assembly entry in the sidebar.",
    },
    {
        "id": "assembly64",
        "name": "Assembly (64-bit)",
        "folder": "Assembly/64bit",
        "source": "TuringMachine.asm",
        "prism": "language-nasm",
        "category": "Systems",
        "extension": ".asm",
        "family": "assembly",
        "note": "This is the 64-bit NASM build. The 32-bit build is the other Assembly entry in the sidebar.",
    },
    {
        "id": "bash",
        "name": "Bash",
        "folder": "Bash",
        "source": "TuringMachine.sh",
        "prism": "language-bash",
        "category": "Scripting",
        "extension": ".sh",
    },
    {
        "id": "c",
        "name": "C",
        "folder": "C",
        "source": "TuringMachine.c",
        "prism": "language-c",
        "category": "Systems",
        "extension": ".c",
    },
    {
        "id": "csharp",
        "name": "C#",
        "folder": "C#",
        "source": "TuringMachine.cs",
        "prism": "language-csharp",
        "category": ".NET",
        "extension": ".cs",
    },
    {
        "id": "cpp",
        "name": "C++",
        "folder": "C++",
        "source": "TuringMachine.cpp",
        "prism": "language-cpp",
        "category": "Systems",
        "extension": ".cpp",
    },
    {
        "id": "clojure",
        "name": "Clojure",
        "folder": "Clojure",
        "source": "TuringMachine.clj",
        "prism": "language-clojure",
        "category": "Functional",
        "extension": ".clj",
    },
    {
        "id": "cobol",
        "name": "COBOL",
        "folder": "COBOL",
        "source": "TuringMachine.cob",
        "prism": "language-cobol",
        "category": "Legacy",
        "extension": ".cob",
    },
    {
        "id": "dart",
        "name": "Dart",
        "folder": "Dart",
        "source": "TuringMachine.dart",
        "prism": "language-dart",
        "category": "Web",
        "extension": ".dart",
    },
    {
        "id": "elixir",
        "name": "Elixir",
        "folder": "Elixir",
        "source": "TuringMachine.exs",
        "prism": "language-elixir",
        "category": "Functional",
        "extension": ".exs",
    },
    {
        "id": "erlang",
        "name": "Erlang",
        "folder": "Erlang",
        "source": "TuringMachine.erl",
        "prism": "language-erlang",
        "category": "Functional",
        "extension": ".erl",
    },
    {
        "id": "fsharp",
        "name": "F#",
        "folder": "F#",
        "source": "TuringMachine.fs",
        "prism": "language-fsharp",
        "category": ".NET",
        "extension": ".fs",
    },
    {
        "id": "fortran",
        "name": "Fortran",
        "folder": "Fortran",
        "source": "TuringMachine.f90",
        "prism": "language-fortran",
        "category": "Scientific",
        "extension": ".f90",
    },
    {
        "id": "go",
        "name": "Go",
        "folder": "Go",
        "source": "TuringMachine.go",
        "prism": "language-go",
        "category": "Systems",
        "extension": ".go",
    },
    {
        "id": "groovy",
        "name": "Groovy",
        "folder": "Groovy",
        "source": "TuringMachine.groovy",
        "prism": "language-groovy",
        "category": "JVM",
        "extension": ".groovy",
    },
    {
        "id": "haskell",
        "name": "Haskell",
        "folder": "Haskell",
        "source": "TuringMachine.hs",
        "prism": "language-haskell",
        "category": "Functional",
        "extension": ".hs",
    },
    {
        "id": "java",
        "name": "Java",
        "folder": "Java",
        "source": "TuringMachine.java",
        "prism": "language-java",
        "category": "JVM",
        "extension": ".java",
    },
    {
        "id": "javascript",
        "name": "JavaScript",
        "folder": "JavaScript",
        "source": "TuringMachine.js",
        "prism": "language-javascript",
        "category": "Web",
        "extension": ".js",
    },
    {
        "id": "julia",
        "name": "Julia",
        "folder": "Julia",
        "source": "TuringMachine.julia",
        "prism": "language-julia",
        "category": "Scientific",
        "extension": ".julia",
    },
    {
        "id": "kotlin",
        "name": "Kotlin",
        "folder": "Kotlin",
        "source": "TuringMachine.kt",
        "prism": "language-kotlin",
        "category": "JVM",
        "extension": ".kt",
    },
    {
        "id": "lua",
        "name": "Lua",
        "folder": "Lua",
        "source": "TuringMachine.lua",
        "prism": "language-lua",
        "category": "Scripting",
        "extension": ".lua",
    },
    {
        "id": "nim",
        "name": "Nim",
        "folder": "Nim",
        "source": "TuringMachine.nim",
        "prism": "language-nim",
        "category": "Systems",
        "extension": ".nim",
    },
    {
        "id": "objectivec",
        "name": "Objective-C",
        "folder": "Objective-C",
        "source": "TuringMachine.m",
        "prism": "language-objectivec",
        "category": "Systems",
        "extension": ".m",
    },
    {
        "id": "ocaml",
        "name": "OCaml",
        "folder": "OCaml",
        "source": "TuringMachine.ml",
        "prism": "language-ocaml",
        "category": "Functional",
        "extension": ".ml",
    },
    {
        "id": "pascal",
        "name": "Pascal",
        "folder": "Pascal",
        "source": "TuringMachine.pas",
        "prism": "language-pascal",
        "category": "Legacy",
        "extension": ".pas",
    },
    {
        "id": "perl",
        "name": "Perl",
        "folder": "Perl",
        "source": "TuringMachine.pl",
        "prism": "language-perl",
        "category": "Scripting",
        "extension": ".pl",
    },
    {
        "id": "php",
        "name": "PHP",
        "folder": "PHP",
        "source": "TuringMachine.php",
        "prism": "language-php",
        "category": "Web",
        "extension": ".php",
    },
    {
        "id": "powershell",
        "name": "PowerShell",
        "folder": "Powershell",
        "source": "TuringMachine.ps1",
        "prism": "language-powershell",
        "category": "Scripting",
        "extension": ".ps1",
    },
    {
        "id": "prolog",
        "name": "Prolog",
        "folder": "Prolog",
        "source": "TuringMachine.pl",
        "prism": "language-prolog",
        "category": "Logic",
        "extension": ".pl",
    },
    {
        "id": "python",
        "name": "Python",
        "folder": "Python",
        "source": "TuringMachine.py",
        "prism": "language-python",
        "category": "Scripting",
        "extension": ".py",
    },
    {
        "id": "r",
        "name": "R",
        "folder": "R",
        "source": "TuringMachine.r",
        "prism": "language-r",
        "category": "Scientific",
        "extension": ".r",
    },
    {
        "id": "ruby",
        "name": "Ruby",
        "folder": "Ruby",
        "source": "TuringMachine.rb",
        "prism": "language-ruby",
        "category": "Scripting",
        "extension": ".rb",
    },
    {
        "id": "rust",
        "name": "Rust",
        "folder": "Rust",
        "source": "TuringMachine.rs",
        "prism": "language-rust",
        "category": "Systems",
        "extension": ".rs",
    },
    {
        "id": "scala",
        "name": "Scala",
        "folder": "Scala",
        "source": "TuringMachine.scala",
        "prism": "language-scala",
        "category": "JVM",
        "extension": ".scala",
    },
    {
        "id": "simula",
        "name": "Simula",
        "folder": "Simula",
        "source": "TuringMachine.sim",
        # Prism has no Simula grammar; Pascal is the closest Algol-family match,
        # so the file is highlighted rather than shown as plain text.
        "prism": "language-pascal",
        "category": "Legacy",
        "extension": ".sim",
    },
    {
        "id": "swift",
        "name": "Swift",
        "folder": "Swift",
        "source": "TuringMachine.swift",
        "prism": "language-swift",
        "category": "Systems",
        "extension": ".swift",
    },
    {
        "id": "typescript",
        "name": "TypeScript",
        "folder": "TypeScript",
        "source": "TuringMachine.ts",
        "prism": "language-typescript",
        "category": "Web",
        "extension": ".ts",
    },
    {
        "id": "v",
        "name": "V",
        "folder": "V",
        "source": "TuringMachine.v",
        "prism": "language-v",
        "category": "Systems",
        "extension": ".v",
    },
    {
        "id": "vbnet",
        "name": "VB.NET",
        "folder": "VB.NET",
        "source": "TuringMachine.vb",
        "prism": "language-vbnet",
        "category": ".NET",
        "extension": ".vb",
    },
    {
        "id": "vba",
        "name": "VBA",
        "folder": "VBA",
        "source": "TuringMachine.vba",
        "prism": "language-visual-basic",
        "category": "Legacy",
        "extension": ".vba",
    },
    {
        "id": "zig",
        "name": "Zig",
        "folder": "Zig",
        "source": "TuringMachine.zig",
        "prism": "language-zig",
        "category": "Systems",
        "extension": ".zig",
    },
]


def read_text(path: Path) -> str:
    """Read a text file and normalise its line endings to ``\\n``."""
    return path.read_text(encoding="utf-8").replace("\r\n", "\n").replace("\r", "\n")


def git_output(*args: str) -> str:
    """Run a read-only git command in the repository; "" when git is unavailable."""
    try:
        result = subprocess.run(
            ["git", "-C", str(ROOT), *args],
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return ""
    return result.stdout.strip() if result.returncode == 0 else ""


def repository_url() -> str:
    """The GitHub project the dashboard links to.

    The clone's remote is the truth, then ``GITHUB_REPOSITORY`` (set inside
    Actions), then the committed default.  ``git@``/``ssh://`` remotes are
    normalised to https and a trailing ``.git`` is dropped, so
    ``git@github.com:owner/repo.git`` becomes ``https://github.com/owner/repo``.
    """
    remote = git_output("config", "--get", "remote.origin.url") or os.environ.get(
        "GITHUB_REPOSITORY", ""
    )
    if not remote:
        return DEFAULT_REPOSITORY
    if remote.startswith("git@"):
        host, _, path = remote[len("git@"):].partition(":")
        remote = "https://%s/%s" % (host, path)
    elif remote.startswith("ssh://"):
        remote = "https://" + remote[len("ssh://"):].replace("git@", "", 1)
    if not remote.startswith("http"):
        remote = "https://github.com/" + remote
    if remote.endswith(".git"):
        remote = remote[: -len(".git")]
    return remote.rstrip("/") or DEFAULT_REPOSITORY


def repository_branch() -> str:
    """The branch the dashboard links into: HEAD's name, else Actions', else main.

    A detached HEAD (a CI checkout) reports ``HEAD``, which is not a usable
    ref, so ``GITHUB_REF_NAME`` takes over there.
    """
    branch = git_output("rev-parse", "--abbrev-ref", "HEAD")
    if not branch or branch == "HEAD":
        branch = os.environ.get("GITHUB_REF_NAME", "") or DEFAULT_BRANCH
    return branch


def discover_languages() -> list[dict]:
    """Read every implementation named in ``LANGUAGE_INFO``.

    A missing folder or source file is skipped rather than guessed at, so the
    payload can never point at a file that is not there.
    """
    found = []
    for meta in LANGUAGE_INFO:
        source = LANG_DIR / meta["folder"] / meta["source"]
        if not source.is_file():
            continue
        entry = {
            "id": meta["id"],
            "name": meta["name"],
            "category": meta["category"],
            "prism": meta["prism"],
            "file": source.relative_to(ROOT).as_posix(),
            # The GitHub link points at the folder inside langs/.
            "folder": (Path("langs") / meta["folder"]).as_posix(),
            "code": read_text(source),
        }
        note = meta.get("note")
        if note:
            entry["note"] = note
        found.append(entry)
    found.sort(key=lambda item: item["name"].casefold())
    return found


def discover_scenarios() -> tuple[list[dict], dict]:
    """Return (scenario list, {scenario id -> verified console capture})."""
    scenarios = []
    outputs = {}
    if not EXPECTED_DIR.is_dir():
        return scenarios, outputs
    files = {path.stem: path for path in EXPECTED_DIR.glob("*.txt")}
    # Keep the curated order from SCENARIO_LABELS; append anything unexpected.
    order = list(SCENARIO_LABELS) + sorted(set(files) - set(SCENARIO_LABELS))
    for scenario_id in order:
        path = files.get(scenario_id)
        if path is None:
            continue
        scenarios.append(
            {
                "id": scenario_id,
                "label": SCENARIO_LABELS.get(
                    scenario_id, scenario_id.replace("_", " ").title()
                ),
            }
        )
        outputs[scenario_id] = read_text(path)
    return scenarios, outputs


def build_payload() -> dict:
    languages = discover_languages()
    scenarios, outputs = discover_scenarios()
    # Count only implementations whose source file was actually found, so the
    # sidebar's totals can never promise a language that is not in the payload.
    present = [
        meta for meta in LANGUAGE_INFO
        if (LANG_DIR / meta["folder"] / meta["source"]).is_file()
    ]
    families = {meta.get("family", meta["id"]) for meta in present}
    extensions = {
        meta["name"]: meta["extension"]
        for meta in present
    }
    return {
        "meta": {
            "updated": datetime.datetime.now(datetime.timezone.utc).strftime(
                "%Y-%m-%d %H:%M UTC"
            ),
            "languageCount": len(families),
            "implementationCount": len(languages),
            "scenarioCount": len(scenarios),
            # The dashboard shows "<repository>/tree/<branch>/<folder>" above
            # each implementation, so the project it lives in travels with the data.
            "repository": repository_url(),
            "branch": repository_branch(),
        },
        "scenarios": scenarios,
        "outputs": outputs,
        "languages": languages,
        "languageExtensions": extensions,
    }


def main() -> int:
    payload = build_payload()
    body = json.dumps(payload, indent=2, ensure_ascii=False)
    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
    OUTPUT_FILE.write_text(
        "window.TURING_MACHINE_DATA = " + body + ";\n", encoding="utf-8"
    )
    print(
        "Wrote %s (%d language(s), %d implementation(s), %d scenario(s))"
        % (
            OUTPUT_FILE.name,
            payload["meta"]["languageCount"],
            payload["meta"]["implementationCount"],
            payload["meta"]["scenarioCount"],
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

