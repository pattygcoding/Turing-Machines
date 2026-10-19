#!/usr/bin/env python3
"""Generate ``langs/<Language>/README.md`` for every implementation.

Each README explains what the implementation is, what toolchain it needs, how to
build and run it, and where the golden output lives.  Everything is derived from
the two sources of truth that already exist:

  * ``tools/generate_dashboard.py`` - display name, folder, source file, category
    and file-extension tag (the same metadata the showcase uses), and
  * ``tests/run_tests.py`` - the build and run commands the master suite uses.

Regenerate them after adding a language (or changing a build command):

    python tools/generate_language_readmes.py

Never hand-edit a generated README; edit the tables below and re-run.
"""

from __future__ import annotations

import importlib.util
import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
LANG_DIR = ROOT / "langs"

sys.path.insert(0, str(ROOT / "tools"))
from generate_dashboard import LANGUAGE_INFO  # noqa: E402  (needs the path above)


def _load_runner():
    """Import tests/run_tests.py for its LANGUAGES registry, without running it."""
    spec = importlib.util.spec_from_file_location("tm_run_tests", ROOT / "tests" / "run_tests.py")
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


RUNNER = _load_runner()
LANGUAGES = {entry["name"]: entry for entry in RUNNER.LANGUAGES}

IMPLEMENTATION_COUNT = len(LANGUAGE_INFO)
LANGUAGE_COUNT = len({meta.get("family", meta["id"]) for meta in LANGUAGE_INFO})

SITE = "https://turing.pattygcoding.com"

# Extra setup lines a language needs before its build command (the runner does
# this in Python; the README shows the equivalent shell).
PRELUDE = {
    "ada": [
        "mkdir -p tests/build/ada",
        "cp langs/Ada/TuringMachine.ada tests/build/ada/turingmachine.adb",
    ],
    "pascal": ["mkdir -p tests/build/pascal"],
    "scala": ["cd langs/Scala"],
}

# Languages with no console entry point at all: the standalone section is prose
# instead of a shell block.
MANUAL_RUN = {
    "vba": [
        "There is no console entry point for VBA. Import `TuringMachine.vba` into a",
        "module, put the tape in `input.txt` beside the workbook, run `TuringMachine`,",
        "then open the Immediate window (`Ctrl+G`) to read the two result lines.",
    ],
}


def _relative(text: str) -> str | None:
    """Repo-relative POSIX path when ``text`` is a path inside the repository."""
    try:
        candidate = Path(text)
        if not candidate.is_absolute():
            return None
        return candidate.resolve().relative_to(ROOT.resolve()).as_posix()
    except (OSError, ValueError):
        return None


def _without_exe(text: str) -> str:
    """Drop the Windows ``.exe`` suffix so the commands read on any platform."""
    return text[:-4] if text.lower().endswith(".exe") else text


def _program(name: str) -> str:
    """A friendly name for a command's first word."""
    if name == sys.executable or Path(name).name.lower().startswith("python"):
        return "python"
    relative = _relative(name)
    if relative:
        return _without_exe("./" + relative if relative.startswith("tests/build") else relative)
    if os.sep in name or "/" in name or "\\" in name:
        return Path(name).stem
    return name


def _render_argument(argument: str) -> str:
    """Repo-relative text for an argument, including ``--flag:<repo path>`` forms."""
    relative = _relative(argument)
    if relative:
        return _without_exe(relative)
    text = str(argument).replace("\\", "/")
    root = str(ROOT).replace("\\", "/")
    if root in text:
        text = text.replace(root, "").replace(":/", ":").replace("/tests/", "tests/")
        text = text.replace("//", "/").lstrip("/")
    return _without_exe(text)


def _argv_text(argv: list) -> str:
    parts = [_program(argv[0])]
    for argument in argv[1:]:
        text = _render_argument(argument)
        parts.append('"%s"' % text if " " in text else text)
    return " ".join(parts)


def _build_commands(language: dict) -> list[str]:
    steps = language.get("build")
    if not steps:
        return []
    if not isinstance(steps[0], list):
        steps = [steps]
    return [_argv_text(step) for step in steps]


def _facts(meta: dict) -> dict:
    """Per-language prose, keyed by the showcase id."""
    return LANGUAGE_FACTS.get(meta["id"], {})


LANGUAGE_FACTS: dict[str, dict] = {
    "ada": {
        "tagline": "Ada with `Unbounded_String` and `Replace_Element` for the carry",
        "toolchain": "GNAT (Ada 2012), via Alire or MSYS2",
        "detect": "gnatmake --version",
        "install": {
            "Windows": "alr toolchain --select gnat_native",
            "macOS": "brew install gnat",
            "Debian/Ubuntu": "apt-get install gnat",
        },
        "notes": [
            "GNAT only accepts `.adb`/`.ads` source files, so the runner copies the repository file (which keeps the Ada 83 `.ada` extension) to `tests/build/ada/turingmachine.adb` before building.",
            "`Get_Line` raises `End_Error` at end of input, which is caught and treated as an empty tape.",
        ],
    },
    "assembly32": {
        "tagline": "Win32 NASM (stdcall) calling the Windows API directly, with no C runtime",
        "toolchain": "NASM plus a 32-bit MinGW-w64 linker",
        "detect": "nasm -v",
        "install": {
            "Windows": "choco install nasm; pacman -S mingw-w64-i686-gcc (MSYS2)",
            "macOS": "brew install nasm",
            "Debian/Ubuntu": "apt-get install nasm gcc-mingw-w64-i686",
        },
        "notes": [
            "This is the 32-bit sibling of the 64-bit build. The runner skips it when no 32-bit runtime is installed; the source is kept so a matching toolchain can build it.",
        ],
    },
    "assembly64": {
        "tagline": "Win64 NASM calling `ReadFile`/`WriteFile` directly, with no C runtime",
        "toolchain": "NASM plus a MinGW-w64 (x86-64) linker",
        "detect": "nasm -v",
        "install": {
            "Windows": "choco install nasm mingw",
            "macOS": "brew install nasm",
            "Debian/Ubuntu": "apt-get install nasm gcc-mingw-w64-x86-64",
        },
        "notes": [
            "The tape and output buffers are fixed-size, and the whole two-line result is assembled in memory and written with one `WriteFile` call.",
        ],
    },
    "bash": {
        "tagline": "Bash with parameter-expansion string surgery and no arrays",
        "toolchain": "Bash 4 or newer (Git for Windows, MSYS2 or WSL)",
        "detect": "bash --version",
        "install": {
            "Windows": "Git for Windows or `pacman -S bash` (MSYS2)",
            "macOS": "preinstalled (`brew install bash` for a newer version)",
            "Debian/Ubuntu": "preinstalled",
        },
        "notes": [
            "The tape stays a single string; the carry overwrites one character with `${tape:0:head}` / `${tape:head+1}` expansion.",
            "The runner uses Git Bash or MSYS2 bash, never the WSL shim in `C:/Windows/System32`.",
        ],
    },
    "c": {
        "tagline": "C11 with a `char` array tape and a bounds-checked carry loop",
        "toolchain": "a C11 compiler (gcc or clang)",
        "detect": "gcc --version",
        "install": {
            "Windows": "choco install mingw",
            "macOS": "xcode-select --install",
            "Debian/Ubuntu": "apt-get install build-essential",
        },
    },
    "clojure": {
        "tagline": "Clojure with a `loop`/`recur` carry over a reversed sequence",
        "toolchain": "the Clojure CLI plus a JVM (17+)",
        "detect": "clojure --version",
        "install": {
            "Windows": "scoop install clojure",
            "macOS": "brew install clojure/tools/clojure",
            "Debian/Ubuntu": "apt-get install clojure",
        },
        "notes": [
            "The runner runs it from `tests/build` so the CLI cannot mistake the repository for a Clojure project.",
        ],
    },
    "cobol": {
        "tagline": "GnuCOBOL with a padded tape record and reference modification",
        "toolchain": "GnuCOBOL 3 (`cobc`)",
        "detect": "cobc --version",
        "install": {
            "Windows": "choco install gnucobol",
            "macOS": "brew install gnu-cobol",
            "Debian/Ubuntu": "apt-get install gnucobol",
        },
        "notes": [
            "`ACCEPT` reads the line, trailing spaces and carriage returns are trimmed, and the carry is applied to a `PIC X(258)` tape with reference modification.",
            "On Windows the runner points `COB_CONFIG_DIR` at GnuCOBOL's own tool directory, otherwise `cobc` looks for its config beside the Chocolatey shim.",
        ],
    },
    "cpp": {
        "tagline": "C++17 with a `std::string` tape and `insert(begin(), '1')` for the overflow digit",
        "toolchain": "a C++17 compiler (g++ or clang++)",
        "detect": "g++ --version",
        "install": {
            "Windows": "choco install mingw",
            "macOS": "xcode-select --install",
            "Debian/Ubuntu": "apt-get install g++",
        },
    },
    "csharp": {
        "tagline": "C# with a `StringBuilder` tape and `Console.ReadLine()`",
        "toolchain": ".NET SDK 8 or newer",
        "detect": "dotnet --version",
        "install": {
            "Windows": "winget install Microsoft.DotNet.SDK.8",
            "macOS": "brew install --cask dotnet-sdk",
            "Debian/Ubuntu": "apt-get install dotnet-sdk-8.0",
        },
    },
    "dart": {
        "tagline": "Dart with a `List<String>` tape and `stdin.readLineSync()`",
        "toolchain": "the Dart SDK 3 or newer",
        "detect": "dart --version",
        "install": {
            "Windows": "choco install dart-sdk",
            "macOS": "brew install dart",
            "Debian/Ubuntu": "apt-get install dart",
        },
    },
    "elixir": {
        "tagline": "Elixir with a recursive `increment/1` over graphemes",
        "toolchain": "Elixir 1.15 or newer on Erlang/OTP",
        "detect": "elixir --version",
        "install": {
            "Windows": "choco install elixir",
            "macOS": "brew install elixir",
            "Debian/Ubuntu": "apt-get install elixir",
        },
    },
    "erlang": {
        "tagline": "Erlang with a quoted module name and a recursive list carry",
        "toolchain": "Erlang/OTP (`erlc` and `erl`)",
        "detect": "erl -version",
        "install": {
            "Windows": "choco install erlang",
            "macOS": "brew install erlang",
            "Debian/Ubuntu": "apt-get install erlang",
        },
        "notes": [
            "The module is named `'TuringMachine'` (quoted) so the file name matches while the source keeps its repository casing.",
        ],
    },
    "fortran": {
        "tagline": "Fortran with a blank-padded tape whose first cell absorbs the carry",
        "toolchain": "gfortran (Fortran 2008 or newer)",
        "detect": "gfortran --version",
        "install": {
            "Windows": "choco install mingw",
            "macOS": "brew install gcc",
            "Debian/Ubuntu": "apt-get install gfortran",
        },
        "notes": [
            "The tape is a `character(len=512)` with cell 1 reserved as the blank that becomes the new leading `1`; an `iostat` read keeps end of input from aborting.",
        ],
    },
    "fsharp": {
        "tagline": "F# with a `StringBuilder` tape and an `EntryPoint` `main`",
        "toolchain": ".NET SDK 8 or newer (F#)",
        "detect": "dotnet --version",
        "install": {
            "Windows": "winget install Microsoft.DotNet.SDK.8",
            "macOS": "brew install --cask dotnet-sdk",
            "Debian/Ubuntu": "apt-get install dotnet-sdk-8.0",
        },
    },
    "go": {
        "tagline": "Go with a `[]byte` tape and a `bufio.Reader`",
        "toolchain": "Go 1.21 or newer",
        "detect": "go version",
        "install": {
            "Windows": "winget install GoLang.Go",
            "macOS": "brew install go",
            "Debian/Ubuntu": "apt-get install golang-go",
        },
    },
    "groovy": {
        "tagline": "Groovy with a mutable `List<Character>` tape and a plain `while` loop",
        "toolchain": "Groovy 4 or newer on a JVM (17+)",
        "detect": "groovy --version",
        "install": {
            "Windows": "scoop install groovy",
            "macOS": "brew install groovy",
            "Debian/Ubuntu": "apt-get install groovy",
        },
        "notes": [
            "The runner runs it from `tests/build` so Groovy does not look for a project descriptor in the repository root.",
        ],
    },
    "haskell": {
        "tagline": "Haskell with a pure `increment` written as a reverse-and-carry fold",
        "toolchain": "GHC 9 or newer",
        "detect": "ghc --version",
        "install": {
            "Windows": "choco install ghc",
            "macOS": "brew install ghc",
            "Debian/Ubuntu": "apt-get install ghc",
        },
        "notes": [
            "`getContents` reads all of stdin, so the first line is taken and any trailing carriage return stripped; the carry is a small recursive function over the reversed string.",
        ],
    },
    "java": {
        "tagline": "Java with a `StringBuilder` tape and a `BufferedReader`",
        "toolchain": "JDK 17 or newer",
        "detect": "java -version",
        "install": {
            "Windows": "winget install Microsoft.OpenJDK.21",
            "macOS": "brew install openjdk",
            "Debian/Ubuntu": "apt-get install default-jdk",
        },
        "notes": [
            "The class is `TuringMachine`, so `javac -d tests/build/java` and `java -cp tests/build/java TuringMachine` work from the repository root.",
        ],
    },
    "javascript": {
        "tagline": "JavaScript as an ES module, reading all of stdin with `fs.readFileSync(0)`",
        "toolchain": "Node.js 18 or newer",
        "detect": "node --version",
        "install": {
            "Windows": "winget install OpenJS.NodeJS.LTS",
            "macOS": "brew install node",
            "Debian/Ubuntu": "apt-get install nodejs",
        },
        "notes": [
            "The repository's `package.json` sets `\"type\": \"module\"`, so the implementation uses `import fs from \"node:fs\"` rather than `require`.",
        ],
    },
    "julia": {
        "tagline": "Julia with a `Vector{Char}` tape and a `carry` flag loop",
        "toolchain": "Julia 1.9 or newer",
        "detect": "julia --version",
        "install": {
            "Windows": "winget install Julialang.Julia",
            "macOS": "brew install julia",
            "Debian/Ubuntu": "apt-get install julia",
        },
        "notes": [
            "`eof(stdin)` is checked before `readline`, so a closed input is treated as an empty tape instead of raising.",
        ],
    },
    "kotlin": {
        "tagline": "Kotlin with a `StringBuilder` tape and a top-level `main()`",
        "toolchain": "the Kotlin compiler (`kotlinc`) plus a JVM (17+)",
        "detect": "kotlinc -version",
        "install": {
            "Windows": "scoop install kotlin",
            "macOS": "brew install kotlin",
            "Debian/Ubuntu": "snap install kotlin --classic",
        },
        "notes": [
            "`kotlinc -include-runtime` produces a self-contained jar that runs with `java -jar`.",
        ],
    },
    "lua": {
        "tagline": "Lua with `io.read('*l')` and a 1-based head index",
        "toolchain": "Lua 5.1 or newer (or LuaJIT)",
        "detect": "lua -v",
        "install": {
            "Windows": "choco install lua",
            "macOS": "brew install lua",
            "Debian/Ubuntu": "apt-get install lua5.4",
        },
        "notes": [
            "`io.read('*l')` keeps the asterisk so the same source runs on Lua 5.1, where the bare `'l'` form is rejected.",
        ],
    },
    "nim": {
        "tagline": "Nim with `strutils.repeat` and slice concatenation",
        "toolchain": "Nim 2.0 or newer plus a C compiler",
        "detect": "nim --version",
        "install": {
            "Windows": "choco install nim",
            "macOS": "brew install nim",
            "Debian/Ubuntu": "apt-get install nim",
        },
        "notes": [
            "`stdin.readLine()` raises `EOFError` on closed input, which is caught and treated as an empty tape.",
        ],
    },
    "objectivec": {
        "tagline": "Objective-C using C stdio only, so it links without an Objective-C runtime",
        "toolchain": "clang with the Objective-C front end",
        "detect": "clang --version",
        "install": {
            "Windows": "pacman -S mingw-w64-clang-x86_64-clang (MSYS2)",
            "macOS": "xcode-select --install",
            "Debian/Ubuntu": "apt-get install clang",
        },
        "notes": [
            "The implementation deliberately uses `stdio.h` and no Foundation/`NSString`, so no `libobjc` is needed and the same source builds with any clang.",
        ],
    },
    "ocaml": {
        "tagline": "OCaml with a recursive carry over a reversed char list",
        "toolchain": "OCaml 5 (`ocamlopt`)",
        "detect": "ocamlopt -version",
        "install": {
            "Windows": "winget install OCaml.opam, then opam init",
            "macOS": "brew install opam",
            "Debian/Ubuntu": "apt-get install opam",
        },
        "notes": [
            "`ocamlopt` links through `flexlink`, so the runner puts the switch's `bin` directory first on `PATH`.",
            "It has no output-directory option and drops `TuringMachine.cmi`/`.cmx`/`.o` beside the source; those extensions are in `.gitignore`.",
        ],
    },
    "pascal": {
        "tagline": "Free Pascal with a `string` tape and explicit `Dec` carry handling",
        "toolchain": "Free Pascal 3.2 or newer (`fpc`)",
        "detect": "fpc -iV",
        "install": {
            "Windows": "scoop install freepascal",
            "macOS": "brew install fpc",
            "Debian/Ubuntu": "apt-get install fpc",
        },
        "notes": [
            "`fpc` will not create its `-FU`/`-FE` directories, so the runner makes `tests/build/pascal` first.",
        ],
    },
    "perl": {
        "tagline": "Perl 5 with an array tape and `unshift` for the overflow digit",
        "toolchain": "Perl 5",
        "detect": "perl -v",
        "install": {
            "Windows": "choco install strawberryperl",
            "macOS": "preinstalled",
            "Debian/Ubuntu": "apt-get install perl",
        },
    },
    "php": {
        "tagline": "PHP 8 with a `str_split` array tape and `array_unshift`",
        "toolchain": "PHP 8 or newer (CLI)",
        "detect": "php --version",
        "install": {
            "Windows": "scoop install php",
            "macOS": "brew install php",
            "Debian/Ubuntu": "apt-get install php-cli",
        },
        "notes": [
            "`str_split('')` is special-cased to an empty array, because older PHP releases returned `['']`.",
        ],
    },
    "powershell": {
        "tagline": "PowerShell with a generic `List[char]` tape",
        "toolchain": "Windows PowerShell 5.1 or PowerShell 7+",
        "detect": "$PSVersionTable.PSVersion",
        "install": {
            "Windows": "winget install Microsoft.PowerShell",
            "macOS": "brew install --cask powershell",
            "Debian/Ubuntu": "apt-get install powershell",
        },
        "notes": [
            "`[Console]::In.ReadLine()` returns `$null` on closed input, which is treated as an empty tape.",
        ],
    },
    "prolog": {
        "tagline": "SWI-Prolog with a recursive carry over a reversed char list",
        "toolchain": "SWI-Prolog 8 or newer (`swipl`)",
        "detect": "swipl --version",
        "install": {
            "Windows": "choco install swi-prolog",
            "macOS": "brew install swi-prolog",
            "Debian/Ubuntu": "apt-get install swi-prolog",
        },
        "notes": [
            "`read_line_to_string/2` returns the atom `end_of_file` at end of input, which is mapped to an empty tape.",
        ],
    },
    "python": {
        "tagline": "Python 3 with a list tape, an explicit `q0` loop and a `TuringMachine` class",
        "toolchain": "Python 3.8 or newer",
        "detect": "python --version",
        "install": {
            "Windows": "winget install Python.Python.3.12",
            "macOS": "brew install python",
            "Debian/Ubuntu": "apt-get install python3",
        },
        "notes": [
            "The runner uses the interpreter that runs `tests/run_tests.py`, so no separate Python install is required.",
        ],
    },
    "r": {
        "tagline": "R with a `strsplit` character vector and a `while` carry loop",
        "toolchain": "R 4 or newer (`Rscript`)",
        "detect": "Rscript --version",
        "install": {
            "Windows": "winget install RProject.R",
            "macOS": "brew install r",
            "Debian/Ubuntu": "apt-get install r-base",
        },
        "notes": [
            "`readLines(con, n = 1)` returns an empty vector on closed input, which is handled as an empty tape.",
        ],
    },
    "ruby": {
        "tagline": "Ruby with an array of characters and `Array#unshift`",
        "toolchain": "Ruby 3 or newer",
        "detect": "ruby --version",
        "install": {
            "Windows": "winget install RubyInstallerTeam.Ruby",
            "macOS": "brew install ruby",
            "Debian/Ubuntu": "apt-get install ruby",
        },
        "notes": [
            "`gets` returns `nil` on closed input and `chomp` removes the line ending, so the empty-tape case needs no special handling.",
        ],
    },
    "rust": {
        "tagline": "Rust with a `Vec<char>` tape and an `isize` head for the left overflow",
        "toolchain": "Rust 1.70 or newer (`rustc`)",
        "detect": "rustc --version",
        "install": {
            "Windows": "winget install Rustlang.Rustup",
            "macOS": "brew install rustup-init",
            "Debian/Ubuntu": "apt-get install rustc",
        },
        "notes": [
            "The head is an `isize` so it can step to `-1` and signal the carry, and `trim_end_matches` strips the line ending.",
        ],
    },
    "scala": {
        "tagline": "Scala 3 with an `ArrayBuffer[Char]` and a `@main` method",
        "toolchain": "Scala CLI 1.x (`scala`)",
        "detect": "scala --version",
        "install": {
            "Windows": "scoop install scala",
            "macOS": "brew install scala",
            "Debian/Ubuntu": "apt-get install scala",
        },
        "notes": [
            "The runner runs it from `langs/Scala` so Scala CLI's `.scala-build` cache lands beside the source (git-ignored) instead of creating a project directory at the repository root.",
        ],
    },
    "simula": {
        "tagline": "Simula 67 with `text` concatenation and a `while` carry loop",
        "toolchain": "a Simula 67 compiler (Cim, or a classic DEC/UNIVAC port)",
        "detect": "cim --version",
        "install": {
            "Windows": "build Cim from source (no packaged Windows build)",
            "macOS": "build Cim from source",
            "Debian/Ubuntu": "build Cim from source",
        },
        "notes": [
            "No Simula compiler is installed on the machine this repository was developed on, so the runner skips this implementation; the source is kept for a machine that has Cim.",
        ],
    },
    "swift": {
        "tagline": "Swift with an `Array<Character>` tape and a `while true` carry loop",
        "toolchain": "Swift 5.9 or newer (`swiftc`)",
        "detect": "swiftc --version",
        "install": {
            "Windows": "winget install Swift.Toolchain",
            "macOS": "xcode-select --install",
            "Debian/Ubuntu": "apt-get install swift",
        },
        "notes": [
            "On Windows `swiftc` needs `SDKROOT` and the MSVC environment; the runner sets both, so the README's snippet assumes a configured Swift shell.",
        ],
    },
    "typescript": {
        "tagline": "TypeScript with a private `TuringMachine` class compiled by `tsc`",
        "toolchain": "the TypeScript compiler (`tsc`) plus Node.js 18+",
        "detect": "tsc --version",
        "install": {
            "Windows": "npm install -g typescript",
            "macOS": "npm install -g typescript",
            "Debian/Ubuntu": "npm install -g typescript",
        },
        "notes": [
            "The runner compiles it with the TypeScript 7 build that has a repository `tsconfig.json`, so it passes `--ignoreConfig` before emitting `tests/build/ts/TuringMachine.js`.",
        ],
    },
    "v": {
        "tagline": "V with direct string slicing and `'0'.repeat(n)` for the overflow",
        "toolchain": "V 0.4 or newer",
        "detect": "v version",
        "install": {
            "Windows": "scoop install v",
            "macOS": "brew install vlang",
            "Debian/Ubuntu": "git clone https://github.com/vlang/v && make",
        },
        "notes": [
            "`os.get_line()` returns the line without its terminator, and the trailing carriage return is trimmed explicitly.",
        ],
    },
    "vbnet": {
        "tagline": "VB.NET with `StringBuilder` and `Do...Loop` carry handling",
        "toolchain": ".NET SDK 8 or newer (VB.NET)",
        "detect": "dotnet --version",
        "install": {
            "Windows": "winget install Microsoft.DotNet.SDK.8",
            "macOS": "brew install --cask dotnet-sdk",
            "Debian/Ubuntu": "apt-get install dotnet-sdk-8.0",
        },
    },
    "vba": {
        "tagline": "VBA with `Mid`/`Left` string surgery, reading `input.txt`",
        "toolchain": "Microsoft Office (the VBA editor)",
        "detect": "Alt+F11 in Excel/Word",
        "install": {
            "Windows": "install Microsoft Office",
            "macOS": "install Microsoft Office",
            "Debian/Ubuntu": "not available (Office is Windows/macOS only)",
        },
        "notes": [
            "VBA has no console stdin, so the tape is read from `input.txt` beside the workbook and the two result lines go to the Immediate window with `Debug.Print`.",
            "No VBA interpreter is available on the machine this repository was developed on, so the runner skips this implementation.",
        ],
    },
    "zig": {
        "tagline": "Zig 0.16 with the new `std.Io` file API and a fixed tape buffer",
        "toolchain": "Zig 0.16 or newer",
        "detect": "zig version",
        "install": {
            "Windows": "choco install zig",
            "macOS": "brew install zig",
            "Debian/Ubuntu": "apt-get install zig",
        },
        "notes": [
            "The stdin read uses `std.Io.File.stdin().readStreaming(...)` and output uses `writeStreamingAll`, both through a `std.Io.Threaded` instance - the API introduced in Zig 0.16.",
            "The tape and the output buffer are fixed-size arrays, so no allocator is needed for the simulation.",
        ],
    },
}


def render(meta: dict, language: dict) -> str:
    facts = _facts(meta)
    name = meta["name"]
    folder = meta["folder"]          # e.g. "PHP" or "Assembly/64bit"
    source = meta["source"]          # e.g. "TuringMachine.php"
    source_path = "langs/%s/%s" % (folder, source)
    tagline = facts.get("tagline", "An implementation of the canonical machine")

    lines: list[str] = []
    lines.append("# %s Implementation" % name)
    lines.append("")
    lines.append("%s." % tagline)
    lines.append("")
    lines.append(
        "One of %d implementations of the same console protocol in the\n"
        "[Turing Machines](../../README.md) repository: every implementation reads\n"
        "the same stdin and prints **byte-for-byte identical output**, so a single\n"
        "master test verifies them all at once." % IMPLEMENTATION_COUNT
    )
    lines.append("")
    lines.append("* **Category:** %s" % meta["category"])
    lines.append("* **Source:** [`%s`](%s)" % (source_path, source))
    lines.append("* **File-extension tag:** `%s`" % meta["extension"])
    lines.append("* **Showcase route:** [%s/%s](%s/%s)" % (SITE, meta["id"], SITE, meta["id"]))
    lines.append("")

    lines.append("## Prerequisites")
    lines.append("")
    lines.append("* **Toolchain:** %s" % facts.get("toolchain", "see the build command below"))
    lines.append("* **Check it is installed:** `%s`" % facts.get("detect", "see below"))
    lines.append("")
    install = facts.get("install") or {}
    if install:
        lines.append("| Platform | Install command |")
        lines.append("| --- | --- |")
        for platform in ("Windows", "macOS", "Debian/Ubuntu"):
            if platform in install:
                lines.append("| %s | `%s` |" % (platform, install[platform]))
        lines.append("")

    lines.append("## How to run")
    lines.append("")
    lines.append("From the repository root, build this implementation and replay every")
    lines.append("scenario:")
    lines.append("")
    lines.append("```sh")
    lines.append("python tests/run_tests.py --only %s" % meta["id"])
    lines.append("```")
    lines.append("")
    lines.append("Build and run it on its own:")
    lines.append("")
    manual = MANUAL_RUN.get(meta["id"])
    if manual:
        lines.extend(manual)
        lines.append("")
    else:
        lines.append("```sh")
        for command in PRELUDE.get(meta["id"], []):
            lines.append(command)
        for command in _build_commands(language):
            lines.append(command)
        lines.append(_argv_text(language["run"]))
        lines.append("```")
        lines.append("")

    lines.append("## Protocol")
    lines.append("")
    lines.append(
        "[`README.md`](../../README.md#the-machine-and-the-console-protocol)\n"
        "documents the machine and the exact output, and\n"
        "[`tests/run_tests.py`](../../tests/run_tests.py) holds the build and run\n"
        "commands the suite uses for every language.\n"
        "[`tests/expected/`](../../tests/expected) is the authoritative golden output."
    )
    lines.append("")
    lines.append(
        "The nine scenarios cover a plain increment (`1011`), `0`, `1`, a full carry\n"
        "chain (`1111` -> `10000`), a leading zero (`00101` -> `00110`), an empty\n"
        "tape, a long tape (`0101010101` -> `0101010110`), rejected invalid input and\n"
        "end of input."
    )
    lines.append("")

    notes = list(facts.get("notes") or [])
    projects = {"csharp": "TuringMachine.csproj", "vbnet": "TuringMachine.vbproj",
                "fsharp": "TuringMachine.fsproj"}
    if meta["id"] in projects:
        notes.append(
            "The `%s` file beside the source is what `dotnet build` compiles, so the project"
            " file and the source must be kept together." % projects[meta["id"]]
        )
    if notes:
        lines.append("## Notes")
        lines.append("")
        for note in notes:
            lines.append("* %s" % note)
        lines.append("")

    lines.append(
        "This README is generated by\n"
        "[`tools/generate_language_readmes.py`](../../tools/generate_language_readmes.py)\n"
        "from the showcase metadata and the test runner - re-run that script rather\n"
        "than editing it by hand."
    )
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    written = 0
    for meta in LANGUAGE_INFO:
        language = LANGUAGES.get(meta["id"])
        if language is None:
            print("warning: %s has no entry in tests/run_tests.py" % meta["id"])
            continue
        if meta["id"] not in LANGUAGE_FACTS:
            print("warning: %s has no LANGUAGE_FACTS entry (generic README text)" % meta["id"])
        destination = LANG_DIR / meta["folder"] / "README.md"
        destination.parent.mkdir(parents=True, exist_ok=True)
        with open(destination, "w", encoding="utf-8", newline="\n") as handle:
            handle.write(render(meta, language))
        written += 1
    print("Wrote %d README.md files under langs/ (%d languages, %d implementations)"
          % (written, LANGUAGE_COUNT, IMPLEMENTATION_COUNT))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

