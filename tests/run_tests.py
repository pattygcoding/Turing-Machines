#!/usr/bin/env python3
"""Master test runner for the Turing Machines Language Tree.

Every implementation reads the same stdin and must print the same stdout for the
canonical machine documented in README.md (a binary incrementer).  This script:

  1. builds every implementation,
  2. runs each one against every scripted scenario in ``tests/inputs/``,
  3. checks that all implementations agree with each other, and
  4. checks that the agreed output matches the golden file in
     ``tests/expected/``.

Usage::

    python tests/run_tests.py                 # run the suite
    python tests/run_tests.py --only python   # just some implementations
    python tests/run_tests.py --strict        # treat a missing toolchain as failure
    python tests/run_tests.py --update        # regenerate the golden files

Adding a language: append an entry to ``LANGUAGES`` below (see AGENTS.md).
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
# Every language lives in its own folder under langs/ (langs/PHP, langs/Java...).
LANG_DIR = ROOT / "langs"
TEST_DIR = ROOT / "tests"
INPUT_DIR = TEST_DIR / "inputs"
EXPECTED_DIR = TEST_DIR / "expected"
BUILD_DIR = TEST_DIR / "build"

TIMEOUT_SECONDS = 30
BUILD_TIMEOUT_SECONDS = 240


def gnucobol_env() -> dict:
    """Env overrides so GnuCOBOL uses its own bundled MinGW toolchain."""
    candidates = []
    home = os.environ.get("COB_MAIN_DIR")
    if home:
        candidates.append(Path(home))
    resolved = shutil.which("cobc")
    if resolved:
        candidates.append(Path(resolved).resolve().parent.parent)
    if os.name == "nt":
        candidates.append(Path(r"C:\ProgramData\chocolatey\lib\gnucobol\tools"))
    for tools_dir in candidates:
        # A Chocolatey shim directory has bin/cobc.exe but no config tree, so
        # the real toolchain directory is the one that carries default.conf.
        if (tools_dir / "bin" / "cobc.exe").exists() and (tools_dir / "config" / "default.conf").exists():
            return {
                "PATH": str(tools_dir / "bin") + os.pathsep + os.environ.get("PATH", ""),
                "COB_CONFIG_DIR": str(tools_dir / "config"),
                "COB_COPY_DIR": str(tools_dir / "copy"),
                "COB_CFLAGS": '-I"%s"' % (tools_dir / "include"),
                "COB_LDFLAGS": '-L"%s"' % (tools_dir / "lib"),
                "COB_LIBRARY_PATH": str(tools_dir / "extras"),
            }
    return {}


def mingw64_env() -> dict:
    """Env overrides so assembly/Nim use the x86-64 MinGW-w64 tools."""
    candidates = []
    resolved = shutil.which("gcc")
    if resolved:
        candidates.append(Path(resolved).resolve().parent)
    if os.name == "nt":
        candidates.append(Path(r"C:\mingw64\bin"))
        candidates.append(Path(r"C:\ProgramData\mingw64\mingw64\bin"))
    for bin_dir in candidates:
        if (bin_dir / "x86_64-w64-mingw32-gcc.exe").exists():
            return {"PATH": str(bin_dir) + os.pathsep + os.environ.get("PATH", "")}
    return {}


def vcvars_env() -> dict:
    """Env overrides so Swift can find the MSVC headers and libraries."""
    if os.name != "nt":
        return {}
    vswhere = Path(os.environ.get("ProgramFiles(x86)", r"C:\Program Files (x86)")) / (
        "Microsoft Visual Studio/Installer/vswhere.exe"
    )
    if not vswhere.exists():
        return {}
    result = subprocess.run(
        [str(vswhere), "-latest", "-property", "installationPath"],
        capture_output=True,
        text=True,
    )
    install = result.stdout.strip()
    if not install:
        return {}
    vcvars = Path(install) / "VC/Auxiliary/Build/vcvars64.bat"
    if not vcvars.exists():
        return {}
    shell = subprocess.run(
        ["cmd", "/c", '"%s" && set' % vcvars],
        capture_output=True,
        text=True,
    )
    env = {}
    for line in shell.stdout.splitlines():
        name, _, value = line.partition("=")
        if name:
            env[name] = value
    return env


def swift_env() -> dict:
    """Env overrides so ``swiftc`` builds and runs on Windows."""
    root = Path(os.environ.get("LOCALAPPDATA", "")) / "Programs" / "Swift"
    toolchains = sorted(root.glob("Toolchains/*/usr/bin"))
    runtimes = sorted(root.glob("Runtimes/*/usr/bin"))
    sdks = sorted(root.glob("Platforms/*/Windows.platform/Developer/SDKs/Windows.sdk"))
    if not (toolchains and runtimes and sdks):
        return {}
    env = dict(vcvars_env())
    parts = [str(toolchains[-1]), str(runtimes[-1])]
    if env.get("PATH"):
        parts.append(env["PATH"])
    env["PATH"] = os.pathsep.join(parts)
    env["SDKROOT"] = str(sdks[-1])
    return env


def fpc_tool() -> str:
    """Path to the Free Pascal compiler."""
    resolved = shutil.which("fpc")
    if resolved:
        return resolved
    if os.name == "nt":
        home = os.environ.get("USERPROFILE", "")
        candidates = [
            Path(home) / "scoop" / "shims" / "fpc.exe",
            Path(r"C:\FPC\3.2.2\bin\i386-win32\fpc.exe"),
            Path(r"C:\tools\fpc\bin\i386-win32\fpc.exe"),
        ]
        for candidate in candidates:
            if candidate.exists():
                return str(candidate)
    return "fpc"


def _fpc_output_dir() -> str:
    """Output directory for the Pascal build, created if it does not exist."""
    path = BUILD_DIR / "pascal"
    path.mkdir(parents=True, exist_ok=True)
    return str(path)


def bash_tool() -> str:
    """Path to a bash interpreter (Git for Windows or MSYS2, never WSL)."""
    resolved = shutil.which("bash")
    if resolved and not (os.name == "nt" and "system32" in resolved.lower()):
        return resolved
    if os.name == "nt":
        candidates = [
            Path(r"C:\Program Files\Git\bin\bash.exe"),
            Path(r"C:\Program Files\Git\usr\bin\bash.exe"),
            Path(r"C:\msys64\usr\bin\bash.exe"),
        ]
        for candidate in candidates:
            if candidate.exists():
                return str(candidate)
    return "bash"


def php_env() -> dict:
    """Env overrides so PHP finds a local unpacked build."""
    candidates = []
    resolved = shutil.which("php")
    if resolved:
        candidates.append(Path(resolved).resolve().parent)
    if os.name == "nt":
        candidates.append(Path(r"C:\tools\php"))
    local = os.environ.get("LOCALAPPDATA")
    if local:
        candidates.append(Path(local) / "php")
    for bin_dir in candidates:
        if (bin_dir / "php.exe").exists():
            return {"PATH": str(bin_dir) + os.pathsep + os.environ.get("PATH", "")}
    return {}


def ocaml_tool() -> str:
    """Path to ``ocamlopt``.

    The opam shim can pause to check for updates, so the switch's compiler is
    preferred directly when it is installed under the default opam prefix.
    """
    resolved = shutil.which("ocamlopt")
    if resolved:
        return resolved
    if os.name == "nt":
        candidates = [
            Path(os.environ.get("LOCALAPPDATA", "")) / "opam" / "default" / "bin" / "ocamlopt.exe",
            Path(r"C:\tools\ocaml\bin\ocamlopt.exe"),
        ]
        for candidate in candidates:
            if candidate.exists():
                return str(candidate)
    return "ocamlopt"


def ocaml_env() -> dict:
    """Put the OCaml switch's bin directory first, so ``flexlink`` is found."""
    tool = ocaml_tool()
    if tool != "ocamlopt":
        return {"PATH": str(Path(tool).parent) + os.pathsep + os.environ.get("PATH", "")}
    return {}


def _gnat_tools_dir() -> Path | None:
    """Directory holding a GNAT (Ada) toolchain, if one is installed."""
    resolved = shutil.which("gnatmake")
    if resolved:
        return Path(resolved).resolve().parent
    if os.name == "nt":
        candidates = [Path(r"C:\msys64\mingw64\bin"), Path(r"C:\GNAT\bin")]
        alire = Path(os.environ.get("LOCALAPPDATA", "")) / "alire" / "cache" / "toolchains"
        candidates.extend(sorted(alire.glob("gnat_native_*/bin")))
        for candidate in candidates:
            if (candidate / "gnatmake.exe").exists():
                return candidate
    return None


def gnat_tool() -> str:
    """Path to ``gnatmake``, the GNAT build driver."""
    tools = _gnat_tools_dir()
    if tools is None:
        return "gnatmake"
    return str(tools / ("gnatmake.exe" if os.name == "nt" else "gnatmake"))


def gnat_env() -> dict:
    """Env overrides so GNAT's own bin directory (Alire, MSYS2) is used."""
    tools = _gnat_tools_dir()
    if tools is None:
        return {}
    return {"PATH": str(tools) + os.pathsep + os.environ.get("PATH", "")}


def _ada_project() -> None:
    """Stage the Ada source under tests/build/ada.

    GNAT only recognises ``.adb``/``.ads`` as Ada source, and a unit's file name
    must match the procedure name, so the repository file (which keeps the Ada 83
    ``.ada`` extension) is copied in under its GNAT name.  Nothing in the
    repository is generated or modified.
    """
    project = BUILD_DIR / "ada"
    project.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(LANG_DIR / "Ada" / "TuringMachine.ada", project / "turingmachine.adb")


DOTNET_ENV = {
    "DOTNET_CLI_TELEMETRY_OPTOUT": "1",
    "DOTNET_SKIP_FIRST_TIME_EXPERIENCE": "1",
    "DOTNET_NOLOGO": "1",
}


def _src(folder: str, filename: str) -> str:
    return str(LANG_DIR / folder / filename)


def _exe(name: str) -> str:
    return str(BUILD_DIR / (name + (".exe" if os.name == "nt" else "")))


def _run_build(language: dict) -> None:
    """Run a language's optional prepare hook and then its build steps.

    Output goes to a log file under tests/build/ rather than a pipe: a toolchain
    that spawns a helper (OCaml's ocamlopt -> flexlink -> ld, for instance) can
    leave that helper holding an inherited pipe, which makes ``subprocess.run``
    block on ``communicate`` even after the direct child is killed.  A file has
    no reader to keep open, and it gives a useful log when a build fails.
    """
    prepare = language.get("prepare")
    if prepare:
        prepare()
    steps = language.get("build")
    if not steps:
        return
    if steps and not isinstance(steps[0], list):
        steps = [steps]
    env = language_env(language)
    for index, step in enumerate(steps):
        log_path = BUILD_DIR / ("%s.build%d.log" % (language["name"], index))
        with open(log_path, "w", encoding="utf-8", errors="replace") as log:
            process = subprocess.Popen(
                resolve_command(step, env),
                stdout=log,
                stderr=subprocess.STDOUT,
                env=env,
                cwd=language.get("cwd"),
            )
            try:
                returncode = process.wait(timeout=BUILD_TIMEOUT_SECONDS)
            except subprocess.TimeoutExpired:
                if os.name == "nt":
                    subprocess.run(
                        ["taskkill", "/F", "/T", "/PID", str(process.pid)],
                        capture_output=True,
                    )
                else:
                    process.kill()
                process.wait()
                raise RuntimeError(
                    "build timed out after %d seconds (see %s)"
                    % (BUILD_TIMEOUT_SECONDS, log_path)
                )
        if returncode != 0:
            output = log_path.read_text(encoding="utf-8", errors="replace") if log_path.exists() else ""
            raise RuntimeError(
                "failed to build %s:\n%s" % (language["name"], output)
            )


LANGUAGES = [
    {
        "name": "python",
        "source": _src("Python", "TuringMachine.py"),
        "artifact": None,
        "build": None,
        "run": [sys.executable, _src("Python", "TuringMachine.py")],
    },
    {
        "name": "javascript",
        "source": _src("JavaScript", "TuringMachine.js"),
        "artifact": None,
        "build": None,
        "run": ["node", _src("JavaScript", "TuringMachine.js")],
    },
    {
        "name": "typescript",
        "source": _src("TypeScript", "TuringMachine.ts"),
        "artifact": str(BUILD_DIR / "ts" / "TuringMachine.js"),
        "build": [
            "tsc",
            _src("TypeScript", "TuringMachine.ts"),
            "--target", "ES2020",
            "--module", "es2020",
            "--types", "node",
            "--outDir", str(BUILD_DIR / "ts"),
            "--skipLibCheck",
            "--ignoreConfig",
        ],
        "run": ["node", str(BUILD_DIR / "ts" / "TuringMachine.js")],
    },
    {
        "name": "dart",
        "source": _src("Dart", "TuringMachine.dart"),
        "artifact": _exe("turing_machine_dart"),
        "build": ["dart", "compile", "exe",
                  "-o", _exe("turing_machine_dart"),
                  _src("Dart", "TuringMachine.dart")],
        "run": [_exe("turing_machine_dart")],
    },
    {
        "name": "powershell",
        "source": _src("Powershell", "TuringMachine.ps1"),
        "artifact": None,
        "build": None,
        "run": ["powershell", "-NoProfile", "-ExecutionPolicy", "Bypass",
                "-File", _src("Powershell", "TuringMachine.ps1")],
    },
    {
        "name": "bash",
        "source": _src("Bash", "TuringMachine.sh"),
        "artifact": None,
        "build": None,
        "run": [bash_tool(), "--noprofile", "--norc",
                _src("Bash", "TuringMachine.sh")],
    },
    {
        "name": "c",
        "source": _src("C", "TuringMachine.c"),
        "artifact": _exe("turing_machine_c"),
        "build": ["gcc", "-std=c11", "-O2",
                  "-o", _exe("turing_machine_c"), _src("C", "TuringMachine.c")],
        "run": [_exe("turing_machine_c")],
    },
    {
        "name": "cpp",
        "source": _src("C++", "TuringMachine.cpp"),
        "artifact": _exe("turing_machine_cpp"),
        "build": ["g++", "-std=c++17", "-O2",
                  "-o", _exe("turing_machine_cpp"), _src("C++", "TuringMachine.cpp")],
        "run": [_exe("turing_machine_cpp")],
    },
    {
        "name": "go",
        "source": _src("Go", "TuringMachine.go"),
        "artifact": _exe("turing_machine_go"),
        "build": ["go", "build", "-o", _exe("turing_machine_go"), _src("Go", "TuringMachine.go")],
        "run": [_exe("turing_machine_go")],
    },
    {
        "name": "assembly64",
        "source": _src("Assembly/64bit", "TuringMachine.asm"),
        "artifact": _exe("turing_machine_asm64"),
        "build": [
            ["nasm", "-f", "win64", "-o", str(BUILD_DIR / "turing_machine_asm64.obj"),
             _src("Assembly/64bit", "TuringMachine.asm")],
            ["gcc", "-o", _exe("turing_machine_asm64"), str(BUILD_DIR / "turing_machine_asm64.obj")],
        ],
        "run": [_exe("turing_machine_asm64")],
        "env": mingw64_env(),
    },
    {
        "name": "assembly32",
        "source": _src("Assembly/32bit", "TuringMachine.asm"),
        "artifact": _exe("turing_machine_asm32"),
        "build": [
            ["nasm", "-f", "win32", "-o", str(BUILD_DIR / "turing_machine_asm32.obj"),
             _src("Assembly/32bit", "TuringMachine.asm")],
            ["gcc", "-m32", "-o", _exe("turing_machine_asm32"),
             str(BUILD_DIR / "turing_machine_asm32.obj")],
        ],
        "run": [_exe("turing_machine_asm32")],
        "env": mingw64_env(),
        "skip": "no 32-bit MinGW runtime is installed (32-bit source is kept for reference)",
    },
    {
        "name": "nim",
        "source": _src("Nim", "TuringMachine.nim"),
        "artifact": _exe("turing_machine_nim"),
        "build": ["nim", "c", "--hints:off", "--warnings:off", "-d:release",
                  "--nimcache:" + str(BUILD_DIR / "nimcache"),
                  "-o:" + _exe("turing_machine_nim"),
                  _src("Nim", "TuringMachine.nim")],
        "run": [_exe("turing_machine_nim")],
        "env": mingw64_env(),
    },
    {
        "name": "zig",
        "source": _src("Zig", "TuringMachine.zig"),
        "artifact": _exe("turing_machine_zig"),
        "build": ["zig", "build-exe", "-O", "ReleaseFast",
                  "--name", "turing_machine_zig",
                  "-femit-bin=" + _exe("turing_machine_zig"),
                  _src("Zig", "TuringMachine.zig")],
        "run": [_exe("turing_machine_zig")],
    },
    {
        "name": "objectivec",
        "source": _src("Objective-C", "TuringMachine.m"),
        "artifact": _exe("turing_machine_objc"),
        "build": ["clang", "-x", "objective-c", "-O2",
                  "-o", _exe("turing_machine_objc"),
                  _src("Objective-C", "TuringMachine.m")],
        "run": [_exe("turing_machine_objc")],
    },
    {
        "name": "ocaml",
        "source": _src("OCaml", "TuringMachine.ml"),
        "artifact": _exe("turing_machine_ocaml"),
        "build": [ocaml_tool(), "-o", _exe("turing_machine_ocaml"),
                  _src("OCaml", "TuringMachine.ml")],
        "run": [_exe("turing_machine_ocaml")],
        "env": ocaml_env(),
    },
    {
        "name": "fortran",
        "source": _src("Fortran", "TuringMachine.f90"),
        "artifact": _exe("turing_machine_fortran"),
        "build": ["gfortran", "-O2", "-o", _exe("turing_machine_fortran"),
                  _src("Fortran", "TuringMachine.f90")],
        "run": [_exe("turing_machine_fortran")],
        "env": mingw64_env(),
    },
    {
        "name": "php",
        "source": _src("PHP", "TuringMachine.php"),
        "artifact": None,
        "build": None,
        "run": ["php", _src("PHP", "TuringMachine.php")],
        "env": php_env(),
    },
    {
        "name": "swift",
        "source": _src("Swift", "TuringMachine.swift"),
        "artifact": _exe("turing_machine_swift"),
        "build": ["swiftc", "-O", "-o", _exe("turing_machine_swift"),
                  _src("Swift", "TuringMachine.swift")],
        "run": [_exe("turing_machine_swift")],
        "env": swift_env(),
    },
    {
        "name": "haskell",
        "source": _src("Haskell", "TuringMachine.hs"),
        "artifact": _exe("turing_machine_haskell"),
        "build": ["ghc", "-O2",
                  "-outputdir", str(BUILD_DIR / "haskell"),
                  "-o", _exe("turing_machine_haskell"),
                  _src("Haskell", "TuringMachine.hs")],
        "run": [_exe("turing_machine_haskell")],
    },
    {
        "name": "rust",
        "source": _src("Rust", "TuringMachine.rs"),
        "artifact": _exe("turing_machine_rust"),
        "build": ["rustc", "-O", "-o", _exe("turing_machine_rust"),
                  _src("Rust", "TuringMachine.rs")],
        "run": [_exe("turing_machine_rust")],
    },
    {
        "name": "java",
        "source": _src("Java", "TuringMachine.java"),
        "artifact": str(BUILD_DIR / "java" / "TuringMachine.class"),
        "build": ["javac", "-d", str(BUILD_DIR / "java"), _src("Java", "TuringMachine.java")],
        "run": ["java", "-cp", str(BUILD_DIR / "java"), "TuringMachine"],
    },
    {
        "name": "csharp",
        "source": _src("C#", "TuringMachine.cs"),
        "artifact": str(BUILD_DIR / "csharp" / "TuringMachine.dll"),
        "build": ["dotnet", "build", _src("C#", "TuringMachine.csproj"),
                  "-c", "Release", "-o", str(BUILD_DIR / "csharp"),
                  "--nologo", "-v", "quiet"],
        "run": ["dotnet", str(BUILD_DIR / "csharp" / "TuringMachine.dll")],
        "env": DOTNET_ENV,
    },
    {
        "name": "vbnet",
        "source": _src("VB.NET", "TuringMachine.vb"),
        "artifact": str(BUILD_DIR / "vbnet" / "TuringMachine.dll"),
        "build": ["dotnet", "build", _src("VB.NET", "TuringMachine.vbproj"),
                  "-c", "Release", "-o", str(BUILD_DIR / "vbnet"),
                  "--nologo", "-v", "quiet"],
        "run": ["dotnet", str(BUILD_DIR / "vbnet" / "TuringMachine.dll")],
        "env": DOTNET_ENV,
    },
    {
        "name": "fsharp",
        "source": _src("F#", "TuringMachine.fs"),
        "artifact": str(BUILD_DIR / "fsharp" / "TuringMachine.dll"),
        "build": ["dotnet", "build", _src("F#", "TuringMachine.fsproj"),
                  "-c", "Release", "-o", str(BUILD_DIR / "fsharp"),
                  "--nologo", "-v", "quiet"],
        "run": ["dotnet", str(BUILD_DIR / "fsharp" / "TuringMachine.dll")],
        "env": DOTNET_ENV,
    },
    {
        "name": "kotlin",
        "source": _src("Kotlin", "TuringMachine.kt"),
        "artifact": str(BUILD_DIR / "TuringMachine-kotlin.jar"),
        "build": ["kotlinc", _src("Kotlin", "TuringMachine.kt"), "-include-runtime",
                  "-d", str(BUILD_DIR / "TuringMachine-kotlin.jar")],
        "run": ["java", "-jar", str(BUILD_DIR / "TuringMachine-kotlin.jar")],
    },
    {
        "name": "scala",
        "source": _src("Scala", "TuringMachine.scala"),
        "artifact": None,
        "build": None,
        # Run from the language folder so Scala CLI's .scala-build cache lands
        # beside the source (git-ignored) instead of creating a project dir at
        # the repository root.
        "run": ["scala", "TuringMachine.scala"],
        "cwd": str(LANG_DIR / "Scala"),
    },
    {
        "name": "clojure",
        "source": _src("Clojure", "TuringMachine.clj"),
        "artifact": None,
        "build": None,
        "run": ["clojure", "-M", _src("Clojure", "TuringMachine.clj")],
        "cwd": str(BUILD_DIR),
    },
    {
        "name": "groovy",
        "source": _src("Groovy", "TuringMachine.groovy"),
        "artifact": None,
        "build": None,
        "run": ["groovy", _src("Groovy", "TuringMachine.groovy")],
        "cwd": str(BUILD_DIR),
    },
    {
        "name": "ruby",
        "source": _src("Ruby", "TuringMachine.rb"),
        "artifact": None,
        "build": None,
        "run": ["ruby", _src("Ruby", "TuringMachine.rb")],
    },
    {
        "name": "perl",
        "source": _src("Perl", "TuringMachine.pl"),
        "artifact": None,
        "build": None,
        "run": ["perl", _src("Perl", "TuringMachine.pl")],
    },
    {
        "name": "lua",
        "source": _src("Lua", "TuringMachine.lua"),
        "artifact": None,
        "build": None,
        "run": ["lua", _src("Lua", "TuringMachine.lua")],
    },
    {
        "name": "elixir",
        "source": _src("Elixir", "TuringMachine.exs"),
        "artifact": None,
        "build": None,
        "run": ["elixir", _src("Elixir", "TuringMachine.exs")],
    },
    {
        "name": "erlang",
        "source": _src("Erlang", "TuringMachine.erl"),
        "artifact": str(BUILD_DIR / "TuringMachine.beam"),
        "build": ["erlc", "-o", str(BUILD_DIR), _src("Erlang", "TuringMachine.erl")],
        "run": ["erl", "-noshell", "-pa", str(BUILD_DIR),
                "-s", "TuringMachine", "main", "-s", "init", "stop"],
    },
    {
        "name": "prolog",
        "source": _src("Prolog", "TuringMachine.pl"),
        "artifact": None,
        "build": None,
        "run": ["swipl", "-q", "-g", "main", "-t", "halt",
                _src("Prolog", "TuringMachine.pl")],
    },
    {
        "name": "julia",
        "source": _src("Julia", "TuringMachine.julia"),
        "artifact": None,
        "build": None,
        "run": ["julia", "--startup-file=no", "--color=no",
                _src("Julia", "TuringMachine.julia")],
    },
    {
        "name": "r",
        "source": _src("R", "TuringMachine.r"),
        "artifact": None,
        "build": None,
        "run": ["Rscript", "--vanilla", _src("R", "TuringMachine.r")],
    },
    {
        "name": "v",
        "source": _src("V", "TuringMachine.v"),
        "artifact": _exe("turing_machine_v"),
        "build": ["v", "-o", _exe("turing_machine_v"), _src("V", "TuringMachine.v")],
        "run": [_exe("turing_machine_v")],
    },
    {
        "name": "pascal",
        "source": _src("Pascal", "TuringMachine.pas"),
        "artifact": _exe("pascal/TuringMachine"),
        "build": [fpc_tool(), "-O2",
                  "-FU" + _fpc_output_dir(),
                  "-FE" + _fpc_output_dir(),
                  _src("Pascal", "TuringMachine.pas")],
        "run": [_exe("pascal/TuringMachine")],
    },
    {
        "name": "cobol",
        "source": _src("COBOL", "TuringMachine.cob"),
        "artifact": _exe("turing_machine_cobol"),
        "build": ["cobc", "-x", "-free", "-o", _exe("turing_machine_cobol"),
                  _src("COBOL", "TuringMachine.cob")],
        "run": [_exe("turing_machine_cobol")],
        "env": gnucobol_env(),
    },
    {
        "name": "ada",
        "source": _src("Ada", "TuringMachine.ada"),
        "artifact": _exe("turing_machine_ada"),
        "prepare": _ada_project,
        "build": [gnat_tool(), "-O2", "-D", str(BUILD_DIR / "ada"),
                  "-o", _exe("turing_machine_ada"),
                  str(BUILD_DIR / "ada" / "turingmachine.adb")],
        "run": [_exe("turing_machine_ada")],
        "env": gnat_env(),
    },
    {
        "name": "simula",
        "source": _src("Simula", "TuringMachine.sim"),
        "artifact": None,
        "build": None,
        "run": ["cim", _src("Simula", "TuringMachine.sim")],
        "skip": "no Simula compiler is installed here",
    },
    {
        "name": "vba",
        "source": _src("VBA", "TuringMachine.vba"),
        "artifact": None,
        "build": None,
        "run": ["cscript", "//nologo", _src("VBA", "TuringMachine.vba")],
        "skip": "no VBA interpreter is installed here (Office is required)",
    },
]


# --------------------------------------------------------------------------
# Scenarios.  Each maps to tests/inputs/<name>.txt and tests/expected/<name>.txt.
# ``expect`` is the line the output must contain.
# --------------------------------------------------------------------------
SCENARIOS = [
    {"name": "basic", "expect": "Final tape: 1100"},
    {"name": "zero", "expect": "Final tape: 1"},
    {"name": "one", "expect": "Final tape: 10"},
    {"name": "carry_chain", "expect": "Final tape: 10000"},
    {"name": "leading_zero", "expect": "Final tape: 00110"},
    {"name": "empty", "expect": "Final tape: 1"},
    {"name": "long", "expect": "Final tape: 0101010110"},
    {"name": "invalid", "expect": "Invalid input: expected a binary string."},
    {"name": "eof", "expect": "Final tape: 1"},
]


def normalize(text: str) -> str:
    """Normalise line endings and trailing whitespace for comparison.

    Trailing whitespace is ignored so an implementation may print the
    ``Initial tape: `` label with a trailing space even when the input is empty.
    """
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    return "\n".join(line.rstrip() for line in text.split("\n")).strip("\n")


def language_env(language: dict) -> dict:
    env = os.environ.copy()
    env.update(language.get("env") or {})
    return env


def resolve_command(argv: list, env: dict | None = None) -> list:
    """Resolve argv[0] on PATH (Windows ``.cmd``/``.bat`` shims included)."""
    if not argv:
        return argv
    program = None
    if env is not None:
        program = shutil.which(argv[0], path=env.get("PATH"))
    if not program:
        program = shutil.which(argv[0])
    if not program:
        candidate = Path(argv[0])
        if candidate.exists():
            program = str(candidate)
    if program:
        return [program] + list(argv[1:])
    return list(argv)


def missing_tool(language: dict) -> str | None:
    """Return the name of the first required program that is not installed."""
    commands = []
    build = language.get("build")
    if build:
        commands.extend(build if isinstance(build[0], list) else [build])
    if language.get("run"):
        commands.append(language["run"])
    for argv in commands:
        if not argv:
            continue
        program = resolve_command(argv)[0]
        # A built program lives under tests/build/ and does not exist until the
        # build has run; only toolchain launchers are checked here.
        try:
            if BUILD_DIR.resolve() in Path(program).resolve().parents:
                continue
        except OSError:
            pass
        if shutil.which(program) is None and not Path(program).exists():
            return program
    return None


def build_language(language: dict) -> None:
    _run_build(language)


def run_language(language: dict, stdin_bytes: bytes) -> str:
    env = language_env(language)
    try:
        result = subprocess.run(
            resolve_command(language["run"], env),
            input=stdin_bytes,
            capture_output=True,
            timeout=TIMEOUT_SECONDS,
            cwd=language.get("cwd"),
            env=env,
        )
    except subprocess.TimeoutExpired:
        return "(%s timed out after %d seconds)" % (language["name"], TIMEOUT_SECONDS)
    return normalize(result.stdout.decode("utf-8", errors="replace"))


def first_difference(actual: str, expected: str) -> str:
    actual_lines = actual.split("\n")
    expected_lines = expected.split("\n")
    for index in range(max(len(actual_lines), len(expected_lines))):
        got = actual_lines[index] if index < len(actual_lines) else "<missing>"
        want = expected_lines[index] if index < len(expected_lines) else "<missing>"
        if got != want:
            return "line %d:\n      expected: %r\n      actual:   %r" % (index + 1, want, got)
    return "outputs differ only by length"


def select_languages(only: str | None) -> list:
    if not only:
        return list(LANGUAGES)
    wanted = [name.strip().lower() for name in only.split(",") if name.strip()]
    return [language for language in LANGUAGES if language["name"].lower() in wanted]


def run_suite(update: bool, only: str | None, strict: bool) -> int:
    EXPECTED_DIR.mkdir(parents=True, exist_ok=True)
    BUILD_DIR.mkdir(parents=True, exist_ok=True)
    failures = []
    skipped = []

    languages = select_languages(only)
    if only:
        known = {language["name"].lower() for language in LANGUAGES}
        for name in (name.strip().lower() for name in only.split(",") if name.strip()):
            if name not in known:
                failures.append("unknown language %r" % name)

    runnable = []
    for language in languages:
        if language.get("skip"):
            skipped.append("%s: %s" % (language["name"], language["skip"]))
            continue
        tool = missing_tool(language)
        if tool:
            skipped.append("%s: required program %r is not installed" % (language["name"], tool))
            continue
        runnable.append(language)

    print("Building implementations...")
    built = []
    for language in runnable:
        try:
            build_language(language)
        except Exception as error:  # noqa: BLE001 - surface the toolchain message
            failures.append("%s: %s" % (language["name"], error))
            print("  FAIL build %s" % language["name"])
            continue
        built.append(language)
        print("  built %s" % language["name"])

    for scenario in SCENARIOS:
        name = scenario["name"]
        input_path = INPUT_DIR / ("%s.txt" % name)
        if not input_path.exists():
            failures.append("%s: missing input file %s" % (name, input_path))
            print("FAIL %s (missing input)" % name)
            continue
        stdin_bytes = input_path.read_bytes()

        outputs = {}
        for language in built:
            outputs[language["name"]] = run_language(language, stdin_bytes)

        names = list(outputs)
        consistent = bool(names)
        for other in names[1:]:
            if outputs[other] != outputs[names[0]]:
                consistent = False
                failures.append(
                    "%s: %s and %s disagree\n    %s"
                    % (name, names[0], other, first_difference(outputs[other], outputs[names[0]]))
                )

        agreed = outputs[names[0]] if names else ""
        if names and scenario["expect"] not in agreed:
            consistent = False
            failures.append("%s: output does not contain %r" % (name, scenario["expect"]))

        if update and consistent:
            golden = agreed + "\n" if not agreed.endswith("\n") else agreed
            # Always LF, so the committed goldens are identical on every platform.
            with open(EXPECTED_DIR / ("%s.txt" % name), "w", encoding="utf-8", newline="\n") as handle:
                handle.write(golden)
            print("WROTE %s" % name)
            continue

        expected_path = EXPECTED_DIR / ("%s.txt" % name)
        if not expected_path.exists():
            failures.append("%s: missing golden file %s" % (name, expected_path))
            print("FAIL %s (missing golden file)" % name)
            continue
        expected = normalize(expected_path.read_text(encoding="utf-8"))
        if agreed != expected:
            consistent = False
            failures.append("%s: output differs from golden file\n    %s"
                            % (name, first_difference(agreed, expected)))
        print("%s %s" % ("PASS" if consistent else "FAIL", name))

    print()
    if skipped:
        print("%d skipped implementation(s):" % len(skipped))
        for line in skipped:
            print("  - %s" % line)
        if strict:
            failures.extend("skipped: %s" % line for line in skipped)
    if failures:
        print("%d failure(s):" % len(failures))
        for failure in failures:
            print("  - %s" % failure)
        return 1
    print("All %d scenarios passed for %d implementation(s)." % (len(SCENARIOS), len(built)))
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--update", action="store_true",
                        help="regenerate the golden output files instead of checking them")
    parser.add_argument("--only", default=None,
                        help="comma-separated implementation names to run")
    parser.add_argument("--strict", action="store_true",
                        help="treat skipped implementations as failures")
    args = parser.parse_args()
    return run_suite(update=args.update, only=args.only, strict=args.strict)


if __name__ == "__main__":
    sys.exit(main())


