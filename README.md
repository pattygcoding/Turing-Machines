# Turing Machines by Patrick Goodwin

Is it Turing Complete? If the language is one of the folders in this project, then yes, it is!

## Language showcase

**<https://turing.pattygcoding.com>** is a browser for every implementation in
this repository. The sidebar lists all 41 languages - 42 implementations, because
Assembly ships 32-bit and 64-bit builds - and each one is shown with syntax
highlighting, line numbers and a link to its folder on GitHub.

Every implementation is addressable on its own path, in the same shape as the
Connect Four showcase:

```
/                  the default implementation (Python)
/python            Python
/csharp            C#
/cpp               C++
/objectivec        Objective-C
/assembly64        Assembly (64-bit)
```

Each language folder also carries its own generated README with prerequisites,
install commands and standalone build/run steps (for example
[`langs/Java/README.md`](langs/Java/README.md) or
[`langs/Assembly/64bit/README.md`](langs/Assembly/64bit/README.md)).

The page is a static Vue 3 + TypeScript + Vite bundle. A language id is resolved
client-side with the History API; `?lang=python` is the fallback shape used by the
GitHub Pages `404.html` shim mentioned below. Each implementation also has a
**Console Output** tab showing the verified capture from `tests/expected/`, with a
picker for every scenario - the same tab the Connect Four showcase has.

## The machine and the console protocol

Every implementation runs the same machine: a **binary incrementer**. The tape
starts as the bits read from stdin; in state `q0` a `1` is rewritten to `0` and the
head moves left (carry), the first `0` is rewritten to `1` and the machine halts,
and a blank becomes a new leading `1` and the machine halts.

The output is fixed, and `tests/expected/` is the authoritative reference:

```
Initial tape: 1011
Final tape: 1100
```

An input that is not a string of `0` and `1` prints exactly:

```
Invalid input: expected a binary string.
```

Scenarios covered: a plain increment (`1011`), `0`, `1`, a full carry chain
(`1111` -> `10000`), a leading zero (`00101` -> `00110`), an empty tape, a long
tape (`0101010101` -> `0101010110`), rejected invalid input, and end of input.

## Running the tests

```sh
python tests/run_tests.py
```

The runner builds every implementation, replays each scenario from
`tests/inputs/` through all of them, and checks that:

1. all implementations agree with each other, and
2. the agreed output matches the golden capture in `tests/expected/`.

Regenerate the goldens after an intentional protocol change:

```sh
python tests/run_tests.py --update
```

Use `--only python,c,rust` while working on one implementation, and `--strict` to
turn a skipped toolchain into a failure. The 32-bit Assembly build, Simula and
VBA are skipped where their toolchain is not installed (no 32-bit runtime, no
Simula compiler, no VBA interpreter outside Office); every other language is built
and compared.

### Running locally

```sh
npm install
npm run dev
```

`npm run dev` serves the data file that is already on disk. `npm start`
regenerates it first, and `npm run data` regenerates it on its own:

```sh
npm run data      # tools/generate_dashboard.py -> public/dashboard-data.js
npm run readmes   # tools/generate_language_readmes.py -> langs/*/README.md
npm run typecheck # vue-tsc --noEmit
npm run build     # data + typecheck + vite build -> dist/
npm run preview   # serve the production build
```

The generated `public/dashboard-data.js` and the built `dist/` are ignored by
git; both are recreated by the commands above. The per-language READMEs are
generated too, but they are committed, so they only change when a language is
added or a build command changes.

### Adding a language

1. Add the implementation as `langs/<Language>/TuringMachine.<ext>`, following the
   console protocol above (no top-of-file banner; read one line and print the two
   result lines).
2. Register it in the `LANGUAGES` list in `tests/run_tests.py` with its `build`
   and `run` commands, then run `python tests/run_tests.py --only <name>` until it
   agrees with the goldens.
3. Add an entry to `LANGUAGE_INFO` in `tools/generate_dashboard.py` with its
   `id` (the URL slug), display `name`, `folder`, `source` file, Prism grammar
   class, category and file extension.
4. Add a `LANGUAGE_FACTS` entry (tagline, toolchain, install commands, notes) to
   `tools/generate_language_readmes.py`.
5. Run `npm run data` to refresh the showcase data, then `npm run readmes` to
   write the new `langs/<Language>/README.md`.

The generator only reads files that exist, so a missing folder or source file is
skipped rather than advertised as a broken link.

### Deep links and GitHub Pages

GitHub Pages has no rewrite rules, so `public/404.html` is part of the routing
contract: it is served for any unknown path with the address bar untouched, and
rebuilds the request as `index.html?lang=<id>`. The app then reads `?lang=`,
switches to that implementation and writes the pretty path back with
`history.replaceState`. A tab and scenario travel the same way, so
`/python/output?scenario=basic` and `?lang=python&tab=output&scenario=basic` are
equivalent. A hop counter stops a junk path from redirecting in a loop.

### Deployment

`.github/workflows/deploy.yml` builds and publishes the site on every push to
`main`: it installs Node 22 and Python, runs `npm ci` and `npm run build` (which
regenerates the data, type-checks and writes `dist/`), then uploads `dist/` as
the Pages artifact.

One-time settings per repository:

* **Settings -> Pages -> Build and deployment -> Source: GitHub Actions**, or
  `gh api -X PUT repos/pattygcoding/Turing-Machines/pages -f build_type=workflow`.
* **Settings -> Pages -> Custom domain**: `turing.pattygcoding.com`, or
  `gh api -X PUT repos/pattygcoding/Turing-Machines/pages -f cname=turing.pattygcoding.com`.
  An artifact-sourced deployment ignores a `CNAME` file, so `public/CNAME` is
  documentation only.

The DNS side is a single record at the domain's provider:

```
turing   CNAME   pattygcoding.github.io
```

If the provider proxies traffic (Cloudflare does by default), switch that record
to **DNS only** (grey cloud); a proxied record breaks Pages' certificate checks.

### Repository layout

```
langs/                          one folder per language (langs/PHP, langs/Java, ...)
  <Language>/TuringMachine.<ext>   the implementations the showcase reads
  <Language>/README.md             generated: prerequisites, build and run
tests/run_tests.py              master test runner (builds + diffs every language)
tests/inputs/                   scripted stdin for each scenario
tests/expected/                 golden stdout for each scenario
tools/generate_dashboard.py     builds public/dashboard-data.js
tools/generate_language_readmes.py  writes the per-language READMEs
tools/generate_art.py           rebuilds logo.png and og-image.png
src/                            Vue app: store, components, styles
public/                         static files copied verbatim into dist/
  404.html                      deep-link fallback for /<language-id>
  CNAME, .nojekyll, logo.png, og-image.png
  dashboard-data.js             generated, not tracked
index.html                      Vite entry: head metadata + mount point
package.json                    data, dev, build, typecheck, preview scripts
vite.config.ts                  relative base so one build works at any prefix
tailwind.config.js              the design tokens
tsconfig.json                   TypeScript configuration
.github/workflows/deploy.yml    builds dist/ and publishes it to GitHub Pages
```
