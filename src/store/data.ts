import type { DashboardData, Item, Scenario, TabId } from "../types";

// Everything the dashboard displays, read straight from the generated payload.
// These helpers are pure lookups over `DATA` - the reactive selection on top of
// them lives in `session.ts`.
export const DATA: DashboardData | undefined = window.TURING_MACHINE_DATA;

// A checkout that has not run the generator yet still boots and shows the empty
// state (which says how to generate the data) instead of throwing.
export const hasData: boolean = !!DATA && Array.isArray(DATA.languages) && DATA.languages.length > 0;

// The implementation the showcase falls back to when the address names none (`/`
// and `index.html` both land here).  Python is the most readable reference, so it
// opens first; a fork without it falls back to the first language present.
export const DEFAULT_LANG_ID = "python";

// The last-resort repository, for the one place a link cannot read `meta`: a
// checkout that has not generated the showcase data yet.  The generator writes the
// real one (git remote, else GITHUB_REPOSITORY, else this same default).
const DEFAULT_REPOSITORY = "https://github.com/pattygcoding/Turing-Machines";

export function languages(): Item[] {
    return DATA?.languages ?? [];
}

export function itemById(id: string | null): Item | undefined {
    if (!id) {
        return undefined;
    }
    return languages().find((item) => item.id === id);
}

export function knownLang(id: string | null | undefined): boolean {
    return !!id && languages().some((item) => item.id === id);
}

export function defaultLangId(): string | null {
    const all = languages();
    return (all.find((lang) => lang.id === DEFAULT_LANG_ID) ?? all[0])?.id ?? null;
}

export function scenarios(): Scenario[] {
    return DATA?.scenarios ?? [];
}

export function knownScenario(id: string | null | undefined): boolean {
    return !!id && scenarios().some((scenario) => scenario.id === id);
}

export function scenarioLabel(id: string | null): string {
    return scenarios().find((scenario) => scenario.id === id)?.label ?? "Console output";
}

// The verified capture the suite checks.  It is shared by every implementation,
// which is the whole point of the byte-for-byte protocol.
export function outputFor(id: string | null): string {
    const text = id ? DATA?.outputs?.[id] : "";
    return text || "(no capture)";
}

// Whether an item can show a tab.  Every implementation has source; the console
// tab needs the captures the test suite generated.
export function tabAvailableFor(tab: TabId): boolean {
    if (tab === "output") {
        return scenarios().length > 0;
    }
    return true;
}

// The tab an implementation opens on.
export function defaultTabFor(): TabId {
    return "code";
}

// The folder an implementation lives in, used for its GitHub link.  The generator
// records it explicitly (Assembly's builds sit in subfolders), with the file's own
// directory as a fallback.
export function repoFolder(item: Item): string {
    if (item.folder) {
        return item.folder;
    }
    return (item.file || "").replace(/\/[^/]*$/, "");
}

// `<repository>/tree/<branch>/<folder>`.  Folder names here are real language
// names (`C#`, `C++`, `F#`, `VB.NET`), so every path segment is URL-encoded - a
// raw `#` would otherwise be read as a fragment and break the link.
export function repoUrlFor(item: Item): string {
    const repository = DATA?.meta?.repository;
    if (!repository) {
        return "";
    }
    const path = repoFolder(item).split("/").map(encodeURIComponent).join("/");
    return repository + "/tree/" + (DATA?.meta?.branch || "main") + "/" + path;
}

// The repository root itself, behind the sidebar's GitHub mark - the same generated
// `meta.repository` the per-folder links are built from, so a fork marks its own
// project.  A checkout that has not run the generator yet has no `meta` at all, so
// the committed default stands in (the generator's own last-resort default).
export function repoHomeUrl(): string {
    return DATA?.meta?.repository || DEFAULT_REPOSITORY;
}

// A language's short file-extension tag (".py", ".cs") - how the sidebar and the
// header identify it.
export function languageTag(name: string): string {
    return DATA?.languageExtensions?.[name] ?? name;
}

// The line-number column beside the code: one number per line, trimmed of the
// trailing blank lines a file ends with.
export function lineNumbersFor(text: string): string {
    const normalized = text.replace(/\n+$/, "");
    const count = normalized.length ? normalized.split("\n").length : 1;
    const numbers: number[] = [];
    for (let line = 1; line <= count; line += 1) {
        numbers.push(line);
    }
    return numbers.join("\n");
}

// Fixed reading order for the Category facet, so the menu matches the palette of
// language families rather than the alphabet of whichever language sorts first.
export const CATEGORY_ORDER = [
    "Systems",
    "Scripting",
    "Functional",
    "JVM",
    ".NET",
    "Web",
    "Scientific",
    "Logic",
    "Legacy",
];
