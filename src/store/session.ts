import { computed, reactive } from "vue";
import type { Item, TabId } from "../types";
import * as data from "./data";

export type MenuId = "category";

export interface FacetOption {
    value: string;
    label: string;
}

// ---- The address ----------------------------------------------------------------
// Every implementation is addressable on its own path: /python, /c, /objectivec.
// The dashboard is a static bundle, so the path is resolved client-side with the
// History API.  A document with a null origin (file://) refuses path writes, so the
// query-string shape (?lang=python) is used there instead - and that query shape is
// also what the deployed site's 404.html shim forwards, so `?lang=` stays readable
// in both forms and older links keep working.
const APP_DIR = window.location.pathname.replace(/[^/]*$/, "");
let applyingHistory = false;

// Can this document write a path?  Probing once here, rather than discovering it
// when the first write fails, keeps the sidebar's hrefs in the right shape from the
// very first render; the temporary URL is restored in the same tick, before
// anything can observe it.
let canWritePath = (function probePathWrites(): boolean {
    const original = window.location.href;
    try {
        window.history.replaceState(window.history.state, "", APP_DIR + "__path-probe__");
    } catch {
        return false;
    }
    try {
        window.history.replaceState(window.history.state, "", original);
    } catch {
        // keep going: the probe already told us path writes are allowed
    }
    return true;
})();

interface LocationState {
    langId: string | null;
    tab: TabId;
    scenarioId: string | null;
    explicitTab: boolean;
}

// What the current address asks for, in either shape.  `/python/output` names the
// tab on a path, `?tab=output` names it in a query - both shapes the 404 shim
// forwards.
function locationState(): LocationState {
    const tail = window.location.pathname.slice(APP_DIR.length).replace(/\/+$/, "");
    const parts = tail ? tail.split("/") : [];
    const pathTab = parts.length === 2 && /^(code|output)$/.test(parts[1]) ? parts[1] : "";
    const first = (parts[0] === "index.html" ? "" : parts[0]).toLowerCase();
    const params = new URLSearchParams(window.location.search || "");
    const fromQuery = (params.get("lang") || "").toLowerCase();
    const candidate = data.knownLang(first) ? first : fromQuery;
    const rawTab = pathTab || params.get("tab");
    return {
        langId: data.knownLang(candidate) ? candidate : null,
        tab: rawTab === "output" ? "output" : "code",
        scenarioId: params.get("scenario"),
        explicitTab: !!(pathTab || params.get("tab")),
    };
}

// The address for what is on screen, in whichever shape the browser accepts.  Used
// for the sidebar's real links too, so a copied or middle-clicked link carries the
// implementation, the tab and the scenario along.
export function stateUrl(langId: string, tab: TabId, scenarioId: string | null): string {
    const params = new URLSearchParams();
    if (!canWritePath) {
        params.set("lang", langId);
    }
    // Addresses stay clean: name a tab only when it is not the one the item opens
    // on anyway.
    if (!data.tabAvailableFor(tab)) {
        tab = "code";
    }
    if (tab !== data.defaultTabFor()) {
        params.set("tab", tab);
        if (tab === "output" && scenarioId) {
            params.set("scenario", scenarioId);
        }
    }
    const base = canWritePath ? APP_DIR + langId : window.location.pathname;
    const query = params.toString();
    return query ? base + "?" + query : base;
}

function syncUrl(mode: "push" | "replace"): void {
    if (applyingHistory || !state.langId) {
        return;
    }
    const url = stateUrl(state.langId, state.tab, state.scenarioId);
    try {
        window.history[mode === "push" ? "pushState" : "replaceState"](null, "", url);
    } catch {
        // Belt and braces: keep the query shape such a document allows.
        canWritePath = false;
        syncUrl(mode);
    }
}

// Back/forward: rebuild the view from the address without writing it again.
function applyLocation(): void {
    const target = locationState();
    applyingHistory = true;
    state.langId = target.langId ?? data.defaultLangId();
    state.tab = target.explicitTab ? target.tab : data.defaultTabFor();
    applyScenario(target.scenarioId);
    applyingHistory = false;
}

// ---- Selection ------------------------------------------------------------------
export const state = reactive({
    langId: null as string | null,
    tab: "code" as TabId,
    /** The console capture the Console Output tab shows. */
    scenarioId: null as string | null,
    query: "",
    category: "All",
    /** The mobile drawer: a slide-in sidebar below `lg`. */
    sidebarOpen: false,
    /** The one filter dropdown allowed open at a time. */
    openMenu: null as MenuId | null,
});

export const currentItem = computed<Item | undefined>(() => data.itemById(state.langId));

export const visibleItems = computed<Item[]>(() => {
    const query = state.query.trim().toLowerCase();
    return data.languages().filter((item) => {
        const matchesCategory = state.category === "All" || item.category === state.category;
        const haystack = (item.name + " " + item.id + " " + (item.category || "")).toLowerCase();
        return matchesCategory && (query === "" || haystack.includes(query));
    });
});

export const listMessage = computed<string>(() => {
    if (data.languages().length === 0) {
        return "No languages yet.";
    }
    return "No languages match your search.";
});

export const searchPlaceholder = "Search languages...";

export const metaLine = computed<string>(() => {
    const meta = data.DATA?.meta;
    const languageCount = meta?.languageCount || data.languages().length;
    const implementationCount = meta?.implementationCount || languageCount;
    let counts = languageCount + " language" + (languageCount === 1 ? "" : "s");
    if (implementationCount !== languageCount) {
        counts += " \u00b7 " + implementationCount + " implementations";
    }
    return counts + " \u00b7 updated " + (meta?.updated || "n/a");
});

export const repoUrl = computed<string>(() => (currentItem.value ? data.repoUrlFor(currentItem.value) : ""));

// The repository root, behind the sidebar's GitHub mark: a real link rather than a
// per-folder one, so the whole project is one click away from every screen.
export const repoHomeUrl = computed<string>(() => data.repoHomeUrl());

export const repoFolderName = computed<string>(() => (currentItem.value ? data.repoFolder(currentItem.value) : ""));

// ---- Console output -------------------------------------------------------------
// The captures are shared by every implementation (that is the byte-for-byte
// protocol), so the tab exists whenever the test suite has produced goldens.
export const showOutputTab = computed<boolean>(() => data.tabAvailableFor("output"));

export const consoleCaption = computed<string>(() => data.scenarioLabel(state.scenarioId));

export const outputText = computed<string>(() => data.outputFor(state.scenarioId));

// ---- Facets ---------------------------------------------------------------------
export const categoryOptions = computed<FacetOption[]>(() => {
    const present: string[] = [];
    for (const item of data.languages()) {
        if (item.category && !present.includes(item.category)) {
            present.push(item.category);
        }
    }
    const ordered = data.CATEGORY_ORDER.filter((name) => present.includes(name)).concat(
        present.filter((name) => !data.CATEGORY_ORDER.includes(name)),
    );
    return [{ value: "All", label: "All categories" }].concat(
        ordered.map((name) => ({ value: name, label: name })),
    );
});

// ---- Actions --------------------------------------------------------------------
export function setQuery(value: string): void {
    state.query = value;
}

export function setFacet(facet: MenuId, value: string): void {
    state[facet] = value;
}

// Move the selection and write the address.  A tap on a list row also ends that
// interaction, so the mobile drawer goes away.
export function selectLanguage(id: string): void {
    state.langId = id;
    syncUrl("push");
    state.sidebarOpen = false;
}

export function switchTab(tab: TabId): void {
    if (!data.tabAvailableFor(tab)) {
        tab = "code";
    }
    const changed = state.tab !== tab;
    state.tab = tab;
    if (changed) {
        syncUrl("push");
    }
}

export function selectScenario(id: string): void {
    state.scenarioId = id;
    syncUrl("replace");
}

// The scenario belongs to the console-output address; an unknown or absent id falls
// back to the preferred capture so a bare URL still shows something.
function applyScenario(id: string | null): void {
    const all = data.scenarios();
    const preferred = all.find((scenario) => scenario.id === "basic");
    state.scenarioId = data.knownScenario(id) ? id : (preferred ?? all[0])?.id ?? null;
}

export function toggleSidebar(): void {
    state.sidebarOpen = !state.sidebarOpen;
}

export function closeSidebar(): void {
    state.sidebarOpen = false;
}

export function toggleMenu(id: MenuId): void {
    state.openMenu = state.openMenu === id ? null : id;
}

export function closeMenus(): void {
    state.openMenu = null;
}

// Mirror the address into the view; the address is only written at the end (the
// selection taken from the URL must not add a history entry).
export function initSession(): void {
    if (!data.hasData) {
        return;
    }
    const target = locationState();
    state.langId = target.langId ?? data.defaultLangId();
    applyScenario(target.scenarioId);
    state.tab = target.explicitTab ? target.tab : data.defaultTabFor();
    syncUrl("replace");   // canonical address for what is on screen

    window.addEventListener("popstate", applyLocation);
}

