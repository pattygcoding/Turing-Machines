// The shape of `public/dashboard-data.js`, which tools/generate_dashboard.py writes
// from the real repository.  Every implementation has the same shape, so the
// sidebar, the header and the code panel stay unaware of which language they show.

export interface Item {
    id: string;
    name: string;
    category: string;
    /** The Prism grammar class, e.g. `language-ada`. */
    prism: string;
    /** The source path shown above the code and linked on GitHub. */
    file: string;
    /** The folder the GitHub link points at (the file's directory by default). */
    folder?: string;
    /** An optional sentence shown above the code. */
    note?: string;
    code: string;
}

export type TabId = "code" | "output";

export interface Scenario {
    id: string;
    label: string;
}

export interface DataMeta {
    updated: string;
    languageCount: number;
    implementationCount: number;
    scenarioCount: number;
    repository: string;
    branch: string;
}

export interface DashboardData {
    meta: DataMeta;
    /** The verified console scenarios, in the order tests/expected lists them. */
    scenarios: Scenario[];
    /** scenario id -> the verified console capture in tests/expected/. */
    outputs: Record<string, string>;
    languages: Item[];
    /** language name -> its file-extension tag (".ada", ".cs", ...). */
    languageExtensions: Record<string, string>;
}

declare global {
    interface Window {
        /** Installed by public/dashboard-data.js, before the app module runs. */
        TURING_MACHINE_DATA?: DashboardData;
    }
}
