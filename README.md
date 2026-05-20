# Injury Outcome Dashboard

An interactive R Shiny dashboard from the [University of Michigan Injury Prevention Center (IPC)](https://injurycenter.umich.edu/) that visualizes U.S. injury-outcome data at the **state** and **county** level. Users can switch between a standard choropleth map (crude death rates) and a Gi\* hotspot map, drill down by clicking on a state or county, and compare crude rate against other variables via a built-in regression panel.

- **Live site (IPC):** https://injurycenter.umich.edu/injury-outcome-dashboard/
- **Hosted deployment:** https://ipcapp.shinyapps.io/injury-outcome-dashboard/

---

## Table of Contents

1. [Purpose](#1-purpose)
2. [What the Dashboard Shows](#2-what-the-dashboard-shows)
3. [Adding New Data (the main user workflow)](#3-adding-new-data-the-main-user-workflow)
4. [How to View Your Changes](#4-how-to-view-your-changes)
5. [Running the App](#5-running-the-app)
6. [Project Structure](#6-project-structure)
7. [Architecture (for Developers)](#7-architecture-for-developers)
8. [Deploying to shinyapps.io](#8-deploying-to-shinyappsio)
9. [Dependencies](#9-dependencies)
10. [Troubleshooting](#10-troubleshooting)

---

## 1. Purpose

The dashboard is a public-facing tool for researchers, clinicians, and community members to explore U.S. injury-outcome data geographically. It launched with **unintentional drug overdose** data sourced from the CDC, and is designed to grow over time as the IPC team adds more injury types (e.g. firearm, suicide, motor-vehicle), more years, and more demographic breakdowns (sex, race/ethnicity, age groups).

The data backend is intentionally **file-driven**: adding a new dataset does **not** require editing R code. As long as a new CDC WONDER export is dropped into the correct folder under `data/` (see [Section 3](#3-adding-new-data-the-main-user-workflow)), the dashboard will pick it up automatically the next time the database is rebuilt — the new injury type / year / demographic will appear in the dropdown menus on the site.

---

## 2. What the Dashboard Shows

- **Level toggle** — switch between U.S. states and counties.
- **Map type toggle**
  - *Standard*: choropleth shaded by crude death rate per 100,000.
  - *Hotspot Analysis*: Gi\* spatial autocorrelation (k = 5 nearest-neighbor graph of polygon centroids; z-scores binned at ±1.96 into Cold / Neutral / Hot).
- **Filter dropdowns** — Injury Type, Demographics, Period (year). The choices are populated automatically from whatever data is present in `data/`.
- **State selector** (only active in county view) — pick which state's counties to show.
- **Click-to-select** — clicking any state or county updates the summary statistics panel.
- **Summary statistics** — crude rate, total deaths, total population, national average for context.
- **Scatter / regression panel** — compares the state-level crude rate for the selected injury type and year against an environmental variable (Annual Mean Temperature °F or Annual Total Precipitation inches), sourced from NOAA nClimDiv. Each point is one state; the panel shows OLS slope, R², Pearson r, and p-value.

`CRUDE_RATE == -1.0` is the sentinel value the app uses for CDC-suppressed "Unreliable" rates (counties with very small counts); they render as "Unreliable" in the summary table rather than as a number.

---

## 3. Adding New Data (the main user workflow)

This is the section to read if you are Dr. Larson, an RA, or anyone else who needs to add a new CDC WONDER export to the dashboard.

### 3.1 The folder rule

Every raw `.txt` export lives under a strict four-level folder hierarchy inside `data/`:

```
data/{level}/{injury_type}/{period}/{demographic}/<file>.txt
```

| Component | What it means | Allowed values |
|-----------|---------------|----------------|
| `{level}` | Geographic granularity | `state` or `county` |
| `{injury_type}` | The injury / cause | snake_case (e.g. `unintentional_drug_overdose`, `firearm_suicide`, `motor_vehicle`) |
| `{period}` | The year (or time window) | A simple string, typically a year (e.g. `2022`, `2023`, `2024`) |
| `{demographic}` | The demographic slice | snake_case (e.g. `all_demographics`, `male`, `female`, `white`, `black_or_african_american`, `hispanic_or_latino`, `age_25_34_years`) |

**Naming rules:**

- All lowercase.
- Replace spaces with underscores (`_`).
- Do **not** use hyphens (`-`), spaces, or special characters in folder names.
- The folder names ARE the metadata — the dashboard parses them directly off the path to populate the dropdowns. Whatever you write as the folder name is what will be converted into the dropdown label (e.g. `all_demographics` → "All Demographics", `unintentional_drug_overdose` → "Unintentional Drug Overdose").

### 3.2 The filename rule

Inside the deepest folder, name the file so it carries the same metadata as the path:

```
{level}_{injury_type}_{period}_{demographic}.txt
```

This is for human readability — if a file is ever moved out of context, the filename alone tells you what it is. The script does not depend on the filename (only on the path), but please follow the convention so the project stays tidy.

### 3.3 Complete example: adding 2024 male overdose data

Suppose Dr. Larson has just downloaded a CDC WONDER export covering **unintentional drug overdose deaths in 2024 for males**, both at state and county level. The two new files would be placed exactly here:

```
data/
└── state/
    └── unintentional_drug_overdose/
        └── 2024/
            └── male/
                └── state_unintentional_drug_overdose_2024_male.txt
└── county/
    └── unintentional_drug_overdose/
        └── 2024/
            └── male/
                └── county_unintentional_drug_overdose_2024_male.txt
```

After [reloading the app](#4-how-to-view-your-changes), the dashboard will automatically:

- Add **2024** to the **Period** dropdown.
- Add **Male** to the **Demographics** dropdown.
- Show the new map when the user selects that combination.

No R code changes are needed, and no manual database step is needed — `./run.sh` detects the new file and rebuilds the SQLite database before the app boots.

### 3.4 The file format

The dashboard expects the file to be a **CDC WONDER tab-separated export**, with these column headers (in this order):

**State-level file:**

```
Notes    State    State Code    Deaths    Population    Crude Rate
```

**County-level file:**

```
Notes    State    State Code    County    County Code    Deaths    Population    Crude Rate
```

Notes about the format:

- Tab-separated (`.txt` as exported by CDC WONDER — do **not** open and re-save as a different delimiter).
- CDC WONDER footer rows (where the `State` column is blank or says `Total`) are automatically stripped on import — leave them in if you want, the script handles them.
- Crude rates that CDC WONDER labels as **"Unreliable"** are stored as `-1.0` internally and rendered as "Unreliable" in the dashboard's summary table.
- Suppressed death counts will become `NA` and will not affect the map color.

If you export from CDC WONDER and untick any of the required columns, the script will error when you rebuild the database — re-export with all columns enabled.

### 3.5 Quick checklist for adding a dataset

1. [ ] Export the dataset from CDC WONDER, save as `.txt`.
2. [ ] Create the four-level folder path inside `data/` (lowercase, underscores).
3. [ ] Drop the `.txt` in and rename it following the `{level}_{injury_type}_{period}_{demographic}.txt` convention.
4. [ ] Run `./run.sh` and check the dropdowns (next section).

---

## 4. How to View Your Changes

This section is the "I just want to see my new data on the site" walkthrough. No prior shell experience needed.

### 4.1 What you need

- You are logged into the U-M Great Lakes / Turbo environment (typically via VS Code Remote-SSH, a terminal SSH session, or TurboVNC).
- The repo is at `~/injury_outcome_dashboard/`.
- You have already placed your new `.txt` file(s) according to [Section 3](#3-adding-new-data-the-main-user-workflow).

### 4.2 Just one command

```bash
cd ~/injury_outcome_dashboard
./run.sh
```

That is it. On every start, the app scans `data/` for `.txt` files that are newer than the SQLite database (or any new files that aren't in it yet) and rebuilds the database automatically before booting. You will see one of these two messages near the top of the output:

```
Data changes detected (or DB missing) — rebuilding SQLite from data/ ...
overdose_by_state: 102 rows written from 2 file(s)
...
```

…meaning your new file was picked up, **or:**

```
SQLite is up to date with data/ — skipping rebuild.
```

…meaning nothing changed since the last run. Either way, after about 10–30 seconds you should see:

```
Listening on http://127.0.0.1:3838
```

> **Note on deletions:** the auto-detect only watches for *new* or *modified* files. If you **delete** a `.txt` file from `data/` and want the database to forget about it, run `FORCE_DB_REBUILD=1 ./run.sh` (or `Rscript setup_db.R` once) — that wipes and rebuilds the DB from scratch.

### 4.3 Opening the dashboard in a browser

- **In VS Code Remote-SSH:** a "Open in Browser" pop-up will appear in the bottom right when port 3838 starts listening — click it. If you miss the pop-up, click the **Ports** tab at the bottom of VS Code and click the globe icon next to port `3838`.
- **In TurboVNC / a desktop session on Great Lakes:** open Firefox or Chrome and go to `http://127.0.0.1:3838`.
- **In a plain SSH terminal:** set up port forwarding when you SSH (`ssh -L 3838:localhost:3838 user@greatlakes.arc-ts.umich.edu`), then on your laptop browser go to `http://localhost:3838`.

### 4.4 Verifying your new data appeared

On the dashboard, open the **Period** and **Demographics** dropdowns. Your new period / demographic should now be in the list. Select it and watch the map repaint.

If the combination has no data (for example, you only added 2024 male state-level data but selected "2024 / Female"), the dashboard will show:

> *No data for the selected Injury Type / Period / Demographic combination.*

That is the expected behavior — it is the app telling you to pick a combination that actually has a file backing it.

### 4.5 Stopping the app

Press **Ctrl+C** in the terminal where `./run.sh` is running.

---

## 5. Running the App

For users comfortable with the shell, the short version:

```bash
cd ~/injury_outcome_dashboard
./run.sh
```

`./run.sh` always runs `global.R` first, which auto-rebuilds `data/injury_outcomes.sqlite` if any `.txt` under `data/` is newer than the database (or if the database is missing). No manual `Rscript setup_db.R` step is needed in the normal workflow.

To **force** a rebuild even when the mtime check would skip it (useful after deleting a file, or if you suspect the DB is out of sync):

```bash
FORCE_DB_REBUILD=1 ./run.sh
```

You can still run the standalone script if you prefer:

```bash
module load Rgeospatial/4.5.1-2025-10-07
Rscript scripts/setup_db.R
```

Inside `run.sh` is just:

```bash
module load Rgeospatial/4.5.1-2025-10-07
Rscript ~/injury_outcome_dashboard/scripts/run_app.R
```

`scripts/run_app.R` calls `shiny::runApp(".", port = 3838, launch.browser = FALSE)`.

### 5.1 First-time setup (only once per user)

Install missing R packages (this can take 10+ minutes the first time):

```bash
bash install_packages.sh
```

### 5.2 Refreshing injury data from CDC (fetch_cdc.sh)

The injury data comes from the CDC's [Mapping Injury, Overdose, and Violence](https://www.cdc.gov/injury-violence-data/data-vis/index.html) dataset (updated monthly). To pull the latest data for all injury types and all years:

```bash
./fetch_cdc.sh
```

You can narrow the fetch with flags:

```bash
./fetch_cdc.sh --period 2024              # latest year only
./fetch_cdc.sh --injury Drug_OD           # one injury type
./fetch_cdc.sh --level state              # state level only
./fetch_cdc.sh --injury Drug_OD --period 2024 --level both
```

Available `--injury` values: `Drug_OD`, `All_Suicide`, `All_Homicide`, `FA_Deaths`, `FA_Homicide`, `FA_Suicide`, or `all` (default).  
Available `--period` values: `2019`–`2024`, or `all` (default).

After fetching, restart the app with `./run.sh` — the DB rebuilds automatically.

### 5.3 Refreshing environmental data from NOAA (fetch_env.sh)

The scatter panel uses annual mean temperature (°F) and total precipitation (inches) from NOAA's [nClimDiv](https://www.ncei.noaa.gov/pub/data/cirs/climdiv/) dataset (updated monthly). To download or refresh:

```bash
./fetch_env.sh
```

This takes about 30–60 seconds (two file downloads). It writes `env_by_state` and `env_by_county` tables directly into `data/injury_outcomes.sqlite` — no DB rebuild needed afterward. Restart the app to pick up the new values.

---

## 6. Project Structure

```
injury_outcome_dashboard/
├── app.R                # Entry point — sources global.R, ui.R, server.R
├── global.R             # SQLite connect, table load, shapefile load, filter-option lists
├── ui.R                 # Layout and controls
├── server.R             # Reactives, filtering, map rendering, summary table, hotspot, scatter
│
├── scripts/
│   ├── setup_db.R       # Scans data/ hierarchy → writes data/injury_outcomes.sqlite
│   ├── fetch_cdc.R      # Downloads injury data from data.cdc.gov Socrata API → data/{level}/...
│   ├── fetch_env.R      # Downloads NOAA nClimDiv climate data → env_by_state/county in SQLite
│   └── run_app.R        # shiny::runApp(".", port = 3838, launch.browser = FALSE)
│
├── run.sh               # Load R module + run app
├── fetch_cdc.sh         # Load R module + run scripts/fetch_cdc.R (pass flags through)
├── fetch_env.sh         # Load R module + run scripts/fetch_env.R
├── install_packages.sh  # Installs missing R packages on Great Lakes
├── deploy.sh            # Deploys to shinyapps.io
│
├── docs/
│   └── DATA_FETCH.md    # CDC Socrata + NOAA nClimDiv source reference; CDC WONDER dead-end notes
│
├── data/
│   ├── injury_outcomes.sqlite   # Generated by setup_db.R; also holds env_by_state/county
│   ├── usa_states_s.rds         # Pre-simplified TIGER state shapefile
│   ├── usa_counties_s.rds       # Pre-simplified TIGER county shapefile
│   │
│   ├── state/                   # Raw injury .txt files (written by fetch_cdc.sh)
│   │   └── {injury_type}/
│   │       └── {period}/
│   │           └── {demographic}/
│   │               └── state_*.txt
│   │
│   └── county/                  # Raw injury .txt files (written by fetch_cdc.sh)
│       └── {injury_type}/
│           └── {period}/
│               └── {demographic}/
│                   └── county_*.txt
│
├── www/                 # Static assets served by Shiny (images, etc.)
├── rsconnect/           # shinyapps.io deployment metadata
└── CLAUDE.md            # Notes for Claude Code when working in this repo
```

---

## 7. Architecture (for Developers)

The app loads everything once at startup in `global.R`, then `server.R` filters the long-format tables per dropdown selection and joins to the shapefile on render. There is no per-request DB query.

**Boot sequence (`global.R`)**

1. Connect to local SQLite at `data/injury_outcomes.sqlite`.
2. Read **long-format** tables `overdose_by_state` and `overdose_by_county`. One row per `(injury_type, period, demographic, GEOID)` combination. Columns are uppercased to `INJURY_TYPE`, `PERIOD`, `DEMOGRAPHIC`, `GEOID`, `STATE`, `COUNTY_NAME` (county only), `DEATHS`, `POPULATION`, `CRUDE_RATE`.
3. Read `env_by_state` and `env_by_county` (written by `fetch_env.R`) if present. Columns: `GEOID`, `YEAR`, `MEAN_TEMP` (°F), `PRECIP` (inches). If absent the scatter panel shows a prompt to run `./fetch_env.sh`.
4. Read the two `.rds` shapefiles (`usa_states_s.rds`, `usa_counties_s.rds`).
5. Build `available_options` — a named list with `injury_types`, `periods`, `demographics` as named character vectors (`c("Display Label" = "internal_key", ...)`) for the UI `choices`.

**Server reactives (`server.R`)**

- `filter_overdose(tbl)` filters a long-format table by `input$var` / `input$selected_period` / `input$demographics`.
- `states_sf()` / `counties_sf_all()` call it, then left-join the result onto the appropriate shapefile by `GEOID`.
- `counties_sf_filtered()` further filters counties to `input$selected_state_on_county_level`.
- `compute_hotspot()` runs `spdep::localG` over a k = 5 nearest-neighbor graph of centroids and bins z-scores at ±1.96.
- Click-to-select stores selections in `selected_state_geoid` / `selected_county_geoid` reactiveVals; defaults are Michigan (`"26"`) / Washtenaw County (`"26161"`).
- If a selected combination has zero rows, both reactives `validate()` out and the user sees an inline "No data" message.

**`setup_db.R` discovery logic**

For each level (`state`, `county`) the script lists `.txt` files under `data/{level}/` recursively, expects them at exactly four sub-levels deep (`injury_type/period/demographic/file.txt`), parses those four tokens straight off the path, reads the tab-separated export, and writes a row per record with those tokens as metadata columns. Files at the wrong depth are skipped with a warning.

**`fetch_cdc.R` / `fetch_env.R` data pipeline**

`fetch_cdc.R` queries the CDC's public Socrata API (`data.cdc.gov`) for the *Mapping Injury, Overdose, and Violence* dataset (state ID `fpsi-y8tj`, county ID `psx4-wq38`). For each `(injury, year)` combination it writes a tab-separated `.txt` file into `data/{level}/{injury_type}/{period}/all_demographics/`, which `setup_db.R` picks up on the next app start.

`fetch_env.R` downloads two fixed-width files from the NOAA NCEI nClimDiv FTP (`climdiv-tmpccy`, `climdiv-pcpncy`), parses monthly county values, computes annual mean temperature and total precipitation, and writes `env_by_state` / `env_by_county` directly into the existing SQLite DB. No DB rebuild is needed after running it.

---

## 8. Deploying to shinyapps.io

### First-time setup

Register your shinyapps.io credentials once (token + secret from https://www.shinyapps.io/admin/#/tokens):

```bash
bash register_account.sh
```

### Deploy

```bash
bash deploy.sh
```

This bundles all project files — **including the generated `data/injury_outcomes.sqlite`** and the contents of `data/state/` and `data/county/` — and pushes to the `ipcapp` account on shinyapps.io. Make sure you have rebuilt the database (`Rscript setup_db.R`) before deploying so that the bundled SQLite reflects whatever's currently in the file hierarchy.

---

## 9. Dependencies

| Package | Purpose |
|---------|---------|
| `shiny`, `shinyjs`, `shinyBS` | App framework and UI components |
| `tmap`, `leaflet` | Interactive map rendering |
| `sf`, `spdep` | Spatial data handling and Gi\* hotspot analysis |
| `DBI`, `RSQLite` | Local SQLite database connection |
| `dplyr`, `ggplot2` | Data manipulation and scatter plot |
| `rsconnect` | Deployment to shinyapps.io |

On Great Lakes these are provided by the `Rgeospatial/4.5.1-2025-10-07` module; `install_packages.sh` adds the remaining ones (`tmap`, `tmaptools`, `XML`, `rsconnect`, `leaflet.extras`) into the user's personal R library.

---

## 10. Troubleshooting

**The new dropdown option doesn't appear after I added a file.**
Restart the app with `./run.sh`. On startup it auto-rebuilds the SQLite DB from the files in `data/`, and the dropdowns are sourced from that DB. If you already restarted and the option is still missing, look at the boot output: a line like `overdose_by_state: ... rows written from N file(s)` confirms the file was picked up. If the line says fewer files than you expect, the path probably doesn't match `data/{level}/{injury_type}/{period}/{demographic}/<file>.txt` — see Section 3.1.

If you suspect the DB is stale (for example, you previously deleted a file and the dashboard still shows it), force a clean rebuild with `FORCE_DB_REBUILD=1 ./run.sh`.

**The map is blank and says "No data for the selected Injury Type / Period / Demographic combination."**
You selected a combination that has no `.txt` file behind it. Pick a different combination, or add the missing file under `data/{level}/{injury_type}/{period}/{demographic}/`.

**`setup_db.R` warns "Ignoring files not at the expected depth".**
A `.txt` file under `data/state/` or `data/county/` is not at exactly four levels deep. The path must be `data/{level}/{injury_type}/{period}/{demographic}/<file>.txt`. Check for missing folders.

**`./run.sh: Permission denied`**
Make the script executable: `chmod +x run.sh deploy.sh install_packages.sh register_account.sh`.

**`module: command not found`**
You are in a shell where Lmod isn't initialized. On Great Lakes, log out and back in, or `source /usr/share/lmod/lmod/init/bash`.

**Port 3838 is already in use.**
Another R session is still running. Find it with `ps -ef | grep run_app` and kill it (`kill <PID>`), or pick a different port by editing `scripts/run_app.R`.
