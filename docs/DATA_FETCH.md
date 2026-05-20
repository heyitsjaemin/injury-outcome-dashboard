# Data Fetch Reference

This document covers the two external data sources powering the dashboard and how
to refresh them. It also records why the originally-investigated CDC WONDER API was
ruled out, so that path is not re-tried.

---

## 1  Injury / overdose data — `scripts/fetch_cdc.R`

### Source

**CDC Mapping Injury, Overdose, and Violence** (NCIPC / NCHS NVSS)

| Level  | Socrata endpoint                                   |
|--------|----------------------------------------------------|
| State  | `https://data.cdc.gov/resource/fpsi-y8tj.json`     |
| County | `https://data.cdc.gov/resource/psx4-wq38.json`     |

No API key required. Data are updated weekly by CDC. Coverage: 2019–present.

### Injury type mapping

| API `intent` value | Folder / internal key | Description (CDC definition) |
|--------------------|----------------------|------------------------------|
| `Drug_OD`          | `drug_overdose`      | Unintentional or undetermined intent drug overdose deaths |
| `All_Suicide`      | `all_suicide`        | All-mechanism suicide deaths |
| `All_Homicide`     | `all_homicide`       | All-mechanism homicide deaths |
| `FA_Deaths`        | `firearm_deaths`     | All firearm injury deaths (any intent) |
| `FA_Homicide`      | `firearm_homicide`   | Firearm homicide deaths (subset of All Homicide + Firearm Deaths) |
| `FA_Suicide`       | `firearm_suicide`    | Firearm suicide deaths (subset of All Suicide + Firearm Deaths) |

### Suppression policy

County rows where 1–9 deaths occurred have `count_sup` set to a range string
(`"1-9"` or `"10-50"`). These are written as blank `Deaths` / `"Unreliable"` crude
rate in the output file, matching the sentinel value `CRUDE_RATE == -1.0` used
throughout the app. True zeros (`count_sup == "0"`) are kept as-is.

### Usage

```bash
./fetch_cdc.sh                                    # all types, all years (2019–2024), state + county
./fetch_cdc.sh --level state                      # state only
./fetch_cdc.sh --injury Drug_OD --period 2023     # one type / one year
./fetch_cdc.sh --level county --injury FA_Deaths  # county, one type, all years
```

Flag reference:

| Flag       | Values                                                    | Default |
|------------|-----------------------------------------------------------|---------|
| `--level`  | `state`, `county`, `both`                                 | `both`  |
| `--injury` | `all`, `Drug_OD`, `All_Suicide`, `All_Homicide`, `FA_Deaths`, `FA_Homicide`, `FA_Suicide` | `all` |
| `--period` | `all`, `2019` – `2024`                                    | `all`   |

Output is written to `data/{level}/{injury_type}/{period}/all_demographics/`.  
Run `./run.sh` afterward to rebuild the SQLite DB and restart the app.

---

## 2  Environmental data — `scripts/fetch_env.R`

### Source

**NOAA National Centers for Environmental Information (NCEI) — nClimDiv**

Base URL: `https://www.ncei.noaa.gov/pub/data/cirs/climdiv/`

No API key required. Files are updated monthly. The script auto-discovers the
latest filename by scraping the NCEI directory listing.

| Variable code | Description                  | Unit        |
|---------------|------------------------------|-------------|
| `tmpccy`      | County mean temperature      | °F, monthly |
| `pcpncy`      | County total precipitation   | inches, monthly |

### File format

Fixed-width, one row = one county × one year:

```
chars 1–5  : 5-digit county FIPS (first 2 = state FIPS)
chars 6–7  : element code (02 = temp, 01 = precip)
chars 8–11 : year
chars 12+  : 12 monthly values, 7 chars each; -9999 = missing
```

### Annual aggregation

- **Mean temperature**: `rowMeans` of up to 12 monthly values (°F).
- **Precipitation**: `rowSums` of 12 monthly values (total annual inches).
  Rows where all 12 months are missing are set to `NA`.
- **State-level**: mean of county annual values within each state × year.

### Output

Two tables are written to `data/injury_outcomes.sqlite`:

| Table          | Rows (approx) | Columns                                     |
|----------------|---------------|---------------------------------------------|
| `env_by_county`| ~18,852       | `geoid`, `state_geoid`, `year`, `mean_temp`, `precip` |
| `env_by_state` | ~300          | `geoid`, `year`, `mean_temp`, `precip`      |

Coverage matches the dashboard years (2019–2024).

### Usage

```bash
./fetch_env.sh
```

Run `./run.sh` afterward to reload the app (the DB already has the new tables;
restarting the app re-reads them through `global.R` section 5).

---

## 3  Why CDC WONDER (D158) was ruled out

During initial development the CDC WONDER web API (`D158` — Underlying Cause of
Death, 2018–2024, Single Race) was investigated as an alternative source.

**Finding**: the API explicitly blocks all location group-by and filters for this
dataset. The server returns:

> *"Only national data are available for this dataset when using the WONDER web
> service. Please check that your query does not group results by region, division,
> state, county or urbanization, (B_1 through B_5), nor limit these location
> variables to any specific values."*

Since state- and county-level breakdowns are the entire purpose of the dashboard,
this API cannot be used for that purpose. The Socrata endpoint at
`data.cdc.gov` (section 1 above) provides the same underlying NVSS data with full
geographic granularity and no such restriction.

The Socrata endpoint at `data.cdc.gov` (section 1 above) is the correct replacement.
