# Injury Outcome Dashboard

An interactive R Shiny dashboard that visualizes U.S. injury-outcome data at the state and county level. Currently displays CDC unintentional drug overdose death data (2018–2023) with standard choropleth and Gi* hotspot analysis views.

Deployed at: https://ipcapp.shinyapps.io/injury-outcome-dashboard/

---

## Features

- **State and county level maps** — toggle between levels with a radio button
- **Choropleth map** — visualizes crude death rate by geography
- **Hotspot analysis** — Gi* spatial autocorrelation (cold/neutral/hot spots) using k=5 nearest neighbors
- **Summary statistics table** — click any state or county on the map to see its stats
- **Scatter plot** — regression analysis comparing crude rate against other variables

---

## Project Structure

```
injury_outcome_dashboard/
├── app.R                  # Entry point — sources global/ui/server
├── global.R               # DB connection, data loading, shapefile joins
├── ui.R                   # Layout and controls
├── server.R               # Reactives, map rendering, summary table
├── data/
│   ├── injury_outcomes.sqlite     # Local SQLite database (built by setup_db.R)
│   ├── usa_states_s.rds           # Pre-simplified TIGER state shapefiles
│   └── usa_counties_s.rds         # Pre-simplified TIGER county shapefiles
├── ipcapp_030_overdose_by_state_2018_2023.txt   # Source data (CDC WONDER export)
├── ipcapp_031_overdose_by_county_2018_2023.txt  # Source data (CDC WONDER export)
├── setup_db.R             # Builds injury_outcomes.sqlite from source .txt files
├── run_app.R              # Starts the app on port 3838
├── run.sh                 # Wrapper script to run app on Great Lakes
├── deploy.sh              # Wrapper script to deploy to shinyapps.io
└── install_packages.sh    # Installs missing R packages on Great Lakes
```

---

## Data

Source data is exported from [CDC WONDER](https://wonder.cdc.gov/) (Multiple Cause of Death, expanded). The `.txt` files are tab-separated with a CDC WONDER footer that is automatically stripped on import.

- `CRUDE_RATE == -1.0` is the sentinel value for CDC-suppressed "Unreliable" rates (counties with small counts)
- Suppressed death counts are hidden in the export and will appear as `NA`

---

## Running Locally (Great Lakes / Turbo)

### First-time setup

**1. Install missing R packages** (only needed once):

```bash
bash install_packages.sh
```

**2. Build the SQLite database** (only needed once, or after adding new source data):

```bash
bash -l -c "module load R && cd ~/injury_outcome_dashboard && Rscript setup_db.R"
```

### Start the app

```bash
bash run.sh
```

The app starts on port 3838. VS Code (Remote SSH) will show a pop-up — click **Open in Browser**. If not, go to the **Ports** tab and open port 3838 manually.

---

## Deploying to shinyapps.io

### First-time setup

Register your shinyapps.io credentials once (token and secret from https://www.shinyapps.io/admin/#/tokens):

```bash
bash register_account.sh
```

### Deploy

```bash
bash deploy.sh
```

This bundles all project files — including `data/injury_outcomes.sqlite` — and pushes to the `ipcapp` account on shinyapps.io.

---

## Adding New Injury Datasets

1. Place the new CDC WONDER `.txt` export in the project root
2. Add a parsing block to `setup_db.R` following the existing pattern:

```r
new_raw <- read.delim("your_new_file.txt", stringsAsFactors = FALSE, colClasses = "character", fill = TRUE)
new_raw <- new_raw[!is.na(new_raw$State) & nzchar(trimws(new_raw$State)), ]

new_table <- data.frame(
  geoid      = new_raw$State.Code,   # or County.Code for county-level
  state_name = new_raw$State,
  deaths     = suppressWarnings(as.integer(new_raw$Deaths)),
  population = suppressWarnings(as.integer(new_raw$Population)),
  crude_rate = ifelse(trimws(new_raw$Crude.Rate) == "Unreliable", -1.0,
                      suppressWarnings(as.numeric(new_raw$Crude.Rate))),
  stringsAsFactors = FALSE
)

dbWriteTable(con, "your_table_name", new_table, overwrite = TRUE)
```

3. Rebuild the database:

```bash
bash -l -c "module load R && cd ~/injury_outcome_dashboard && Rscript setup_db.R"
```

4. Redeploy:

```bash
bash deploy.sh
```

---

## Dependencies

| Package | Purpose |
|---------|---------|
| shiny, shinyjs, shinyBS | App framework and UI components |
| tmap, leaflet | Interactive map rendering |
| sf, spdep | Spatial data handling and hotspot analysis |
| DBI, RSQLite | Local SQLite database connection |
| dplyr, ggplot2 | Data manipulation and scatter plot |
| rsconnect | Deployment to shinyapps.io |
