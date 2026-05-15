# setup_db.R
# Run once (or whenever source data files change) to build data/injury_outcomes.sqlite.
# Usage: Rscript setup_db.R
# Dependencies: DBI, RSQLite (base R only — no dplyr needed)

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

db_path <- file.path("data", "injury_outcomes.sqlite")

if (file.exists(db_path)) {
  file.remove(db_path)
  message("Removed existing SQLite DB: ", db_path)
}

con <- dbConnect(RSQLite::SQLite(), db_path)

read_wonder <- function(path, col_classes) {
  raw <- read.delim(path, stringsAsFactors = FALSE, colClasses = "character", fill = TRUE)
  # drop CDC WONDER footer rows (blank State column)
  raw[!is.na(raw$State) & nzchar(trimws(raw$State)) & trimws(raw$State) != "Total", ]
}

parse_rate <- function(x) {
  ifelse(trimws(x) == "Unreliable", -1.0, suppressWarnings(as.numeric(x)))
}

# ---- State data ----
state_raw <- read_wonder("ipcapp_030_overdose_by_state_2018_2023.txt")

overdose_by_state <- data.frame(
  geoid      = state_raw$State.Code,
  state_name = state_raw$State,
  deaths     = suppressWarnings(as.integer(state_raw$Deaths)),
  population = suppressWarnings(as.integer(state_raw$Population)),
  crude_rate = parse_rate(state_raw$Crude.Rate),
  stringsAsFactors = FALSE
)

dbWriteTable(con, "overdose_by_state", overdose_by_state, overwrite = TRUE)
message("overdose_by_state: ", nrow(overdose_by_state), " rows written")

# ---- County data ----
county_raw <- read_wonder("ipcapp_031_overdose_by_county_2018_2023.txt")
county_raw <- county_raw[!is.na(county_raw$County) & nzchar(trimws(county_raw$County)), ]

overdose_by_county <- data.frame(
  geoid       = county_raw$County.Code,
  state_name  = county_raw$State,
  county_name = county_raw$County,
  deaths      = suppressWarnings(as.integer(county_raw$Deaths)),
  population  = suppressWarnings(as.integer(county_raw$Population)),
  crude_rate  = parse_rate(county_raw$Crude.Rate),
  stringsAsFactors = FALSE
)

dbWriteTable(con, "overdose_by_county", overdose_by_county, overwrite = TRUE)
message("overdose_by_county: ", nrow(overdose_by_county), " rows written")

dbDisconnect(con)
message("Done. SQLite DB written to: ", db_path)
