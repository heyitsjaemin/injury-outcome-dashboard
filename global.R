# global.R

# ---- 1. Load required packages ----
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(sf)
  library(tmap)
  library(leaflet)
  library(ggplot2)
  library(shinyjs)
})

# ---- 2. Auto-rebuild SQLite if data/ has newer .txt files than the DB ----
# Lets contributors drop a new CDC WONDER export under data/{level}/{injury_type}/
# {period}/{demographic}/ and see it on the next ./run.sh, with no manual
# setup_db.R step. Set FORCE_DB_REBUILD=1 in the environment to override
# the mtime check and always rebuild (useful after a file deletion, which
# this check can't detect on its own).
db_path <- "data/injury_outcomes.sqlite"

data_changed_since_db <- function(db_path) {
  txt_files <- c(
    list.files("data/state",  pattern = "\\.txt$", recursive = TRUE, full.names = TRUE),
    list.files("data/county", pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
  )
  if (length(txt_files) == 0) return(FALSE)        # no source data; nothing to rebuild from
  if (!file.exists(db_path))  return(TRUE)         # DB missing but files exist
  max(file.info(txt_files)$mtime) > file.info(db_path)$mtime
}

force_rebuild <- toupper(Sys.getenv("FORCE_DB_REBUILD", "")) %in% c("1", "TRUE", "T", "YES")

if (force_rebuild || data_changed_since_db(db_path)) {
  message(if (force_rebuild) "FORCE_DB_REBUILD set — rebuilding SQLite..."
          else                 "Data changes detected (or DB missing) — rebuilding SQLite from data/ ...")
  tryCatch(
    source("scripts/setup_db.R", local = TRUE),
    error = function(e) {
      warning("Auto-rebuild via setup_db.R failed: ", e$message,
              "\nFalling back to existing DB (if any).")
    }
  )
} else {
  message("SQLite is up to date with data/ — skipping rebuild.")
}

# ---- 3. Connect to local SQLite ----
db_con <- tryCatch({
  message("Connecting to local SQLite: ", db_path)
  con <- dbConnect(RSQLite::SQLite(), db_path)
  message("SQLite connection successful.")
  con
}, error = function(e) {
  warning("Could not open SQLite (", db_path, "): ", e$message,
          "\nRun setup_db.R to build the database.")
  NULL
})

# ---- 4. Load overdose tables (long format with metadata columns) ----
overdose_state  <- NULL
overdose_county <- NULL

if (!is.null(db_con)) {
  tryCatch({
    message("Loading overdose tables from SQLite...")

    overdose_state <- dbReadTable(db_con, "injury_by_state") %>%
      rename(
        INJURY_TYPE = injury_type,
        PERIOD      = period,
        DEMOGRAPHIC = demographic,
        GEOID       = geoid,
        STATE       = state_name,
        DEATHS      = deaths,
        POPULATION  = population,
        CRUDE_RATE  = crude_rate
      ) %>%
      filter(STATE != "District of Columbia")

    overdose_county <- dbReadTable(db_con, "injury_by_county") %>%
      rename(
        INJURY_TYPE = injury_type,
        PERIOD      = period,
        DEMOGRAPHIC = demographic,
        GEOID       = geoid,
        STATE       = state_name,
        COUNTY_NAME = county_name,
        DEATHS      = deaths,
        POPULATION  = population,
        CRUDE_RATE  = crude_rate
      ) %>%
      filter(STATE != "District of Columbia")

    message("Tables loaded — state rows: ", nrow(overdose_state),
            ", county rows: ", nrow(overdose_county))
  }, error = function(e) {
    warning("Could not read tables from SQLite: ", e$message)
  })
}

# ---- 5. Load environmental data (written by fetch_env.R) ----
env_state  <- NULL
env_county <- NULL

if (!is.null(db_con)) {
  tryCatch({
    tables <- dbListTables(db_con)
    if ("env_by_state" %in% tables) {
      env_state <- dbReadTable(db_con, "env_by_state") %>%
        rename(GEOID = geoid, YEAR = year, MEAN_TEMP = mean_temp, PRECIP = precip)
      env_state$GEOID <- formatC(as.integer(env_state$GEOID), width = 2, flag = "0")
      message("env_by_state loaded: ", nrow(env_state), " rows")
    } else {
      message("env_by_state not found — run fetch_env.R to load climate data")
    }
    if ("env_by_county" %in% tables) {
      env_county <- dbReadTable(db_con, "env_by_county") %>%
        rename(GEOID = geoid, STATE_GEOID = state_geoid,
               YEAR = year, MEAN_TEMP = mean_temp, PRECIP = precip)
      message("env_by_county loaded: ", nrow(env_county), " rows")
    }
  }, error = function(e) {
    warning("Could not load environmental tables: ", e$message)
  })
}

# ---- 6. Load shapefiles ----
usa_states   <- NULL
usa_counties <- NULL
tryCatch({
  message("Loading local shapefiles...")
  usa_states   <- readRDS("data/usa_states_s.rds") |>
    dplyr::filter(as.character(GEOID) != "11")   # exclude DC — no injury data
  usa_counties <- readRDS("data/usa_counties_s.rds")
  message("Shapefiles loaded.")
}, error = function(e) {
  warning("Could not read shapefiles: ", e$message)
})

# ---- 7. Build filter-option lists for the UI ----
# prettify("unintentional_drug_overdose") -> "Unintentional Drug Overdose"
prettify <- function(s) {
  if (length(s) == 0) return(character(0))
  out <- gsub("_", " ", s, fixed = TRUE)
  out <- tools::toTitleCase(out)
  # toTitleCase keeps short stop-words ("all", "or") lowercase even at the start.
  paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
}

# Returns a named character vector: names = display label, values = internal key.
# Used directly as `choices` in selectInput().
option_choices <- function(values) {
  values <- sort(unique(values[!is.na(values) & nzchar(values)]))
  if (length(values) == 0) return(character(0))
  setNames(values, prettify(values))
}

available_options <- list(
  injury_types = option_choices(c(overdose_state$INJURY_TYPE, overdose_county$INJURY_TYPE)),
  periods      = option_choices(c(overdose_state$PERIOD,      overdose_county$PERIOD)),
  demographics = option_choices(c(overdose_state$DEMOGRAPHIC, overdose_county$DEMOGRAPHIC))
)

message("Available filters — injury_types: ", length(available_options$injury_types),
        ", periods: ", length(available_options$periods),
        ", demographics: ", length(available_options$demographics))

# ---- 8. Helper used by server ----
compute_hotspot <- function(spatial_data) {
  stopifnot("CRUDE_RATE" %in% names(spatial_data))
  coords <- sf::st_coordinates(sf::st_centroid(spatial_data))
  nb     <- spdep::knearneigh(coords, k = 5) |> spdep::knn2nb()
  listw  <- spdep::nb2listw(nb, style = "W")
  gi     <- spdep::localG(spatial_data$CRUDE_RATE, listw)
  spatial_data$hotspot_score    <- gi
  spatial_data$hotspot_category <- cut(
    gi, c(-Inf, -1.96, 1.96, Inf),
    labels = c("Cold Spot", "Neutral", "Hot Spot")
  )
  return(spatial_data)
}

# ---- 9. Clean up connection on app stop ----
onStop(function() {
  if (!is.null(db_con)) {
    message("Closing SQLite connection...")
    dbDisconnect(db_con)
  }
})

message("Global setup complete. App ready to launch.")
