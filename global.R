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

# ---- 2. Connect to local DuckDB ----
db_path <- "data/injury_outcomes.sqlite"

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

# ---- 3. Load overdose tables ----
if (!is.null(db_con)) {
  tryCatch({
    message("Loading overdose tables from DuckDB...")

    overdose_state <- dbReadTable(db_con, "overdose_by_state") %>%
      rename(
        GEOID      = geoid,
        STATE      = state_name,
        DEATHS     = deaths,
        POPULATION = population,
        CRUDE_RATE = crude_rate
      )

    overdose_county <- dbReadTable(db_con, "overdose_by_county") %>%
      rename(
        GEOID       = geoid,
        STATE       = state_name,
        COUNTY_NAME = county_name,
        DEATHS      = deaths,
        POPULATION  = population,
        CRUDE_RATE  = crude_rate
      )

    message("Tables loaded — state rows: ", nrow(overdose_state),
            ", county rows: ", nrow(overdose_county))
  }, error = function(e) {
    warning("Could not read tables from DuckDB: ", e$message)
    overdose_state <- overdose_county <- NULL
  })
}

# ---- 4. Load shapefiles ----
tryCatch({
  message("Loading local shapefiles...")
  usa_states   <- readRDS("data/usa_states_s.rds")
  usa_counties <- readRDS("data/usa_counties_s.rds")
  message("Shapefiles loaded.")
}, error = function(e) {
  warning("Could not read shapefiles: ", e$message)
  usa_states <- usa_counties <- NULL
})

# ---- 5. Join overdose data onto shapefiles ----
if (!is.null(usa_states) && exists("overdose_state") && !is.null(overdose_state)) {
  merged_state_data <- usa_states %>%
    left_join(overdose_state, by = "GEOID")
}

if (!is.null(usa_counties) && exists("overdose_county") && !is.null(overdose_county)) {
  merged_county_data <- usa_counties %>%
    left_join(overdose_county, by = "GEOID")
}

# ---- 6. Helper used by server ----
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

# ---- 7. Clean up connection on app stop ----
onStop(function() {
  if (!is.null(db_con)) {
    message("Closing SQLite connection...")
    dbDisconnect(db_con)
  }
})

message("Global setup complete. App ready to launch.")
