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

# ---- 2. Connect to local SQLite ----
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

# ---- 3. Load overdose tables (long format with metadata columns) ----
overdose_state  <- NULL
overdose_county <- NULL

if (!is.null(db_con)) {
  tryCatch({
    message("Loading overdose tables from SQLite...")

    overdose_state <- dbReadTable(db_con, "overdose_by_state") %>%
      rename(
        INJURY_TYPE = injury_type,
        PERIOD      = period,
        DEMOGRAPHIC = demographic,
        GEOID       = geoid,
        STATE       = state_name,
        DEATHS      = deaths,
        POPULATION  = population,
        CRUDE_RATE  = crude_rate
      )

    overdose_county <- dbReadTable(db_con, "overdose_by_county") %>%
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
      )

    message("Tables loaded — state rows: ", nrow(overdose_state),
            ", county rows: ", nrow(overdose_county))
  }, error = function(e) {
    warning("Could not read tables from SQLite: ", e$message)
  })
}

# ---- 4. Load shapefiles ----
usa_states   <- NULL
usa_counties <- NULL
tryCatch({
  message("Loading local shapefiles...")
  usa_states   <- readRDS("data/usa_states_s.rds")
  usa_counties <- readRDS("data/usa_counties_s.rds")
  message("Shapefiles loaded.")
}, error = function(e) {
  warning("Could not read shapefiles: ", e$message)
})

# ---- 5. Build filter-option lists for the UI ----
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
