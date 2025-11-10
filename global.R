# global.R

# ---- 1. Load required packages ----
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(sf)
  library(tmap)
  library(leaflet)
  library(ggplot2)
  library(shinyjs)
})

# Read DB config from environment (set locally in .Renviron; set in host after deploy)
pg_cfg <- list(
  dbname   = Sys.getenv("PG_DBNAME", ""),
  host     = Sys.getenv("PG_HOST",   ""),
  port     = as.integer(Sys.getenv("PG_PORT", "5432")),
  user     = Sys.getenv("PG_USER",   ""),
  password = Sys.getenv("PG_PASSWORD", ""),
  sslmode  = Sys.getenv("PG_SSLMODE", "require")  # Supabase typically requires SSL
)

# ---- 2. Read environment variables ----
connect_db <- function(cfg = pg_cfg) {
  tryCatch({
    message("📡 Connecting to Supabase PostgreSQL...")
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname   = cfg$dbname,
      host     = cfg$host,
      port     = cfg$port,
      user     = cfg$user,
      password = cfg$password,
      sslmode  = cfg$sslmode
    )
    message("✅ Database connection successful.")
    return(con)
  }, error = function(e) {
    message("❌ Database connection failed: ", e$message)
    return(NULL)
  })
}

# ---- 4. Establish a global connection ----
pg_con <- connect_db()
if (is.null(pg_con)) {
  warning("Could not connect to Supabase. Check credentials or network.")
}

# ---- 5. Load data from Supabase tables ----
if (!is.null(pg_con)) {
  tryCatch({
    message("Loading overdose data from Supabase...")
    
    overdose_state <- dbReadTable(pg_con, "overdose_by_state") %>%
      rename(
        GEOID       = geoid,
        STATE       = state_name,
        STATE_CODE  = state_code,
        DEATHS      = deaths,
        POPULATION  = population,
        CRUDE_RATE  = crude_rate,
        NOTES       = notes
      ) %>% select(-c(STATE_CODE, NOTES))
    
    overdose_county <- dbReadTable(pg_con, "overdose_by_county") %>%
      rename(
        GEOID       = geoid,
        STATE       = state_name,
        STATE_CODE  = state_code,
        COUNTY_NAME = county_name,
        COUNTY_CODE = county_code,
        DEATHS      = deaths,
        POPULATION  = population,
        CRUDE_RATE  = crude_rate,
        NOTES       = notes
      ) %>% select(-c(COUNTY_CODE, NOTES))
    
    message("✅ Supabase tables loaded successfully.")
  }, error = function(e) {
    warning("Could not fetch tables from Supabase: ", e$message)
    overdose_state <- overdose_county <- NULL
  })
}

# ---- 6. Load shapefiles (.rds) ----
tryCatch({
  message("🗺️ Loading local shapefiles...")
  usa_states   <- readRDS("data/usa_states_s.rds")
  usa_counties <- readRDS("data/usa_counties_s.rds")
  message("✅ Shapefiles loaded successfully.")
}, error = function(e) {
  warning("⚠️ Could not read shapefiles: ", e$message)
  usa_states <- usa_counties <- NULL
})

# ---- 7. Join Supabase data with shapefiles ----
if (!is.null(usa_states) && !is.null(overdose_state)) {
  merged_state_data <- usa_states %>%
    left_join(overdose_state, by = c("GEOID" = "GEOID"))
}

if (!is.null(usa_counties) && !is.null(overdose_county)) {
  merged_county_data <- usa_counties %>%
    left_join(overdose_county, by = c("GEOID" = "GEOID"))
}

# ---- 8. Define any helper utilities for server use ----
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
  if (!is.null(pg_con)) {
    message("🔌 Closing database connection...")
    DBI::dbDisconnect(pg_con)
  }
})

# --- quick health checks ---
# app_health <- list(
#   has_con      = !is.null(pg_con),
#   tables       = tryCatch(DBI::dbListTables(pg_con), error = function(e) character()),
#   files_exist  = c(
#     states  = file.exists("data/usa_states_s.rds"),
#     counties= file.exists("data/usa_counties_s.rds")
#   ),
#   nrows = c(
#     od_state  = if (exists("overdose_state"))  nrow(overdose_state)  else NA_integer_,
#     od_county = if (exists("overdose_county")) nrow(overdose_county) else NA_integer_
#   )
# )
# message(str(app_health))


# ---- 10. Global status message ----
message("🌐 Global setup complete. App ready to launch.")