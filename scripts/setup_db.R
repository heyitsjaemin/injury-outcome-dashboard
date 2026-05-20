# setup_db.R
# Run once (or whenever source data files change) to build data/injury_outcomes.sqlite.
# Usage: Rscript setup_db.R
# Dependencies: DBI, RSQLite (base R only — no dplyr needed)
#
# Layout expected under data/:
#   data/{level}/{injury_type}/{period}/{demographic}/<file>.txt
# where {level} is "state" or "county". Drop new CDC WONDER exports anywhere
# under that hierarchy and re-run this script — the metadata columns
# (injury_type, period, demographic) are read straight from the path.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

data_dir <- "data"
db_path  <- file.path(data_dir, "injury_outcomes.sqlite")

if (file.exists(db_path)) {
  file.remove(db_path)
  message("Removed existing SQLite DB: ", db_path)
}

con <- dbConnect(RSQLite::SQLite(), db_path)

# ---- helpers ----

read_wonder <- function(path) {
  raw <- read.delim(path, stringsAsFactors = FALSE, colClasses = "character", fill = TRUE)
  # Drop CDC WONDER footer rows (blank/Total State column)
  raw[!is.na(raw$State) & nzchar(trimws(raw$State)) & trimws(raw$State) != "Total", ]
}

parse_rate <- function(x) {
  ifelse(trimws(x) == "Unreliable", -1.0, suppressWarnings(as.numeric(x)))
}

# Returns a data.frame with columns: level, injury_type, period, demographic, path.
# Files must sit at data/<level>/<injury_type>/<period>/<demographic>/<file>.txt
discover_files <- function(level) {
  root <- file.path(data_dir, level)
  if (!dir.exists(root)) return(NULL)

  paths <- list.files(root, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
  if (length(paths) == 0) return(NULL)

  rel   <- substring(paths, nchar(root) + 2)         # drop "data/<level>/"
  parts <- strsplit(rel, .Platform$file.sep, fixed = TRUE)

  ok <- vapply(parts, length, integer(1)) == 4       # injury_type / period / demographic / file
  if (any(!ok)) {
    warning("Ignoring files not at the expected depth under data/", level, "/:\n  ",
            paste(paths[!ok], collapse = "\n  "))
  }
  parts <- parts[ok]; paths <- paths[ok]
  if (length(paths) == 0) return(NULL)

  data.frame(
    level       = level,
    injury_type = vapply(parts, `[`, character(1), 1),
    period      = vapply(parts, `[`, character(1), 2),
    demographic = vapply(parts, `[`, character(1), 3),
    path        = paths,
    stringsAsFactors = FALSE
  )
}

# ---- state ----
state_manifest <- discover_files("state")
if (is.null(state_manifest) || nrow(state_manifest) == 0) {
  warning("No state files found under data/state/ — injury_by_state will be empty.")
  injury_by_state <- data.frame(
    injury_type = character(0), period = character(0), demographic = character(0),
    geoid = character(0), state_name = character(0),
    deaths = integer(0), population = integer(0), crude_rate = numeric(0),
    stringsAsFactors = FALSE
  )
} else {
  state_frames <- lapply(seq_len(nrow(state_manifest)), function(i) {
    m   <- state_manifest[i, ]
    raw <- read_wonder(m$path)
    if (nrow(raw) == 0) return(NULL)
    data.frame(
      injury_type = m$injury_type,
      period      = m$period,
      demographic = m$demographic,
      geoid       = raw$State.Code,
      state_name  = raw$State,
      deaths      = suppressWarnings(as.integer(raw$Deaths)),
      population  = suppressWarnings(as.integer(raw$Population)),
      crude_rate  = parse_rate(raw$Crude.Rate),
      stringsAsFactors = FALSE
    )
  })
  injury_by_state <- do.call(rbind, state_frames)
}

dbWriteTable(con, "injury_by_state", injury_by_state, overwrite = TRUE)
message("injury_by_state: ", nrow(injury_by_state), " rows written from ",
        if (is.null(state_manifest)) 0 else nrow(state_manifest), " file(s)")

# ---- county ----
county_manifest <- discover_files("county")
if (is.null(county_manifest) || nrow(county_manifest) == 0) {
  warning("No county files found under data/county/ — injury_by_county will be empty.")
  injury_by_county <- data.frame(
    injury_type = character(0), period = character(0), demographic = character(0),
    geoid = character(0), state_name = character(0), county_name = character(0),
    deaths = integer(0), population = integer(0), crude_rate = numeric(0),
    stringsAsFactors = FALSE
  )
} else {
  county_frames <- lapply(seq_len(nrow(county_manifest)), function(i) {
    m   <- county_manifest[i, ]
    raw <- read_wonder(m$path)
    raw <- raw[!is.na(raw$County) & nzchar(trimws(raw$County)), ]
    if (nrow(raw) == 0) return(NULL)
    data.frame(
      injury_type = m$injury_type,
      period      = m$period,
      demographic = m$demographic,
      geoid       = raw$County.Code,
      state_name  = raw$State,
      county_name = raw$County,
      deaths      = suppressWarnings(as.integer(raw$Deaths)),
      population  = suppressWarnings(as.integer(raw$Population)),
      crude_rate  = parse_rate(raw$Crude.Rate),
      stringsAsFactors = FALSE
    )
  })
  injury_by_county <- do.call(rbind, county_frames)
}

dbWriteTable(con, "injury_by_county", injury_by_county, overwrite = TRUE)
message("injury_by_county: ", nrow(injury_by_county), " rows written from ",
        if (is.null(county_manifest)) 0 else nrow(county_manifest), " file(s)")

dbDisconnect(con)
message("Done. SQLite DB written to: ", db_path)
