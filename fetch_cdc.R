# fetch_cdc.R
# Downloads injury/overdose data from the CDC's data.cdc.gov Socrata API and
# writes properly-formatted tab-separated files into the data/ hierarchy so
# that ./run.sh picks them up automatically on next start.
#
# Data sources (updated weekly by CDC):
#   State:  https://data.cdc.gov/resource/fpsi-y8tj  (Mapping Injury, Overdose, and Violence - State)
#   County: https://data.cdc.gov/resource/psx4-wq38  (Mapping Injury, Overdose, and Violence - County)
#
# Usage:
#   Rscript fetch_cdc.R                         # fetch everything
#   Rscript fetch_cdc.R --level state           # state only
#   Rscript fetch_cdc.R --injury Drug_OD        # one injury type
#   Rscript fetch_cdc.R --period 2023           # one year
#   Rscript fetch_cdc.R --injury Drug_OD --period 2023 --level both
#
# Injury type options (--injury):
#   all (default), Drug_OD, All_Suicide, All_Homicide, FA_Deaths, FA_Homicide, FA_Suicide
#
# Period options (--period):
#   all (default), 2019, 2020, 2021, 2022, 2023, 2024
#   Note: "TTM" (trailing twelve months) is excluded by default.
#
# Output path convention:
#   data/{level}/{injury_type}/{period}/all_demographics/{level}_{injury_type}_{period}_all_demographics.txt
#
# Dependencies: jsonlite (bundled with Rgeospatial module)

suppressPackageStartupMessages(library(jsonlite))

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

STATE_API  <- "https://data.cdc.gov/resource/fpsi-y8tj.json"
COUNTY_API <- "https://data.cdc.gov/resource/psx4-wq38.json"

# API intent value → folder/file name used in data/ hierarchy
INTENT_MAP <- c(
  Drug_OD      = "drug_overdose",
  All_Suicide  = "all_suicide",
  All_Homicide = "all_homicide",
  FA_Deaths    = "firearm_deaths",
  FA_Homicide  = "firearm_homicide",
  FA_Suicide   = "firearm_suicide"
)

VALID_PERIODS <- c("2019", "2020", "2021", "2022", "2023", "2024")

STATE_ABBREVS <- c(
  Alabama = "AL", Alaska = "AK", Arizona = "AZ", Arkansas = "AR",
  California = "CA", Colorado = "CO", Connecticut = "CT", Delaware = "DE",
  "District of Columbia" = "DC", Florida = "FL", Georgia = "GA",
  Hawaii = "HI", Idaho = "ID", Illinois = "IL", Indiana = "IN",
  Iowa = "IA", Kansas = "KS", Kentucky = "KY", Louisiana = "LA",
  Maine = "ME", Maryland = "MD", Massachusetts = "MA", Michigan = "MI",
  Minnesota = "MN", Mississippi = "MS", Missouri = "MO", Montana = "MT",
  Nebraska = "NE", Nevada = "NV", "New Hampshire" = "NH",
  "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
  "North Carolina" = "NC", "North Dakota" = "ND", Ohio = "OH",
  Oklahoma = "OK", Oregon = "OR", Pennsylvania = "PA",
  "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD",
  Tennessee = "TN", Texas = "TX", Utah = "UT", Vermont = "VT",
  Virginia = "VA", Washington = "WA", "West Virginia" = "WV",
  Wisconsin = "WI", Wyoming = "WY"
)

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default) {
  i <- which(args == flag)
  if (length(i) == 0 || i == length(args)) return(default)
  args[i + 1]
}

arg_level  <- get_arg("--level",  "both")
arg_injury <- get_arg("--injury", "all")
arg_period <- get_arg("--period", "all")

if (!arg_level %in% c("state", "county", "both"))
  stop("--level must be state, county, or both")

target_intents <- if (arg_injury == "all") names(INTENT_MAP) else {
  if (!arg_injury %in% names(INTENT_MAP))
    stop("--injury must be one of: all, ", paste(names(INTENT_MAP), collapse = ", "))
  arg_injury
}

target_periods <- if (arg_period == "all") VALID_PERIODS else {
  if (!arg_period %in% VALID_PERIODS)
    stop("--period must be one of: all, ", paste(VALID_PERIODS, collapse = ", "))
  arg_period
}

do_state  <- arg_level %in% c("state",  "both")
do_county <- arg_level %in% c("county", "both")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Fetch all rows from a Socrata JSON endpoint with pagination.
fetch_all <- function(base_url, where_clause, limit = 5000) {
  url <- paste0(base_url, "?$limit=", limit, "&$where=", URLencode(where_clause, reserved = TRUE))
  message("  GET ", url)
  rows <- tryCatch(fromJSON(url, simplifyDataFrame = TRUE), error = function(e) {
    warning("API request failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  # Paginate if we hit the limit exactly
  if (nrow(rows) == limit) {
    offset <- limit
    repeat {
      url2 <- paste0(base_url, "?$limit=", limit, "&$offset=", offset,
                     "&$where=", URLencode(where_clause, reserved = TRUE))
      more <- tryCatch(fromJSON(url2, simplifyDataFrame = TRUE), error = function(e) NULL)
      if (is.null(more) || nrow(more) == 0) break
      rows <- rbind(rows, more)
      if (nrow(more) < limit) break
      offset <- offset + limit
    }
  }
  rows
}

# Parse count_sup: returns numeric count, NA for suppressed ranges ("1-9", "10-50"), 0 for "0"
parse_count <- function(x) {
  suppressWarnings(as.numeric(x))  # "1-9" and "10-50" become NA; "0", "346" etc. parse fine
}

# Returns TRUE for rows where count is a suppressed range (not a true zero or real count)
is_suppressed <- function(x) {
  is.na(suppressWarnings(as.numeric(x))) & nzchar(trimws(x))
}

pad_fips <- function(x, width) formatC(as.integer(x), width = width, flag = "0")

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# ---------------------------------------------------------------------------
# Write state .txt
# ---------------------------------------------------------------------------

write_state_file <- function(rows, injury_type, period) {
  out_dir  <- file.path("data", "state", injury_type, period, "all_demographics")
  out_file <- file.path(out_dir, paste0("state_", injury_type, "_", period, "_all_demographics.txt"))
  ensure_dir(out_dir)

  # Build output data frame in CDC WONDER column order
  notes      <- rep("", nrow(rows))
  state_name <- paste0('"', rows$name, '"')
  state_code <- paste0('"', pad_fips(rows$geoid, 2), '"')

  count   <- parse_count(rows$count_sup)
  rate    <- suppressWarnings(as.numeric(rows$rate))
  suppr   <- is_suppressed(rows$count_sup)

  deaths <- ifelse(suppr, "", as.character(as.integer(count)))
  pop    <- ifelse(
    suppr | is.na(count) | is.na(rate) | rate == 0,
    "",
    as.character(round(count * 1e5 / rate))
  )
  crude  <- ifelse(suppr, "Unreliable", ifelse(is.na(rate), "", as.character(round(rate, 1))))

  df <- data.frame(
    Notes       = notes,
    State       = state_name,
    State.Code  = state_code,
    Deaths      = deaths,
    Population  = pop,
    Crude.Rate  = crude,
    stringsAsFactors = FALSE
  )

  # Header row (mimic CDC WONDER quoting)
  header <- paste('"Notes"', '"State"', '"State Code"', "Deaths", "Population", "Crude Rate",
                  sep = "\t")
  data_lines <- apply(df, 1, paste, collapse = "\t")
  writeLines(c(header, data_lines), out_file)
  message("    wrote ", nrow(rows), " rows → ", out_file)
}

# ---------------------------------------------------------------------------
# Write county .txt
# ---------------------------------------------------------------------------

write_county_file <- function(rows, injury_type, period) {
  out_dir  <- file.path("data", "county", injury_type, period, "all_demographics")
  out_file <- file.path(out_dir, paste0("county_", injury_type, "_", period, "_all_demographics.txt"))
  ensure_dir(out_dir)

  notes       <- rep("", nrow(rows))
  state_name  <- paste0('"', rows$st_name, '"')
  state_code  <- paste0('"', pad_fips(rows$st_geoid, 2), '"')
  county_fips <- pad_fips(rows$geoid, 5)
  abbrev      <- STATE_ABBREVS[rows$st_name]
  abbrev[is.na(abbrev)] <- rows$st_name[is.na(abbrev)]  # fallback to full name
  county_name <- paste0('"', rows$name, ", ", abbrev, '"')
  county_code <- paste0('"', county_fips, '"')

  count  <- parse_count(rows$count_sup)
  rate   <- suppressWarnings(as.numeric(rows$rate))
  suppr  <- is_suppressed(rows$count_sup)

  deaths <- ifelse(suppr, "", as.character(as.integer(count)))
  pop    <- ifelse(
    suppr | is.na(count) | is.na(rate) | rate == 0,
    "",
    as.character(round(count * 1e5 / rate))
  )
  crude  <- ifelse(suppr, "Unreliable", ifelse(is.na(rate), "", as.character(round(rate, 1))))

  df <- data.frame(
    Notes       = notes,
    State       = state_name,
    State.Code  = state_code,
    County      = county_name,
    County.Code = county_code,
    Deaths      = deaths,
    Population  = pop,
    Crude.Rate  = crude,
    stringsAsFactors = FALSE
  )

  header <- paste('"Notes"', '"State"', '"State Code"', '"County"', '"County Code"',
                  "Deaths", "Population", "Crude Rate", sep = "\t")
  data_lines <- apply(df, 1, paste, collapse = "\t")
  writeLines(c(header, data_lines), out_file)
  message("    wrote ", nrow(rows), " rows → ", out_file)
}

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------

total_files <- 0

for (intent in target_intents) {
  injury_type <- INTENT_MAP[[intent]]

  for (period in target_periods) {
    message("\n[", intent, " / ", period, "]")

    where <- paste0("intent='", intent, "' AND period='", period, "'")

    if (do_state) {
      rows <- fetch_all(STATE_API, where)
      if (!is.null(rows) && nrow(rows) > 0) {
        write_state_file(rows, injury_type, period)
        total_files <- total_files + 1
      } else {
        message("    no state data returned")
      }
    }

    if (do_county) {
      rows <- fetch_all(COUNTY_API, where)
      if (!is.null(rows) && nrow(rows) > 0) {
        write_county_file(rows, injury_type, period)
        total_files <- total_files + 1
      } else {
        message("    no county data returned")
      }
    }
  }
}

message("\nDone. ", total_files, " file(s) written to data/.")
message("Run ./run.sh to rebuild the SQLite DB and start the app.")
