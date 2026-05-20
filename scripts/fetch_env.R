# fetch_env.R
# Downloads NOAA nClimDiv county-level mean temperature and precipitation,
# computes annual summaries, and writes env_by_county and env_by_state into
# data/injury_outcomes.sqlite (same DB the app already uses).
#
# Data source: NOAA National Centers for Environmental Information (NCEI)
#   nClimDiv - https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
#   No API key required. Files updated monthly.
#   Variables used:
#     tmpccy = county mean temperature (°F, monthly)
#     pcpncy = county total precipitation (inches, monthly)
#
# File format (fixed-width, one row = one county × one year):
#   chars 1-5  : 5-digit county FIPS  (first 2 = state FIPS)
#   chars 6-7  : element code         (02 = temp, 01 = precip)
#   chars 8-11 : year
#   chars 12+  : 12 monthly values, 7 chars each; -9999 = missing
#
# Usage: Rscript fetch_env.R

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

BASE_URL    <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv"
DB_PATH     <- "data/injury_outcomes.sqlite"
VALID_YEARS <- 2019:2024
MISSING_FLAG <- -9990  # values <= this are the -9999 sentinel

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Scrape the NCEI directory listing to find the current filename for a given
# variable code (e.g. "tmpccy", "pcpncy"). Returns just the filename.
find_latest_filename <- function(var_code) {
  listing <- tryCatch(
    readLines(BASE_URL, warn = FALSE),
    error = function(e) stop("Cannot reach NCEI directory: ", conditionMessage(e))
  )
  pat     <- paste0("climdiv-", var_code, "-v1\\.0\\.0-[0-9]{8}")
  matches <- unique(regmatches(listing, regexpr(pat, listing)))
  if (length(matches) == 0)
    stop("No file found for variable code '", var_code, "' at ", BASE_URL)
  tail(sort(matches), 1)   # most recent date suffix
}

# Download a file to a temp directory and return its local path.
download_ncei <- function(filename) {
  url  <- paste0(BASE_URL, "/", filename)
  dest <- file.path(tempdir(), filename)
  message("  GET ", url)
  download.file(url, destfile = dest, quiet = TRUE, method = "libcurl")
  dest
}

# Parse a nClimDiv county fixed-width file.
# Returns a list: $geoid (chr), $year (int), $monthly (numeric matrix 12 cols).
parse_county_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[grepl("^[0-9]{5}", lines)]   # drop any header/blank lines

  n       <- length(lines)
  geoid   <- substr(lines, 1, 5)
  year    <- as.integer(substr(lines, 8, 11))
  monthly <- matrix(NA_real_, nrow = n, ncol = 12)

  for (m in seq_len(12)) {
    start <- 12L + (m - 1L) * 7L
    vals  <- suppressWarnings(as.numeric(trimws(substr(lines, start, start + 6L))))
    vals[!is.na(vals) & vals <= MISSING_FLAG] <- NA
    monthly[, m] <- vals
  }

  list(geoid = geoid, year = year, monthly = monthly)
}

# ---------------------------------------------------------------------------
# Download and parse
# ---------------------------------------------------------------------------
message("=== fetch_env.R ===\n")

message("[Mean Temperature — tmpccy]")
temp_raw  <- parse_county_file(download_ncei(find_latest_filename("tmpccy")))

message("\n[Precipitation — pcpncy]")
pcpn_raw  <- parse_county_file(download_ncei(find_latest_filename("pcpncy")))

# ---------------------------------------------------------------------------
# Compute annual summaries
# ---------------------------------------------------------------------------

# Annual mean temperature (°F): average of up to 12 monthly means
temp_df <- data.frame(
  geoid     = temp_raw$geoid,
  year      = temp_raw$year,
  mean_temp = rowMeans(temp_raw$monthly, na.rm = TRUE),
  stringsAsFactors = FALSE
)

# Annual total precipitation (inches): sum of 12 monthly totals
pcpn_df <- data.frame(
  geoid  = pcpn_raw$geoid,
  year   = pcpn_raw$year,
  precip = rowSums(pcpn_raw$monthly, na.rm = TRUE),
  stringsAsFactors = FALSE
)

# Drop rows where all months were missing (rowSums on all-NA gives 0)
pcpn_df$precip[rowSums(!is.na(pcpn_raw$monthly)) == 0] <- NA

# Join temperature + precipitation by county × year, keep dashboard years only
env_county <- merge(temp_df, pcpn_df, by = c("geoid", "year"))
env_county <- env_county[env_county$year %in% VALID_YEARS, ]
env_county$state_geoid <- substr(env_county$geoid, 1, 2)

message("\nCounty rows after year filter (", paste(range(VALID_YEARS), collapse = "-"), "): ",
        nrow(env_county))

# ---------------------------------------------------------------------------
# State-level aggregation: mean of county values within each state × year.
# Mean temperature: straight mean of county annual means.
# Precipitation: mean of county annual totals (comparable across states).
# ---------------------------------------------------------------------------
env_state <- aggregate(
  cbind(mean_temp, precip) ~ state_geoid + year,
  data  = env_county,
  FUN   = function(x) mean(x, na.rm = TRUE)
)
names(env_state)[names(env_state) == "state_geoid"] <- "geoid"

message("State rows: ", nrow(env_state))

# ---------------------------------------------------------------------------
# Write to SQLite
# ---------------------------------------------------------------------------
if (!file.exists(DB_PATH))
  stop("SQLite DB not found at ", DB_PATH, " — run setup_db.R or ./run.sh first.")

con <- dbConnect(RSQLite::SQLite(), DB_PATH)

dbWriteTable(con, "env_by_county",
             env_county[, c("geoid", "state_geoid", "year", "mean_temp", "precip")],
             overwrite = TRUE)

dbWriteTable(con, "env_by_state",
             env_state[, c("geoid", "year", "mean_temp", "precip")],
             overwrite = TRUE)

dbDisconnect(con)

message("\nDone.")
message("  env_by_county: ", nrow(env_county), " rows")
message("  env_by_state:  ", nrow(env_state),  " rows")
message("  Written to: ", DB_PATH)
message("\nRestart the app (./run.sh) to load the new environmental data.")
