#!/bin/bash
# Wrapper for fetch_cdc.R — loads the R module then runs the script.
# Usage: ./fetch_cdc.sh [--level state|county|both] [--injury all|Drug_OD|...] [--period all|2019|...]
module load Rgeospatial/4.5.1-2025-10-07
Rscript ~/injury_outcome_dashboard/scripts/fetch_cdc.R "$@"
