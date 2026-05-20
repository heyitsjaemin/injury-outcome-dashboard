#!/bin/bash
# Wrapper for fetch_env.R — loads the R module then downloads NOAA climate data.
# Usage: ./fetch_env.sh
module load Rgeospatial/4.5.1-2025-10-07
Rscript ~/injury_outcome_dashboard/scripts/fetch_env.R
