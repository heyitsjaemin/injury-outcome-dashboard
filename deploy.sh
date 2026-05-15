#!/bin/bash
module load Rgeospatial/4.5.1-2025-10-07
Rscript -e 'rsconnect::deployApp(".")'
