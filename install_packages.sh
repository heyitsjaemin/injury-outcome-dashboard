#!/bin/bash
module load Rgeospatial/4.5.1-2025-10-07
XML_CONFIG=/usr/bin/xml2-config \
  Rscript -e 'install.packages(c("XML","tmaptools","tmap","rsconnect","leaflet.extras"), repos="https://cloud.r-project.org")'
