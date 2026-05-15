library(shiny)     # <- for fluidPage, tags, etc.
library(shinyjs)   # <- for useShinyjs()
library(tmap)      # <- for tmapOutput/renderTmap
library(shinyBS)   # <- for bsTooltip()

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)