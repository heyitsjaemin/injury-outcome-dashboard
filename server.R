# server.R
library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(leaflet)
library(spdep)
library(ggplot2)

tmap_mode("view")

server <- function(input, output, session) {
  
  # --- quick sanity log once ---
  observe({
    cat("\n=== BOOT: GLOBALS SUMMARY ===\n")
    cat("[merged_state_data] rows:", if (exists("merged_state_data")) nrow(merged_state_data) else NA,
        " sf:", if (exists("merged_state_data")) inherits(merged_state_data,"sf") else NA, "\n")
    cat("[merged_county_data] rows:", if (exists("merged_county_data")) nrow(merged_county_data) else NA,
        " sf:", if (exists("merged_county_data")) inherits(merged_county_data,"sf") else NA, "\n\n")
  })
  
  # ------------------------------
  # Inputs debug (helps a ton)
  # ------------------------------
  observeEvent(input$level,            { cat("[INPUT] level:",            input$level, "\n") })
  observeEvent(input$map_type,         { cat("[INPUT] map_type:",         input$map_type, "\n") })
  observeEvent(input$selected_period,  { cat("[INPUT] selected_period:",  input$selected_period, "\n") })
  observeEvent(input$var,              { cat("[INPUT] var:",              input$var, "\n") })
  observeEvent(input$selected_state_on_county_level, {
    cat("[INPUT] county-state filter:", input$selected_state_on_county_level, "\n")
  })
  
  # ------------------------------
  # Helpers
  # ------------------------------
  compute_hotspot <- function(spatial_data) {
    validate(need("CRUDE_RATE" %in% names(spatial_data), "CRUDE_RATE not present"))
    coords <- sf::st_coordinates(sf::st_centroid(spatial_data))
    nb     <- spdep::knearneigh(coords, k = 5) |> spdep::knn2nb()
    listw  <- spdep::nb2listw(nb, style = "W")
    gi     <- spdep::localG(spatial_data$CRUDE_RATE, listw)
    spatial_data$hotspot_score    <- gi
    spatial_data$hotspot_category <- cut(
      gi, c(-Inf, -1.96, 1.96, Inf),
      labels = c("Cold Spot", "Neutral", "Hot Spot")
    )
    spatial_data
  }
  
  # ------------------------------
  # Reactive data sources
  # ------------------------------
  states_sf <- reactive({
    validate(need(exists("merged_state_data"), "merged_state_data is missing (check global.R)"))
    d <- merged_state_data |>
      mutate(
        DEATHS     = suppressWarnings(as.numeric(DEATHS)),
        POPULATION = suppressWarnings(as.numeric(POPULATION)),
        CRUDE_RATE = suppressWarnings(as.numeric(CRUDE_RATE)),
        GEOID      = as.character(GEOID)
      )
    cat("[states_sf] rows:", nrow(d), " NA CRUDE_RATE:", sum(is.na(d$CRUDE_RATE)), "\n")
    d
  })
  
  # Populate county-state dropdown once we have county data
  observe({
    validate(need(exists("merged_county_data"), FALSE))
    d <- merged_county_data
    if ("STATE" %in% names(d)) {
      ch <- sort(unique(na.omit(d$STATE)))
      updateSelectInput(session, "selected_state_on_county_level",
                        choices = ch,
                        selected = if ("Michigan" %in% ch) "Michigan" else head(ch, 1))
    }
  })
  
  counties_sf_all <- reactive({
    validate(need(exists("merged_county_data"), "merged_county_data is missing (check global.R)"))
    d <- merged_county_data |>
      mutate(
        DEATHS     = suppressWarnings(as.numeric(DEATHS)),
        POPULATION = suppressWarnings(as.numeric(POPULATION)),
        CRUDE_RATE = suppressWarnings(as.numeric(CRUDE_RATE)),
        GEOID      = as.character(GEOID)
      )
    # stable row id for clicks/popups
    d$ROWNUM <- seq_len(nrow(d))
    cat("[counties_sf_all] rows:", nrow(d), " NA CRUDE_RATE:", sum(is.na(d$CRUDE_RATE)), "\n")
    d
  })
  
  counties_sf_filtered <- reactive({
    d <- counties_sf_all()
    if (!is.null(input$selected_state_on_county_level) &&
        nzchar(input$selected_state_on_county_level) &&
        "STATE" %in% names(d)) {
      d <- d |> filter(STATE == input$selected_state_on_county_level)
      cat("[counties_sf_filtered] after STATE filter:", input$selected_state_on_county_level,
          " -> rows:", nrow(d), "\n")
    }
    d
  })
  
  # ------------------------------
  # Selections (click interaction)
  # ------------------------------
  selected_state_geoid <- reactiveVal("26")    # default MI
  selected_county_row  <- reactiveVal(NA_integer_)
  
  observeEvent(input$usa_map_shape_click, {
    click_id <- input$usa_map_shape_click$id
    if (is.null(click_id)) return()
    
    # tmap sends "X1", "X2", ...; leaflet we set layerId=ROWNUM (integer)
    if (input$level == "state") {
      idx <- suppressWarnings(as.numeric(gsub("^X|_1$", "", click_id)))
      d <- states_sf()
      if (!is.na(idx) && idx >= 1 && idx <= nrow(d)) {
        selected_state_geoid(as.character(d$GEOID[idx]))
        cat("✅ State clicked. GEOID=", selected_state_geoid(), "\n")
      }
    } else if (input$level == "county") {
      ridx <- suppressWarnings(as.integer(click_id))
      if (!is.na(ridx)) {
        selected_county_row(ridx)
        cat("✅ County clicked. ROWNUM=", ridx, "\n")
      }
    }
  })
  
  # ------------------------------
  # Summary table (switch by level)
  # ------------------------------
  state_summary <- reactive({
    d <- states_sf()
    row <- d |> filter(GEOID == selected_state_geoid())
    if (nrow(row) == 0) return(data.frame(Field = "State not found", Value = ""))
    crude   <- suppressWarnings(as.numeric(row$CRUDE_RATE))
    deaths  <- suppressWarnings(as.numeric(row$DEATHS))
    nat_avg <- mean(suppressWarnings(as.numeric(d$CRUDE_RATE)), na.rm = TRUE)
    est_pop <- if (is.finite(crude) && crude > 0) round((deaths * 1e5) / crude) else NA
    
    data.frame(
      Field = c("National Average", "State", "Crude Death Rate", "Total Deaths", "Total Population"),
      Value = c(
        formatC(nat_avg, format = "f", digits = 2),
        row$NAME,
        formatC(crude,  format = "f", digits = 2),
        formatC(deaths, format = "f", big.mark = ",", digits = 0),
        ifelse(is.na(est_pop), "Not Available",
               formatC(est_pop, format = "f", big.mark = ",", digits = 0))
      ),
      stringsAsFactors = FALSE
    )
  })
  
  county_summary <- reactive({
    d <- counties_sf_filtered()
    ridx <- selected_county_row()
    if (is.na(ridx) || ridx < 1 || ridx > nrow(d)) {
      return(data.frame(Field = "No County Selected", Value = ""))
    }
    row <- d[ridx, , drop = FALSE]
    pop_val <- ifelse(is.na(row$POPULATION) | row$POPULATION == 0,
                      "Not Available",
                      formatC(row$POPULATION, format = "f", big.mark = ",", digits = 0))
    crude_val <- if (is.na(row$POPULATION) | row$POPULATION == 0) {
      "Not Available"
    } else if (isTRUE(row$CRUDE_RATE == -1.0)) {
      "Unreliable"
    } else {
      formatC(row$CRUDE_RATE, format = "f", digits = 2)
    }
    data.frame(
      Field = c("County", "Crude Death Rate", "Total Deaths", "Total Population"),
      Value = c(row$NAME, crude_val,
                formatC(row$DEATHS, format = "f", big.mark = ",", digits = 0),
                pop_val),
      stringsAsFactors = FALSE
    )
  })
  
  output$my_table <- renderTable({
    if (input$level == "state") state_summary() else county_summary()
  })
  
  # ------------------------------
  # Map output (switch by level)
  # ------------------------------
  output$usa_map <- tmap::renderTmap({
    req(input$level == "state")
    d <- states_sf()
    cat("🗺️ Rendering STATE map. n=", nrow(d), " mode=", input$map_type, "\n")
    if (identical(input$map_type, "Hotspot Analysis")) {
      d2 <- compute_hotspot(d)
      tm_shape(d2) +
        tm_borders() +
        tm_fill("hotspot_category",
                palette = c("blue", "white", "red"),
                title = "Hotspot Analysis",
                popup.vars = c("State" = "NAME", "Hotspot" = "hotspot_category")) +
        tm_layout(scale = 1.5)
    } else {
      tm_shape(d) +
        tm_borders() +
        tm_fill("CRUDE_RATE",
                palette = "Blues",
                style   = "quantile",
                title   = "Crude Rate",
                popup.vars = c("State"="NAME","Rate"="CRUDE_RATE","Deaths"="DEATHS")) +
        tm_layout(scale = 1.5)
    }
  })
  
  observe({
    req(input$level == "county")
    d <- counties_sf_filtered()
    cat("🗺️ Rendering COUNTY map. n=", nrow(d), "\n")
    
    # dynamic bins
    qs  <- unique(stats::quantile(d$CRUDE_RATE, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE))
    pal <- colorBin("Blues", domain = d$CRUDE_RATE, bins = qs, na.color = "gray")
    bb  <- try(sf::st_bbox(d), silent = TRUE)
    
    output$usa_map <- leaflet::renderLeaflet({
      m <- leaflet(d) |>
        addTiles() |>
        addPolygons(
          fillColor = ~pal(CRUDE_RATE),
          weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
          highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          layerId = ~ROWNUM,
          popup = ~paste0(
            "<b>County:</b> ", NAME, "<br>",
            "<b>Crude Rate:</b> ",
            ifelse(is.na(CRUDE_RATE), "Not Available",
                   ifelse(CRUDE_RATE == -1.0, "Unreliable", formatC(CRUDE_RATE, format = "f", digits = 2))),
            "<br><b>Deaths:</b> ", formatC(DEATHS, format = "f", big.mark = ",", digits = 0)
          )
        )
      if (!inherits(bb, "try-error")) {
        m <- fitBounds(m, bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
      m
    })
  })
  
  # ------------------------------
  # Scatter (state-level)
  # ------------------------------
  output$scatter_stats <- renderUI({
    req(input$level == "state", !is.null(input$scatter_var))
    s <- sf::st_drop_geometry(states_sf())
    xvec <- switch(input$scatter_var,
                   "Mean Temperature" = s$POPULATION,   # placeholder
                   "Precipitation"    = s$DEATHS)       # placeholder
    fit <- lm(CRUDE_RATE ~ xvec, data = s)
    r2  <- summary(fit)$r.squared
    r   <- suppressWarnings(cor(xvec, s$CRUDE_RATE, use = "complete.obs"))
    tagList(
      tags$p(HTML(paste0("<b>📈 Slope:</b> ", round(coef(fit)[2], 4)))),
      tags$p(HTML(paste0("<b>📉 R-squared (R²):</b> ", round(r2, 4)))),
      tags$p(HTML(paste0("<b>🔗 Pearson’s r:</b> ", round(r, 4)))),
      tags$p(HTML(ifelse(abs(r) > .7, "Strong correlation.",
                         ifelse(abs(r) > .3, "Moderate correlation.", "Weak or no correlation."))))
    )
  })
  
  output$scatter_plot <- renderPlot({
    req(input$level == "state", !is.null(input$scatter_var))
    s <- sf::st_drop_geometry(states_sf())
    xvec <- switch(input$scatter_var,
                   "Mean Temperature" = s$POPULATION,   # placeholder
                   "Precipitation"    = s$DEATHS,
                   NULL)
    validate(need(!is.null(xvec), "Invalid scatter variable"))
    ggplot(s, aes(x = xvec, y = CRUDE_RATE)) +
      geom_point(size = 3, alpha = .8) +
      geom_smooth(method = "lm", linetype = "dashed") +
      scale_x_log10() +
      labs(title = paste("Crude Rate vs.", input$scatter_var, "(State Level)"),
           x = paste(input$scatter_var, "(Log Scale)"), y = "Crude Rate") +
      theme_minimal()
  })
}
