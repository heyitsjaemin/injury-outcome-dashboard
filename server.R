# server.R
library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(leaflet)
library(spdep)
library(ggplot2)

server <- function(input, output, session) {
  
  # --- quick sanity log once ---
  observe({
    cat("\n=== BOOT: GLOBALS SUMMARY ===\n")
    cat("[overdose_state]  rows:", if (exists("overdose_state"))  nrow(overdose_state)  else NA, "\n")
    cat("[overdose_county] rows:", if (exists("overdose_county")) nrow(overdose_county) else NA, "\n")
    cat("[usa_states]      rows:", if (exists("usa_states"))      nrow(usa_states)      else NA, "\n")
    cat("[usa_counties]    rows:", if (exists("usa_counties"))    nrow(usa_counties)    else NA, "\n\n")
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
  # Filter the long-format overdose tables by the dropdown selections,
  # then left-join onto the shapefile for the chosen level.
  filter_overdose <- function(tbl) {
    req(input$var, input$selected_period, input$demographics)
    tbl %>%
      filter(
        INJURY_TYPE == input$var,
        PERIOD      == input$selected_period,
        DEMOGRAPHIC == input$demographics
      ) %>%
      mutate(
        DEATHS     = suppressWarnings(as.numeric(DEATHS)),
        POPULATION = suppressWarnings(as.numeric(POPULATION)),
        CRUDE_RATE = suppressWarnings(as.numeric(CRUDE_RATE)),
        GEOID      = as.character(GEOID)
      )
  }

  states_sf <- reactive({
    validate(
      need(exists("overdose_state") && !is.null(overdose_state),
           "overdose_state is missing (check global.R / setup_db.R)"),
      need(exists("usa_states") && !is.null(usa_states), "usa_states shapefile missing")
    )
    slice <- filter_overdose(overdose_state)
    validate(need(nrow(slice) > 0,
                  "No data for the selected Injury Type / Period / Demographic combination."))
    usa_states %>%
      mutate(GEOID = as.character(GEOID)) %>%
      left_join(slice, by = "GEOID")
  })

  # Populate county-state dropdown once we have county data
  observe({
    validate(need(exists("overdose_county") && !is.null(overdose_county), FALSE))
    if ("STATE" %in% names(overdose_county)) {
      ch <- sort(unique(na.omit(overdose_county$STATE)))
      updateSelectInput(session, "selected_state_on_county_level",
                        choices = ch,
                        selected = if ("Michigan" %in% ch) "Michigan" else head(ch, 1))
    }
  })

  counties_sf_all <- reactive({
    validate(
      need(exists("overdose_county") && !is.null(overdose_county),
           "overdose_county is missing (check global.R / setup_db.R)"),
      need(exists("usa_counties") && !is.null(usa_counties), "usa_counties shapefile missing")
    )
    slice <- filter_overdose(overdose_county)
    validate(need(nrow(slice) > 0,
                  "No data for the selected Injury Type / Period / Demographic combination."))
    d <- usa_counties %>%
      mutate(GEOID = as.character(GEOID)) %>%
      left_join(slice, by = "GEOID")
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
  selected_county_geoid  <- reactiveVal("26161")
  
  observeEvent(input$usa_map_shape_click, {
    id <- input$usa_map_shape_click$id
    # tmap v4 replaces spaces with underscores in leaflet layerIds
    id_name <- gsub("_", " ", id)

    if (input$level == "state") {
      d  <- states_sf()
      d$GEOID <- as.character(d$GEOID)
      if ("NAME" %in% names(d)) d$NAME <- as.character(d$NAME)

      if (!is.null(id)) {
        if (id %in% d$GEOID) {
          selected_state_geoid(id)
        } else if ("NAME" %in% names(d) && id_name %in% d$NAME) {
          selected_state_geoid(d$GEOID[match(id_name, d$NAME)])
        }
      }
    } else if (input$level == "county") {
      d <- counties_sf_filtered()
      d$GEOID <- as.character(d$GEOID)
      d$NAME  <- as.character(d$NAME)
      if (!is.null(id)) {
        if (id %in% d$GEOID) {
          selected_county_geoid(id)
        } else if (id_name %in% d$NAME) {
          selected_county_geoid(d$GEOID[match(id_name, d$NAME)])
        }
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
    gid <- selected_county_geoid()
    
    # choose a default if nothing clicked yet
    if (is.null(gid) || !gid %in% as.character(d$GEOID)) {
      if (nrow(d) == 0) return(data.frame(Field = "No County Available", Value = ""))
      gid <- as.character(d$GEOID[1])
    }
    
    row <- d %>% dplyr::filter(as.character(GEOID) == gid)
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
  output$usa_map <- renderLeaflet({

    if (input$level == "state") {
      d <- states_sf()

      map_obj <- if (identical(input$map_type, "Hotspot Analysis")) {
        d2 <- compute_hotspot(d)
        tm_shape(d2) +
          tm_polygons(
            fill = "hotspot_category",
            fill.scale = tm_scale_categorical(
              values = c("Cold Spot" = "blue", "Neutral" = "white", "Hot Spot" = "red")
            ),
            fill.legend = tm_legend(title = "Hotspot Analysis"),
            id = "NAME",
            popup.vars = c("State" = "NAME", "Hotspot" = "hotspot_category")
          )
      } else {
        tm_shape(d) +
          tm_polygons(
            fill = "CRUDE_RATE",
            fill.scale = tm_scale_intervals(style = "quantile", values = "Blues"),
            fill.legend = tm_legend(title = "Crude Rate"),
            id = "NAME",
            popup.vars = c("State" = "NAME", "Rate" = "CRUDE_RATE", "Deaths" = "DEATHS")
          )
      }

    } else {
      d <- counties_sf_filtered()

      map_obj <- if (identical(input$map_type, "Hotspot Analysis")) {
        d2 <- compute_hotspot(d)
        tm_shape(d2) +
          tm_polygons(
            fill = "hotspot_category",
            fill.scale = tm_scale_categorical(
              values = c("Cold Spot" = "blue", "Neutral" = "white", "Hot Spot" = "red")
            ),
            fill.legend = tm_legend(title = "Hotspot Analysis"),
            id = "NAME",
            popup.vars = c("County" = "NAME", "Hotspot" = "hotspot_category")
          )
      } else {
        tm_shape(d) +
          tm_polygons(
            fill = "CRUDE_RATE",
            fill.scale = tm_scale_intervals(style = "quantile", values = "Blues"),
            fill.legend = tm_legend(title = "Crude Rate"),
            id = "NAME",
            popup.vars = c("County" = "NAME", "Rate" = "CRUDE_RATE", "Deaths" = "DEATHS")
          )
      }
    }

    tmap_leaflet(map_obj)
  })
  
  # ------------------------------
  # Scatter (state-level)
  # ------------------------------

  # Join injury data for current filter to env data for the same year.
  scatter_data <- reactive({
    req(!is.null(input$scatter_var))

    if (input$level == "state") {
      validate(need(!is.null(env_state),
                    "Climate data not loaded. Run: ./fetch_env.sh"))

      injury <- sf::st_drop_geometry(states_sf()) %>%
        select(GEOID, label = STATE, CRUDE_RATE) %>%
        mutate(GEOID      = as.character(GEOID),
               CRUDE_RATE = ifelse(CRUDE_RATE == -1.0, NA, CRUDE_RATE))

      env <- env_state %>%
        filter(YEAR == as.integer(input$selected_period)) %>%
        mutate(GEOID = as.character(GEOID))

      merged <- left_join(injury, env, by = "GEOID")
      validate(need(sum(!is.na(merged$MEAN_TEMP)) > 5,
                    "Not enough climate data for the selected period."))
      merged

    } else {
      validate(need(!is.null(env_county),
                    "Climate data not loaded. Run: ./fetch_env.sh"))

      injury <- sf::st_drop_geometry(counties_sf_filtered()) %>%
        select(GEOID, label = NAME, CRUDE_RATE) %>%
        mutate(GEOID      = as.character(GEOID),
               CRUDE_RATE = ifelse(CRUDE_RATE == -1.0, NA, CRUDE_RATE))

      env <- env_county %>%
        filter(YEAR == as.integer(input$selected_period)) %>%
        mutate(GEOID = as.character(GEOID))

      merged <- left_join(injury, env, by = "GEOID")
      validate(need(sum(!is.na(merged$MEAN_TEMP)) > 5,
                    "Not enough climate data for the selected period."))
      merged
    }
  })

  output$scatter_stats <- renderUI({
    d <- scatter_data()

    xvec <- switch(input$scatter_var,
                   "Mean Temperature" = d$MEAN_TEMP,
                   "Precipitation"    = d$PRECIP)
    yvec <- d$CRUDE_RATE

    complete <- complete.cases(xvec, yvec)
    validate(need(sum(complete) > 3, "Not enough complete observations."))

    fit <- lm(yvec[complete] ~ xvec[complete])
    r2  <- summary(fit)$r.squared
    r   <- cor(xvec[complete], yvec[complete])
    p   <- summary(fit)$coefficients[2, 4]

    x_label <- switch(input$scatter_var,
                      "Mean Temperature" = "Mean Temp (°F)",
                      "Precipitation"    = "Annual Precip (in)")

    sig_label <- if (p < 0.001) "p < 0.001" else paste0("p = ", round(p, 3))
    strength  <- if (abs(r) > 0.7) "Strong correlation."
                 else if (abs(r) > 0.3) "Moderate correlation."
                 else "Weak or no correlation."

    tagList(
      tags$p(HTML(paste0("<b>X axis:</b> ", x_label))),
      tags$p(HTML(paste0("<b>Y axis:</b> Crude Death Rate (per 100k)"))),
      tags$hr(),
      tags$p(HTML(paste0("<b>Slope:</b> ",      round(coef(fit)[2], 4)))),
      tags$p(HTML(paste0("<b>R²:</b> ",    round(r2, 4)))),
      tags$p(HTML(paste0("<b>Pearson r:</b> ",  round(r,  4)))),
      tags$p(HTML(paste0("<b>", sig_label, "</b>"))),
      tags$hr(),
      tags$p(strength)
    )
  })

  output$scatter_plot <- renderPlot({
    d <- scatter_data()

    xvec <- switch(input$scatter_var,
                   "Mean Temperature" = d$MEAN_TEMP,
                   "Precipitation"    = d$PRECIP,
                   NULL)
    validate(need(!is.null(xvec), "Invalid scatter variable."))

    x_label <- switch(input$scatter_var,
                      "Mean Temperature" = "Annual Mean Temperature (°F)",
                      "Precipitation"    = "Annual Total Precipitation (inches)")

    injury_label <- prettify(input$var)
    period_label <- input$selected_period

    point_subtitle <- if (input$level == "state")
      "Each point = one state.  Line = OLS regression."
    else
      paste0("Each point = one county (", input$selected_state_on_county_level,
             ").  Line = OLS regression.")

    plot_df <- data.frame(x = xvec, y = d$CRUDE_RATE,
                          label = d$label, stringsAsFactors = FALSE)

    ggplot(plot_df, aes(x = x, y = y)) +
      geom_point(size = 3, alpha = 0.75, color = "#00274c") +
      geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
                  color = "#ffcb05", fill = "#ffcb05", alpha = 0.2) +
      geom_text(aes(label = label), size = 2.5, vjust = -0.7,
                color = "#444444", check_overlap = TRUE) +
      labs(
        title    = paste0(injury_label, " vs. ", input$scatter_var, " (", period_label, ")"),
        subtitle = point_subtitle,
        x        = x_label,
        y        = "Crude Death Rate (per 100,000)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold", color = "#00274c"),
            plot.subtitle = element_text(color = "#666666", size = 10))
  })
}
