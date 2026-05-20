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

      merged     <- left_join(injury, env, by = "GEOID")
      xvar       <- switch(input$scatter_var, "Mean Temperature" = "MEAN_TEMP", "Precipitation" = "PRECIP")
      n_complete <- sum(complete.cases(merged[[xvar]], merged$CRUDE_RATE))
      validate(need(n_complete >= 5,
                    paste0("Not enough complete data for this selection (n = ", n_complete, ").")))
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

      merged     <- left_join(injury, env, by = "GEOID")
      xvar       <- switch(input$scatter_var, "Mean Temperature" = "MEAN_TEMP", "Precipitation" = "PRECIP")
      n_complete <- sum(complete.cases(merged[[xvar]], merged$CRUDE_RATE))
      validate(need(n_complete >= 5,
                    paste0("Not enough data for scatter plot for ",
                           input$selected_state_on_county_level,
                           " with the selected filters (n = ", n_complete, "). ",
                           "Try a different state, period, or demographic.")))
      merged
    }
  })

  output$scatter_stats <- renderUI({
    d <- scatter_data()

    xvec <- switch(input$scatter_var,
                   "Mean Temperature" = d$MEAN_TEMP,
                   "Precipitation"    = d$PRECIP)
    yvec <- d$CRUDE_RATE

    complete      <- complete.cases(xvec, yvec)
    n             <- sum(complete)
    injury_label  <- prettify(input$var)
    level_label   <- if (input$level == "state") "states" else
                       paste0("counties in ", input$selected_state_on_county_level)
    x_label       <- switch(input$scatter_var,
                             "Mean Temperature" = "annual mean temperature",
                             "Precipitation"    = "annual precipitation")

    # Hard stop: n < 10 — insufficient for any regression inference
    # Threshold per Cohen et al. (1988) and county-level epidemiology guidelines
    if (n < 10) {
      return(
        tags$div(
          style = "background:#f8d7da; border-left:4px solid #dc3545; padding:14px; border-radius:4px;",
          tags$strong(paste0("Not enough data for meaningful statistical analysis (n = ", n, ")")),
          tags$p(
            style = "font-size:13px; margin:8px 0 0 0; color:#58151c; line-height:1.5;",
            paste0(
              "Only ", n, " ", level_label, " have both injury and climate data for this selection. ",
              "A minimum of 10 observations is required to fit a regression line. ",
              "Green (1991) recommends N ≥ 50 + 8m (m = predictors); with 1 environmental predictor, ",
              "the recommended minimum is N ≥ 58. ",
              "Try a different state, period, or demographic filter."
            )
          )
        )
      )
    }

    fit   <- lm(yvec[complete] ~ xvec[complete])
    r2    <- summary(fit)$r.squared
    r     <- cor(xvec[complete], yvec[complete])
    p     <- summary(fit)$coefficients[2, 4]
    slope <- coef(fit)[2]

    # Data quality note based on n tier
    data_note <- if (n < 30) {
      tags$div(
        style = "background:#fff3cd; border-left:4px solid #ffc107; padding:10px; border-radius:4px; margin-bottom:12px;",
        tags$strong(paste0("Limited data (n = ", n, " ", level_label, ")")),
        tags$p(
          style = "font-size:12px; margin:4px 0 0 0; color:#664d03; line-height:1.4;",
          "Fewer than 30 complete observations — estimates may be unstable. Results are exploratory; interpret with caution."
        )
      )
    } else {
      tags$p(
        style = "font-size:12px; color:#666; margin-bottom:8px;",
        paste0("n = ", n, " ", level_label)
      )
    }

    slope_dir <- if (slope > 0) "increases" else "decreases"
    dir_label <- if (r > 0) "positive" else "negative"

    # Strength classification: Cohen (1988) benchmarks applied in ecological public health research
    abs_r  <- abs(r)
    strength_label <- if (abs_r >= 0.7) "very strong"
                      else if (abs_r >= 0.5) "strong"
                      else if (abs_r >= 0.3) "moderate"
                      else if (abs_r >= 0.1) "weak"
                      else "negligible"

    sig_label  <- if (p < 0.001) "p < 0.001"
                  else paste0("p = ", round(p, 3))
    sig_detail <- if (p < 0.001)
                    "Highly significant (≤0.1% probability this pattern is due to chance)."
                  else if (p < 0.05)
                    paste0("Statistically significant (p < 0.05) — less than 5% chance this is random variation.")
                  else
                    "Not statistically significant (p ≥ 0.05) — the relationship could plausibly be due to chance."

    level_cap <- paste0(toupper(substr(level_label, 1, 1)), substr(level_label, 2, nchar(level_label)))

    tagList(
      data_note,
      tags$hr(style = "margin:6px 0 12px 0;"),

      # Slope
      tags$div(style = "margin-bottom:14px;",
        tags$p(style = "font-weight:bold; font-size:14px; margin-bottom:3px;",
               HTML(paste0("Slope: ", round(slope, 4)))),
        tags$p(style = "font-size:12px; color:#555; line-height:1.5; margin:0;",
               paste0("For each 1-unit rise in ", x_label, ", the expected crude death rate for ",
                      injury_label, " ", slope_dir, " by ", round(abs(slope), 4),
                      " deaths per 100,000 people."))
      ),

      # R²
      tags$div(style = "margin-bottom:14px;",
        tags$p(style = "font-weight:bold; font-size:14px; margin-bottom:3px;",
               HTML(paste0("R² = ", round(r2, 4)))),
        tags$p(style = "font-size:12px; color:#555; line-height:1.5; margin:0;",
               paste0(round(r2 * 100, 1), "% of the geographic variation in ", injury_label,
                      " death rates across ", level_label, " is statistically accounted for by ",
                      x_label, ". The remaining ", round((1 - r2) * 100, 1),
                      "% is explained by other factors."))
      ),

      # Pearson r
      tags$div(style = "margin-bottom:14px;",
        tags$p(style = "font-weight:bold; font-size:14px; margin-bottom:3px;",
               HTML(paste0("Pearson r = ", round(r, 4)))),
        tags$p(style = "font-size:12px; color:#555; line-height:1.5; margin:0;",
               paste0("A ", strength_label, " ", dir_label, " linear correlation. ",
                      level_cap, " with higher ", x_label, " tend to have ",
                      if (dir_label == "positive") "higher" else "lower",
                      " ", injury_label, " crude death rates."))
      ),

      # p-value
      tags$div(style = "margin-bottom:14px;",
        tags$p(style = "font-weight:bold; font-size:14px; margin-bottom:3px;",
               HTML(sig_label)),
        tags$p(style = "font-size:12px; color:#555; line-height:1.5; margin:0;",
               sig_detail)
      ),

      tags$hr(style = "margin:10px 0 8px 0;"),
      tags$p(style = "font-size:11px; color:#888; font-style:italic; line-height:1.4;",
             paste0("Ecological association: these patterns reflect aggregate trends across ",
                    level_label, " and should not be interpreted as individual-level causal ",
                    "effects (ecological fallacy).")),
      tags$p(style = "font-size:11px; color:#aaa; line-height:1.4; margin-top:4px;",
             "Data adequacy thresholds follow Green (1991, ",
             tags$em("Multivariate Behavioral Research"),
             ", 26(3), 499–510), who recommends N ≥ 50 + 8m, where m is the number of predictors. ",
             "With m = 1 (one environmental variable), the recommended minimum is N ≥ 58. ",
             "The n ≥ 30 warning threshold used here is conservative relative to this formula.")
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

    plot_df    <- data.frame(x = xvec, y = d$CRUDE_RATE,
                              label = d$label, stringsAsFactors = FALSE)
    n_complete <- sum(complete.cases(plot_df$x, plot_df$y))

    point_subtitle <- if (input$level == "state")
      paste0("Each point = one state (n = ", n_complete, ").  Line = OLS regression.")
    else
      paste0("Each point = one county in ", input$selected_state_on_county_level,
             " (n = ", n_complete, ").",
             if (n_complete >= 10) "  Line = OLS regression." else "  Too few points for regression line.")

    p <- ggplot(plot_df, aes(x = x, y = y)) +
      geom_point(size = 3, alpha = 0.75, color = "#00274c") +
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

    if (n_complete >= 10) {
      p <- p + geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
                           color = "#ffcb05", fill = "#ffcb05", alpha = 0.2)
    }
    p
  })
}
