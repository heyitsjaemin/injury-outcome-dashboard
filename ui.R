# ui.R
library(shiny)
library(shinyjs)
library(leaflet)

ui <- fluidPage(
  useShinyjs(),
  title = "Injury Outcome Dashboard", 
  
  # ---- HEAD (fonts, icons, styles) ----
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap",
      rel  = "stylesheet"
    ),
    # Font Awesome for social icons
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    ),
    # Custom CSS
    tags$style(HTML("
      body { background-color:#FFFFFF; color:#000000; font-family:'Roboto', Arial, sans-serif; overflow-x:hidden; }
      .container-fluid { padding:0; }
      .header-container { width:100%; background:#ffffff; border-bottom:2px solid #00796b; padding:10px 0; }
      .top-bar { display:flex; justify-content:flex-end; font-size:14px; padding:5px 20px; background:#f5f5f5; }
      .top-bar a { color:#003366; font-weight:bold; margin-left:15px; text-decoration:underline; }
      .top-bar a:hover { color:#00579c; }
      .header { display:flex; align-items:center; justify-content:space-between; padding:30px 20px; }
      .logo { height:30px; }
      .nav-links { display:flex; gap:15px; }
      .nav-links a { text-decoration:none; color:#000000; font-size:16px; }
      .title-panel { text-align:center; font-size:30px; font-weight:bold; color:#ffffff; background:#00274c; padding:30px; }
      .update-date { text-align:right; padding:20px; }

      .radio-toolbar { display:flex; justify-content:center; gap:15px; background:#f8fbff; padding:12px; }
      .shiny-input-radiogroup { display:flex; gap:15px; }
      .shiny-input-radiogroup label {
        padding:10px 20px; font-size:16px; font-weight:bold; border-radius:5px; cursor:pointer;
        background:#e0e0e0; border:2px solid #ccc; transition:all .3s ease-in-out;
      }
      .shiny-input-radiogroup input[type='radio'] { display:none; }
      .shiny-input-radiogroup .active { background:#007BFF; color:#fff; border-color:#0056b3; text-decoration:underline; }

      .dropdown-container {
        display:flex; justify-content:center; align-items:center; gap:20px; padding:15px;
        background:#f8fbff; border-radius:5px; width:100%;
      }
      .selectize-control { width:100%; }
      .shiny-input-container { font-family:'Roboto', Arial, sans-serif; font-size:16px; font-weight:500; }
      .selectize-dropdown, .selectize-input {
        border:2px solid #ccc; border-radius:5px; padding:10px; background:#fff; transition:all .3s ease-in-out;
      }
      .selectize-input:focus { border-color:#007BFF; box-shadow:0 0 5px rgba(0,123,255,.5); }
      .selectize-dropdown:hover { background:#eef6ff; }
      .selectize-input, .selectize-dropdown-content { color:#333; }

      .sidebar-layout { padding:20px; }
      .sidebar { padding:15px; background:#f9f9f9; border-radius:5px; }
      .main-panel { padding-left:20px; }
      .main-content { margin-bottom:50px; }

      .scatter-layout { display:flex; flex-direction:row; width:100%; }
      .scatter-sidebar {
        width:20%; padding:15px; background:#f8f9fa; border-right:2px solid #ddd; text-align:left;
      }
      .scatter-content { width:80%; padding:15px; text-align:center; }
      .scatter-title { font-size:20px; font-weight:bold; text-align:center; margin-bottom:15px; }
      .scatter-dropdown { margin-bottom:15px; }
      .explanation-box {
        padding:15px; background:#ffffff; border-left:5px solid #0073e6; border-radius:8px;
        box-shadow:0 4px 8px rgba(0,0,0,.1);
      }
      .explanation-box h5 { font-size:18px; font-weight:bold; color:#0073e6; margin-bottom:10px; }
      .explanation-box p { font-size:14px; line-height:1.6; color:#333; }

      .site-footer { background:#00274c; color:#fff; font-family:'Roboto', Arial, sans-serif; padding:40px 0; text-align:left; }
      .footer-container { display:flex; justify-content:space-between; max-width:1200px; margin:auto; padding:0 40px; }
      .footer-column { flex:1; padding:10px; }
      .footer-column h3 { font-size:18px; font-weight:bold; }
      .footer-column p { font-size:14px; color:#ccc; }
      .contact-btn, .donate-btn, .membership-btn {
        background:#ffcb05; color:#00274c; font-weight:bold; padding:10px 20px; border:none; border-radius:5px; cursor:pointer; margin-top:10px; font-size:14px;
      }
      .contact-btn:hover, .donate-btn:hover, .membership-btn:hover { background:#e0b804; }
      .social-icons { margin-top:10px; }
      .social-icons a { color:white; font-size:20px; margin-right:10px; text-decoration:none; }
      .footer-line { border:0; border-top:1px solid #ccc; margin:30px auto; width:90%; }
      .footer-bottom { text-align:center; font-size:12px; color:#ccc; }
      .footer-bottom a { color:#ffcb05; text-decoration:none; }
      .footer-bottom a:hover { text-decoration:underline; }

      .data-sources-section {
        background:#f5f7fa; border-top:3px solid #00274c; padding:30px 40px; margin-top:30px;
      }
      .data-sources-section h4 {
        color:#00274c; font-size:18px; font-weight:bold; margin-bottom:16px;
      }
      .data-source-entry {
        margin-bottom:14px; padding:14px 16px; background:#ffffff;
        border-left:4px solid #ffcb05; border-radius:4px;
        box-shadow:0 1px 3px rgba(0,0,0,.08);
      }
      .data-source-entry p { margin:3px 0; font-size:13px; color:#444; line-height:1.5; }
      .data-source-entry strong { color:#00274c; }
      .data-source-entry a { color:#0073e6; text-decoration:none; }
      .data-source-entry a:hover { text-decoration:underline; }
      .data-sources-note {
        margin-top:14px; font-size:12px; color:#777; font-style:italic;
      }
    "))
  ),
  
  # ---- CONTROLS ----
  div(class = "radio-toolbar",
      radioButtons(
        inputId  = "level",
        label    = NULL,
        choices  = c("State" = "state", "County" = "county"),
        selected = "state",
        inline   = TRUE
      )
  ),
  
  div(class = "dropdown-container",
      fluidRow(
        column(4,
               selectInput(
                 inputId = "map_type",
                 label   = "Select Map Type",
                 choices = c("Standard", "Hotspot Analysis"),
                 selected = "Standard"
               )
        ),
        column(4,
               selectInput(
                 inputId  = "demographics",
                 label    = "Demographics",
                 choices  = available_options$demographics,
                 selected = if ("all_demographics" %in% available_options$demographics)
                              "all_demographics"
                            else head(available_options$demographics, 1)
               )
        ),
        column(4,
               selectInput(
                 inputId  = "selected_period",
                 label    = "Period",
                 choices  = available_options$periods,
                 selected = tail(available_options$periods, 1)
               )
        ),
        column(4,
               conditionalPanel(
                 condition = "input.level == 'county'",
                 # choices are set server-side via updateSelectInput()
                 selectInput(
                   inputId  = "selected_state_on_county_level",
                   label    = "State",
                   choices  = character(0)
                 )
               )
        )
      )
  ),
  
  # ---- MAIN LAYOUT ----
  div(class = "main-content",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId  = "var",
            label    = "Choose a variable to visualize:",
            choices  = available_options$injury_types,
            selected = head(available_options$injury_types, 1)
          ),
          tags$h4("Summary Statistics"),
          tableOutput("my_table")
        ),
        mainPanel(
          leafletOutput("usa_map", height = "500px", width = "100%")
        )
      )
  ),
  
  # ---- SCATTER SECTION ----
  div(class = "scatter-layout",
      div(class = "scatter-sidebar",
          div(class = "scatter-dropdown",
              selectInput(
                inputId  = "scatter_var",
                label    = "Select Environmental Variable:",
                choices  = c("Mean Temperature (deg F)" = "Mean Temperature",
                             "Annual Precipitation (in)" = "Precipitation"),
                selected = "Mean Temperature"
              )
          ),
          div(class = "explanation-box",
              tags$h5("Regression Analysis"),
              uiOutput("scatter_stats"),
              tags$hr(),
              tags$p("Hover over each metric to see what it means.")
          )
      ),
      div(class = "scatter-content",
          plotOutput("scatter_plot", height = "450px", width = "450px")
      )
  ),
  
  # ---- DATA SOURCES ----
  div(class = "data-sources-section",
      tags$h4("About the Data"),

      div(class = "data-source-entry",
          tags$p(tags$strong("Source: CDC Mapping Injury, Overdose, and Violence Dashboard")),
          tags$p(
            "Centers for Disease Control and Prevention, National Center for Injury Prevention ",
            "and Control (NCIPC), based on National Center for Health Statistics (NCHS) ",
            "National Vital Statistics System (NVSS) data. ",
            "Coverage: 2019–present, updated monthly."
          ),
          tags$p(
            tags$a(
              href = "https://www.cdc.gov/injury-violence-data/data-vis/index.html",
              target = "_blank",
              "www.cdc.gov/injury-violence-data/data-vis/index.html"
            ),
            HTML("&nbsp;|&nbsp;"),
            "State dataset: ",
            tags$a(href = "https://data.cdc.gov/d/fpsi-y8tj", target = "_blank", "data.cdc.gov/d/fpsi-y8tj"),
            HTML("&nbsp;|&nbsp;"),
            "County dataset: ",
            tags$a(href = "https://data.cdc.gov/d/psx4-wq38", target = "_blank", "data.cdc.gov/d/psx4-wq38")
          ),
          tags$p(
            tags$em(
              "Suggested citation: Centers for Disease Control and Prevention, National Center for ",
              "Injury Prevention and Control, Mapping Injury, Overdose, and Violence Dashboard. ",
              "Available at: https://www.cdc.gov/injury-violence-data/data-vis/index.html"
            )
          )
      ),

      tags$h4("Injury Category Definitions"),
      tags$p(
        style = "font-size:13px; color:#555; margin-bottom:12px;",
        "Definitions below are quoted directly from the ",
        tags$a(
          href   = "https://data.cdc.gov/api/views/fpsi-y8tj/files/165ef96f-aac7-4677-835d-7c7a809a51ff",
          target = "_blank",
          "CDC Injury Data Dictionary"
        ), "."
      ),
      tags$table(
        style = "width:100%; border-collapse:collapse; font-size:13px;",
        tags$thead(
          tags$tr(
            tags$th(style = "text-align:left; padding:8px 12px; background:#00274c; color:#fff; width:22%;",
                    "Category (dropdown label)"),
            tags$th(style = "text-align:left; padding:8px 12px; background:#00274c; color:#fff;",
                    "Definition")
          )
        ),
        tags$tbody(
          tags$tr(
            style = "background:#fff;",
            tags$td(style = "padding:8px 12px; font-weight:bold; border-bottom:1px solid #e0e0e0; vertical-align:top;",
                    "Drug Overdose"),
            tags$td(style = "padding:8px 12px; border-bottom:1px solid #e0e0e0; color:#333;",
                    "Deaths from ", tags$strong("unintentional or undetermined intent"), " drug overdose.")
          ),
          tags$tr(
            style = "background:#f9f9f9;",
            tags$td(style = "padding:8px 12px; font-weight:bold; border-bottom:1px solid #e0e0e0; vertical-align:top;",
                    "All Homicide"),
            tags$td(style = "padding:8px 12px; border-bottom:1px solid #e0e0e0; color:#333;",
                    "Deaths from homicide, including ", tags$em("any"),
                    " mechanism: firearm, cut/pierce, suffocation, blunt force, poisoning, and all other methods.")
          ),
          tags$tr(
            style = "background:#fff;",
            tags$td(style = "padding:8px 12px; font-weight:bold; border-bottom:1px solid #e0e0e0; vertical-align:top;",
                    "All Suicide"),
            tags$td(style = "padding:8px 12px; border-bottom:1px solid #e0e0e0; color:#333;",
                    "Deaths from suicide, including ", tags$em("any"),
                    " mechanism: firearm, poisoning, suffocation, falls, cut/pierce, and all other methods.")
          ),
          tags$tr(
            style = "background:#f9f9f9;",
            tags$td(style = "padding:8px 12px; font-weight:bold; border-bottom:1px solid #e0e0e0; vertical-align:top;",
                    "Firearm Deaths"),
            tags$td(style = "padding:8px 12px; border-bottom:1px solid #e0e0e0; color:#333;",
                    "Deaths from ", tags$em("any"),
                    " type of firearm injury, including suicide, homicide, unintentional injury, ",
                    "legal intervention, and undetermined intent.")
          ),
          tags$tr(
            style = "background:#fff;",
            tags$td(style = "padding:8px 12px; font-weight:bold; border-bottom:1px solid #e0e0e0; vertical-align:top;",
                    "Firearm Homicide"),
            tags$td(style = "padding:8px 12px; border-bottom:1px solid #e0e0e0; color:#333;",
                    "Deaths from firearm homicide only (subset of All Homicide and Firearm Deaths).")
          ),
          tags$tr(
            style = "background:#f9f9f9;",
            tags$td(style = "padding:8px 12px; font-weight:bold; vertical-align:top;",
                    "Firearm Suicide"),
            tags$td(style = "padding:8px 12px; color:#333;",
                    "Deaths from firearm suicide only (subset of All Suicide and Firearm Deaths).")
          )
        )
      ),

      tags$p(class = "data-sources-note",
             "Rates are crude death rates per 100,000 population. ",
             "Counts of 1-9 deaths in a county or state are suppressed per NCHS confidentiality standards and shown as \"Unreliable\". ",
             "Drug overdose data carry an approximate 6-month reporting lag; ",
             "suicide, homicide, and firearm data carry an approximate 4-month lag.")
  )

  # ---- FOOTER (commented out — restore when header/nav is re-enabled) ----
  # tags$footer(class = "site-footer", ...)

)