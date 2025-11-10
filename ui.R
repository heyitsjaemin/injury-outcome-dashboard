# ui.R
library(shiny)
library(shinyjs)
library(tmap)   # for tmapOutput

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
    "))
  ),
  
  # ---- HEADER ----
  div(class = "header-container",
      div(class = "top-bar",
          tags$a(href = "https://injurycenter.umich.edu/about-us/membership/", "Become a Member"),
          tags$a(href = "https://injurycenter.umich.edu/#", "Donate")
      ),
      div(class = "header",
          # Put templogo.png inside ./www/image/templogo.png
          tags$img(src = "image/templogo.png", class = "logo"),
          div(class = "nav-links",
              tags$a(href = "https://injurycenter.umich.edu/about-us/", "About"),
              tags$a(href = "https://injurycenter.umich.edu/injury-focus-areas/", "Focus Areas"),
              tags$a(href = "https://injurycenter.umich.edu/education/", "Education"),
              tags$a(href = "https://injurycenter.umich.edu/research/", "Research"),
              tags$a(href = "https://injurycenter.umich.edu/services-resources/", "Resources"),
              tags$a(href = "https://injurycenter.umich.edu/events", "Events"),
              tags$a(href = "https://injurycenter.umich.edu/about-us/contact-us/", "Contact Us")
          )
      )
  ),
  
  # ---- TITLE / META ----
  div(class = "title-panel", "Injury Related Outcome Data"),
  div(class = "update-date", "Last Updated on Nov 09, 2025"),
  
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
                 choices  = c(
                   "All Demographics", "Male", "Female", "Asian", "Black or African American",
                   "White", "Hispanic or Latino", "Age < 1 year", "Age 1-4 years", "Age 5-14 years",
                   "Age 15-24 years", "Age 25-34 years", "Age 35-44 years", "Age 45-54 years",
                   "Age 55-64 years", "Age 65-74 years", "Age 75-84 years", "Age 85+ years"
                 ),
                 selected = "All Demographics"
               )
        ),
        column(4,
               selectInput(
                 inputId  = "selected_period",
                 label    = "Period",
                 # choices  = c("2019", "2020", "2021", "2022", "2023", "TTM"),
                 choices = c("2023"),
                 selected = "2023"
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
            # choices  = c("Unintentional Drug Overdose Death Rate", "Firearm", "Suicide", "Homicide"),
            choices = c("Unintentional Drug Overdose Death Rate"),
            selected = "Unintentional Drug Overdose Death Rate"
          ),
          tags$h4("Summary Statistics"),
          tableOutput("my_table")
        ),
        mainPanel(
          tmapOutput("usa_map")
        )
      )
  ),
  
  # ---- SCATTER SECTION ----
  div(class = "scatter-layout",
      div(class = "scatter-sidebar",
          div(class = "scatter-dropdown",
              selectInput(
                inputId  = "scatter_var",
                label    = "Select Variable to Compare with Crude Rate:",
                choices  = c("Precipitation", "Mean Temperature"),
                selected = "Precipitation"
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
          plotOutput("scatter_plot", height = "450px", width = "100%")
      )
  ),
  
  # ---- FOOTER ----
  tags$footer(
    class = "site-footer",
    div(class = "footer-container",
        div(class = "footer-column",
            h3("Contact Us"),
            p("734-936-9312"),
            p("2800 Plymouth Road, Suite B10-G080"),
            p("Ann Arbor, MI 48109-2800"),
            tags$a(
              href   = "https://injurycenter.umich.edu/about-us/contact-us/",
              target = "_blank",
              class  = "contact-btn",
              "CONTACT FORM"
            ),
            h3("Follow Us"),
            div(class = "social-icons",
                tags$a(href = "#", tags$i(class = "fa fa-facebook")),
                tags$a(href = "#", tags$i(class = "fa fa-twitter"))
            )
        ),
        div(class = "footer-column",
            h3("Make a Donation"),
            p("If you share our passion for reducing injury in our state, region, and beyond, let’s talk."),
            tags$a(
              href  = "https://leadersandbest.umich.edu/find/#!/give/basket/fund/322195",
              class = "donate-btn",
              "DONATE"
            )
        ),
        div(class = "footer-column",
            h3("Become A Member"),
            p("We warmly invite all interested in injury prevention to become a member."),
            tags$a(
              href  = "https://injurycenter.umich.edu/about-us/membership/becoming-a-member/",
              class = "membership-btn",
              "LEARN ABOUT MEMBERSHIP"
            )
        )
    ),
    hr(class = "footer-line"),
    div(class = "footer-bottom",
        p(
          "Copyright © 2025 ",
          tags$a(href = "https://regents.umich.edu/", "Regents of the University of Michigan"), " — ",
          tags$a(href = "https://umich.edu/", "U-M Gateway"), " — ",
          tags$a(href = "https://ecrt.umich.edu/", "Non-Discrimination Policy"), " — ",
          tags$a(href = "https://procurement.umich.edu/suppliers/boxcar-studio/", "Michigan Web Design"),
          " by Boxcar Studio"
        )
    )
  )
)
