## NASA POWER Daily Data Downloader for SWAT Model
## You'll need: shiny, leaflet, leaflet.extras, nasapower, tidyverse, terra

library(shiny)
library(leaflet)
library(leaflet.extras)
library(nasapower)
library(tidyverse)
library(terra)

options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB

# Null-coalesce helper: returns `a` if it's not NULL, otherwise `b`.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Converts NASA POWER fill values (-999, -999.0) and anything non-finite to NA.
# NASA POWER uses -999 for missing days — without cleaning these, they show up as
# massive spikes in plots and trash any statistics we compute.
clean_missing <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- NA            # Inf, -Inf, NaN -> NA
  x[x <= -990] <- NA                # -999 sentinel (and similar large negatives)
  x
}

# Lightweight card widget — swaps out shinydashboard's box() with plain divs and
# CSS so we don't need that extra dependency. `accent` sets the left-border/title color.
card <- function(title = NULL, ..., accent = "teal") {
  div(class = paste0("ui-card ui-card-", accent),
      if (!is.null(title)) div(class = "ui-card-header", title),
      div(class = "ui-card-body", ...)
  )
}

# Collapsible section using native <details>/<summary> — collapsed by default
# so the customization controls stay out of the way until the user wants them.
collapsible <- function(title, ..., open = FALSE) {
  d <- tags$details(class = "ui-collapsible",
                    tags$summary(class = "ui-collapsible-summary", title),
                    div(class = "ui-collapsible-body", ...)
  )
  if (isTRUE(open)) d <- tagAppendAttributes(d, open = NA)
  d
}

VAR_CHOICES <- c(
  "Relative Humidity (RH2M)"            = "RH2M",
  "Wind Speed (WS10M)"                  = "WS10M",
  "Solar Radiation (ALLSKY_SFC_SW_DWN)"     = "ALLSKY_SFC_SW_DWN",
  "Precipitation (PRECTOTCORR)"         = "PRECTOTCORR"
)

VAR_FOLDERS <- c(
  RH2M              = "Relative Humidity",
  WS10M             = "Wind Speed",
  ALLSKY_SFC_SW_DWN = "Solar Radiation",
  PRECTOTCORR       = "Precipitation",
  T2M_MAX           = "Temperature",
  T2M_MIN           = "Temperature"
)

# Human-readable labels for the visualization dropdown (Tmax/Tmin are kept separate)
# plus the default y-axis unit string for each variable.
VIZ_VAR_LABELS <- c(
  RH2M              = "Relative Humidity (RH2M)",
  WS10M             = "Wind Speed (WS10M)",
  ALLSKY_SFC_SW_DWN = "Solar Radiation (ALLSKY_SFC_SW_DWN)",
  PRECTOTCORR       = "Precipitation (PRECTOTCORR)",
  T2M_MAX           = "Max Air Temperature (T2M_MAX)",
  T2M_MIN           = "Min Air Temperature (T2M_MIN)"
)

VIZ_VAR_UNITS <- c(
  RH2M              = "Relative Humidity (%)",
  WS10M             = "Wind Speed (m/s)",
  ALLSKY_SFC_SW_DWN = "Solar Radiation (MJ/m^2/day)",
  PRECTOTCORR       = "Precipitation (mm)",
  T2M_MAX           = "Max Air Temperature (\u00b0C)",
  T2M_MIN           = "Min Air Temperature (\u00b0C)"
)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "NASA POWER Daily Data Downloader for SWAT",
  
  tags$head(tags$style(HTML("    /* ── Page base ── */
    body { background: #ffffff; font-family: 'Segoe UI', Arial, sans-serif; }
    /* Remove fluidPage's default container padding so the sidebar sits
       flush against the left edge and content fills the full width. */
    .container-fluid { padding-left: 0 !important; padding-right: 0 !important; }
    .app-body, .app-header { width: 100%; }

    /* ── Fixed teal header bar ── */
    .app-header {
      position: fixed; top: 0; left: 0; right: 0; height: 54px; z-index: 1030;
      background: #0e8a7a; color: #ffffff;
      display: flex; align-items: center; gap: 10px;
      padding: 0 18px; font-size: 19px; font-weight: 600;
      box-shadow: 0 1px 4px rgba(0,0,0,0.15);
    }
    .app-header .ver { font-size: 13px; font-weight: 400; opacity: 0.85; }

    /* Push everything below the fixed header */
    .app-body { margin-top: 54px; }

    /* ── navlistPanel as a sidebar + content layout using flex ── */
    /* Only the top-level row (direct child of .app-body) gets the flex treatment.
       Nested rows inside cards (e.g. the checkbox fluidRows) stay as normal Bootstrap. */
    .app-body > .row {
      display: flex;
      flex-wrap: nowrap;
      margin: 0;
      min-height: calc(100vh - 54px);
      align-items: stretch;
    }

    /* Sidebar column (the navlist) — dark slate, full height */
    .app-body > .row > div:first-child {
      background: #1e3a5f;
      padding: 0 !important;
      flex: 0 0 260px;
      max-width: 260px;
    }
    /* Content column — white background, takes up the remaining width */
    .app-body > .row > div:last-child {
      background: #ffffff;
      padding: 22px 26px !important;
      flex: 1 1 auto;
      min-width: 0;            /* let inner content wrap rather than overflow */
    }

    /* Nested rows inside cards keep default Bootstrap layout */
    .app-body .ui-card .row { display: flex; flex-wrap: wrap; min-height: 0; margin-right: -15px; margin-left: -15px; }

    /* Citation blockquotes — smaller, tighter text */
    .ui-card blockquote {
      font-size: 13px;
      line-height: 1.55;
      padding: 8px 14px;
      margin: 8px 0 12px;
    }

    /* Comfortable spacing for bullet/number lists inside cards */
    .ui-card ul, .ui-card ol { margin: 4px 0; padding-left: 22px; }
    .ui-card li { margin-bottom: 10px; line-height: 1.6; }
    .ui-card li:last-child { margin-bottom: 0; }

    .app-body .well, .app-body .nav-pills { background: transparent; border: none; box-shadow: none; }
    .app-body .nav-pills { padding: 0; margin: 0; }
    .app-body .nav-pills > li { margin: 0; width: 100%; }
    .app-body .nav-pills > li > a {
      color: #cdd9e5 !important;
      border-radius: 0;
      padding: 13px 18px;
      font-size: 15px;
      border-bottom: 1px solid #2a4a70;
    }
    .app-body .nav-pills > li > a:hover {
      background: #254d7a !important; color: #ffffff !important;
    }
    .app-body .nav-pills > li.active > a,
    .app-body .nav-pills > li.active > a:hover,
    .app-body .nav-pills > li.active > a:focus {
      background: #0e8a7a !important; color: #ffffff !important; font-weight: 600;
    }

    /* Inside the content area, the tab-content holds the cards */
    .app-body .tab-content { background: #ffffff; }

    /* ── Card (box) styling ── */
    .ui-card {
      background: #ffffff;
      border: 1px solid #e3e8ee;
      border-top: 3px solid #0e8a7a;
      border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      margin-bottom: 18px;
    }
    .ui-card-warning { border-top-color: #f0ad4e; }
    .ui-card-header {
      padding: 12px 18px;
      font-size: 17px; font-weight: 600; color: #0b6e61;
      border-bottom: 1px solid #e3e8ee;
    }
    .ui-card-warning .ui-card-header { color: #b9770f; }
    .ui-card-body { padding: 16px 18px; }

    /* Collapsible customization sections */
    .ui-collapsible {
      border: 1px solid #bfe0da; border-radius: 4px;
      margin-bottom: 12px; background: #ffffff; overflow: visible;
    }
    .ui-collapsible-summary {
      cursor: pointer; padding: 11px 16px; font-weight: 600; color: #0b6e61;
      list-style: none; user-select: none;
      background: #e7f4f1; border-bottom: 1px solid transparent;
      display: flex; align-items: center;
      border-radius: 4px 4px 0 0;
    }
    .ui-collapsible[open] > .ui-collapsible-summary { border-bottom-color: #bfe0da; }
    .ui-collapsible-summary::-webkit-details-marker { display: none; }
    /* Chevron drawn with CSS borders — avoids special characters that might not render */
    .ui-collapsible-summary::before {
      content: ''; display: inline-block;
      width: 7px; height: 7px; margin-right: 10px;
      border-right: 2px solid #0e8a7a; border-bottom: 2px solid #0e8a7a;
      transform: rotate(-45deg); transition: transform 0.15s;
      position: relative; top: -1px;
    }
    .ui-collapsible[open] > .ui-collapsible-summary::before { transform: rotate(45deg); top: 1px; }
    .ui-collapsible-summary:hover { background: #d8ece7; }
    .ui-collapsible-body { padding: 12px 16px 14px; }

    /* Make form inputs (selects, text/number fields) clearly visible */
    .form-control, select.form-control {
      border: 1px solid #b8c4d0 !important;
      border-radius: 4px;
      box-shadow: inset 0 1px 1px rgba(0,0,0,0.04);
      background-color: #ffffff;
    }
    .form-control:focus, select.form-control:focus {
      border-color: #0e8a7a !important;
      box-shadow: 0 0 0 2px rgba(14,138,122,0.15);
    }
    .selectize-input {
      border: 1px solid #b8c4d0 !important;
      box-shadow: inset 0 1px 1px rgba(0,0,0,0.04) !important;
    }
    .selectize-input.focus { border-color: #0e8a7a !important; }

    h4, h5 { color: #0b6e61; font-weight: 600; }
    .checkbox label { line-height: 1.4; }

    /* ── Buttons ── */
    .btn-fetch { background-color: #0e8a7a; color: #ffffff; border: none; font-weight: 600; }
    .btn-fetch:hover { background-color: #0b6e61; color: #ffffff; }
    .btn-success {
      background-color: #2e9e5b; border-color: #277f4a;
      color: #ffffff !important; font-weight: 600;
    }
    .btn-success:hover { background-color: #277f4a; color: #ffffff !important; }
    .btn-success.disabled, .btn-success:disabled {
      background-color: #9bbfa8; border-color: #9bbfa8;
      color: #2c3e34 !important; opacity: 1; font-weight: 600;
    }
    .btn-primary {
      background-color: #1a6ebd; border-color: #155a9b;
      color: #ffffff !important; font-weight: 600;
    }
    .btn-primary:hover { background-color: #155a9b; color: #ffffff !important; }
    .btn-secondary {
      background-color: #6c757d; border-color: #5a6268;
      color: #ffffff !important; font-weight: 600;
    }
    .btn-secondary.disabled, .btn-secondary:disabled {
      background-color: #b8bdc2; border-color: #b8bdc2;
      color: #2c3033 !important; opacity: 1; font-weight: 600;
    }
    .btn-block { width: 100%; margin-bottom: 6px; text-align: center; }

    /* ── Status box (terminal) ── */
    .status-box {
      background: #0d0d0d; color: #ffffff; border: none; border-radius: 4px;
      padding: 14px 16px; font-family: 'Courier New', Consolas, monospace;
      font-size: 14px; line-height: 1.7; max-height: 260px; overflow-y: auto;
      white-space: pre-wrap;
    }
    .status-box pre {
      background: transparent !important; border: none !important;
      color: #ffffff !important; font-size: 14px !important; margin: 0; padding: 0;
    }

    .hint-text { color: #777; font-size: 13px; margin-top: 4px; }
    .badge-ready { background: #27ae60; color: #fff; padding: 3px 8px;
                   border-radius: 10px; font-size: 11px; font-weight: 600; }
    .footer-credit { font-size: 12px; color: #999; text-align: center;
                     padding: 14px 0 6px; border-top: 1px solid #e0e0e0; margin-top: 20px; }
  "))),
  # ── Fixed header ────────────────────────────────────────────────────────────
  div(class = "app-header",
      icon("satellite-dish"),
      "NASA POWER Daily Data Downloader for SWAT",
      span(class = "ver", "v1.0")
  ),
  
  # ── Body: navlistPanel (native Shiny vertical tabs) ─────────────────────────
  div(class = "app-body",
      navlistPanel(
        id = "main_nav",
        widths = c(3, 9),
        well = FALSE,
        
        # ── About ───────────────────────────────────────────────────────────────
        tabPanel(
          "About this software", icon = icon("info-circle"),
          card("About this software",
               tags$ul(
                 tags$li("This tool downloads daily climate data from the ",
                         tags$strong("NASA POWER"), " database and formats it for use in ",
                         tags$a("SWAT", href = "https://swat.tamu.edu/", target = "_blank",
                                onclick = "window.open('https://swat.tamu.edu/', '_blank'); return false;"), ", ",
                         tags$a("SWAT+", href = "https://swat.tamu.edu/software/plus/", target = "_blank",
                                onclick = "window.open('https://swat.tamu.edu/software/plus/', '_blank'); return false;"),
                         ", and related hydrological models."),
                 tags$li("Supported variables: Relative Humidity, Wind Speed, Solar Radiation, Air Temperature (Minimum & Maximum), and Precipitation."),
                 tags$li("Station grids and output files are automatically structured in the folder format expected by SWAT."),
                 tags$li("An optional DEM raster can be imported to extract station elevations for the lookup table."),
                 tags$li("Visualize timeseries of fetched station data and compare it with observed data.")
               )
          ),
          card("Usage",
               tags$ul(
                 tags$li(tags$strong("1. Define Study Area"), " — Draw a bounding box on the map or enter coordinates manually."),
                 tags$li(tags$strong("2. Filter & Fetch Data"), " — Select variables, year range, then click Fetch Data to download from NASA POWER."),
                 tags$li(tags$strong("3. Export Data"), " — Set output folder and export as .txt (SWAT format) or .csv."),
                 tags$li(tags$strong("4. Lookup Table"), " — Optionally import a DEM for elevation and export the station lookup table."),
                 tags$li(tags$strong("5. Visualize Timeseries Data"), " — Produce publication-ready timeseries plots and scatterplots for your fetched data and/or compare them with observed data."),
                 tags$li(tags$strong("Reset"), " — Start over with a new study area or new parameters.")
               )
          ),
          card("How to cite?",
               p("If you use this tool in your research or project, please cite it as:"),
               tags$blockquote(
                 style = "border-left: 4px solid #0e8a7a; padding: 10px 16px; background: #f0faf8;
               border-radius: 0 4px 4px 0; font-style: italic; color: #333;",
                 "Cabigao, K.M.F. (", format(Sys.Date(), "%Y"), "). ",
                 tags$em("NASA POWER Daily Data Downloader for SWAT Model (v1.0)"), ". ",
                 "Retrieved from ",
                 tags$a("https://github.com/YourUsername/NASAPower-SWAT-Downloader",
                        href = "https://github.com/YourUsername/NASAPower-SWAT-Downloader",
                        target = "_blank"),
                 "."
               ),
               p("You may also cite the underlying NASA POWER data source:"),
               tags$blockquote(
                 style = "border-left: 4px solid #0e8a7a; padding: 10px 16px; background: #f0faf8;
               border-radius: 0 4px 4px 0; font-style: italic; color: #333;",
                 "Sparks, A.H. (2018). nasapower: A NASA POWER Global Meteorology, Surface Solar Energy and Climatology Data Client for R. ",
                 tags$em("Journal of Open Source Software"), ", 3(30), 1035. ",
                 tags$a("https://doi.org/10.21105/joss.01035",
                        href = "https://doi.org/10.21105/joss.01035",
                        target = "_blank")
               ),
               div(class = "footer-credit",
                   "Created by ", tags$strong("Kean Michael F. Cabigao"), " · ",
                   tags$a("knmchlcabigao@gmail.com", href = "mailto:knmchlcabigao@gmail.com")
               )
          )
        ),
        
        # ── Define Study Area ─────────────────────────────────────────────────────
        tabPanel(
          "1. Define Study Area", icon = icon("map-marker-alt"),
          card("Define your study area",
               radioButtons("bbox_mode", label = NULL,
                            choices  = c("Draw on map" = "draw", "Enter coordinates" = "manual"),
                            selected = "draw", inline = TRUE),
               conditionalPanel(
                 condition = "input.bbox_mode == 'draw'",
                 p("Use the rectangle tool on the map to define your bounding box.",
                   class = "hint-text")
               ),
               conditionalPanel(
                 condition = "input.bbox_mode == 'manual'",
                 fluidRow(
                   column(3, numericInput("man_lon_min", "Lon Min", value = 120.44, step = 0.01)),
                   column(3, numericInput("man_lon_max", "Lon Max", value = 122.48, step = 0.01)),
                   column(3, numericInput("man_lat_min", "Lat Min", value = 13.37, step = 0.01)),
                   column(3, numericInput("man_lat_max", "Lat Max", value = 15.39, step = 0.01))
                 ),
                 actionButton("apply_manual_btn", "Apply Coordinates",
                              class = "btn-info", icon = icon("check"))
               )
          ),
          card("Bounding Box Preview",
               leafletOutput("map", height = "430px"),
               br(),
               tags$strong("Current bounding box:", style = "display:block; margin-bottom:8px;"),
               verbatimTextOutput("bbox_display")
          )
        ),
        
        # ── Filter & Fetch ────────────────────────────────────────────────────────
        tabPanel(
          "2. Filter & Fetch Data", icon = icon("filter"),
          card("Select Variables",
               fluidRow(
                 column(3, checkboxInput("var_rh",    "Relative Humidity (RH2M)",            value = TRUE)),
                 column(3, checkboxInput("var_ws",    "Wind Speed (WS10M)",                  value = TRUE)),
                 column(3, checkboxInput("var_solar", "Solar Radiation (ALLSKY_SFC_SW_DWN)", value = TRUE)),
                 column(3, checkboxInput("var_prec",  "Precipitation (PRECTOTCORR)",         value = TRUE))
               ),
               fluidRow(
                 column(6, checkboxInput("var_temp",  "Air Temperature (T2M_MAX & T2M_MIN)", value = TRUE))
               )
          ),
          card("Select Year Range",
               fluidRow(
                 column(3, numericInput("year_start", "From", value = 1995,
                                        min = 1981, max = as.integer(format(Sys.Date(), "%Y")) - 1, step = 1)),
                 column(3, numericInput("year_end", "To", value = 1996,
                                        min = 1981, max = as.integer(format(Sys.Date(), "%Y")) - 1, step = 1))
               ),
               uiOutput("year_range_warning")
          ),
          card("Fetch Data",
               actionButton("fetch_btn", "Fetch Data from NASA POWER",
                            class = "btn-fetch btn-block",
                            icon  = icon("satellite")),
               br(),
               conditionalPanel(
                 condition = "input.fetch_btn > 0",
                 h5(icon("terminal"), " Status"),
                 div(class = "status-box", verbatimTextOutput("log_output"))
               ),
               conditionalPanel(
                 condition = "output.data_ready == 'yes'",
                 br(),
                 uiOutput("fetch_summary_badge")
               )
          ),
          conditionalPanel(
            condition = "output.data_ready == 'yes'",
            card("Station Points",
                 leafletOutput("station_map", height = "350px")
            ),
            card("Sample Data",
                 div(style = "overflow-x: auto;", tableOutput("preview_table"))
            )
          )
        ),
        
        # ── Export Data ───────────────────────────────────────────────────────────
        tabPanel(
          "3. Export Data", icon = icon("file-export"),
          card("Output Folder",
               textInput("out_dir", "Save files to", value = getwd(), width = "100%"),
               p("Files will be organized by variable subfolder (e.g. Precipitation/, Wind Speed/, Temperature/).",
                 class = "hint-text")
          ),
          card("Export Files",
               uiOutput("txt_btn_ui"),
               br(),
               uiOutput("csv_btn_ui"),
               br(),
               conditionalPanel(
                 condition = "input.fetch_btn > 0",
                 h5(icon("terminal"), " Status"),
                 div(class = "status-box", verbatimTextOutput("export_log_output"))
               )
          )
        ),
        
        # ── Lookup Table ──────────────────────────────────────────────────────────
        tabPanel(
          "4. Lookup Table", icon = icon("table"),
          card("DEM for Elevation (optional)",
               uiOutput("dem_file_ui"),
               verbatimTextOutput("dem_status")
          ),
          card("Export Lookup Table",
               uiOutput("lookup_btn_ui"),
               br(),
               conditionalPanel(
                 condition = "output.lookup_ready == 'yes'",
                 h5(icon("eye"), " Lookup Table Preview ",
                    tags$span(class = "badge-ready", "Generated")),
                 div(style = "overflow-x: auto;", tableOutput("lookup_preview"))
               )
          )
        ),
        
        # ── Visualize Timeseries Data ─────────────────────────────────────────────
        tabPanel(
          "5. Visualize Timeseries Data", icon = icon("chart-line"),
          conditionalPanel(
            condition = "output.data_ready != 'yes'",
            card("Visualize Timeseries Data",
                 p("No data loaded yet. Please fetch data in the ",
                   tags$strong("Filter & Fetch Data"), " tab first, then return here to plot it.",
                   class = "hint-text")
            )
          ),
          conditionalPanel(
            condition = "output.data_ready == 'yes'",
            card("Timeseries Plot",
                 fluidRow(
                   column(6,
                          selectInput("viz_station", "Station",
                                      choices = NULL, width = "100%")
                   ),
                   column(6,
                          selectInput("viz_var", "Variable",
                                      choices = NULL, width = "100%")
                   )
                 ),
                 p("Tip: choose \"All stations\" to overlay every station for the selected variable.",
                   class = "hint-text"),
                 collapsible("Customize timeseries plot",
                             fluidRow(
                               column(4, textInput("viz_title", "Plot title", value = "", width = "100%")),
                               column(4, textInput("viz_xlab",  "X-axis label", value = "Date", width = "100%")),
                               column(4, textInput("viz_ylab",  "Y-axis label", value = "", width = "100%"))
                             ),
                             fluidRow(
                               column(4, selectInput("viz_datefmt", "Date axis format",
                                                     choices = c("Auto (by time span)" = "auto",
                                                                 "Month + Year (Jan 1995)" = "monthyear",
                                                                 "Year only (1995)" = "year",
                                                                 "Full date (1995-01-15)" = "full"),
                                                     selected = "auto", width = "100%")),
                               column(4, textInput("viz_color", "Line color",
                                                   value = "#0e8a7a", width = "100%")),
                               column(4, textInput("viz_fit_label", "Fetched legend label",
                                                   value = "NASA POWER", width = "100%"))
                             ),
                             fluidRow(
                               column(4, numericInput("viz_linewidth", "Line width",
                                                      value = 0.7, min = 0.2, max = 4, step = 0.1, width = "100%")),
                               column(4,
                                      checkboxInput("viz_points", "Show points", value = FALSE),
                                      checkboxInput("viz_legend", "Show legend", value = TRUE))
                             ),
                             fluidRow(
                               column(12,
                                      actionButton("viz_apply", "Apply Customization",
                                                   class = "btn-fetch", icon = icon("paint-brush")),
                                      actionButton("viz_reset", "Reset to Defaults",
                                                   class = "btn-secondary", icon = icon("undo"),
                                                   style = "margin-left: 8px;")
                               )
                             )
                 ),
                 plotOutput("viz_plot", height = "440px")
            ),
            card("Compare with Observed Data (optional)",
                 p("Upload a CSV of observed values to overlay on the plot and compute fit statistics. ",
                   "The file should have a date column and a value column. ",
                   "The overlay updates automatically when columns or date format are changed.",
                   class = "hint-text"),
                 fluidRow(
                   column(6, uiOutput("obs_file_ui")),
                   column(6,
                          selectInput("obs_compare_to", "Compare observed against",
                                      choices = c("Selected single station" = "station",
                                                  "Daily average (all stations)" = "average"),
                                      selected = "station", width = "100%"))
                 ),
                 conditionalPanel(
                   condition = "output.obs_loaded == 'yes'",
                   collapsible("Observed data setup & styling",
                               fluidRow(
                                 column(4, selectInput("obs_date_col", "Date column",
                                                       choices = NULL, width = "100%")),
                                 column(4, selectInput("obs_val_col", "Value column",
                                                       choices = NULL, width = "100%")),
                                 column(4, selectInput("obs_dateformat", "Observed date format",
                                                       choices = c("YYYY-MM-DD" = "%Y-%m-%d",
                                                                   "MM/DD/YYYY" = "%m/%d/%Y",
                                                                   "DD/MM/YYYY" = "%d/%m/%Y",
                                                                   "YYYYMMDD"   = "%Y%m%d"),
                                                       selected = "%Y-%m-%d", width = "100%"))
                               ),
                               fluidRow(
                                 column(4, textInput("obs_color", "Observed line color",
                                                     value = "#d1495b", width = "100%")),
                                 column(4, textInput("obs_label", "Observed legend label",
                                                     value = "Observed", width = "100%")),
                                 column(4, selectInput("obs_stat_pos", "Statistics position",
                                                       choices = c("Top-left (inside)"     = "topleft",
                                                                   "Top-right (inside)"    = "topright",
                                                                   "Bottom-left (inside)"  = "bottomleft",
                                                                   "Bottom-right (inside)" = "bottomright",
                                                                   "Outside (right side)"  = "right",
                                                                   "Outside (below)"       = "below",
                                                                   "Hidden"                = "none"),
                                                       selected = "topleft", width = "100%"))
                               )
                   ),
                   fluidRow(
                     column(12,
                            actionButton("obs_apply", "Overlay & Compute Stats",
                                         class = "btn-fetch", icon = icon("code-compare")),
                            actionButton("obs_clear", "Remove Observed",
                                         class = "btn-secondary", icon = icon("xmark"),
                                         style = "margin-left: 8px;"))
                   )
                 ),
                 conditionalPanel(
                   condition = "input.obs_file != null",
                   div(class = "status-box", verbatimTextOutput("obs_status"))
                 )
            ),
            conditionalPanel(
              condition = "output.obs_stats_ready == 'yes'",
              card("Observed vs Fetched — Scatter Plot",
                   p("Each point is a day where both observed and fetched values exist. ",
                     "The solid line is the linear fit. Statistics suitable for climatological ",
                     "timeseries are shown (R\u00b2, KGE and its components, bias, RMSE, MAE).",
                     class = "hint-text"),
                   plotOutput("obs_scatter_plot", height = "460px"),
                   collapsible("Customize scatter plot",
                               fluidRow(
                                 column(6, textInput("sc_title", "Plot title", value = "", width = "100%")),
                                 column(3, textInput("sc_xlab", "X-axis label", value = "", width = "100%")),
                                 column(3, textInput("sc_ylab", "Y-axis label", value = "", width = "100%"))
                               ),
                               fluidRow(
                                 column(4, textInput("sc_point_col", "Point color", value = "#d1495b", width = "100%")),
                                 column(4, textInput("sc_fit_col",   "Fit-line color", value = "#0e8a7a", width = "100%")),
                                 column(4, numericInput("sc_point_size", "Point size", value = 1.8, min = 0.3, max = 6, step = 0.1, width = "100%"))
                               ),
                               fluidRow(
                                 column(3, checkboxInput("sc_show_fit", "Show fit line", value = TRUE)),
                                 column(3, checkboxInput("sc_show_legend", "Show legend", value = TRUE)),
                                 column(6,
                                        actionButton("sc_apply", "Apply Scatter Style",
                                                     class = "btn-fetch", icon = icon("paint-brush")),
                                        actionButton("sc_reset", "Reset Scatter Style",
                                                     class = "btn-secondary", icon = icon("undo"),
                                                     style = "margin-left: 8px;"))
                               )
                   )
              )
            ),
            conditionalPanel(
              condition = "output.obs_missing_ready == 'yes'",
              card("Missing-Data Summary",
                   p("Counts of valid vs missing values after treating -999 and non-finite numbers as missing. ",
                     "Statistics use only days where both series have a valid value.",
                     class = "hint-text"),
                   tableOutput("obs_missing_table")
              )
            ),
            card("Export Graph",
                 fluidRow(
                   column(3, selectInput("viz_which", "Plot to export",
                                         choices = c("Timeseries" = "timeseries",
                                                     "Scatter (obs vs fetched)" = "scatter"),
                                         selected = "timeseries", width = "100%")),
                   column(2, selectInput("viz_format", "Format",
                                         choices = c("PNG" = "png", "JPG" = "jpg"),
                                         selected = "png", width = "100%")),
                   column(2, numericInput("viz_width",  "Width (in)",  value = 9,   min = 1, max = 40, step = 0.5, width = "100%")),
                   column(2, numericInput("viz_height", "Height (in)", value = 5,   min = 1, max = 40, step = 0.5, width = "100%")),
                   column(3, numericInput("viz_dpi",    "Resolution (DPI)", value = 300, min = 50, max = 1200, step = 10, width = "100%"))
                 ),
                 p("Saved to the output folder set in the Export Data tab. Scatter export needs observed data overlaid first.",
                   class = "hint-text"),
                 actionButton("viz_export", "Export Graph",
                              class = "btn-success btn-block", icon = icon("image")),
                 br(),
                 conditionalPanel(
                   condition = "input.viz_export > 0",
                   div(class = "status-box", verbatimTextOutput("viz_export_log"))
                 )
            )
          )
        ),
        
        # ── Reset ─────────────────────────────────────────────────────────────────
        tabPanel(
          "Reset / New Request", icon = icon("redo"),
          card("Reset / New Request", accent = "warning",
               p("This will clear all fetched data, the current bounding box, DEM, logs, and all inputs. ",
                 "You will be returned to a clean starting state.", style = "color: #555;"),
               br(),
               actionButton("reset_btn", "Reset Everything",
                            class = "btn-warning btn-block",
                            icon  = icon("trash-alt")),
               br(), br(),
               p("Note: This does NOT delete any files you have already exported to disk.",
                 class = "hint-text")
          )
        )
      )
  )
)



# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Build the list of selected variables from the individual checkboxes
  selected_vars <- reactive({
    vars <- c()
    if (isTRUE(input$var_rh))    vars <- c(vars, "RH2M")
    if (isTRUE(input$var_ws))    vars <- c(vars, "WS10M")
    if (isTRUE(input$var_solar)) vars <- c(vars, "ALLSKY_SFC_SW_DWN")
    if (isTRUE(input$var_prec))  vars <- c(vars, "PRECTOTCORR")
    if (isTRUE(input$var_temp))  vars <- c(vars, "T2M_MAX", "T2M_MIN")
    vars
  })
  
  rv <- reactiveValues(
    bbox         = NULL,
    daily_data   = NULL,
    log_lines    = character(0),
    export_log   = character(0),
    dem          = NULL,
    dem_name     = NULL,
    lookup_table = NULL,
    dem_reset    = 0,      # bump this to rebuild a fresh (empty) DEM fileInput
    viz_export_log = character(0),
    obs_raw      = NULL,   # the raw data.frame from the uploaded observed CSV
    obs_reset    = 0,      # bump this to rebuild a fresh (empty) observed fileInput
    obs_series   = NULL,   # cleaned observed series: data.frame(DATE, OBS)
    obs_stats    = NULL,   # fit statistics table
    obs_merged   = NULL,   # matched obs/fitted pairs used for the scatter plot
    obs_stat_vals = NULL,  # numeric stats for annotating the plot directly
    obs_missing  = NULL,   # missing-data summary table
    obs_msg      = character(0),
    obs_auto_trigger = 0L  # incrementing this fires the overlay automatically
  )
  
  # Re-render the DEM file input dynamically so Reset can actually clear it.
  # Shiny's fileInput can't be emptied with update*; re-rendering with a new
  # rv$dem_reset value forces a brand-new empty widget.
  output$dem_file_ui <- renderUI({
    rv$dem_reset  # take a dependency so this re-renders on reset
    fileInput("dem_file", "Import DEM raster",
              accept      = c(".tif", ".tiff", ".img", ".asc", ".vrt", ".grd", ".nc"),
              buttonLabel = "Browse...",
              placeholder = "GeoTIFF, IMG, ASC, ...")
  })
  
  # ── Base map ──────────────────────────────────────────────────────────────
  make_map <- function() {
    leaflet() |>
      addTiles() |>
      setView(lng = 121.5, lat = 14.0, zoom = 6) |>
      addDrawToolbar(
        targetGroup         = "drawn",
        rectangleOptions    = drawRectangleOptions(repeatMode = FALSE),
        polylineOptions     = FALSE,
        polygonOptions      = FALSE,
        circleOptions       = FALSE,
        markerOptions       = FALSE,
        circleMarkerOptions = FALSE,
        editOptions         = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) |>
      addLayersControl(
        overlayGroups = "drawn",
        options       = layersControlOptions(collapsed = FALSE)
      )
  }
  
  output$map <- renderLeaflet({ make_map() })
  
  # Grab the bounding box whenever the user draws a rectangle on the map
  observeEvent(input$map_draw_new_feature, {
    feat   <- input$map_draw_new_feature
    coords <- feat$geometry$coordinates[[1]]
    lons   <- sapply(coords, `[[`, 1)
    lats   <- sapply(coords, `[[`, 2)
    rv$bbox <- c(lon_min = min(lons), lat_min = min(lats),
                 lon_max = max(lons), lat_max = max(lats))
  })
  
  observeEvent(input$map_draw_deleted_features, { rv$bbox <- NULL })
  
  # Apply manually entered coordinates
  observeEvent(input$apply_manual_btn, {
    lon_min <- input$man_lon_min; lon_max <- input$man_lon_max
    lat_min <- input$man_lat_min; lat_max <- input$man_lat_max
    if (lon_min >= lon_max || lat_min >= lat_max) {
      showNotification("Min values must be less than Max values.", type = "error"); return()
    }
    rv$bbox <- c(lon_min = lon_min, lat_min = lat_min, lon_max = lon_max, lat_max = lat_max)
    leafletProxy("map") |>
      clearGroup("drawn") |>
      addRectangles(lng1 = lon_min, lat1 = lat_min, lng2 = lon_max, lat2 = lat_max,
                    group = "drawn", color = "#3388ff", weight = 2, fillOpacity = 0.1) |>
      fitBounds(lon_min, lat_min, lon_max, lat_max)
    showNotification("Coordinates applied.", type = "message")
  })
  
  observeEvent(input$bbox_mode, {
    if (input$bbox_mode == "draw") {
      rv$bbox <- NULL
      output$map <- renderLeaflet({ make_map() })
    }
  })
  
  output$bbox_display <- renderText({
    if (is.null(rv$bbox)) "No bounding box set yet."
    else {
      b <- rv$bbox
      sprintf("Lon: %.4f  to  %.4f\nLat: %.4f  to  %.4f",
              b["lon_min"], b["lon_max"], b["lat_min"], b["lat_max"])
    }
  })
  
  # ── DEM import ─────────────────────────────────────────────────────────────
  observeEvent(input$dem_file, {
    req(input$dem_file)
    tryCatch({
      r <- terra::rast(input$dem_file$datapath)
      if (terra::nlyr(r) > 1) r <- r[[1]]
      rv$dem      <- r
      rv$dem_name <- input$dem_file$name
      log_msg("DEM loaded successfully.")
      showNotification("DEM loaded successfully.", type = "message")
    }, error = function(e) {
      rv$dem <- NULL; rv$dem_name <- NULL
      showNotification(paste("Could not read DEM:", conditionMessage(e)), type = "error", duration = 10)
    })
  })
  
  output$dem_status <- renderText({
    if (is.null(rv$dem)) "No DEM loaded."
    else {
      r   <- rv$dem
      ext <- as.vector(terra::ext(r))
      crs_name <- tryCatch({
        d <- terra::crs(r, describe = TRUE)
        if (!is.null(d$name) && !is.na(d$name)) d$name else "unknown"
      }, error = function(e) "unknown")
      sprintf("File: %s\nCRS: %s\nRes: %.5f, %.5f\nExtent X: %.4f to %.4f\nExtent Y: %.4f to %.4f",
              rv$dem_name, crs_name, terra::res(r)[1], terra::res(r)[2],
              ext["xmin"], ext["xmax"], ext["ymin"], ext["ymax"])
    }
  })
  
  # ── Export buttons (disabled until data has been fetched) ────────────────
  output$txt_btn_ui <- renderUI({
    if (is.null(rv$daily_data))
      actionButton("txt_btn", "Export as .txt Files (SWAT format)",
                   class = "btn btn-success btn-block disabled", icon = icon("file-alt"))
    else
      actionButton("txt_btn", "Export as .txt Files (SWAT format)",
                   class = "btn btn-success btn-block", icon = icon("file-alt"))
  })
  
  output$csv_btn_ui <- renderUI({
    if (is.null(rv$daily_data))
      actionButton("csv_btn", "Export as CSV",
                   class = "btn btn-success btn-block disabled", icon = icon("file-csv"))
    else
      actionButton("csv_btn", "Export as CSV",
                   class = "btn btn-success btn-block", icon = icon("file-csv"))
  })
  
  output$lookup_btn_ui <- renderUI({
    if (is.null(rv$daily_data) || is.null(rv$dem))
      actionButton("lookup_btn", "Export Lookup Table",
                   class = "btn btn-secondary btn-block disabled", icon = icon("table"))
    else
      actionButton("lookup_btn", "Export Lookup Table",
                   class = "btn btn-primary btn-block", icon = icon("table"))
  })
  
  # ── Helpers ────────────────────────────────────────────────────────────────
  log_msg <- function(msg) {
    rv$log_lines <- c(rv$log_lines,
                      paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
  }
  export_log_msg <- function(msg) {
    rv$export_log <- c(rv$export_log,
                       paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
  }
  
  output$log_output <- renderText({ paste(rv$log_lines, collapse = "\n") })
  output$export_log_output <- renderText({ paste(rv$export_log, collapse = "\n") })
  
  export_station_files <- function(data, var_name, out_dir) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    data |>
      mutate(Date = str_replace_all(as.character(YYYYMMDD), "-", "")) |>
      group_by(station_id) |>
      group_split() |>
      walk(function(df_station) {
        id         <- unique(df_station$station_id)
        start_date <- df_station$Date[1]
        values     <- df_station[[var_name]]
        out        <- c(start_date, values)
        writeLines(as.character(out),
                   con = file.path(out_dir, paste0("station_", id, ".txt")))
      })
  }
  
  # Temperature export — Tmax and Tmin go into the same file as "Tmax,Tmin" per row
  export_temp_files <- function(data, out_dir) {
    if (!all(c("T2M_MAX", "T2M_MIN") %in% names(data))) return()
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    data |>
      mutate(Date = str_replace_all(as.character(YYYYMMDD), "-", "")) |>
      group_by(station_id) |>
      group_split() |>
      walk(function(df_station) {
        id         <- unique(df_station$station_id)
        start_date <- df_station$Date[1]
        rows       <- paste0(df_station$T2M_MAX, ",", df_station$T2M_MIN)
        out        <- c(start_date, rows)
        writeLines(as.character(out),
                   con = file.path(out_dir, paste0("station_", id, ".txt")))
      })
  }
  
  validate_out_dir <- function(out_base) {
    if (!dir.exists(out_base)) {
      tryCatch(dir.create(out_base, recursive = TRUE),
               error = function(e) {
                 showNotification(paste("Cannot create folder:", out_base), type = "error")
                 return(FALSE)
               })
    }
    TRUE
  }
  
  attach_solar_nearest <- function(met_data, solar_df, solar_var) {
    solar_cells <- solar_df |> distinct(LAT, LON) |> rename(LAT_solar = LAT, LON_solar = LON)
    met_cells   <- met_data |> distinct(LAT, LON)
    nearest_map <- met_cells |>
      rowwise() |>
      mutate(
        .best = list({
          dlat <- solar_cells$LAT_solar - LAT
          dlon <- (solar_cells$LON_solar - LON) * cos(LAT * pi / 180)
          d2   <- dlat^2 + dlon^2
          j    <- which.min(d2)
          c(LAT_solar = solar_cells$LAT_solar[j], LON_solar = solar_cells$LON_solar[j])
        }),
        LAT_solar = .best["LAT_solar"],
        LON_solar = .best["LON_solar"]
      ) |>
      ungroup() |>
      select(LAT, LON, LAT_solar, LON_solar)
    solar_lookup <- solar_df |>
      select(LAT_solar = LAT, LON_solar = LON, YYYYMMDD, all_of(solar_var))
    met_data |>
      left_join(nearest_map, by = c("LAT", "LON")) |>
      left_join(solar_lookup, by = c("LAT_solar", "LON_solar", "YYYYMMDD")) |>
      select(-LAT_solar, -LON_solar)
  }
  
  # ── Date range validator ───────────────────────────────────────────────────
  NASA_POWER_START <- 1981
  NASA_POWER_END   <- as.integer(format(Sys.Date(), "%Y")) - 1
  
  output$year_range_warning <- renderUI({
    s <- input$year_start; e <- input$year_end
    msgs <- c()
    if (!is.null(s) && s < NASA_POWER_START)
      msgs <- c(msgs, sprintf("'From' year %d is before NASA POWER's earliest available year (%d).", s, NASA_POWER_START))
    if (!is.null(e) && e > NASA_POWER_END)
      msgs <- c(msgs, sprintf("'To' year %d exceeds the last complete year available (%d).", e, NASA_POWER_END))
    if (!is.null(s) && !is.null(e) && s > e)
      msgs <- c(msgs, "'From' year must be less than or equal to 'To' year.")
    if (length(msgs) == 0) return(NULL)
    div(style = "margin-top:10px; padding:10px 14px; background:#fff3cd;
                 border:1px solid #ffc107; border-radius:4px; color:#856404; font-size:13px;",
        icon("triangle-exclamation"), " ",
        paste(msgs, collapse = " "))
  })
  
  # ── Fetch summary badge ────────────────────────────────────────────────────
  output$fetch_summary_badge <- renderUI({
    req(rv$daily_data)
    n_stations <- dplyr::n_distinct(rv$daily_data$station_id)
    n_rows     <- nrow(rv$daily_data)
    div(style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:4px;",
        span(class = "badge-ready", icon("tower-broadcast"), sprintf(" %d station%s", n_stations, if (n_stations == 1) "" else "s")),
        span(class = "badge-ready", icon("calendar-days"),   sprintf(" %s day-rows fetched", formatC(n_rows, format = "d", big.mark = ",")))
    )
  })
  
  # ── Auto-overlay observed when columns/format change ──────────────────────
  observeEvent(list(input$obs_date_col, input$obs_val_col, input$obs_dateformat), {
    req(rv$obs_raw, rv$daily_data, input$viz_var,
        input$obs_date_col, input$obs_val_col, input$obs_dateformat)
    # Silently trigger the same logic as obs_apply by incrementing a counter
    rv$obs_auto_trigger <- (rv$obs_auto_trigger %||% 0L) + 1L
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$fetch_btn, {
    if (is.null(rv$bbox)) {
      showNotification("Please define a bounding box first (Tab 1).", type = "error"); return()
    }
    if (length(selected_vars()) == 0) {
      showNotification("Please select at least one variable.", type = "error"); return()
    }
    if (input$year_start > input$year_end) {
      showNotification("'From' year must be <= 'To' year.", type = "error"); return()
    }
    
    rv$log_lines  <- character(0)
    rv$daily_data <- NULL
    
    yearList <- input$year_start:input$year_end
    lonlat   <- as.numeric(rv$bbox[c("lon_min", "lat_min", "lon_max", "lat_max")])
    sel_vars <- selected_vars()
    
    lon_span <- lonlat[3] - lonlat[1]
    lat_span <- lonlat[4] - lonlat[2]
    if (lon_span < 2 || lat_span < 2) {
      showNotification(
        sprintf("Region too small (%.2f° × %.2f°). Minimum is 2° in each direction.", lon_span, lat_span),
        type = "error", duration = 12)
      log_msg(sprintf("ERROR: Region too small — lon span %.2f°, lat span %.2f°.", lon_span, lat_span))
      return()
    }
    
    log_msg(sprintf("Bounding box: lon [%.4f, %.4f], lat [%.4f, %.4f]",
                    lonlat[1], lonlat[3], lonlat[2], lonlat[4]))
    log_msg(sprintf("Years: %d – %d (%d year(s))", input$year_start, input$year_end, length(yearList)))
    log_msg(paste("Variables:", paste(sel_vars, collapse = ", ")))
    
    # T2M_MAX and T2M_MIN each need their own separate API call per year
    temp_vars   <- intersect(sel_vars, c("T2M_MAX", "T2M_MIN"))
    single_vars <- setdiff(sel_vars, c("T2M_MAX", "T2M_MIN"))
    fetch_temp  <- length(temp_vars) == 2   # only fetch if both are selected
    
    var_lists <- setNames(lapply(sel_vars, function(v) list()), sel_vars)
    
    # Total calls = single vars + 2 temp calls (one each for T2M_MAX, T2M_MIN) if needed
    n_total <- length(yearList) * (length(single_vars) + if (fetch_temp) 2 else 0)
    
    withProgress(message = "Fetching NASA POWER data...", value = 0, {
      for (i in seq_along(yearList)) {
        yr <- yearList[i]
        
        # Single-variable fetches (everything except temperature)
        for (v in single_vars) {
          incProgress(1 / n_total, detail = paste(v, "-", yr))
          log_msg(sprintf("Fetching %s for %d...", v, yr))
          tryCatch({
            var_lists[[v]][[as.character(yr)]] <- get_power(
              community    = "ag",
              lonlat       = lonlat,
              dates        = c(paste0(yr, "-01-01"), paste0(yr, "-12-31")),
              pars         = v,
              temporal_api = "daily"
            )
          }, error = function(e) log_msg(paste("ERROR", v, yr, ":", conditionMessage(e))))
        }
        
        # Fetch T2M_MAX and T2M_MIN in separate calls per year
        # (the NASA POWER API only accepts one parameter at a time)
        if (fetch_temp) {
          for (tv in c("T2M_MAX", "T2M_MIN")) {
            incProgress(1 / n_total, detail = paste(tv, "-", yr))
            log_msg(sprintf("Fetching %s for %d...", tv, yr))
            tryCatch({
              var_lists[[tv]][[as.character(yr)]] <- get_power(
                community    = "ag",
                lonlat       = lonlat,
                dates        = c(paste0(yr, "-01-01"), paste0(yr, "-12-31")),
                pars         = tv,
                temporal_api = "daily"
              )
            }, error = function(e) log_msg(paste("ERROR", tv, yr, ":", conditionMessage(e))))
          }
        }
      }
    })
    
    var_dfs    <- lapply(var_lists, bind_rows)
    empty_vars <- sel_vars[sapply(var_dfs, function(df) nrow(df) == 0)]
    if (length(empty_vars) > 0)
      log_msg(paste("WARNING: No data returned for:", paste(empty_vars, collapse = ", ")))
    if (all(sapply(var_dfs, function(df) nrow(df) == 0))) {
      showNotification("No data returned. Please try a different bounding box.", type = "error", duration = 10)
      log_msg("ERROR: No data returned from NASA POWER.")
      return()
    }
    
    log_msg("Merging tables...")
    valid_dfs <- var_dfs[sapply(var_dfs, function(df) nrow(df) > 0)]
    SOLAR_VAR <- "ALLSKY_SFC_SW_DWN"
    is_solar  <- names(valid_dfs) == SOLAR_VAR
    met_dfs   <- valid_dfs[!is_solar]
    solar_df  <- if (any(is_solar)) valid_dfs[[which(is_solar)[1]]] else NULL
    met_keys  <- c("LAT", "LON", "YYYYMMDD", "YEAR", "MM", "DD", "DOY")
    
    if (length(met_dfs) > 0) {
      daily_data <- Reduce(function(a, b) left_join(a, b, by = met_keys), met_dfs)
      if (!is.null(solar_df)) {
        log_msg("Assigning solar radiation by nearest grid cell...")
        daily_data <- attach_solar_nearest(daily_data, solar_df, SOLAR_VAR)
      }
    } else {
      daily_data <- solar_df
    }
    
    daily_data <- daily_data |>
      group_by(LAT, LON) |> mutate(station_id = cur_group_id()) |> ungroup()
    
    if (!is.null(solar_df) && SOLAR_VAR %in% names(daily_data)) {
      n_na <- sum(is.na(daily_data[[SOLAR_VAR]]))
      if (n_na > 0) log_msg(sprintf("Note: %d solar values still NA.", n_na))
      else log_msg("Solar radiation assigned to all stations with no NA gaps.")
    }
    
    rv$daily_data <- daily_data
    log_msg("Fetch complete. Proceed to Tab 3 to export data.")
    
  })
  
  # Station map — drawn once data is available. It lives inside a
  # conditionalPanel (data_ready == 'yes'), so the container is already visible
  # and properly sized when this renders (no blank-map-in-hidden-div issue).
  output$station_map <- renderLeaflet({
    req(rv$daily_data)
    stations <- rv$daily_data |> dplyr::distinct(LAT, LON, station_id)
    leaflet(stations) |>
      addTiles() |>
      fitBounds(
        lng1 = min(stations$LON), lat1 = min(stations$LAT),
        lng2 = max(stations$LON), lat2 = max(stations$LAT)
      ) |>
      addCircleMarkers(
        lng         = ~LON,
        lat         = ~LAT,
        radius      = 7,
        color       = "#1a6ebd",
        fillColor   = "#4da6ff",
        fillOpacity = 0.9,
        weight      = 1.5,
        label       = ~paste0("Station ", station_id,
                              " (", round(LAT, 3), "°, ",
                              round(LON, 3), "°)")
      )
  })
  
  # ── Export .txt ────────────────────────────────────────────────────────────
  observeEvent(input$txt_btn, {
    if (is.null(rv$daily_data)) {
      showNotification("No data to export. Fetch data first.", type = "error"); return()
    }
    out_base <- normalizePath(trimws(input$out_dir), winslash = "/", mustWork = FALSE)
    if (!validate_out_dir(out_base)) return()
    export_log_msg(paste("Exporting .txt files to:", out_base))
    # Export single-variable files (everything except temperature)
    single_vars <- intersect(selected_vars(), c("RH2M", "WS10M", "ALLSKY_SFC_SW_DWN", "PRECTOTCORR"))
    single_vars <- intersect(single_vars, names(rv$daily_data))
    for (v in single_vars) {
      folder <- file.path(out_base, VAR_FOLDERS[v])
      tryCatch({
        export_station_files(rv$daily_data, v, folder)
        export_log_msg(paste(v, "→", folder))
      }, error = function(e) export_log_msg(paste("ERROR exporting", v, ":", conditionMessage(e))))
    }
    # Temperature: Tmax + Tmin go into one combined file per station
    if (isTRUE(input$var_temp) && all(c("T2M_MAX", "T2M_MIN") %in% names(rv$daily_data))) {
      temp_folder <- file.path(out_base, "Temperature")
      tryCatch({
        export_temp_files(rv$daily_data, temp_folder)
        export_log_msg(paste("Temperature (T2M_MAX, T2M_MIN) →", temp_folder))
      }, error = function(e) export_log_msg(paste("ERROR exporting Temperature:", conditionMessage(e))))
    }
    export_log_msg(".txt export done!")
    showNotification(".txt files exported successfully.", type = "message")
  })
  
  # ── Export CSV ─────────────────────────────────────────────────────────────
  observeEvent(input$csv_btn, {
    if (is.null(rv$daily_data)) {
      showNotification("No data to export. Fetch data first.", type = "error"); return()
    }
    out_base <- normalizePath(trimws(input$out_dir), winslash = "/", mustWork = FALSE)
    if (!validate_out_dir(out_base)) return()
    csv_path <- file.path(out_base, "daily_data.csv")
    tryCatch({
      write.csv(rv$daily_data, file = csv_path, row.names = FALSE)
      export_log_msg(paste("CSV saved:", csv_path))
      showNotification(paste("CSV saved to:", csv_path), type = "message")
    }, error = function(e) export_log_msg(paste("CSV ERROR:", conditionMessage(e))))
  })
  
  # ── Export Lookup Table ────────────────────────────────────────────────────
  observeEvent(input$lookup_btn, {
    if (is.null(rv$daily_data)) {
      showNotification("No data. Fetch data first.", type = "error"); return()
    }
    if (is.null(rv$dem)) {
      showNotification("Please import a DEM first.", type = "error"); return()
    }
    out_base <- normalizePath(trimws(input$out_dir), winslash = "/", mustWork = FALSE)
    if (!validate_out_dir(out_base)) return()
    log_msg("Building lookup table and sampling DEM elevations...")
    tryCatch({
      stations <- rv$daily_data |> distinct(station_id, LAT, LON) |> arrange(station_id)
      pts <- terra::vect(data.frame(lon = stations$LON, lat = stations$LAT),
                         geom = c("lon", "lat"), crs = "EPSG:4326")
      dem_crs  <- terra::crs(rv$dem)
      pts_proj <- if (!is.na(dem_crs) && nchar(dem_crs) > 0)
        tryCatch(terra::project(pts, rv$dem), error = function(e) pts)
      else pts
      samp      <- terra::extract(rv$dem, pts_proj, ID = FALSE)
      elevation <- samp[[1]]
      n_na <- sum(is.na(elevation))
      if (n_na > 0) {
        log_msg(sprintf("WARNING: %d of %d stations outside DEM extent (elevation = NA).",
                        n_na, length(elevation)))
        showNotification(sprintf("%d station(s) outside DEM extent — elevation is NA.", n_na),
                         type = "warning", duration = 10)
      }
      lookup <- data.frame(ID = stations$station_id,
                           NAME = paste0("station_", stations$station_id),
                           LAT = stations$LAT, LONG = stations$LON,
                           ELEVATION = elevation, stringsAsFactors = FALSE)
      lookup_path <- file.path(out_base, "lookup_table.txt")
      write.table(lookup, file = lookup_path, sep = ",",
                  row.names = FALSE, col.names = TRUE, quote = FALSE)
      rv$lookup_table <- lookup
      log_msg(paste("Lookup table saved:", lookup_path))
      showNotification(paste("Lookup table saved to:", lookup_path), type = "message")
    }, error = function(e) {
      log_msg(paste("LOOKUP ERROR:", conditionMessage(e)))
      showNotification(paste("Lookup export failed:", conditionMessage(e)), type = "error", duration = 10)
    })
  })
  
  # ── Reactives used by conditionalPanel visibility checks ──────────────────
  output$data_ready <- reactive({
    if (!is.null(rv$daily_data)) "yes" else "no"
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
  
  output$lookup_ready <- reactive({
    if (!is.null(rv$lookup_table)) "yes" else "no"
  })
  outputOptions(output, "lookup_ready", suspendWhenHidden = FALSE)
  
  # ── Previews ───────────────────────────────────────────────────────────────
  output$preview_table <- renderTable({
    req(rv$daily_data); head(rv$daily_data, 100)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$lookup_preview <- renderTable({
    req(rv$lookup_table); rv$lookup_table
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ── Visualize Timeseries Data ───────────────────────────────────────────────
  
  # Only show variables that are actually present in the fetched data
  viz_available_vars <- reactive({
    req(rv$daily_data)
    intersect(names(VIZ_VAR_LABELS), names(rv$daily_data))
  })
  
  # Refresh the station and variable dropdowns whenever new data comes in
  observeEvent(rv$daily_data, {
    req(rv$daily_data)
    ids <- sort(unique(rv$daily_data$station_id))
    station_choices <- c("All stations" = "__ALL__",
                         setNames(as.character(ids), paste("Station", ids)))
    updateSelectInput(session, "viz_station", choices = station_choices,
                      selected = "__ALL__")
    
    vars <- viz_available_vars()
    var_choices <- setNames(vars, unname(VIZ_VAR_LABELS[vars]))
    updateSelectInput(session, "viz_var", choices = var_choices,
                      selected = if (length(vars)) vars[1] else character(0))
  })
  
  # Axis labels follow the selected variable/station by default. Changing either
  # dropdown refreshes these; the user can still override them in the fields
  # and hit Apply. (Reset to Defaults brings them back here.)
  viz_set_defaults <- function() {
    v <- input$viz_var
    if (is.null(v) || !nzchar(v)) return()
    who <- if (identical(input$viz_station, "__ALL__")) "All Stations"
    else paste("Station", input$viz_station)
    updateTextInput(session, "viz_title",
                    value = paste0(unname(VIZ_VAR_LABELS[v]), " — ", who))
    updateTextInput(session, "viz_ylab", value = unname(VIZ_VAR_UNITS[v]))
  }
  
  observeEvent(input$viz_var,     { viz_set_defaults() }, ignoreInit = TRUE)
  observeEvent(input$viz_station, { viz_set_defaults() }, ignoreInit = TRUE)
  
  # Reset all timeseries plot customization back to defaults
  observeEvent(input$viz_reset, {
    viz_set_defaults()
    updateTextInput(session, "viz_xlab", value = "Date")
    updateSelectInput(session, "viz_datefmt", selected = "auto")
    updateTextInput(session, "viz_color", value = "#0e8a7a")
    updateTextInput(session, "viz_fit_label", value = "NASA POWER")
    updateNumericInput(session, "viz_linewidth", value = 0.7)
    updateCheckboxInput(session, "viz_points", value = FALSE)
    updateCheckboxInput(session, "viz_legend", value = TRUE)
    showNotification("Graph settings reset to defaults.", type = "message")
  })
  
  # Builds the ggplot. Triggered by viz_apply but also reacts to station/variable
  # changes so the preview stays current without needing an explicit Apply click.
  build_viz_plot <- function() {
    req(rv$daily_data, input$viz_var)
    v <- input$viz_var
    if (!v %in% names(rv$daily_data)) return(NULL)
    
    df <- rv$daily_data
    df$DATE <- as.Date(df$YYYYMMDD)
    
    title <- if (!is.null(input$viz_title) && nzchar(input$viz_title)) input$viz_title else NULL
    xlab  <- if (!is.null(input$viz_xlab))  input$viz_xlab  else "Date"
    ylab  <- if (!is.null(input$viz_ylab) && nzchar(input$viz_ylab)) input$viz_ylab else unname(VIZ_VAR_UNITS[v])
    lw    <- if (!is.null(input$viz_linewidth)) input$viz_linewidth else 0.7
    show_pts <- isTRUE(input$viz_points)
    
    col <- input$viz_color %||% "#0e8a7a"
    col <- tryCatch({ grDevices::col2rgb(col); col },
                    error = function(e) "#0e8a7a")  # fall back to default if color is invalid
    
    has_obs <- !is.null(rv$obs_series) && nrow(rv$obs_series) > 0
    # Legend label for the fetched line — falls back to a sensible default if blank
    fit_label <- input$viz_fit_label %||% ""
    if (!nzchar(trimws(fit_label)))
      fit_label <- if (identical(input$viz_station, "__ALL__")) "Fetched (avg)" else "Fetched"
    
    if (identical(input$viz_station, "__ALL__")) {
      # All stations: faint grey line per station, bold colored daily average on top.
      d <- df[, c("DATE", "station_id", v)]
      names(d)[3] <- "value"
      d$value <- clean_missing(d$value)          # -999 / non-finite -> NA
      d$station_id <- factor(d$station_id)
      
      # Daily mean across stations — keep ALL dates so fully-missing days break
      # the line rather than getting bridged over. All-NA days become NA.
      all_dates <- data.frame(DATE = sort(unique(d$DATE)))
      avg_raw <- stats::aggregate(value ~ DATE, data = d, FUN = function(z) {
        z <- z[is.finite(z)]; if (length(z)) mean(z) else NA_real_
      }, na.action = stats::na.pass)
      avg <- merge(all_dates, avg_raw, by = "DATE", all.x = TRUE)
      avg <- avg[order(avg$DATE), , drop = FALSE]
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = d,
          ggplot2::aes(x = DATE, y = value, group = station_id),
          color = "grey75", linewidth = max(lw * 0.6, 0.3), alpha = 0.6,
          na.rm = TRUE
        ) +
        ggplot2::geom_line(
          data = avg,
          ggplot2::aes(x = DATE, y = value, color = fit_label),
          linewidth = max(lw * 1.4, 0.9), na.rm = TRUE
        )
      if (show_pts)
        p <- p + ggplot2::geom_point(data = avg,
                                     ggplot2::aes(x = DATE, y = value, color = fit_label),
                                     size = max(lw * 1.4, 0.9), na.rm = TRUE)
    } else {
      # Single station: just one colored line.
      sid <- suppressWarnings(as.integer(input$viz_station))
      d <- df[df$station_id == sid, c("DATE", v)]
      names(d)[2] <- "value"
      d$value <- clean_missing(d$value)          # -999 / non-finite -> NA
      d <- d[order(d$DATE), , drop = FALSE]
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = d,
          ggplot2::aes(x = DATE, y = value, color = fit_label),
          linewidth = lw, na.rm = TRUE
        )
      if (show_pts)
        p <- p + ggplot2::geom_point(data = d,
                                     ggplot2::aes(x = DATE, y = value, color = fit_label),
                                     size = lw, na.rm = TRUE)
    }
    
    # Observed overlay (optional) — also builds the combined color legend
    if (has_obs) {
      obs_col <- input$obs_color %||% "#d1495b"
      obs_col <- tryCatch({ grDevices::col2rgb(obs_col); obs_col },
                          error = function(e) "#d1495b")
      obs_label <- input$obs_label %||% ""
      if (!nzchar(trimws(obs_label))) obs_label <- "Observed"
      p <- p + ggplot2::geom_line(
        data = rv$obs_series,
        ggplot2::aes(x = DATE, y = OBS, color = obs_label),
        linewidth = max(lw, 0.8)
      )
      p <- p + ggplot2::scale_color_manual(
        name   = NULL,
        values = stats::setNames(c(col, obs_col), c(fit_label, obs_label))
      )
    } else {
      p <- p + ggplot2::scale_color_manual(
        name   = NULL,
        values = stats::setNames(col, fit_label)
      )
    }
    
    # ── X-axis date formatting ──
    # "auto" picks a format based on the time span; explicit choices honor the
    # user's selection. ggplot still decides how many break labels fit the width.
    span_days  <- as.numeric(diff(range(df$DATE, na.rm = TRUE)))
    span_years <- span_days / 365.25
    fmt_choice <- input$viz_datefmt %||% "auto"
    date_fmt <-
      if (fmt_choice == "monthyear") "%b %Y"
    else if (fmt_choice == "year") "%Y"
    else if (fmt_choice == "full") "%Y-%m-%d"
    else {  # auto
      if (span_years <= 2)      "%b %Y"      # short range: Jan 1995
      else if (span_years <= 6) "%b %Y"      # medium: still month-year, thinned
      else                      "%Y"         # long range (20 yrs): year only
    }
    # For year-only labels, snap breaks to year boundaries.
    # Wider intervals for very long ranges keep the labels from crowding.
    year_break <-
      if (span_years <= 12)      "1 year"
    else if (span_years <= 25) "2 years"
    else                       "5 years"
    date_scale <-
      if (identical(date_fmt, "%Y"))
        ggplot2::scale_x_date(date_labels = date_fmt, date_breaks = year_break)
    else
      ggplot2::scale_x_date(date_labels = date_fmt)
    
    # Tilt labels slightly when they're long to avoid overlap
    xtext_angle <- if (identical(date_fmt, "%Y-%m-%d")) 45 else 0
    xtext_hjust <- if (xtext_angle == 0) 0.5 else 1
    
    p <- p +
      date_scale +
      ggplot2::labs(title = title, x = xlab, y = ylab) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title  = ggplot2::element_text(face = "bold", color = "#0b6e61"),
        axis.title  = ggplot2::element_text(color = "#333333"),
        axis.text.x = ggplot2::element_text(angle = xtext_angle, hjust = xtext_hjust)
      )
    
    # Apply legend visibility last so it always wins over any earlier theme() calls
    if (!isTRUE(input$viz_legend)) {
      p <- p + ggplot2::theme(legend.position = "none")
    }
    p
  }
  
  # Reactive plot object — recomputes on Apply or when the selection changes
  viz_plot_obj <- reactive({
    input$viz_apply  # rerun when the user hits Apply for fetched customization
    input$obs_apply  # rerun when observed data is overlaid
    input$obs_clear  # rerun when observed data is removed
    # Also live-update as timeseries style fields change (including axis labels)
    input$viz_title; input$viz_xlab; input$viz_ylab; input$viz_datefmt
    input$viz_color; input$viz_fit_label; input$viz_linewidth
    input$viz_points; input$viz_legend
    build_viz_plot()
  })
  
  output$viz_plot <- renderPlot({
    p <- viz_plot_obj()
    validate(need(!is.null(p), "Select a station and variable to plot."))
    p
  })
  
  # ── Scatter plot: observed vs fetched, with 1:1 line, regression line, and
  #    statistics annotated on the plot. ──────────────────────────────────────
  build_scatter_plot <- function() {
    m  <- rv$obs_merged
    sv <- rv$obs_stat_vals
    if (is.null(m) || nrow(m) < 2 || is.null(sv)) return(NULL)
    
    v       <- input$viz_var
    varname <- if (!is.null(v) && v %in% names(VIZ_VAR_UNITS)) unname(VIZ_VAR_UNITS[v]) else "Value"
    obs_lab <- input$obs_label %||% "Observed"
    if (!nzchar(trimws(obs_lab))) obs_lab <- "Observed"
    fit_lab <- input$viz_fit_label %||% "NASA POWER"
    if (!nzchar(trimws(fit_lab))) fit_lab <- "NASA POWER"
    
    safe_col <- function(x, fallback) {
      x <- x %||% fallback
      tryCatch({ grDevices::col2rgb(x); x }, error = function(e) fallback)
    }
    pt_col  <- safe_col(input$sc_point_col,   "#d1495b")  # scatter point color
    fit_col <- safe_col(input$sc_fit_col,     "#0e8a7a")  # regression line color
    pt_size <- if (!is.null(input$sc_point_size)) input$sc_point_size else 1.8
    show_fit    <- isTRUE(input$sc_show_fit)
    show_legend <- isTRUE(input$sc_show_legend)
    
    # Titles and axis labels: use scatter-specific fields, fall back to sensible defaults
    sc_title <- input$sc_title %||% ""
    if (!nzchar(trimws(sc_title))) sc_title <- paste0("Observed vs Fetched — ", varname)
    sc_x <- input$sc_xlab %||% ""
    if (!nzchar(trimws(sc_x))) sc_x <- paste0(obs_lab, " (observed)")
    sc_y <- input$sc_ylab %||% ""
    if (!nzchar(trimws(sc_y))) sc_y <- paste0(fit_lab, " (fetched)")
    
    xlims <- range(m$OBS, na.rm = TRUE)
    ylims <- range(m$FIT, na.rm = TRUE)
    
    # Stats annotation text
    fmt <- function(x, d = 3) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d))
    label_txt <- paste0(
      "n = ", sv$n, "\n",
      "R\u00b2 = ", fmt(sv$r2), "\n",
      "KGE = ", fmt(sv$kge), "\n",
      "PBIAS = ", fmt(sv$pbias, 2), " %\n",
      "RMSE = ", fmt(sv$rmse)
    )
    label_inline <- paste0(
      "n=", sv$n, "   R\u00b2=", fmt(sv$r2), "   KGE=", fmt(sv$kge),
      "   PBIAS=", fmt(sv$pbias, 2), "%   RMSE=", fmt(sv$rmse)
    )
    
    pos <- input$obs_stat_pos %||% "topleft"
    padx <- diff(xlims) * 0.02
    pady <- diff(ylims) * 0.02
    anchor <- switch(pos,
                     topleft     = list(x = xlims[1] + padx, y = ylims[2] - pady, h = 0, vj = 1),
                     topright    = list(x = xlims[2] - padx, y = ylims[2] - pady, h = 1, vj = 1),
                     bottomleft  = list(x = xlims[1] + padx, y = ylims[1] + pady, h = 0, vj = 0),
                     bottomright = list(x = xlims[2] - padx, y = ylims[1] + pady, h = 1, vj = 0),
                     NULL
    )
    
    # Legend keys for the mapped elements (scatter points and regression line)
    pts_key <- "Data points"
    fit_key <- "Linear fit"
    legend_vals <- stats::setNames(pt_col, pts_key)
    if (show_fit) legend_vals[fit_key] <- fit_col
    
    # Compute the least-squares fit explicitly (FIT ~ OBS) so the line always
    # reflects the true regression over the full dataset, regardless of any
    # coordinate zoom or clipping in the equal-aspect view.
    fit_ok <- FALSE; fit_slope <- NA_real_; fit_int <- NA_real_
    if (show_fit) {
      lm_fit <- tryCatch(stats::lm(FIT ~ OBS, data = m), error = function(e) NULL)
      if (!is.null(lm_fit) && length(stats::coef(lm_fit)) == 2 &&
          all(is.finite(stats::coef(lm_fit)))) {
        fit_int   <- unname(stats::coef(lm_fit)[1])
        fit_slope <- unname(stats::coef(lm_fit)[2])
        fit_ok <- TRUE
      }
    }
    
    p <- ggplot2::ggplot(m, ggplot2::aes(x = OBS, y = FIT)) +
      ggplot2::geom_point(ggplot2::aes(color = pts_key), alpha = 0.6, size = pt_size)
    if (show_fit && fit_ok) {
      # Route color through a named constant so ggplot picks it up for the legend;
      # slope and intercept are passed as fixed values (the actual regression coefficients).
      p <- p + ggplot2::geom_abline(
        data = data.frame(.k = fit_key),
        ggplot2::aes(slope = fit_slope, intercept = fit_int, color = .k),
        linewidth = 0.9, inherit.aes = FALSE
      )
    }
    p <- p + ggplot2::scale_color_manual(name = NULL, values = legend_vals,
                                         breaks = names(legend_vals))
    
    # Stats box pinned to whichever corner the user chose
    if (!is.null(anchor)) {
      p <- p + ggplot2::annotate("label",
                                 x = anchor$x, y = anchor$y, label = label_txt,
                                 hjust = anchor$h, vjust = anchor$vj,
                                 size = 4, label.size = 0, fill = "white", alpha = 0.78)
    }
    
    p <- p +
      ggplot2::coord_cartesian(xlim = xlims, ylim = ylims, expand = TRUE) +
      ggplot2::labs(title = sc_title, x = sc_x, y = sc_y) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", color = "#0b6e61"),
        axis.title = ggplot2::element_text(color = "#333333"),
        legend.position = if (show_legend) "right" else "none"
      )
    
    # Outside placement options (below the plot or to the right)
    if (identical(pos, "below")) {
      p <- p + ggplot2::labs(caption = label_inline) +
        ggplot2::theme(plot.caption = ggplot2::element_text(
          hjust = 0.5, size = 11, color = "#333333", margin = ggplot2::margin(t = 8)))
    } else if (identical(pos, "right")) {
      # Stick the multi-line stats to the right of the panel using ggplot's tag slot
      p <- p + ggplot2::labs(tag = label_txt) +
        ggplot2::theme(
          plot.tag = ggplot2::element_text(size = 11, hjust = 0, vjust = 1,
                                           color = "#333333"),
          plot.tag.position = c(1.02, 0.98),
          plot.margin = ggplot2::margin(5.5, 90, 5.5, 5.5)
        )
    }
    # pos == "none": skip annotation entirely
    
    p
  }
  
  scatter_plot_obj <- reactive({
    # Recompute when data or overlay actions fire...
    input$obs_apply
    input$obs_clear
    input$obs_stat_pos
    input$sc_apply
    # ...and live-update as scatter style fields change so edits show right away.
    input$sc_title; input$sc_xlab; input$sc_ylab
    input$sc_point_col; input$sc_fit_col; input$sc_point_size
    input$sc_show_fit; input$sc_show_legend
    build_scatter_plot()
  })
  
  output$obs_scatter_plot <- renderPlot({
    p <- scatter_plot_obj()
    validate(need(!is.null(p),
                  "Load observed data and click 'Overlay & Compute Stats' to see the scatter plot."))
    p
  })
  
  # Reset scatter plot style back to defaults
  observeEvent(input$sc_reset, {
    updateTextInput(session, "sc_title", value = "")
    updateTextInput(session, "sc_xlab",  value = "")
    updateTextInput(session, "sc_ylab",  value = "")
    updateTextInput(session, "sc_point_col",    value = "#d1495b")
    updateTextInput(session, "sc_fit_col",      value = "#0e8a7a")
    updateNumericInput(session, "sc_point_size", value = 1.8)
    updateCheckboxInput(session, "sc_show_fit",    value = TRUE)
    updateCheckboxInput(session, "sc_show_legend", value = TRUE)
    updateSelectInput(session, "obs_stat_pos", selected = "topleft")
    showNotification("Scatter plot style reset to defaults.", type = "message")
  })
  
  # ── Export the graph ────────────────────────────────────────────────────────
  observeEvent(input$viz_export, {
    if (is.null(rv$daily_data)) {
      showNotification("No data to plot. Fetch data first.", type = "error"); return()
    }
    which_plot <- input$viz_which %||% "timeseries"
    if (identical(which_plot, "scatter")) {
      p <- build_scatter_plot()
      if (is.null(p)) {
        showNotification("No scatter plot yet — overlay observed data and compute stats first.",
                         type = "error"); return()
      }
      plot_tag <- "scatter"
    } else {
      p <- build_viz_plot()
      if (is.null(p)) {
        showNotification("Nothing to export — pick a station and variable.", type = "error"); return()
      }
      plot_tag <- "timeseries"
    }
    out_base <- normalizePath(trimws(input$out_dir), winslash = "/", mustWork = FALSE)
    if (!validate_out_dir(out_base)) return()
    
    fmt   <- input$viz_format %||% "png"
    w     <- input$viz_width  %||% 9
    h     <- input$viz_height %||% 5
    dpi   <- input$viz_dpi    %||% 300
    
    who <- if (identical(input$viz_station, "__ALL__")) "all_stations"
    else paste0("station_", input$viz_station)
    fname <- sprintf("%s_%s_%s_%s.%s", plot_tag, input$viz_var, who,
                     format(Sys.time(), "%Y%m%d_%H%M%S"), fmt)
    fpath <- file.path(out_base, fname)
    
    tryCatch({
      ggplot2::ggsave(filename = fpath, plot = p,
                      width = w, height = h, units = "in", dpi = dpi,
                      device = fmt)
      msg <- sprintf("[%s] Saved %s (%.1f x %.1f in, %d dpi)",
                     format(Sys.time(), "%H:%M:%S"), fpath, w, h, as.integer(dpi))
      rv$viz_export_log <- c(rv$viz_export_log, msg)
      showNotification(paste("Graph saved to:", fpath), type = "message")
    }, error = function(e) {
      rv$viz_export_log <- c(rv$viz_export_log,
                             paste("ERROR:", conditionMessage(e)))
      showNotification(paste("Export failed:", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })
  
  output$viz_export_log <- renderText({
    paste(rv$viz_export_log, collapse = "\n")
  })
  
  # ── Observed data comparison ────────────────────────────────────────────────
  
  obs_log <- function(msg) {
    rv$obs_msg <- c(rv$obs_msg,
                    paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
  }
  
  # Dynamic observed CSV file input — re-rendered on reset so the widget clears properly
  output$obs_file_ui <- renderUI({
    rv$obs_reset
    fileInput("obs_file", "Observed data (CSV)",
              accept = c(".csv", ".txt"),
              buttonLabel = "Browse...",
              placeholder = "CSV with a date and value column")
  })
  
  # Read the uploaded CSV and populate the column pickers with smart defaults
  observeEvent(input$obs_file, {
    req(input$obs_file)
    tryCatch({
      raw <- utils::read.csv(input$obs_file$datapath, stringsAsFactors = FALSE,
                             check.names = TRUE)
      if (ncol(raw) < 2) stop("CSV needs at least two columns (date and value).")
      rv$obs_raw <- raw
      cols <- names(raw)
      # Best-guess defaults: pick a column that looks like a date, and the first numeric one
      date_guess <- cols[which(grepl("date|time|day", cols, ignore.case = TRUE))[1]]
      if (is.na(date_guess)) date_guess <- cols[1]
      num_cols <- cols[sapply(raw, is.numeric)]
      val_guess <- if (length(num_cols)) num_cols[1] else cols[2]
      updateSelectInput(session, "obs_date_col", choices = cols, selected = date_guess)
      updateSelectInput(session, "obs_val_col",  choices = cols, selected = val_guess)
      rv$obs_msg <- character(0)
      obs_log(sprintf("Loaded '%s': %d rows, %d columns.",
                      input$obs_file$name, nrow(raw), ncol(raw)))
      obs_log("Columns detected — overlay will apply automatically. Adjust columns or date format if needed, then click 'Overlay & Compute Stats' to recompute.")
      # Kick off an automatic overlay with the guessed columns
      rv$obs_auto_trigger <- (rv$obs_auto_trigger %||% 0L) + 1L
    }, error = function(e) {
      rv$obs_raw <- NULL
      obs_log(paste("ERROR reading CSV:", conditionMessage(e)))
    })
  })
  
  output$obs_loaded <- reactive({ if (!is.null(rv$obs_raw)) "yes" else "no" })
  outputOptions(output, "obs_loaded", suspendWhenHidden = FALSE)
  
  # Parse the observed date column using whatever format the user selected
  parse_obs_dates <- function(x, fmt) {
    if (is.null(fmt) || !nzchar(fmt)) fmt <- "%Y-%m-%d"
    suppressWarnings(as.Date(as.character(x), format = fmt))
  }
  
  # Builds the observed series and computes fit statistics against the fetched data.
  # Runs when the user clicks the button or automatically when columns/format change.
  obs_compute <- function() {
    req(rv$obs_raw, rv$daily_data, input$viz_var)
    raw <- rv$obs_raw
    dcol <- input$obs_date_col; vcol <- input$obs_val_col
    if (is.null(dcol) || is.null(vcol) || !dcol %in% names(raw) || !vcol %in% names(raw)) {
      obs_log("Please select valid date and value columns."); return()
    }
    obs <- data.frame(DATE = parse_obs_dates(raw[[dcol]], input$obs_dateformat),
                      OBS  = clean_missing(raw[[vcol]]))
    n_obs_total <- nrow(obs)
    n_bad_date  <- sum(is.na(obs$DATE))
    n_obs_na    <- sum(is.na(obs$OBS) & !is.na(obs$DATE))  # values missing on valid dates
    obs <- obs[!is.na(obs$DATE) & !is.na(obs$OBS), , drop = FALSE]
    if (nrow(obs) == 0) {
      obs_log("No valid date/value rows after parsing — check the column and date format choices.")
      rv$obs_series <- NULL; rv$obs_stats <- NULL
      rv$obs_merged <- NULL; rv$obs_stat_vals <- NULL; rv$obs_missing <- NULL; return()
    }
    if (n_bad_date > 0) obs_log(sprintf("Note: %d row(s) had unparseable dates and were dropped.", n_bad_date))
    obs <- obs[!duplicated(obs$DATE), , drop = FALSE]
    rv$obs_series <- obs
    
    # Pull the fetched series we're comparing against (clean out sentinel/invalid values)
    v  <- input$viz_var
    df <- rv$daily_data
    df$DATE <- as.Date(df$YYYYMMDD)
    df[[v]] <- clean_missing(df[[v]])
    if (identical(input$obs_compare_to, "average") ||
        identical(input$viz_station, "__ALL__")) {
      # Use the daily average across stations. Also the sensible fallback when
      # "All stations" is active but the user asked to compare to a single station
      # (there's no individual station to pick from in that view).
      if (identical(input$obs_compare_to, "station") &&
          identical(input$viz_station, "__ALL__")) {
        obs_log("'All stations' is selected, so comparing against the daily average across stations.")
      }
      fitted <- stats::aggregate(stats::as.formula(paste(v, "~ DATE")),
                                 data = df, FUN = function(z) {
                                   z <- z[is.finite(z)]; if (length(z)) mean(z) else NA_real_
                                 }, na.action = stats::na.pass)
      names(fitted)[2] <- "FIT"
    } else {
      sid <- suppressWarnings(as.integer(input$viz_station))
      sub <- df[df$station_id %in% sid, c("DATE", v)]
      names(sub)[2] <- "FIT"
      fitted <- sub
    }
    n_fit_total <- nrow(fitted)
    n_fit_na    <- sum(is.na(fitted$FIT))
    
    merged <- merge(obs, fitted, by = "DATE")
    n_date_overlap <- nrow(merged)
    merged <- merged[stats::complete.cases(merged), , drop = FALSE]
    n_used <- nrow(merged)
    
    # ── Missing-data summary ──
    pct <- function(a, b) if (b > 0) sprintf("%.1f%%", 100 * a / b) else "NA"
    rv$obs_missing <- data.frame(
      Item = c("Observed rows read",
               "  Observed missing values (incl. -999)",
               "  Observed valid values",
               "Fetched days",
               "  Fetched missing values (incl. -999)",
               "  Fetched valid values",
               "Dates shared by both series",
               "Days used for statistics (both present)"),
      Count = c(as.character(n_obs_total),
                sprintf("%d (%s)", n_obs_na, pct(n_obs_na, n_obs_total)),
                sprintf("%d (%s)", n_obs_total - n_obs_na - n_bad_date,
                        pct(n_obs_total - n_obs_na - n_bad_date, n_obs_total)),
                as.character(n_fit_total),
                sprintf("%d (%s)", n_fit_na, pct(n_fit_na, n_fit_total)),
                sprintf("%d (%s)", n_fit_total - n_fit_na, pct(n_fit_total - n_fit_na, n_fit_total)),
                as.character(n_date_overlap),
                as.character(n_used)),
      stringsAsFactors = FALSE
    )
    
    if (n_used < 2) {
      if (n_date_overlap == 0) {
        obs_log("No shared dates between observed and fetched. Check the observed date format and that the observed dates fall within the fetched year range.")
      } else {
        obs_log(sprintf("%d shared date(s), but only %d have a valid value in BOTH series — too few to compute statistics.",
                        n_date_overlap, n_used))
      }
      rv$obs_stats <- NULL
      rv$obs_merged <- NULL; rv$obs_stat_vals <- NULL
      obs_log("Observed series will still overlay on the timeseries plot. See the missing-data summary above.")
      return()
    }
    
    o <- merged$OBS; f <- merged$FIT
    rmse <- sqrt(mean((f - o)^2))
    bias <- mean(f - o)
    mae  <- mean(abs(f - o))
    r    <- suppressWarnings(stats::cor(o, f))
    r2   <- if (is.na(r)) NA_real_ else r^2
    pbias <- 100 * sum(f - o) / sum(o)
    
    # Kling-Gupta Efficiency — well-suited for climatological series — broken down into
    # its three components: correlation (r), bias ratio (beta), variability ratio (gamma).
    # Formula: KGE = 1 - sqrt((r-1)^2 + (beta-1)^2 + (gamma-1)^2)
    mo <- mean(o); mf <- mean(f)
    sdo <- stats::sd(o); sdf <- stats::sd(f)
    beta  <- if (mo != 0) mf / mo else NA_real_                 # bias ratio
    gamma <- if (!is.na(sdo) && sdo != 0 && mo != 0 && mf != 0) # variability ratio (CV-based)
      (sdf / mf) / (sdo / mo) else NA_real_
    kge <- if (any(is.na(c(r, beta, gamma)))) NA_real_
    else 1 - sqrt((r - 1)^2 + (beta - 1)^2 + (gamma - 1)^2)
    
    rv$obs_stats <- data.frame(
      Statistic = c("Overlapping days (n)", "R-squared", "KGE", "Percent bias (%)", "RMSE"),
      Value = c(as.character(nrow(merged)),
                ifelse(is.na(r2),  "NA", sprintf("%.3f", r2)),
                ifelse(is.na(kge), "NA", sprintf("%.3f", kge)),
                sprintf("%.2f", pbias),
                sprintf("%.3f", rmse)),
      stringsAsFactors = FALSE
    )
    # Stash the matched pairs and key stats so the scatter plot can use them
    rv$obs_merged <- merged
    rv$obs_stat_vals <- list(r2 = r2, kge = kge, pbias = pbias, rmse = rmse,
                             n = nrow(merged))
    obs_log(sprintf("Computed statistics over %d overlapping days. See the scatter plot below.",
                    nrow(merged)))
    
  }  # end obs_compute()
  
  observeEvent(input$obs_apply,        { obs_compute() })
  observeEvent(rv$obs_auto_trigger,    { obs_compute() }, ignoreInit = TRUE)
  
  output$obs_stats_ready <- reactive({ if (!is.null(rv$obs_stats)) "yes" else "no" })
  outputOptions(output, "obs_stats_ready", suspendWhenHidden = FALSE)
  
  output$obs_missing_ready <- reactive({ if (!is.null(rv$obs_missing)) "yes" else "no" })
  outputOptions(output, "obs_missing_ready", suspendWhenHidden = FALSE)
  
  output$obs_missing_table <- renderTable({
    req(rv$obs_missing); rv$obs_missing
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  output$obs_status <- renderText({ paste(rv$obs_msg, collapse = "\n") })
  
  # Wipe all observed data and reset the related inputs back to their defaults
  observeEvent(input$obs_clear, {
    rv$obs_raw    <- NULL
    rv$obs_series <- NULL
    rv$obs_stats  <- NULL
    rv$obs_merged <- NULL
    rv$obs_stat_vals <- NULL
    rv$obs_missing <- NULL
    rv$obs_msg    <- character(0)
    rv$obs_reset  <- rv$obs_reset + 1
    updateSelectInput(session, "obs_date_col", choices = character(0))
    updateSelectInput(session, "obs_val_col",  choices = character(0))
    updateTextInput(session, "obs_label", value = "Observed")
    updateTextInput(session, "obs_color", value = "#d1495b")
    updateSelectInput(session, "obs_stat_pos", selected = "topleft")
    showNotification("Observed data removed.", type = "message")
  })
  
  # ── Reset ──────────────────────────────────────────────────────────────────
  observeEvent(input$reset_btn, {
    rv$bbox <- NULL; rv$daily_data <- NULL; rv$log_lines <- character(0)
    rv$export_log <- character(0); rv$dem <- NULL; rv$dem_name <- NULL
    rv$lookup_table <- NULL
    rv$viz_export_log <- character(0)
    rv$obs_raw <- NULL; rv$obs_series <- NULL; rv$obs_stats <- NULL
    rv$obs_merged <- NULL; rv$obs_stat_vals <- NULL; rv$obs_missing <- NULL
    rv$obs_msg <- character(0); rv$obs_reset <- rv$obs_reset + 1
    rv$dem_reset <- rv$dem_reset + 1   # force a fresh empty DEM file input
    updateSelectInput(session, "viz_station", choices = character(0))
    updateSelectInput(session, "viz_var", choices = character(0))
    updateNumericInput(session, "year_start", value = 1995)
    updateNumericInput(session, "year_end",   value = 1996)
    updateTextInput(session, "out_dir", value = getwd())
    updateRadioButtons(session, "bbox_mode", selected = "draw")
    updateNumericInput(session, "man_lon_min", value = 120.44)
    updateNumericInput(session, "man_lon_max", value = 122.48)
    updateNumericInput(session, "man_lat_min", value = 13.37)
    updateNumericInput(session, "man_lat_max", value = 15.39)
    updateCheckboxInput(session, "var_rh",    value = TRUE)
    updateCheckboxInput(session, "var_ws",    value = TRUE)
    updateCheckboxInput(session, "var_solar", value = TRUE)
    updateCheckboxInput(session, "var_prec",  value = TRUE)
    updateCheckboxInput(session, "var_temp",  value = TRUE)
    output$map <- renderLeaflet({ make_map() })
    # The station map lives inside a conditionalPanel tied to data_ready, so
    # clearing rv$daily_data above hides it automatically — nothing else to do.
    showNotification("App reset. Start fresh from the Define Study Area tab.", type = "message")
  })
}

shinyApp(ui, server)
