# =============================================================================
# NRSA DEMO APPLICATION — inst/app/app.R
# Runs via nrsaDemo::run_app()
# AWQMS helpers are provided by the nrsaDemo package (R/awqms.R).
# Data-processing helpers live alongside this file in R/helpers.R.
# =============================================================================

library(shiny)
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(DT)
library(leaflet)
library(shinyjs)
library(shinydashboard)
library(readr)
library(readxl)
library(nrsaDemo)          # awqms_* functions

# Source app-local helpers (data processing, UI pieces).
# system.file() works whether the app is run via run_app() or directly with
# shiny::runApp("inst/app").
source(
  system.file("app", "R", "helpers.R", package = "nrsaDemo",
              mustWork = FALSE) |>
    (\(p) if (nzchar(p)) p else file.path("R", "helpers.R"))(),
  local = TRUE
)

options(shiny.maxRequestSize = 5 * 1024^3)

# -----------------------------------------------------------------------------
# COLUMN NAME CONSTANTS
# -----------------------------------------------------------------------------
COL_SITE     <- "monitoring_location_id"
COL_DATE     <- "activity_start_date"
COL_LAT      <- "monitoring_location_latitude"
COL_LON      <- "monitoring_location_longitude"
COL_TRANSECT <- "sampling_component_name"
COL_PARAM    <- "characteristic_name"
COL_VALUE    <- "result_measure"
COL_GROUP    <- "parent_activity_id"
COL_LOC_NAME <- "monitoring_location_name"
PROJECT_COLS <- paste0("project_id", 1:6)

NRSA_PROJECT_IDS <- c(
  "FWProb08-12", "FWProb13-17", "FWProb18-22", "FWProb23-27",
  "FWTrend", "FWFire2019", "FWProbAll", "AncillaryHabitat", "NRSA-Data"
)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = "NRSA Demo",
    tags$li(
      class = "dropdown",
      actionButton("show_help", icon("question-circle"), class = "btn-header",
                   style = "margin-top:8px;margin-right:10px;", title = "Help")
    )
  ),

  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "tabs",
      menuItem("Summary",                        tabName = "summary", icon = icon("table")),
      menuItem("Site Map",                       tabName = "map",     icon = icon("map")),
      menuItem("Add Files / Switch Data Source", tabName = "source",  icon = icon("database"))
    )
  ),

  dashboardBody(
    useShinyjs(),

    tags$head(tags$style(HTML("
      .btn-header { background-color:transparent; border:none; color:white; font-size:18px; }
      .btn-header:hover { background-color:rgba(255,255,255,0.1); }
      .content-wrapper { padding-bottom:50px; }
    "))),

    tags$script(HTML("
      $(function(){
        $(document).on('keydown', function(e){
          if((e.ctrlKey||e.metaKey) && e.keyCode===72){ e.preventDefault(); $('#show_help').click(); }
        });
      });
    ")),

    tabItems(

      # ── Summary ──────────────────────────────────────────────────────────────
      tabItem(tabName = "summary",
              uiOutput("connection_status"),
              uiOutput("data_summary_box"),
              uiOutput("download_buttons"),
              br(),
              DTOutput("summary_table", width = "100%")
      ),

      # ── Map ───────────────────────────────────────────────────────────────────
      tabItem(tabName = "map",
              div(style = "margin-bottom:8px;",
                  actionButton("refresh_map",
                               label = tagList(icon("compress-arrows-alt"), " Reset View"),
                               class = "btn-sm btn-default")),
              leafletOutput("site_map", height = 580)
      ),

      tabItem(tabName = "source")  # handled entirely via modal
    ),

    uiOutput("status_bar")
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Reactive state
  # ---------------------------------------------------------------------------
  source_mode      <- reactiveVal(NULL)
  source_data      <- reactiveVal(NULL)
  loaded_dates     <- reactiveVal(list(start = NULL, end = NULL))
  pending_combined <- reactiveVal(NULL)
  current_tab      <- reactiveVal("summary")
  con_obj          <- NULL

  active_filters <- reactiveValues(
    start_date = NULL,
    end_date   = NULL,
    protocol   = "NRSA Protocol",
    project    = sort(NRSA_PROJECT_IDS)
  )

  apply_modal_filters <- function(id_suffix = "", start_override = NULL, end_override = NULL) {
    s <- safe_date(if (!is.null(start_override)) start_override else input[[paste0("start_date", id_suffix)]])
    e <- safe_date(if (!is.null(end_override))   end_override   else input[[paste0("end_date",   id_suffix)]])
    p <- input[[paste0("protocol", id_suffix)]]
    j <- input[[paste0("project",  id_suffix)]]
    active_filters$start_date <- s
    active_filters$end_date   <- e
    active_filters$protocol   <- p %||% "NRSA Protocol"
    active_filters$project    <- if (length(j) > 0) j else sort(NRSA_PROJECT_IDS)
  }

  # ---------------------------------------------------------------------------
  # Startup modal
  # ---------------------------------------------------------------------------
  observe({
    showModal(modalDialog(
      title     = tags$h3("Welcome to the NRSA Demo Application",
                          style = "margin-top:0;text-align:center;"),
      size      = "m",
      easyClose = FALSE,
      footer    = NULL,

      div(style = "text-align:center;margin-bottom:20px;",
          p("Please select how you would like to load your NRSA data:",
            style = "font-size:16px;color:#666;")),

      fluidRow(
        column(6,
               actionButton("startup_upload",
                            label = div(
                              icon("upload", style = "font-size:48px;"),
                              br(), br(),
                              tags$strong("Upload Files"),
                              br(),
                              tags$small("Excel (.xlsx / .xls)", style = "color:#888;")
                            ),
                            width = "100%", style = "height:160px;font-size:16px;")),
        column(6,
               actionButton("startup_odbc",
                            label = div(
                              icon("database", style = "font-size:48px;"),
                              br(), br(),
                              tags$strong("Connect to AWQMS"),
                              br(),
                              tags$small("ODBC connection", style = "color:#888;")
                            ),
                            width = "100%", style = "height:160px;font-size:16px;"))
      )
    ))
  })

  # ---------------------------------------------------------------------------
  # Upload modal
  # ---------------------------------------------------------------------------
  show_upload_modal <- function() {
    showModal(modalDialog(
      title  = "Upload NRSA Data Files", size = "l",
      fileInput("upload_files", "Select Excel files (.xlsx or .xls)",
                multiple = TRUE, accept = c(".xlsx", ".xls")),
      p("Files will be combined into a single dataset.",
        style = "color:#666;font-style:italic;"),
      uiOutput("upload_status"),
      hr(),
      filter_panel_ui("_upload"),
      footer = tagList(
        actionButton("confirm_upload", "Load Data", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  }

  observeEvent(input$startup_upload, { removeModal(); show_upload_modal() })
  observeEvent(input$choose_upload,  { removeModal(); show_upload_modal() })

  # ---------------------------------------------------------------------------
  # AWQMS / ODBC modal
  # ---------------------------------------------------------------------------
  show_odbc_modal <- function() {
    showModal(modalDialog(
      title     = "Connect to AWQMS Database",
      size      = "m",
      easyClose = FALSE,
      p("Select a date range and filters, then connect to AWQMS.", style = "color:#666;"),
      p(icon("info-circle"), " Ensure you have network access and valid credentials.",
        style = "color:#1976d2;margin-bottom:10px;"),
      div(style = "background-color:#f0f4ff;padding:12px;border-radius:6px;margin-bottom:10px;",
          tags$strong(icon("calendar"), " Date range used to fetch data from AWQMS (required):"),
          br(), br(),
          fluidRow(
            column(6, dateInput("odbc_start_date", "Start Date", value = NULL)),
            column(6, dateInput("odbc_end_date",   "End Date",   value = NULL))
          ),
          p(icon("info-circle"),
            " A narrower range loads faster. You can re-load with a different range later.",
            style = "color:#666;font-size:12px;margin-top:5px;margin-bottom:0;")
      ),
      uiOutput("odbc_date_warning"),
      filter_panel_ui("_odbc", include_dates = FALSE),
      footer = tagList(
        actionButton("confirm_odbc", "Connect & Load", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  }

  observeEvent(input$startup_odbc, { removeModal(); show_odbc_modal() })
  observeEvent(input$choose_odbc,  { removeModal(); show_odbc_modal() })

  # ---------------------------------------------------------------------------
  # Help modal
  # ---------------------------------------------------------------------------
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "NRSA Demo Application Help", size = "l",
      div(style = "max-height:500px;overflow-y:auto;",
          h4("Getting Started"),
          tags$ul(
            tags$li("Use 'Add Files / Switch Data Source' to load data"),
            tags$li("Date range, protocol, and project filters are set inside the data-source modal"),
            tags$li("Click a row in the Summary table to highlight that site on the map")
          ),
          hr(),
          h4("Tabs"),
          tags$ul(
            tags$li(tags$strong("Summary"), " — site-visit overview with transect completeness"),
            tags$li(tags$strong("Site Map"), " — geographic view of all filtered sites")
          ),
          hr(),
          h4("Keyboard Shortcuts"),
          tags$ul(
            tags$li("Ctrl/Cmd + H — Help")
          )
      ),
      footer = modalButton("Close")
    ))
  })

  # ---------------------------------------------------------------------------
  # Tab tracking + Source modal (sidebar re-entry)
  # ---------------------------------------------------------------------------
  observeEvent(input$tabs, {
    req(input$tabs)
    if (input$tabs != "source") current_tab(input$tabs)

    if (input$tabs == "source") {
      updateTabItems(session, "tabs", current_tab())
      showModal(modalDialog(
        title = "Choose Data Source", size = "m", easyClose = FALSE,
        p("Select how you want to load NRSA data:", style = "margin-bottom:20px;"),
        fluidRow(
          column(6, actionButton("choose_upload",
                                 label = div(icon("upload", style = "font-size:48px;"),
                                             br(), br(), "Upload Files"),
                                 width = "100%", style = "height:150px;font-size:18px;")),
          column(6, actionButton("choose_odbc",
                                 label = div(icon("database", style = "font-size:48px;"),
                                             br(), br(), "Connect to AWQMS"),
                                 width = "100%", style = "height:150px;font-size:18px;"))
        ),
        footer = tagList(modalButton("Cancel"))
      ))
    }
  })

  # ---------------------------------------------------------------------------
  # Upload status preview
  # ---------------------------------------------------------------------------
  output$upload_status <- renderUI({
    req(input$upload_files)
    div(style = "margin-top:15px;padding:10px;background-color:#e3f2fd;border-radius:4px;",
        tags$strong("Files ready:"),
        tags$ul(lapply(input$upload_files$name, tags$li)))
  })

  # ---------------------------------------------------------------------------
  # Confirm Upload
  # ---------------------------------------------------------------------------
  observeEvent(input$confirm_upload, {
    req(input$upload_files)
    withProgress(message = "Loading files...", value = 0, {
      fl            <- input$upload_files
      out           <- list()
      date_failures <- list()
      for (i in seq_len(nrow(fl))) {
        incProgress(1/nrow(fl), detail = fl$name[i])
        tryCatch({
          df <- read_uploaded_file(fl$datapath[i], fl$name[i])
          df <- harmonize_to_odbc_schema(df)
          df <- normalize_core_types(df)
          fails <- attr(df, "date_parse_failures")
          if (!is.null(fails))
            date_failures[[length(date_failures) + 1]] <- c(list(file = fl$name[i]), fails)
          out[[length(out) + 1]] <- df
        }, error = function(e)
          showNotification(paste("Error reading", fl$name[i], ":", e$message),
                           type = "error", duration = 10))
      }
      if (length(out) > 0) {
        combined <- bind_rows(out)
        if ("activity_id" %in% names(combined))
          combined$parent_activity_id <- sub(":.*$", "", combined$activity_id)
        if ("activity_media" %in% names(combined))
          combined <- combined %>% filter(activity_media == "Habitat")
        if ("monitoring_location_type" %in% names(combined))
          combined <- combined %>% filter(monitoring_location_type == "River/Stream")
        combined <- combined %>% add_project_col() %>% coerce_dates() %>% coerce_numeric()

        if (nrow(combined) == 0) {
          showNotification(
            "No Habitat/River/Stream records found. Check activity_media and monitoring_location_type columns.",
            type = "warning", duration = 10)
          return()
        }
        if (COL_DATE %in% names(combined) && any(!is.na(combined[[COL_DATE]]))) {
          dr <- range(combined[[COL_DATE]], na.rm = TRUE)
          loaded_dates(list(start = dr[1], end = dr[2]))
        }

        apply_modal_filters("_upload")

        if ("project_id" %in% names(combined)) {
          projects <- sort(unique(combined$project_id[!is.na(combined$project_id)]))
          active_filters$project <- projects
        }

        if (length(date_failures) > 0) {
          total_failed <- sum(sapply(date_failures, `[[`, "n"))
          detail_lines <- lapply(date_failures, function(f) {
            paste0(f$file, ": ", f$n, " record(s) — e.g. ", paste(f$samples, collapse = ", "))
          })
          removeModal()
          showModal(modalDialog(
            title = tags$h4(icon("exclamation-triangle", style = "color:#c62828;"),
                            " Date Parse Warning", style = "margin-top:0;"),
            size  = "m",
            div(style = "background:#fff3cd;padding:12px;border-radius:4px;margin-bottom:12px;",
                tags$strong(paste0(total_failed, " record(s) have unrecognised date values. ",
                                   "Their activity_start_date has been set to NA.")),
                br(), br(),
                "These records are still loaded but will be EXCLUDED from date-filtered views.",
                br(), br(),
                tags$strong("Affected files:"),
                tags$ul(lapply(detail_lines, tags$li))
            ),
            p("Please verify that ", tags$code("activity_start_date"), " uses a recognised format ",
              "such as ", tags$code("YYYY-MM-DD"), " or ", tags$code("M/D/YYYY"),
              " before re-uploading."),
            footer = tagList(
              actionButton("dismiss_date_warning", "Continue Anyway", class = "btn-warning"),
              actionButton("abort_date_warning",   "Cancel Load",     class = "btn-danger")
            )
          ))
          pending_combined(combined)
          return()
        }

        source_data(combined)
        source_mode("upload")
        removeModal()
        updateTabItems(session, "tabs", current_tab())
        showNotification(
          paste0("Loaded ", format(nrow(combined), big.mark = ","),
                 " records from ", nrow(fl), " file(s)"),
          type = "message", duration = 5)
      } else {
        showNotification("No valid files loaded — check file format",
                         type = "error", duration = 8)
      }
    })
  })

  observeEvent(input$dismiss_date_warning, {
    d <- pending_combined(); req(d)
    source_data(d); source_mode("upload"); pending_combined(NULL)
    removeModal(); updateTabItems(session, "tabs", current_tab())
    showNotification(
      paste0("Loaded ", format(nrow(d), big.mark = ","),
             " records (some dates set to NA — see warning)"),
      type = "warning", duration = 8)
  })

  observeEvent(input$abort_date_warning, {
    pending_combined(NULL); removeModal()
    showNotification("Load cancelled. Please fix the date format and re-upload.",
                     type = "error", duration = 8)
  })

  # ---------------------------------------------------------------------------
  # ODBC date warning
  # ---------------------------------------------------------------------------
  output$odbc_date_warning <- renderUI({
    tryCatch({
      s <- input$odbc_start_date; e <- input$odbc_end_date
      if (is.null(s) || is.null(e) || length(s) == 0 || length(e) == 0) return(NULL)
      s <- suppressWarnings(as.Date(s)); e <- suppressWarnings(as.Date(e))
      if (is.na(s) || is.na(e)) return(NULL)
      if (s > e)
        div(style = "padding:10px;background-color:#f8d7da;border-radius:4px;margin-bottom:8px;",
            icon("times-circle"), " Start date must be before end date.")
    }, error = function(e) NULL)
  })

  # ---------------------------------------------------------------------------
  # Confirm ODBC
  # ---------------------------------------------------------------------------
  observeEvent(input$confirm_odbc, {
    start <- as.Date(input$odbc_start_date)
    end   <- as.Date(input$odbc_end_date)
    if (isTRUE(start > end)) {
      showNotification("Start date must be before end date.", type = "warning", duration = 6)
      return()
    }
    withProgress(message = "Connecting to AWQMS...", value = 0.2, {
      con_obj <<- safe_awqms_connect()
      if (is.null(con_obj)) return()
      incProgress(0.3, detail = "Fetching NRSA records...")
      tryCatch({
        raw <- tbl(con_obj, "results_standard_vw") %>%
          filter(
            activity_media           == "Habitat",
            monitoring_location_type == "River/Stream",
            (project_id1 %in% NRSA_PROJECT_IDS | project_id2 %in% NRSA_PROJECT_IDS |
               project_id3 %in% NRSA_PROJECT_IDS | project_id4 %in% NRSA_PROJECT_IDS |
               project_id5 %in% NRSA_PROJECT_IDS | project_id6 %in% NRSA_PROJECT_IDS),
            activity_start_date >= as.Date(start),
            activity_start_date <= as.Date(end)
          ) %>% collect()
        incProgress(0.4, detail = "Processing...")
        data <- raw %>%
          mutate(parent_activity_id = sub(":.*$", "", activity_id)) %>%
          add_project_col() %>% coerce_dates() %>% coerce_numeric()
        if (nrow(data) == 0) {
          showNotification(
            paste0("No NRSA records found between ", start, " and ", end, ". Try a wider date range."),
            type = "warning", duration = 15)
          safe_awqms_disconnect(con_obj); con_obj <<- NULL; return()
        }
        loaded_dates(list(start = start, end = end))
        apply_modal_filters("_odbc", start_override = start, end_override = end)

        if ("project_id" %in% names(data)) {
          projects <- sort(unique(data$project_id[!is.na(data$project_id)]))
          active_filters$project <- projects
        }

        source_data(data); source_mode("odbc")
        removeModal(); updateTabItems(session, "tabs", current_tab())
        showNotification(
          paste0("Loaded ", format(nrow(data), big.mark = ","),
                 " NRSA records (", start, " to ", end, ")"),
          type = "message", duration = 6)
      }, error = function(e) {
        showNotification(paste("Error fetching data:", e$message), type = "error", duration = 10)
        safe_awqms_disconnect(con_obj); con_obj <<- NULL
      })
    })
  })

  # ---------------------------------------------------------------------------
  # Filtered data
  # ---------------------------------------------------------------------------
  filtered_data <- reactive({
    req(source_data())
    d  <- source_data()
    ld <- loaded_dates()

    s_date <- active_filters$start_date %||% safe_date(ld$start)
    e_date <- active_filters$end_date   %||% safe_date(ld$end)

    if (!is.null(s_date) && COL_DATE %in% names(d))
      d <- d %>% filter(.data[[COL_DATE]] >= s_date)
    if (!is.null(e_date) && COL_DATE %in% names(d))
      d <- d %>% filter(.data[[COL_DATE]] <= e_date)
    if ("project_id" %in% names(d) && length(active_filters$project) > 0)
      d <- d %>% filter(project_id %in% active_filters$project)
    d
  })

  # ---------------------------------------------------------------------------
  # Connection status banner
  # ---------------------------------------------------------------------------
  output$connection_status <- renderUI({
    mode <- source_mode()
    if (is.null(mode))
      div(style = "padding:10px;background-color:#fff3cd;border-radius:4px;margin-bottom:15px;",
          icon("exclamation-triangle"), " No data loaded. Use ",
          tags$strong("Add Files / Switch Data Source"), " to begin.")
    else if (mode == "upload")
      div(style = "padding:10px;background-color:#d4edda;border-radius:4px;margin-bottom:15px;",
          icon("check-circle"), " Uploaded data — ",
          tags$strong(format(nrow(source_data()), big.mark = ",")), " records.")
    else
      div(style = "padding:10px;background-color:#d1ecf1;border-radius:4px;margin-bottom:15px;",
          icon("database"), " AWQMS (NRSA filter) — ",
          tags$strong(format(nrow(source_data()), big.mark = ",")), " records.")
  })

  # ---------------------------------------------------------------------------
  # Dataset summary box
  # ---------------------------------------------------------------------------
  output$data_summary_box <- renderUI({
    req(filtered_data())
    d        <- filtered_data()
    n_visits <- n_distinct(d[[COL_GROUP]], na.rm = TRUE)
    n_sites  <- n_distinct(d[[COL_SITE]],  na.rm = TRUE)
    n_params <- if (COL_PARAM %in% names(d)) n_distinct(d[[COL_PARAM]], na.rm = TRUE) else NA
    dr       <- if (COL_DATE %in% names(d) && any(!is.na(d[[COL_DATE]])))
      range(d[[COL_DATE]], na.rm = TRUE) else c(NA, NA)

    div(style = "background-color:#f5f5f5;padding:15px;border-radius:4px;margin-bottom:20px;",
        h4("Dataset Summary", style = "margin-top:0;"),
        fluidRow(
          column(3, div(style = "text-align:center;",
                        h2(style = "color:#1976d2;margin:5px;", format(nrow(d), big.mark = ",")),
                        p("Total Records", style = "margin:0;color:#666;"))),
          column(3, div(style = "text-align:center;",
                        h2(style = "color:#388e3c;margin:5px;", n_sites),
                        p("Sites", style = "margin:0;color:#666;"))),
          column(3, div(style = "text-align:center;",
                        h2(style = "color:#7b1fa2;margin:5px;", n_visits),
                        p("Site Visits", style = "margin:0;color:#666;"))),
          column(3, div(style = "text-align:center;",
                        h2(style = "color:#d32f2f;margin:5px;",
                           ifelse(is.na(n_params), "—", n_params)),
                        p("Parameters", style = "margin:0;color:#666;")))
        ),
        if (!any(is.na(dr)))
          p(style = "margin-top:15px;text-align:center;color:#666;",
            icon("calendar"), " Date Range: ",
            tags$strong(format(dr[1], "%Y-%m-%d")), " to ",
            tags$strong(format(dr[2], "%Y-%m-%d")))
    )
  })

  # ---------------------------------------------------------------------------
  # Download buttons
  # ---------------------------------------------------------------------------
  output$download_buttons <- renderUI({
    req(filtered_data())
    div(style = "margin-bottom:20px;",
        downloadButton("export_csv",     "Export Filtered Data (CSV)", class = "btn-primary",
                       style = "margin-right:10px;"),
        downloadButton("export_summary", "Export Summary (CSV)",       class = "btn-info"))
  })

  output$export_csv <- downloadHandler(
    filename = function() paste0("NRSA_Filtered_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(filtered_data(), file)
  )

  output$export_summary <- downloadHandler(
    filename = function() paste0("NRSA_Summary_", Sys.Date(), ".csv"),
    content  = function(file) {
      d <- filtered_data()
      d %>%
        group_by(.data[[COL_GROUP]], .data[[COL_SITE]]) %>%
        summarize(
          first_date   = min(.data[[COL_DATE]], na.rm = TRUE),
          last_date    = max(.data[[COL_DATE]], na.rm = TRUE),
          n_records    = n(),
          n_parameters = n_distinct(.data[[COL_PARAM]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        write_csv(file)
    }
  )

  # ---------------------------------------------------------------------------
  # Summary table
  # ---------------------------------------------------------------------------
  output$summary_table <- renderDT({
    req(filtered_data())
    d <- filtered_data()

    summary_df <- d %>%
      group_by(.data[[COL_GROUP]]) %>%
      summarize(
        Site    = first(.data[[COL_SITE]]),
        Date    = first(.data[[COL_DATE]]),
        Project = if ("project_id" %in% names(d)) first(project_id) else NA_character_,
        Records = n(),
        .groups = "drop"
      ) %>%
      rename(`Visit Group` = 1)

    if (all(c(COL_TRANSECT, COL_GROUP) %in% names(d))) {
      ts <- compute_transect_summary(d)
      summary_df <- summary_df %>%
        left_join(ts, by = c("Visit Group" = "group_id")) %>%
        mutate(
          Transects = coalesce(as.character(transect_summary), "None"),
          Complete  = coalesce(as.character(transect_complete), "")
        ) %>%
        select(-transect_summary, -transect_complete)
    }

    datatable(summary_df, selection = "single", escape = FALSE, rownames = FALSE,
              extensions = "Buttons",
              options = list(pageLength = 25, scrollX = TRUE,
                             dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  }, server = FALSE)

  # ---------------------------------------------------------------------------
  # Status bar
  # ---------------------------------------------------------------------------
  output$status_bar <- renderUI({
    if (is.null(source_data())) return(NULL)
    d        <- source_data()
    filtered <- tryCatch(filtered_data(), error = function(e) d)
    n_sites  <- if (!is.null(filtered)) n_distinct(filtered[[COL_SITE]], na.rm = TRUE) else "—"
    div(style = paste0("position:fixed;bottom:0;left:0;right:0;",
                       "background-color:#2c3e50;color:white;",
                       "padding:7px 20px;z-index:1000;font-size:13px;"),
        fluidRow(
          column(4, icon("database"), " Total: ",  format(nrow(d), big.mark = ",")),
          column(4, icon("filter"),   " Filtered: ",
                 if (!is.null(filtered)) format(nrow(filtered), big.mark = ",") else "—"),
          column(4, icon("map-marker-alt"), " Sites: ", n_sites)
        )
    )
  })

  # ---------------------------------------------------------------------------
  # Map helpers
  # ---------------------------------------------------------------------------
  make_popup <- function(site_id, loc, last_dt, n_vis, n_rec, n_par, miss_pct, n_trans) {
    paste0(
      "<div style='font-size:14px;min-width:200px;'>",
      "<strong style='font-size:15px;color:#1976d2;'>", loc, "</strong><br>",
      "<span style='color:#888;font-size:12px;'>", site_id, "</span>",
      "<hr style='margin:6px 0;'>",
      "<strong>Last Visit:</strong> ", last_dt, "<br>",
      "<strong>Total Visits:</strong> ", n_vis, "<br>",
      "<strong>Records:</strong> ", format(n_rec, big.mark = ","), "<br>",
      "<strong>Parameters:</strong> ", n_par, "<br>",
      "<strong>Missing Values:</strong> ", miss_pct, "%<br>",
      "<strong>Transects:</strong> ", n_trans, " / 11",
      "</div>"
    )
  }

  build_coords <- function(d) {
    d %>%
      filter(!is.na(.data[[COL_LAT]]), !is.na(.data[[COL_LON]])) %>%
      group_by(.data[[COL_SITE]], .data[[COL_LAT]], .data[[COL_LON]]) %>%
      summarize(
        loc_name    = if (COL_LOC_NAME %in% names(d)) first(.data[[COL_LOC_NAME]]) else first(.data[[COL_SITE]]),
        last_visit  = { dv <- .data[[COL_DATE]][!is.na(.data[[COL_DATE]])]; if (length(dv) == 0) "Unknown" else format(max(dv), "%Y-%m-%d") },
        n_visits    = n_distinct(.data[[COL_GROUP]], na.rm = TRUE),
        n_records   = n(),
        parameters  = n_distinct(.data[[COL_PARAM]], na.rm = TRUE),
        missing_pct = round(sum(is.na(.data[[COL_VALUE]])) / n() * 100, 1),
        n_transects = n_distinct(.data[[COL_TRANSECT]][!is.na(.data[[COL_TRANSECT]])], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(site = 1, lat = 2, lon = 3) %>%
      mutate(popup = as.character(mapply(
        make_popup, site, loc_name, last_visit,
        n_visits, n_records, parameters, missing_pct, n_transects,
        SIMPLIFY = TRUE
      )))
  }

  # ---------------------------------------------------------------------------
  # Site Map
  # ---------------------------------------------------------------------------
  output$site_map <- renderLeaflet({
    d <- tryCatch(filtered_data(), error = function(e) NULL)
    base <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)

    if (is.null(d) || nrow(d) == 0 || !all(c(COL_LAT, COL_LON, COL_SITE) %in% names(d)))
      return(base %>% setView(lng = -97, lat = 36, zoom = 6))

    coords <- tryCatch(build_coords(d), error = function(e) NULL)
    if (is.null(coords) || nrow(coords) == 0)
      return(base %>% setView(lng = -97, lat = 36, zoom = 6))

    sel_row  <- input$summary_table_rows_selected
    sel_site <- NULL
    if (!is.null(sel_row) && length(sel_row) > 0) {
      group_ids <- d %>% group_by(.data[[COL_GROUP]]) %>% summarize(.groups = "drop") %>% pull(1)
      if (sel_row <= length(group_ids)) {
        sel_id   <- group_ids[sel_row]
        site_row <- d %>%
          filter(.data[[COL_GROUP]] == sel_id,
                 !is.na(.data[[COL_LAT]]), !is.na(.data[[COL_LON]])) %>%
          slice(1)
        if (nrow(site_row) > 0) sel_site <- site_row[[COL_SITE]]
      }
    }

    grey <- if (!is.null(sel_site)) coords[coords$site != sel_site, ] else coords
    m    <- base

    if (nrow(grey) > 0)
      m <- m %>% addCircleMarkers(
        data = grey, lng = ~lon, lat = ~lat, layerId = ~site,
        radius = 6, color = "#555555", fillColor = "#888888",
        fillOpacity = 0.5, stroke = TRUE, weight = 1,
        label = ~paste0(loc_name, " — ", site),
        labelOptions = labelOptions(direction = "auto", sticky = FALSE),
        popup = ~popup
      )

    if (!is.null(sel_site)) {
      sc <- coords[coords$site == sel_site, ]
      if (nrow(sc) > 0)
        m <- m %>% addCircleMarkers(
          data = sc, lng = ~lon, lat = ~lat, layerId = ~paste0("sel_", site),
          radius = 14, color = "#006400", fillColor = "#00cc00",
          fillOpacity = 0.85, stroke = TRUE, weight = 3,
          label = ~paste0(loc_name, " (selected)"),
          labelOptions = labelOptions(direction = "auto", sticky = FALSE),
          popup = ~popup
        )
      m <- m %>% setView(
        lng  = coords$lon[coords$site == sel_site][1],
        lat  = coords$lat[coords$site == sel_site][1],
        zoom = 10
      )
    } else {
      m <- m %>% fitBounds(
        min(coords$lon) - 0.1, min(coords$lat) - 0.1,
        max(coords$lon) + 0.1, max(coords$lat) + 0.1
      )
    }
    m
  })

  observeEvent(input$refresh_map, {
    d <- tryCatch(filtered_data(), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) return()
    coords <- tryCatch(build_coords(d), error = function(e) NULL)
    if (is.null(coords) || nrow(coords) == 0) return()
    leafletProxy("site_map") %>%
      fitBounds(
        min(coords$lon) - 0.1, min(coords$lat) - 0.1,
        max(coords$lon) + 0.1, max(coords$lat) + 0.1
      )
    showNotification("View reset", type = "message", duration = 2)
  })

  observeEvent(input$site_map_marker_click, {
    req(filtered_data())
    clicked_site <- sub("^sel_", "", input$site_map_marker_click$id)
    d <- filtered_data()
    group_ids <- d %>%
      group_by(.data[[COL_GROUP]]) %>%
      summarize(site = first(.data[[COL_SITE]]), .groups = "drop")
    match_row <- which(group_ids$site == clicked_site)
    if (length(match_row) > 0)
      dataTableProxy("summary_table") %>% selectRows(match_row[1])
  })

  # ---------------------------------------------------------------------------
  # Session cleanup
  # ---------------------------------------------------------------------------
  session$onSessionEnded(function() {
    if (!is.null(con_obj)) safe_awqms_disconnect(con_obj)
  })

} # end server

shinyApp(ui = ui, server = server)
