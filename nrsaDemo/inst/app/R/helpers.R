# =============================================================================
# inst/app/R/helpers.R
# Data-processing and UI helper functions for the NRSA Demo Shiny app.
# Sourced by inst/app/app.R at startup.
# =============================================================================

# ---- Column-name constants (kept here so helpers are self-contained) --------

COL_SITE     <- "monitoring_location_id"
COL_DATE     <- "activity_start_date"
COL_TRANSECT <- "sampling_component_name"
COL_PARAM    <- "characteristic_name"
COL_VALUE    <- "result_measure"
COL_GROUP    <- "parent_activity_id"
PROJECT_COLS <- paste0("project_id", 1:6)

NRSA_PROJECT_IDS <- c(
  "FWProb08-12", "FWProb13-17", "FWProb18-22", "FWProb23-27",
  "FWTrend", "FWFire2019", "FWProbAll", "AncillaryHabitat", "NRSA-Data"
)

odbc_columns <- c(
  "activity_media", "monitoring_location_type", "activity_start_date",
  "sample_collection_method_id",
  "project_id1", "project_id2", "project_id3",
  "project_id4", "project_id5", "project_id6",
  "activity_id", "sampling_component_name",
  "monitoring_location_id", "monitoring_location_name",
  "monitoring_location_latitude", "monitoring_location_longitude",
  "result_measure", "characteristic_name", "analytical_method_id",
  "result_measure_unit", "method_speciation", "result_status"
)

# ---- Data helpers -----------------------------------------------------------

add_project_col <- function(data) {
  present <- intersect(PROJECT_COLS, names(data))
  if (length(present) == 0) { data$project_id <- NA_character_; return(data) }
  data$project_id <- apply(data[, present, drop = FALSE], 1, function(r) {
    v <- r[!is.na(r) & nchar(trimws(r)) > 0]
    if (length(v)) v[[1]] else NA_character_
  })
  data
}

coerce_dates <- function(data) {
  if (COL_DATE %in% names(data) && !inherits(data[[COL_DATE]], "Date"))
    data[[COL_DATE]] <- as.Date(data[[COL_DATE]])
  data
}

coerce_numeric <- function(data) {
  if (COL_VALUE %in% names(data)) {
    v <- data[[COL_VALUE]]
    if (is.list(v)) v <- unlist(v)
    if (!is.numeric(v))
      v <- suppressWarnings(as.numeric(gsub("^[<>]=?\\s*", "", as.character(v))))
    data[[COL_VALUE]] <- v
  }
  data
}

compute_transect_summary <- function(data) {
  expected <- paste(LETTERS[1:11], collapse = "-")
  data %>%
    dplyr::filter(!is.na(.data[[COL_TRANSECT]]) & .data[[COL_TRANSECT]] != "") %>%
    dplyr::group_by(.data[[COL_GROUP]]) %>%
    dplyr::summarize(
      transect_summary = paste(sort(unique(.data[[COL_TRANSECT]])), collapse = "-"),
      .groups = "drop"
    ) %>%
    dplyr::rename(group_id = 1) %>%
    dplyr::mutate(
      transect_complete = ifelse(
        transect_summary == expected,
        "<span style='color:#2e7d32;font-weight:700;'>&#10004;</span>",
        "<span style='color:#c62828;font-weight:700;'>&#10006;</span>"
      )
    )
}

read_uploaded_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls"))
    readxl::read_excel(path, col_types = "text")
  else
    stop(sprintf("Unsupported file type: .%s — please upload .xlsx or .xls files only", ext))
}

to_snake_case <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

harmonize_to_odbc_schema <- function(df) {
  names(df) <- to_snake_case(names(df))
  synonym_map <- c(
    sampling_component_quadrat = "sampling_component_name",
    result_value               = "result_measure",
    activity_latitude          = "activity_latitude",
    activity_longitude         = "activity_longitude"
  )
  for (src in names(synonym_map)) {
    dest <- synonym_map[[src]]
    if (src %in% names(df) && !(dest %in% names(df)))
      names(df)[names(df) == src] <- dest
  }
  if (!("monitoring_location_latitude" %in% names(df)) && "activity_latitude" %in% names(df))
    df$monitoring_location_latitude <- df$activity_latitude
  if (!("monitoring_location_longitude" %in% names(df)) && "activity_longitude" %in% names(df))
    df$monitoring_location_longitude <- df$activity_longitude
  for (col in setdiff(odbc_columns, names(df)))
    df[[col]] <- NA
  df
}

normalize_core_types <- function(df) {
  if ("activity_start_date" %in% names(df)) {
    raw <- df$activity_start_date
    parsed <- suppressWarnings({
      out <- as.Date(raw, format = "%Y-%m-%d")
      na_idx <- is.na(out) & !is.na(raw) & nchar(trimws(raw)) > 0
      if (any(na_idx)) out[na_idx] <- as.Date(raw[na_idx], format = "%m/%d/%Y")
      na_idx <- is.na(out) & !is.na(raw) & nchar(trimws(raw)) > 0
      if (any(na_idx)) out[na_idx] <- as.Date(raw[na_idx], format = "%d-%b-%Y")
      na_idx <- is.na(out) & !is.na(raw) & nchar(trimws(raw)) > 0
      if (any(na_idx)) {
        serial <- suppressWarnings(as.numeric(raw[na_idx]))
        valid  <- !is.na(serial)
        if (any(valid))
          out[na_idx][valid] <- as.Date(serial[valid] - 1, origin = "1899-12-30")
      }
      out
    })
    failed_mask <- is.na(parsed) & !is.na(raw) & nchar(trimws(raw)) > 0
    if (any(failed_mask))
      attr(df, "date_parse_failures") <- list(
        n       = sum(failed_mask),
        samples = head(unique(raw[failed_mask]), 10)
      )
    df$activity_start_date <- parsed
  }
  for (col in c("monitoring_location_latitude", "monitoring_location_longitude"))
    if (col %in% names(df))
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  df
}

safe_awqms_connect <- function() {
  tryCatch(awqms_get_con(), error = function(e) {
    shiny::showNotification(paste("ODBC connection failed:", conditionMessage(e)),
                            type = "error", duration = 10)
    NULL
  })
}

safe_awqms_disconnect <- function(con) {
  if (!is.null(con)) suppressWarnings(try(awqms_disconnect(), silent = TRUE))
}

safe_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NULL)
  if (is.character(x) && nchar(trimws(x)) == 0) return(NULL)
  d <- suppressWarnings(as.Date(x))
  if (is.na(d)) NULL else d
}

# ---- UI helpers -------------------------------------------------------------

#' Filter panel widget for use inside modals
#'
#' @param id_suffix     String appended to every input ID so multiple instances
#'   can coexist (e.g. `"_upload"`, `"_odbc"`).
#' @param include_dates If `FALSE`, the Start/End date inputs are omitted
#'   (used in the AWQMS modal where dates are captured separately).
filter_panel_ui <- function(id_suffix = "", include_dates = TRUE) {
  shiny::div(
    style = "background-color:#f5f5f5;padding:15px;border-radius:6px;margin-top:10px;",
    shiny::tags$strong(shiny::icon("filter"), " Filter Options"),
    shiny::br(), shiny::br(),
    if (include_dates) shiny::fluidRow(
      shiny::column(6, shiny::dateInput(paste0("start_date", id_suffix), "Start Date", value = NULL)),
      shiny::column(6, shiny::dateInput(paste0("end_date",   id_suffix), "End Date",   value = NULL))
    ),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectInput(paste0("protocol", id_suffix), "Protocol",
                                       choices  = c("NRSA Protocol", "State Protocol"),
                                       selected = "NRSA Protocol")),
      shiny::column(6,
                    shinyWidgets::pickerInput(
                      paste0("project", id_suffix), "Project",
                      choices  = sort(NRSA_PROJECT_IDS),
                      selected = sort(NRSA_PROJECT_IDS),
                      multiple = TRUE,
                      options  = list(`actions-box` = TRUE,
                                      `live-search` = TRUE,
                                      `selected-text-format` = "count > 3")
                    ))
    )
  )
}
