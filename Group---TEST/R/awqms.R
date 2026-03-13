#' @title AWQMS Database Connection Helpers
#' @description
#' Functions for connecting to the Oklahoma AWQMS database via ODBC and
#' querying the results_standard_vw view used by the NRSA Demo application.
#'
#' Credentials are resolved in this order:
#' 1. keyring: keyring::key_get(service, username)
#' 2. Environment variables: AWQMS_UID / AWQMS_PWD
#'
#' @name awqms-helpers
NULL

#' AWQMS connection defaults
#'
#' A named list of configuration values. Edit this list to change server
#' details without touching any other function.
#'
#' @format A named list.
#' @export
AWQMS <- list(
  service            = "awqms_credentials",
  server             = "owrb.gselements.com",
  port               = 1433,
  database           = NULL,
  default_schema     = "ext",
  driver             = "SQL Server",
  default_user       = "oklahomawrb",
  allow_env_fallback = TRUE,
  env_uid_name       = "AWQMS_UID",
  env_pwd_name       = "AWQMS_PWD"
)

.awqms_state <- new.env(parent = emptyenv())

# ---- Credential resolution --------------------------------------------------

resolve_awqms_credentials <- function(
    service            = AWQMS$service,
    user               = AWQMS$default_user,
    allow_env_fallback = AWQMS$allow_env_fallback,
    env_uid_name       = AWQMS$env_uid_name,
    env_pwd_name       = AWQMS$env_pwd_name
) {
  pwd <- tryCatch(
    keyring::key_get(service = service, username = user),
    error = function(e) NULL
  )
  if (!is.null(pwd) && nzchar(pwd))
    return(list(uid = user, pwd = pwd, source = "keyring"))

  if (isTRUE(allow_env_fallback)) {
    uid2 <- Sys.getenv(env_uid_name)
    pwd2 <- Sys.getenv(env_pwd_name)
    if (nzchar(uid2) && nzchar(pwd2))
      return(list(uid = uid2, pwd = pwd2, source = "env"))
  }

  stop(
    "AWQMS credentials unavailable. ",
    "Store them with keyring::key_set() or set the ",
    env_uid_name, " / ", env_pwd_name, " environment variables.",
    call. = FALSE
  )
}

# ---- Connection -------------------------------------------------------------

#' Open a new AWQMS database connection
#'
#' Creates a fresh DBI connection using ODBC. Prefer \code{\link{awqms_get_con}}
#' for normal use as it caches and reuses a single connection per R session.
#'
#' @param server Hostname or IP of the SQL Server instance.
#' @param port TCP port (default 1433).
#' @param database Database name; NULL uses the server default.
#' @param driver ODBC driver name (default "SQL Server").
#' @param trusted_connection Pass "Yes" to use Windows Authentication.
#'
#' @return A DBIConnection object.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- awqms_connect()
#' DBI::dbListTables(con)
#' DBI::dbDisconnect(con)
#' }
awqms_connect <- function(
    server             = AWQMS$server,
    port               = AWQMS$port,
    database           = AWQMS$database,
    driver             = AWQMS$driver,
    trusted_connection = "No"
) {
  creds <- resolve_awqms_credentials()
  DBI::dbConnect(
    odbc::odbc(),
    Driver             = driver,
    Server             = server,
    Port               = port,
    Database           = database,
    UID                = creds$uid,
    PWD                = creds$pwd,
    Trusted_Connection = trusted_connection
  )
}

#' Get or reopen a cached AWQMS connection
#'
#' Returns the cached connection, opening a new one via
#' \code{\link{awqms_connect}} if none exists or the existing one is invalid.
#'
#' @return A DBIConnection object.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- awqms_get_con()
#' }
awqms_get_con <- function() {
  if (!exists("con", envir = .awqms_state) ||
      !DBI::dbIsValid(.awqms_state$con)) {
    .awqms_state$con <- awqms_connect()
  }
  .awqms_state$con
}

#' Close the cached AWQMS connection
#'
#' Disconnects and removes the connection stored by \code{\link{awqms_get_con}}.
#' Safe to call even if no connection is open.
#'
#' @return TRUE invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' awqms_disconnect()
#' }
awqms_disconnect <- function() {
  if (exists("con", envir = .awqms_state) &&
      DBI::dbIsValid(.awqms_state$con)) {
    DBI::dbDisconnect(.awqms_state$con)
  }
  rm(list = "con", envir = .awqms_state)
  invisible(TRUE)
}

# ---- Query helpers ----------------------------------------------------------

awqms_norm <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.character(x)) {
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (length(x) == 0) return(NULL)
  }
  x
}

#' Filter a dbplyr query by project IDs
#'
#' Adds an OR condition across project_id1 through project_idN columns so that
#' any row matching at least one of the supplied project IDs is retained.
#'
#' @param q A dbplyr lazy table or query.
#' @param projects Character vector of project ID values to keep.
#' @param prefix Column name prefix (default "project_id").
#' @param n Number of project-ID columns to check (default 8).
#'
#' @return The filtered lazy query.
#' @export
awqms_filter_projects <- function(q, projects, prefix = "project_id", n = 8) {
  projects <- awqms_norm(projects)
  if (is.null(projects)) return(q)
  conds <- lapply(
    seq_len(n),
    function(i) {
      col <- paste0(prefix, i)
      rlang::expr(!!rlang::sym(col) %in% !!projects)
    }
  )
  dplyr::filter(q, Reduce(function(a, b) rlang::expr((!!a) | (!!b)), conds))
}

#' Reference an AWQMS database table
#'
#' Wraps dplyr::tbl() with automatic schema resolution. Use dot-notation
#' ("schema.table") to override the default schema.
#'
#' @param name Table or view name, optionally schema-qualified ("schema.object").
#' @param default_schema Schema to use when name is unqualified.
#'
#' @return A dbplyr lazy table.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- awqms_tbl("results_standard_vw")
#' }
awqms_tbl <- function(name, default_schema = AWQMS$default_schema) {
  con <- awqms_get_con()
  if (grepl("\\.", name)) {
    parts  <- strsplit(name, "\\.", fixed = FALSE)[[1]]
    schema <- parts[1]
    object <- parts[2]
  } else {
    schema <- default_schema
    object <- name
  }
  dplyr::tbl(con, dbplyr::in_schema(schema, object))
}

#' Load filtered NRSA records from AWQMS
#'
#' Queries results_standard_vw and returns records matching the supplied
#' date range, activity media, monitoring location type, optional protocol,
#' and optional project IDs.
#'
#' @param start_date Start of the date range (Date or coercible character).
#' @param end_date End of the date range.
#' @param columns Optional character vector of column names to select.
#'   NULL returns all columns.
#' @param protocol "NRSA Protocol" filters by standard NRSA
#'   sample_collection_method_id values; any other value skips this filter.
#' @param projects Character vector of project IDs to keep.
#'   NULL skips project filtering.
#' @param activity_media Value to match against activity_media
#'   (default "Habitat").
#' @param monitoring_location_type Value to match against
#'   monitoring_location_type (default "River/Stream").
#' @param project_col_count Number of project_idN columns to search
#'   (default 6).
#' @param collect If TRUE (default), calls dplyr::collect() and returns a
#'   local data frame. Set to FALSE to return the lazy query.
#'
#' @return A tibble (or lazy query if collect = FALSE).
#' @export
#'
#' @examples
#' \dontrun{
#' df <- awqms_get_results_standard_filtered(
#'   start_date = "2022-01-01",
#'   end_date   = "2022-12-31",
#'   protocol   = "NRSA Protocol"
#' )
#' }
awqms_get_results_standard_filtered <- function(
    start_date,
    end_date,
    columns                  = NULL,
    protocol                 = NULL,
    projects                 = NULL,
    activity_media           = "Habitat",
    monitoring_location_type = "River/Stream",
    project_col_count        = 6,
    collect                  = TRUE
) {
  if (length(start_date) != 1 || is.na(start_date) ||
      length(end_date)   != 1 || is.na(end_date)) {
    stop("No date range specified", call. = FALSE)
  }

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  q <- awqms_tbl("results_standard_vw") |>
    dplyr::filter(
      .data$activity_media           == .env$activity_media,
      .data$monitoring_location_type == .env$monitoring_location_type,
      .data$activity_start_date      >= .env$start_date,
      .data$activity_start_date      <= .env$end_date
    )

  if (!is.null(columns))
    q <- dplyr::select(q, dplyr::any_of(columns))

  if (identical(protocol, "NRSA Protocol"))
    q <- dplyr::filter(
      q,
      .data$sample_collection_method_id %in% c(
        "Wadeable", "Boatable", "DRYVISIT",
        "INTWADE", "PARBYBOAT", "PARBYWADE"
      )
    )

  q <- awqms_filter_projects(q, projects,
                              prefix = "project_id",
                              n      = project_col_count)

  if (isTRUE(collect)) dplyr::collect(q) else q
}
