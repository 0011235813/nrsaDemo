#' Launch the NRSA Demo Shiny Application
#'
#' Starts the interactive NRSA field-data review app. The app lets users load
#' habitat data either by uploading Excel files or by connecting directly to an
#' AWQMS database via ODBC.
#'
#' @param ... Additional arguments forwarded to \code{\link[shiny]{runApp}},
#'   e.g. port, launch.browser, host.
#'
#' @return Called for its side-effect; does not return a value.
#' @export
#'
#' @examples
#' \dontrun{
#' nrsaDemo::run_app()
#' nrsaDemo::run_app(port = 4321, launch.browser = FALSE)
#' }
run_app <- function(...) {
  app_dir <- system.file("app", package = "nrsaDemo")
  if (!nzchar(app_dir) || !dir.exists(app_dir))
    stop("Could not find the app directory. Try re-installing nrsaDemo.",
         call. = FALSE)
  shiny::runApp(app_dir, ...)
}
