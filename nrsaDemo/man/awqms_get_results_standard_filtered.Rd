\name{awqms_get_results_standard_filtered}
\alias{awqms_get_results_standard_filtered}
\title{Load Filtered NRSA Records from AWQMS}
\usage{
awqms_get_results_standard_filtered(
  start_date,
  end_date,
  columns = NULL,
  protocol = NULL,
  projects = NULL,
  activity_media = "Habitat",
  monitoring_location_type = "River/Stream",
  project_col_count = 6,
  collect = TRUE
)
}
\arguments{
\item{start_date}{Start of the date range (Date or coercible character).}
\item{end_date}{End of the date range.}
\item{columns}{Optional character vector of column names to select.
NULL returns all columns.}
\item{protocol}{"NRSA Protocol" filters by standard NRSA method IDs;
any other value skips this filter.}
\item{projects}{Character vector of project IDs to keep.
NULL skips project filtering.}
\item{activity_media}{Value to match against activity_media
(default "Habitat").}
\item{monitoring_location_type}{Value to match against
monitoring_location_type (default "River/Stream").}
\item{project_col_count}{Number of project_idN columns to search
(default 6).}
\item{collect}{If TRUE (default), returns a local data frame.
Set to FALSE to return the lazy query.}
}
\value{
A tibble (or lazy query if collect = FALSE).
}
\description{
Queries results_standard_vw and returns records matching the supplied
date range, activity media, monitoring location type, optional protocol,
and optional project IDs.
}
\examples{
\dontrun{
df <- awqms_get_results_standard_filtered(
  start_date = "2022-01-01",
  end_date   = "2022-12-31",
  protocol   = "NRSA Protocol"
)
}
}
