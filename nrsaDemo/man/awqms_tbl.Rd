\name{awqms_tbl}
\alias{awqms_tbl}
\title{Reference an AWQMS Database Table}
\usage{
awqms_tbl(name, default_schema = AWQMS$default_schema)
}
\arguments{
\item{name}{Table or view name, optionally schema-qualified ("schema.table").}
\item{default_schema}{Schema to use when name is unqualified.}
}
\value{
A dbplyr lazy table.
}
\description{
Wraps dplyr::tbl() with automatic schema resolution via the cached
AWQMS connection.
}
\examples{
\dontrun{
tbl <- awqms_tbl("results_standard_vw")
}
}
