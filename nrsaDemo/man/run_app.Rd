\name{run_app}
\alias{run_app}
\title{Launch the NRSA Demo Shiny Application}
\usage{
run_app(...)
}
\arguments{
\item{...}{Additional arguments forwarded to \code{\link[shiny]{runApp}}.}
}
\value{
Called for its side-effect; does not return a value.
}
\description{
Starts the interactive NRSA field-data review application.
}
\examples{
\dontrun{
run_app()
run_app(port = 4321, launch.browser = FALSE)
}
}
