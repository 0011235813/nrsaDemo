\name{awqms_get_con}
\alias{awqms_get_con}
\title{Get or Reopen a Cached AWQMS Connection}
\usage{
awqms_get_con()
}
\value{
A DBIConnection object.
}
\description{
Returns the cached connection, opening a new one if none exists or the
existing connection is no longer valid.
}
\examples{
\dontrun{
con <- awqms_get_con()
}
}
