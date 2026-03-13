\name{awqms_disconnect}
\alias{awqms_disconnect}
\title{Close the Cached AWQMS Connection}
\usage{
awqms_disconnect()
}
\value{
TRUE invisibly.
}
\description{
Disconnects and removes the connection stored by \code{\link{awqms_get_con}}.
Safe to call even if no connection is open.
}
\examples{
\dontrun{
awqms_disconnect()
}
}
