\name{awqms_connect}
\alias{awqms_connect}
\title{Open a New AWQMS Database Connection}
\usage{
awqms_connect(
  server = AWQMS$server,
  port = AWQMS$port,
  database = AWQMS$database,
  driver = AWQMS$driver,
  trusted_connection = "No"
)
}
\arguments{
\item{server}{Hostname or IP of the SQL Server instance.}
\item{port}{TCP port (default 1433).}
\item{database}{Database name; NULL uses the server default.}
\item{driver}{ODBC driver name (default "SQL Server").}
\item{trusted_connection}{Pass "Yes" to use Windows Authentication.}
}
\value{
A DBIConnection object.
}
\description{
Creates a fresh DBI connection to the AWQMS SQL Server database using ODBC.
}
\examples{
\dontrun{
con <- awqms_connect()
DBI::dbDisconnect(con)
}
}
