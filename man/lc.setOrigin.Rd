% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lctools.r
\name{lc.setOrigin}
\alias{lc.setOrigin}
\title{Set Origin}
\usage{
lc.setOrigin(lc, origin)
}
\arguments{
\item{lc}{required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE}

\item{origin}{required numeric: Origin time to shift start of the light curve to}
}
\value{
Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
}
\description{
Shifts the light curve in time so that it begins at specified origin
}
\examples{
\dontrun{
lc <- lc.setOrigin(lc, 0)
}
}
\author{
Derek Blue
}
