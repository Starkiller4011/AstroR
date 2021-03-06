% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.r
\name{hr.plot}
\alias{hr.plot}
\title{Plot Hardness Ratio}
\usage{
hr.plot(
  hr.df,
  xlab = "Time [s]",
  ylab = "Ratio",
  plt.title = "Hardness Ratio",
  show.origin = FALSE
)
}
\arguments{
\item{hr.df}{required data frame: Hardness Ratio data frame with structure: TIME, RATIO, ERROR}

\item{xlab}{optional string: X axis label}

\item{ylab}{optional string: Y axis label}

\item{plt.title}{optional string: Plot title}

\item{show.origin}{optional boolean: If TRUE add a horizontal line at y=0, defaults to FALSE}
}
\value{
Plot of the source and background light curve
}
\description{
Creates a plot of the source and background of a light curve
}
\examples{
\dontrun{
plt <- hr.plot(lightcurve)
plot(plt)
}
}
\author{
Derek Blue
}
