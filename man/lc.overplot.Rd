% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.r
\name{lc.overplot}
\alias{lc.overplot}
\title{Overplot Light Curves}
\usage{
lc.overplot(
  lc1,
  lc2,
  xlab = "Time [s]",
  y1lab = "Rate [count/s]",
  y2lab = "Rate [count/s]",
  plt.title = "LC1 vs LC2",
  bg1 = FALSE,
  bg2 = FALSE,
  names = c("lc1", "lc2")
)
}
\arguments{
\item{lc1}{required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE}

\item{lc2}{required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE}

\item{xlab}{optional string: X axis label}

\item{y1lab}{optional string: Left Y axis label}

\item{y2lab}{optional string: Right Y axis label}

\item{plt.title}{optional string: Plot title}

\item{bg1}{optional boolean: If TRUE plot the background of the first light curve, defaults to FALSE}

\item{bg2}{optional boolean: If TRUE plot the background of the second light curve, defaults to FALSE}

\item{names}{optional list of strings: Names for the two light curves}
}
\value{
Plot of the source and background light curve
}
\description{
Creates a plot of the source and background of two light curves
}
\examples{
\dontrun{
plt <- lc.plot(lightcurve)
plot(plt)
}
}
\author{
Derek Blue
}
