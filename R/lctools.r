#' @title Bin Light Curve
#' @description Bins a light curve into evenly spaced time bins
#' @author Derek Blue
#' @param lc required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param bin.width required numeric: Time delta width of the time bins, must be in the same units as lc$TIME
#' @return Light curve data frame binned into evenly spaced time bins with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @examples \dontrun{
#' lightcurve <- bin.lc(lightcurve, 100)
#' }
#' @export
bin.lc <- function(lc, bin.width) {
  lc <- subset(lc, is.nan(lc$RATE) == FALSE)
  nbins <- floor((lc$TIME[[length(lc$TIME)]]-lc$TIME[[1]])/bin.width)
  new.time <- seq(from = (bin.width/2), to = ((bin.width*nbins)-(bin.width/2)), by = bin.width)
  new.time <- new.time + lc$TIME[1]
  new.timed <- rep((bin.width/2), length(new.time))
  new.rate <- rep(0, length(new.time))
  new.error <- rep(0, length(new.time))
  new.backv <- rep(0, length(new.time))
  new.backe <- rep(0, length(new.time))
  for (i in seq(from=1,to=length(new.time),by=1)) {
    sub <- lc[which(lc$TIME >= (new.time[i]-(bin.width/2))),]
    sub <- sub[which(sub$TIME < (new.time[i]+(bin.width/2))),]
    new.rate[i] <- mean(sub$RATE)
    new.error[i] <- sqrt(sum(sub$ERROR))/length(sub$ERROR)
    new.backv[i] <- mean(sub$BACKV)
    new.backe[i] <- sqrt(sum(sub$BACKE))/length(sub$BACKE)
  }
  new.lc <- data.frame(new.time, new.timed, new.rate, new.error, new.backv, new.backe)
  colnames(new.lc) <- c("TIME", "TIMED", "RATE", "ERROR", "BACKV", "BACKE")
  return(new.lc)
}

#' @title Normalize Light Curve
#' @description Normalizes a light curve based on its mean count rate
#' @author Derek Blue
#' @param lc required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @return Light curve data frame normalized by it mean with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @examples \dontrun{
#' lc <- xmm.pn.lc(fits_file_path)
#' lc <- normalize.lc(lc)
#' }
#' @importFrom stats fft rnorm
#' @export
normalize.lc <- function(lc) {
  lc.mean <- mean(lc$RATE)
  lc.mean.err <- mean(lc$ERROR)
  lc$RATE <- (lc$RATE / lc.mean)
  lc$ERROR <- (lc$ERROR / lc.mean)
  return(lc)
}

#' @title Simulate Light Curve
#' @description Simulates a light curve from a given power spectrum
#' @author Derek Blue
#' @param beta required numeric: Slope of the PSD for the simulated light curve
#' @param bins optional numeric: Number of data points for the simulated light curve, defaults to 1024
#' @param length optional numeric: Observation length, in seconds, for the simulated light curve, defaults to 100000
#' @param scale.factor optional numeric: Scaling factor for the simulated light curve, defaults to 1
#' @param shift.factor optional numeric: Shift factor for the simulated light curve, defaults to 0
#' @return Simulated light curve data frame with structure: TIME, TIMED, RATE
#' @examples \dontrun{
#' lightcurve <- sim.lc(lightcurve, 100)
#' }
#' @importFrom stats fft rnorm
#' @export
sim.lc <- function(beta, bins = 1024, length = 100000, scale.factor = 1, shift.factor = 0) {
  bins <- 2*bins
  time <- seq(1,2*length, length.out = bins)
  fourier.frequencies <- seq(1,bins)/length
  step.one <- rnorm(bins)
  step.two <- (1/fourier.frequencies)^(beta/2.0)
  step.three <- step.one*step.two
  step.four <- fft(step.three, inverse = TRUE)
  step.five <- Re(step.four)
  simulated.lc <- data.frame(TIME = time, TIMED = time, RATE = step.five, ERROR = step.five)
  simulated.lc$ERROR <- 0.1 * scale.factor
  simulated.lc$TIMED <- (simulated.lc$TIME[2]-simulated.lc$TIME[1])/2
  simulated.lc <- subset(simulated.lc, simulated.lc$TIME < length)
  simulated.lc$RATE <- simulated.lc$RATE + abs(min(simulated.lc$RATE))
  simulated.lc$RATE <- simulated.lc$RATE / max(simulated.lc$RATE)
  simulated.lc$RATE <- simulated.lc$RATE * scale.factor
  simulated.lc$RATE <- simulated.lc$RATE + shift.factor
  return(simulated.lc)
}

#' @title Hardness Ratio
#' @description Calculates the hardness ratio
#' @author Derek Blue
#' @param slc required data frame: 0.3 - 1 keV light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param hlc required data frame: 2 - 10 keV light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @return Hardness ratio data frame with structure: TIME, TIMED, RATIO, ERROR
#' @examples \dontrun{
#' hrat <- bin.lc(soft.lc, hard.lc)
#' }
#' @export
hard.ratio <- function(slc, hlc) {
  time <- hlc$TIME
  timed <- hlc$TIMED
  ratio <- (hlc$RATE-slc$RATE)/(hlc$RATE+slc$RATE)
  error <- sqrt(((((2*slc$RATE)/((hlc$RATE+slc$RATE)^2))*hlc$ERROR)^2)+((((-2*hlc$RATE)/((hlc$RATE+slc$RATE)^2))*slc$ERROR)^2))
  hr.df <- data.frame(time, timed, ratio, error)
  colnames(hr.df) <- c("TIME", "TIMED", "RATIO", "ERROR")
  return(hr.df)
}

#' @title Set Origin
#' @description Shifts the light curve in time so that it begins at specified origin
#' @author Derek Blue
#' @param lc required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param origin required numeric: Origin time to shift start of the light curve to
#' @return Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @examples \dontrun{
#' lc <- lc.setOrigin(lc, 0)
#' }
#' @export
lc.setOrigin <- function(lc, origin) {
  diff <- origin - lc$TIME[1]
  lc$TIME <- lc$TIME + diff
  return(lc)
}

#' @title Prep Flux Flux
#' @description Creates a flux flux data frame from the hard and soft band light curves
#' @author Derek Blue
#' @param slc required data frame: 0.3 - 1 keV light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param hlc required data frame: 2 - 10 keV light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @return Flux flux data frame with structre: SOFT.RATE, SOFT.ERROR, HARD.RATE, HARD.ERROR
#' @examples \dontrun{
#' ff.df <- prep.ff(soft.lc, hard.lc)
#' }
#' @export
prep.ff <- function(slc, hlc) {
  sr <- slc$RATE
  se <- slc$ERROR
  hr <- hlc$RATE
  he <- hlc$ERROR
  ff.df <- data.frame(sr, se, hr, he)
  colnames(ff.df) <- c("SOFT.RATE", "SOFT.ERROR", "HARD.RATE", "HARD.ERROR")
  return(ff.df)
}

#' @title Fractional Variability (Ponti)
#' @description Calculates the fractional variability following Ponti et al. 2004, A&A, 417, 451
#' @author Derek Blue
#' @param lc required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @return List containing fractional variability and error for the light curve
#' @examples \dontrun{
#' fv <- fvar.ponti(lightcurve)
#' fvar <- fv[1]
#' fvar.err <- fv[2]
#' }
#' @importFrom stats var
#' @export
fvar.ponti <- function(lc) {
  return(c(sqrt(sum(((lc$RATE-mean(lc$RATE))^2)/(length(lc$RATE)-1)))/mean(lc$RATE),
           sqrt(sum(((lc$RATE-mean(lc$RATE))*lc$ERROR)^2))/(mean(lc$RATE)*sqrt((length(lc$RATE)-1)*sum((lc$RATE-mean(lc$RATE))^2)))))
}
