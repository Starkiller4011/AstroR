#' @title Bin Light Curve
#' @description Bins a light curve into evenly spaced time bins
#' @author Derek Blue
#' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param bin.width Time delta width of the time bins, must be in the same units as lc$TIME
#' @return Light curve data frame binned into evenly spaced time bins with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @examples \dontrun{
#' lightcurve <- bin.lc(lightcurve, 100)
#' }
#' @export
bin.lc <- function(lc, bin.width) {
  lc <- subset(lc, is.nan(lc$RATE) == FALSE)
  time <- lc$TIME
  time <- time - lc$TIME[1]
  rate <- lc$RATE
  error <- lc$ERROR
  backv <- lc$BACKV
  backe <- lc$BACKE
  tmp.lc <- data.frame(time, rate, error, backv, backe)
  colnames(tmp.lc) <- c("time", "rate", "error", "backv", "backe")
  new.time <- seq(from = min(time), to = max(time), by = bin.width)
  new.rate <- rep(0, length(new.time))
  new.error <- rep(0, length(new.time))
  new.backv <- rep(0, length(new.time))
  new.backe <- rep(0, length(new.time))
  for (i in 1:length(time)) {
    sub <- subset(tmp.lc, tmp.lc$time >= (new.time[i]-(bin.width/2)))
    sub <- subset(sub, sub$time < (new.time[i]+(bin.width/2)))
    new.rate[i] <- mean(sub$rate)
    new.error[i] <- mean(sub$error^2)
    new.backv[i] <- mean(sub$backv)
    new.backe[i] <- mean(sub$backe^2)
  }
  new.timed <- rep(bin.width/2, length(new.time))
  new.lc <- data.frame(new.time, new.timed, new.error, new.backv, new.backe)
  colnames(new.lc) <- c("TIME", "TIMED", "RATE", "ERROR", "BACKV", "BACKE")
  return(new.lc)
}

#' @title Fractional Variability (Edelson)
#' @description Calculates the fractional variability following Edelson et al. 2002, ApJ, 568, 610
#' @author Derek Blue
#' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @return List containing fractional variability and error for the light curve
#' @examples \dontrun{
#' fv <- fvar.edelson(lightcurve)
#' fvar <- fv[1]
#' fvar.err <- fv[2]
#' }
#' @export
fvar.edelson <- function(lc) {
  return(c((sqrt((stats::var(ls$RATE)-mean(lc$ERROR^2))/(mean(lc$RATE)^2))),
           ((1/(sqrt((stats::var(ls$RATE)-mean(lc$ERROR^2))/(mean(lc$RATE)^2))))*sqrt(1/(2*length(lc$RATE)))*(stats::var(ls$RATE)/(mean(lc$RATE)^2)))))
}

#' @title Fractional Variability (Vaughan)
#' @description Calculates the fractional variability following Vaughan et al. 2003, MNRAS, 345, 1271
#' @author Derek Blue
#' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @return List containing fractional variability and error for the light curve
#' @examples \dontrun{
#' fv <- fvar.vaughan(lightcurve)
#' fvar <- fv[1]
#' fvar.err <- fv[2]
#' }
#' @export
fvar.vaughan <- function(lc) {
  return(c((sqrt((stats::var(lc$RATE)-mean(lc$ERROR^2))/(mean(lc$RATE)^2))),
           sqrt((((sqrt(1/(2*length(lc$RATE))))*(mean(lc$ERROR^2)/((mean(lc$RATE)^2)*(sqrt((stats::var(ls$RATE)-mean(lc$ERROR^2))/(mean(lc$RATE)^2))))))^2)+(((sqrt(mean(lc$ERROR^2)/length(lc$RATE)))*(1/mean(lc$RATE)))^2))))
}


#' @title Fractional Variability (Ponti)
#' @description Calculates the fractional variability following Ponti et al. 2004, A&A, 417, 451
#' @author Derek Blue
#' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @return List containing fractional variability and error for the light curve
#' @examples \dontrun{
#' fv <- fvar.ponti(lightcurve)
#' fvar <- fv[1]
#' fvar.err <- fv[2]
#' }
#' @export
fvar.ponti <- function(lc) {
  return(c(sqrt(sum(((lc$RATE-mean(lc$RATE))^2)/(length(lc$RATE)-1)))/mean(lc$RATE),
           sqrt(sum(((lc$RATE-mean(lc$RATE))*lc$ERROR)^2))/(mean(lc$RATE)*sqrt((length(lc$RATE)-1)*sum((lc$RATE-mean(lc$RATE))^2)))))
}
