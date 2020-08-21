#' @title Structure Function
#' @description Calculates the structure funcction
#' @author Derek Blue
#' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @return List containing structure function and error for the light curve
#' @examples \dontrun{
#' sfdf <- structure.function(lightcurve)
#' sf <- sfdf[1]
#' sf.err <- sfdf[2]
#' }
#' @importFrom stats var
#' @export
structure.function <- function(lc) {
  duration <- lc$TIME[length(lc$TIME)] - lc$TIME[1]
  deltas <- list()
  for (i in 2:length(lc$TIME)) {
    deltas[i-1] <- lc$TIME[i] - lc$TIME[i-1]
  }
  resolution <- median(deltas)
  bins <- duration/resolution

}
