#' @title Load Suzaku Light Curve
#' @description Loads Suzaku light curves created using xselect, requires both source and background light curves
#' @author Derek Blue
#' @param scpath Absolute or relative file path to the source light curve
#' @param bgpath Absolute or relative file path to the background light curve
#' @return Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @examples \dontrun{
#' lc <- suzaku.lc("obsid_xi03_300-10000_sc.fits", "obsid_xi03_300-10000_bg.fits")
#' }
#' @importFrom FITSio readFITS
#' @export
suzaku.lc <- function(scpath, bgpath) {
  sc_raw <- readFITS(scpath)
  bg_raw <- readFITS(bgpath)
  lc <- data.frame(sc_raw$col[1],sc_raw$col[1],sc_raw$col[2],sc_raw$col[3],bg_raw$col[2],bg_raw$col[3])
  colnames(lc) <- c("TIME", "TIMED", "RATE", "ERROR", "BACKV", "BACKE")
  lc$TIMED <- (lc$TIME[2]-lc$TIME[1])/2
  lc <- subset(lc, is.nan(lc$RATE) == FALSE)
  lc <- subset(lc, lc$RATE > 0)
  return(lc)
}
