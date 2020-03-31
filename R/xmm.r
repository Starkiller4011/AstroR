#' @title Load XMM Newton Light Curve
#' @description Loads XMM Newton corrected light curves generated by the XMM SAS epiclccorr task
#' @author Derek Blue
#' @param filepath Absolute or relative file path to the "*lccorr*.fits" file
#' @return Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @examples \dontrun{
#' lc <- xmm.lc("obsid_pn_lccor_300-10000.fits")
#' }
#' @importFrom FITSio readFITS
#' @export
xmm.lc <- function(filepath) {
  raw_lc <- readFITS(filepath)
  lc <- data.frame(raw_lc$col[1],raw_lc$col[1],raw_lc$col[2],raw_lc$col[3],raw_lc$col[5],raw_lc$col[6])
  colnames(lc) <- c("TIME", "TIMED", "RATE", "ERROR", "BACKV", "BACKE")
  lc$TIMED <- (lc$TIME[2]-lc$TIME[1])/2
  return(lc)
}