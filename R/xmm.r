#' @title Load XMM Newton EPIC PN Light Curve
#' @description Loads XMM Newton EPIC PN corrected light curves generated by the XMM SAS epiclccorr task
#' @author Derek Blue
#' @param filepath Absolute or relative file path to the "*lccorr*.fits" file
#' @return Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @examples \dontrun{
#' lc <- xmm.pn.lc("obsid_pn_lccor_300-10000.fits")
#' }
#' @importFrom FITSio readFITS
#' @export
xmm.pn.lc <- function(filepath) {
  raw_lc <- readFITS(filepath)
  lc <- data.frame(raw_lc$col[1],raw_lc$col[1],raw_lc$col[2],raw_lc$col[3],raw_lc$col[5],raw_lc$col[6])
  colnames(lc) <- c("TIME", "TIMED", "RATE", "ERROR", "BACKV", "BACKE")
  lc$TIMED <- (lc$TIME[2]-lc$TIME[1])/2
  lc <- subset(lc, is.nan(lc$RATE) == FALSE)
  lc <- subset(lc, lc$RATE > 0)
  return(lc)
}

#' @title Load XMM Newton UVW1 Light Curve
#' @description Loads XMM Newton UVW1 light curves
#' @author Derek Blue
#' @param filepath Absolute or relative file path to the "*lccorr*.fits" file
#' @return Light curve data frame with structure: TIME, TIMED, RATE
#' @examples \dontrun{
#' lc <- xmm.uvw1.lc("obsid_uvw1.lc")
#' }
#' @importFrom FITSio readFITS
#' @export
xmm.uvw1.lc <- function(filepath) {
  raw_lc <- readFITS(filepath, fixHdr = 'remove')
  lc <- data.frame(raw_lc$col[1],raw_lc$col[1],raw_lc$col[2],raw_lc$col[3],raw_lc$col[5],raw_lc$col[6])
  colnames(lc) <- c("TIME", "TIMED", "RATE", "ERROR", "BACKV", "BACKE")
  lc$TIMED <- (lc$TIME[2]-lc$TIME[1])/2
  lc <- subset(lc, is.nan(lc$RATE) == FALSE)
  lc <- subset(lc, lc$RATE > 0)
  return(lc)
}

#' @title Load XMM Newton EPIC PN ARF
#' @description Loads XMM Newton EPIC PN ARF file generated by the XMM SAS pipeline
#' @author Derek Blue
#' @param filepath Absolute or relative file path to the "*.arf" file
#' @return ARF data frame with structure: ENERGY_LO ENERGY_HI SPECRESP
#' @examples \dontrun{
#' arf <- xmm.arf("xmm_pn.arf")
#' }
#' @importFrom FITSio readFITS
#' @export
xmm.arf <- function(filepath) {
  raw_data <- readFITS(filepath)
  arf <- data.frame(raw_data$col[1],raw_data$col[2],raw_data$col[3])
  colnames(arf) <- c("ENERGY_LO", "ENERGY_HI", "SPECRESP")
  return(arf)
}

#' @title Load XMM Newton EPIC PN RMF
#' @description Loads XMM Newton EPIC PN RMF file generated by the XMM SAS pipeline
#' @author Derek Blue
#' @param filepath Absolute or relative file path to the "*.rmf" file
#' @return ARF data frame with structure: ENERGY_LO ENERGY_HI SPECRESP
#' @examples \dontrun{
#' rmf <- xmm.rmf("xmm_pn.arf")
#' }
#' @importFrom FITSio readFITS
#' @export
xmm.rmf <- function(filepath) {
  raw_data <- readFITS(filepath)
  rmf <- data.frame(raw_data$col[1],raw_data$col[2],raw_data$col[3])
  colnames(rmf) <- c("ENERGY_LO", "ENERGY_HI", "SPECRESP")
  return(rmf)
}
