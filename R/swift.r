#' @title Load Swift data table
#' @description Loads Swift data from text file
#' @author Derek Blue
#' @param filepath required string: Absolute or relative file path to the data file
#' @param skip.rows optional numeric: Number of rows to skip in the data file, defaults to 0
#' @return Light curve data frame with structure: MJD, CR_X, X.ERR, HR, HR.ERR, V.MAG, V.ERR, B.MAG, B.ERR, U.MAG, U.ERR, W1.MAG, W1.ERR, M2.MAG, M2.ERR, W2.MAG, W2.ERR
#' @examples \dontrun{
#' lc <- xmm.pn.lc("obsid_pn_lccor_300-10000.fits")
#' }
#' @importFrom utils read.table
#' @export
swift.table <- function(filepath, skip.rows = 0) {
  raw_lc <- read.table(filepath, header = FALSE, sep = "", skip = skip.rows)
  colnames(raw_lc) <- c("MJD", "CR_X", "X.ERR", "HR", "HR.ERR", "V.MAG", "V.ERR", "B.MAG", "B.ERR", "U.MAG", "U.ERR", "W1.MAG", "W1.ERR", "M2.MAG", "M2.ERR", "W2.MAG", "W2.ERR")
  return(raw_lc)
}
