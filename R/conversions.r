#' @title Swift UVW2 Magnitude to Count Rate
#' @description Converts the Swift UVW2 Magnitude to count rate
#' @author Derek Blue
#' @param mag required numeric: Swift UVW2 magnitude
#' @return Swift UVW2 count rate
#' @examples \dontrun{
#' swift.uvw2$RATE <- swift.w2mag2cr(swift.uvw2$MAG)
#' }
#' @export
swift.w2mag2cr <- function(mag) {
  return(10^((13.77-mag)/2.512)*2.29)
}

#' @title Swift B Magnitude to Count Rate
#' @description Converts the Swift B Magnitude to count rate
#' @author Derek Blue
#' @param mag required numeric: Swift B magnitude
#' @return Swift B count rate
#' @examples \dontrun{
#' swift.b$RATE <- swift.bmag2cr(swift.b$MAG)
#' }
#' @export
swift.bmag2cr <- function(mag) {
  return(10^((14.88-mag)/2.512)*4.52)
}
