#' @title Correct area
#' @description From ARF?
#' @author Derek Blue
#' @param spectrum Absolute or relative file path to the spectrum file
#' @param arf_path Absolute or relative file path to the ARF file
#' @param energy_bins Number of energy bins
#' @return Fluxed spectrum data
#' @examples \dontrun{
#' lc <- xmm.lc("obsid_pn_lccor_300-10000.fits")
#' }
#' @importFrom FITSio readFITS
#' @export
correct.area <- function(spectrum, arf.path, energy.bins) {
  arf <- xmm.arf(arf.path)
  energies <- list()
  areas <- list()
  for(i in 1:length(arf$SPECRESP)) {
    energies[[i]] <- ((arf$ENERGY_LO[[i]]+arf$ENERGY_HI[[i]])/2)
    areas[[i]] <- arf$SPECRESP[[i]]
  }
  area.spline <- lm(areas ~ bs(energies))
  return(arf)
}
