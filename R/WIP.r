# TODO: Fix this function
#' #' @title Simulate Lagged Light Curves
#' #' @description Simulates two light curve from a given power spectrum with a time lag
#' #' @author Derek Blue
#' #' @param beta Slope of the PSD for the simulated light curve
#' #' @param bins Number of data points for the simulated light curve, defaults to 1024
#' #' @param length Observation length, in seconds, for the simulated light curve, defaults to 100000
#' #' @param time.lag Lag in seconds to apply to the second light curve
#' #' @param freq.range Frequency range in Fourier space to apply the time lag
#' #' @param scale.factor Scaling factor for the simulated light curves, defaults to 1
#' #' @param shift.factor Shift factor for the simulated light curves, defaults to 0
#' #' @return Simulated light curve data frames with structure: TIME, TIMED, RATE
#' #' @examples \dontrun{
#' #' lcdf <- sim.lc.lag(lightcurve, 100, time.lag = 114)
#' #' lc1 <- lcdf[[1]]
#' #' lc2 <- lcdf[[2]]
#' #' }
#' #' @importFrom stats fft rnorm
#' #' @export
#' sim.lc.lag <- function(beta, bins = 1024, length = 100000, time.lag = 0, freq.range = c(1e-4,1e-3), scale.factor = 1, shift.factor = 0) {
#'   bins <- 20*bins
#'   time <- seq(1,2*length*10, length.out = bins)
#'   fourier.frequencies <- seq(1,bins)/length
#'   step.one <- rnorm(bins)
#'   step.two <- (1/fourier.frequencies)^(beta/2.0)
#'   step.three <- step.one*step.two
#'   std.dft <- step.three
#'   lag.dft <- step.three
#'   for (i in 1:length(fourier.frequencies)) {
#'     f <- fourier.frequencies[[i]]
#'     if ((f > freq.range[[1]]) && (f < freq.range[[2]])) {
#'       lag.dft[[i]] <- (2*pi*time.lag*f)
#'     }
#'   }
#'   std.lc <- Re(fft(std.dft, inverse = TRUE))
#'   lag.lc <- Re(fft(lag.dft, inverse = TRUE))
#'   simulated.lcs <- data.frame(TIME = time, TIMED = time, LC1.RATE = std.lc, LC1.ERROR = std.lc, LC2.RATE = lag.lc, LC2.ERROR = lag.lc)
#'   simulated.lcs$LC1.ERROR <- 0.1 * scale.factor
#'   simulated.lcs$LC2.ERROR <- 0.1 * scale.factor
#'   simulated.lcs$TIMED <- (simulated.lcs$TIME[2]-simulated.lcs$TIME[1])/2
#'   simulated.lcs <- subset(simulated.lcs, simulated.lcs$TIME < length)
#'   simulated.lcs$LC1.RATE <- simulated.lcs$LC1.RATE + abs(min(simulated.lcs$LC1.RATE))
#'   simulated.lcs$LC1.RATE <- simulated.lcs$LC1.RATE / max(simulated.lcs$LC1.RATE)
#'   simulated.lcs$LC1.RATE <- simulated.lcs$LC1.RATE * scale.factor
#'   simulated.lcs$LC1.RATE <- simulated.lcs$LC1.RATE + shift.factor
#'   simulated.lcs$LC2.RATE <- simulated.lcs$LC2.RATE + abs(min(simulated.lcs$LC2.RATE))
#'   simulated.lcs$LC2.RATE <- simulated.lcs$LC2.RATE / max(simulated.lcs$LC2.RATE)
#'   simulated.lcs$LC2.RATE <- simulated.lcs$LC2.RATE * scale.factor
#'   simulated.lcs$LC2.RATE <- simulated.lcs$LC2.RATE + shift.factor
#'   return(simulated.lcs)
#' }

# TODO: Fix this function
#' #' @title Fractional Variability (Edelson)
#' #' @description Calculates the fractional variability following Edelson et al. 2002, ApJ, 568, 610
#' #' @author Derek Blue
#' #' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' #' @return List containing fractional variability and error for the light curve
#' #' @examples \dontrun{
#' #' fv <- fvar.edelson(lightcurve)
#' #' fvar <- fv[1]
#' #' fvar.err <- fv[2]
#' #' }
#' #' @importFrom stats var
#' #' @export
#' fvar.edelson <- function(lc) {
#'   rate <- lc$RATE
#'   error <- lc$ERROR
#'   X <- mean(rate)
#'   s2 <- var(rate)
#'   s2err <- mean(error^2)
#'   fvar <- sqrt((s2-s2err)/(X^2))
#'   fvar.err <- ((1/fvar)*sqrt(1/(2*length(rate)))*(s2/(X^2)))
#'   if (is.nan(fvar)) {
#'     print(rate)
#'     print(error)
#'     print(fvar)
#'   }
#'   return(c(fvar,fvar.err))
#'   # return(c((sqrt((var(lc$RATE)-mean(lc$ERROR^2))/(mean(lc$RATE)^2))),
#'   #          ((1/(sqrt((var(lc$RATE)-mean(lc$ERROR^2))/(mean(lc$RATE)^2))))*sqrt(1/(2*length(lc$RATE)))*(var(lc$RATE)/(mean(lc$RATE)^2)))))
#' }

# TODO: Fix this function
#' #' @title Fractional Variability (Vaughan)
#' #' @description Calculates the fractional variability following Vaughan et al. 2003, MNRAS, 345, 1271
#' #' @author Derek Blue
#' #' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' #' @return List containing fractional variability and error for the light curve
#' #' @examples \dontrun{
#' #' fv <- fvar.vaughan(lightcurve)
#' #' fvar <- fv[1]
#' #' fvar.err <- fv[2]
#' #' }
#' #' @importFrom stats var
#' #' @export
#' fvar.vaughan <- function(lc) {
#'   return(c((sqrt((var(lc$RATE)-mean(lc$ERROR^2))/(mean(lc$RATE)^2))),
#'            sqrt((((sqrt(1/(2*length(lc$RATE))))*(mean(lc$ERROR^2)/((mean(lc$RATE)^2)*(sqrt((var(lc$RATE)-mean(lc$ERROR^2))/(mean(lc$RATE)^2))))))^2)+(((sqrt(mean(lc$ERROR^2)/length(lc$RATE)))*(1/mean(lc$RATE)))^2))))
#' }

# TODO: Fix this function
#' #' @title Correct area
#' #' @description From ARF?
#' #' @author Derek Blue
#' #' @param spectrum Absolute or relative file path to the spectrum file
#' #' @param arf.path Absolute or relative file path to the ARF file
#' #' @param energy.bins Number of energy bins
#' #' @return Fluxed spectrum data
#' #' @examples \dontrun{
#' #' lc <- xmm.lc("obsid_pn_lccor_300-10000.fits")
#' #' }
#' correct.area <- function(spectrum, arf.path, energy.bins) {
#'   arf <- xmm.arf(arf.path)
#'   energies <- list()
#'   areas <- list()
#'   for(i in 1:length(arf$SPECRESP)) {
#'     energies[[i]] <- ((arf$ENERGY_LO[[i]]+arf$ENERGY_HI[[i]])/2)
#'     areas[[i]] <- arf$SPECRESP[[i]]
#'   }
#'   area.spline <- lm(areas ~ bs(energies))
#'   return(arf)
#' }

# TODO: Finish this function
#' #' @title Structure Function
#' #' @description Calculates the structure funcction
#' #' @author Derek Blue
#' #' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' #' @return List containing structure function and error for the light curve
#' #' @examples \dontrun{
#' #' sfdf <- structure.function(lightcurve)
#' #' sf <- sfdf[1]
#' #' sf.err <- sfdf[2]
#' #' }
#' #' @importFrom stats var
#' #' @export
#' structure.function <- function(lc) {
#'   duration <- lc$TIME[length(lc$TIME)] - lc$TIME[1]
#'   deltas <- list()
#'   for (i in 2:length(lc$TIME)) {
#'     deltas[i-1] <- lc$TIME[i] - lc$TIME[i-1]
#'   }
#'   resolution <- median(deltas)
#'   bins <- duration/resolution
#'
#' }
