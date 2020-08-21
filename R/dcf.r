#' @title Correct area
#' @description Runs a discrete correlation function on the input light curves following Edelson & Krolik 1988, using https://github.com/svdataman/sour
#' @author Derek Blue
#' @param base Light curve data frame to use as base for DCF. Expects structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param compare Light curve data frame to compare to base for DCF. Expects structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param beta PSD slope to use when simmulating light curves, should be the PSD slope of compare
#' @param bin.width DCF bin widths for binning of the final DCF
#' @param nsims number of simulations to run for DCF confidence contours
#' @return DCF data frame with strcture: tau, dcf, p90, p95, p99, n90, n95, n99
#' @examples \dontrun{
#' xmm.lc <- xmm.pn.lc(xmm.pn.file.path)
#' xmm.uv.lc <- xmm.uvw1.lc(xmm.uvw1.file.path)
#' dcf <- run.dcf(xmm.uv.lc, xmm.lc, beta, bin.width = 1, nsims = 10)
#' }
#' @importFrom sour cross_correlate
#' @export
run.dcf <- function(base, compare, beta, bin.width = 1, nsims = 10) {
  ## Get LC diff
  tdiff <- base$TIME[1] - compare$TIME[1]
  ## Set light curve origins to 0
  base <- lc.setOrigin(base, 0)
  compare <- lc.setOrigin(compare, 0)
  compare$TIME <- compare$TIME + tdiff
  ## Calculate scale and shift factors for simmulated light curves
  shift.factor <- mean(compare$RATE)
  scale.factor <- max(abs(compare$RATE))-mean(compare$RATE)
  ## Prepare real light curves for DCF
  base.df <- data.frame(t = base$TIME, y = base$RATE, dy = base$ERROR)
  compare.df <- data.frame(t = compare$TIME, y = compare$RATE, dy = compare$ERROR)
  ## Run real DCF
  rdcf <- cross_correlate(base.df, compare.df, method = "dcf", dtau = bin.width, use.errors = TRUE)
  rdcf.df <- data.frame(tau = rdcf$tau, dtau = rdcf$tau, dcf = rdcf$ccf)
  ## Generate first calibration light curve
  calib.lc <- sim.lc(beta = beta, bins = length(compare$TIME), length = compare$TIME[length(compare$TIME)], scale.factor = scale.factor, shift.factor = shift.factor)
  calib.lc$TIME <- calib.lc$TIME + tdiff
  ## Prepare calibration light curve for DCF
  calib.lc.df <- data.frame(t = calib.lc$TIME, y = calib.lc$RATE, dy = calib.lc$ERROR)
  ## Run calibration DCF
  cdcf <- cross_correlate(base.df, calib.lc.df, method = "dcf", dtau = bin.width, use.errors = TRUE)
  cdcf.df <- data.frame(tau = cdcf$tau, dtau = cdcf$tau, dcf = cdcf$ccf)
  ## Prepare for simmulations
  sim.results <- data.frame(numeric(0))
  for (i in 1:length(cdcf.df$tau)) {
    sim.results <- cbind(sim.results, data.frame(numeric(0)))
  }
  colnames(sim.results) <- cdcf.df$tau
  run <- 1
  ## Run simulations
  while (run <= nsims) {
    simmed.lc <- sim.lc(beta = beta, bins = length(compare$TIME), length = compare$TIME[length(compare$TIME)], scale.factor = scale.factor, shift.factor = shift.factor)
    simmed.lc$TIME <- simmed.lc$TIME + tdiff
    simmed.lc.df <- data.frame(t = simmed.lc$TIME, y = simmed.lc$RATE, dy = simmed.lc$ERROR)
    sdcf <- cross_correlate(base.df, simmed.lc.df, method = "dcf", dtau = bin.width, use.errors = TRUE)
    cdf <- data.frame(sdcf$ccf[1])
    for (i in 2:length(sdcf$tau)) {
      cdf <- cbind(cdf, data.frame(sdcf$ccf[i]))
    }
    colnames(cdf) <- sdcf$tau
    sim.results <- rbind(sim.results, cdf)
    run <- run + 1
  }
  ## Calculate quantiles
  contours <- data.frame(del=c(0,0,0))
  for (i in 1:length(sdcf$tau)) {
    t <- sim.results[[i]]
    p1 <- quantile(t, 0.90)
    p2 <- quantile(t, 0.95)
    p3 <- quantile(t, 0.9999)
    r <- data.frame(c(p1[[1]],p2[[1]],p3[[1]]))
    colnames(r) <- c(sdcf$tau[i])
    contours <- cbind(contours, r)
  }
  ## Collect results
  contours <- contours[-1]
  t <- as.data.frame(t(data.frame(sdcf$tau)))
  colnames(t) <- sdcf$tau
  contours <- rbind(contours, t)
  contours <- as.data.frame(t(contours))
  colnames(contours) <- c("p90","p95","p99","tau")
  contours $n90 <- (-1)*contours$p90
  contours $n95 <- (-1)*contours$p95
  contours $n99 <- (-1)*contours$p99
  ## Create and return DCF data frame
  dcf.df <- data.frame(tau = rdcf.df$tau, dcf = rdcf.df$dcf, p90 = contours$p90, p95 = contours$p95, p99 = contours$p99, n90 = contours$n90, n95 = contours$n95, n99 = contours$n99)
  return(dcf.df)
}
