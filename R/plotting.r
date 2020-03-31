#' @title Plot Light Curve
#' @description Plots the source and background of a light curve
#' @author Derek Blue
#' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param xlab optional: X axis label
#' @param ylab optional: Y axis label
#' @return Plot of the source and background light curve
#' @examples \dontrun{
#' plt <- lc.plot(lightcurve)
#' plot(plt)
#' }
#' @import ggplot2
#' @export
lc.plot <- function(lc, xlab = "Time [s]", ylab = "Rate [count/s]") {
  plt <- ggplot() +
    geom_pointrange(data = lc, aes(x = lc$TIME, xmin = lc$TIME-lc$TIMED, xmax = lc$TIME+lc$TIMED,
                                   y = lc$RATE, ymin = lc$RATE-lc$ERROR, ymax = lc$RATE+lc$ERROR),
                    size = 1, color = "black") +
    geom_pointrange(data = lc, aes(x = lc$TIME, xmin = lc$TIME-lc$TIMED, xmax = lc$TIME+lc$TIMED,
                                   y = lc$BACKV, ymin = lc$BACKV-lc$BACKE, ymax = lc$BACKV+lc$BACKE),
                    size = 1, color = "grey") +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(plot.title = element_text(size = 18, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.ticks.length = unit(-3, "pt"),
          axis.text.x = element_text(margin=margin(6,6,10,6,"pt")),
          axis.text.y = element_text(margin=margin(6,6,10,6,"pt")),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(plt)
}
