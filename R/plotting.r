#' @title Plot Light Curve
#' @description Plots the source and background of a light curve
#' @author Derek Blue
#' @param lc required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param xlab optional string: X axis label
#' @param ylab optional string: Y axis label
#' @param plt.title optional: Plot title
#' @param background optional bool: If TRUE plot the background of the light curve, defaults to FALSE
#' @return Plot of the source and background light curve
#' @examples \dontrun{
#' plot(lc.plot(lightcurve, background = TRUE))
#' }
#' @import ggplot2
#' @export
lc.plot <- function(lc, xlab = "Time [s]", ylab = "Rate [count/s]", plt.title = "Lightcurve", background = FALSE, color = "black") {
  plt <- ggplot() +
    geom_linerange(data = lc, aes(x = TIME, ymin = RATE-ERROR, ymax = RATE+ERROR), color = color) +
    geom_linerange(data = lc, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = RATE), color = color) +
    labs(x = xlab, y = ylab, title = plt.title) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          axis.ticks.length = unit(-3, "pt"),
          axis.text.x = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          axis.text.y = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  if (background == TRUE) {
    plt <- plt + geom_linerange(data = lc, aes(x = TIME, ymin = BACKV-BACKE, ymax = BACKV+BACKE), color = "grey")
    plt <- plt + geom_linerange(data = lc, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = BACKV), color = "grey")
  }
  return(plt)
}

#' @title Overplot Light Curves
#' @description Overplots the source and background of two light curves
#' @author Derek Blue
#' @param lc1 required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param lc2 required data frame: Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param xlab optional string: X axis label
#' @param ylab optional string: Y axis label
#' @param bg1 optional bool: If TRUE plot the background of the first light curve, defaults to FALSE
#' @param bg2 optional bool: If TRUE plot the background of the second light curve, defaults to FALSE
#' @param names optional list: Names for the two light curves
#' @return Plot of the source and background light curve
#' @examples \dontrun{
#' plt <- lc.plot(lightcurve)
#' plot(plt)
#' }
#' @import ggplot2
#' @export
lc.overplot <- function(lc1, lc2, xlab = "Time [s]", y1lab = "Rate [count/s]", y2lab = "Rate [count/s]", plt.title = "LC1 vs LC2", bg1 = FALSE, bg2 = FALSE, names = c("lc1", "lc2")) {
  lc1$NAME <- names[1]
  lc2$NAME <- names[2]
  colours <- c("black", "red", "green", "blue")
  plt <- ggplot() +
    geom_linerange(data = lc1, aes(x = TIME, ymin = RATE-ERROR, ymax = RATE+ERROR, group = NAME, color = NAME)) +
    geom_linerange(data = lc1, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = RATE, group = NAME, color = NAME)) +
    geom_linerange(data = lc2, aes(x = TIME, ymin = RATE-ERROR, ymax = RATE+ERROR, group = NAME, color = NAME)) +
    geom_linerange(data = lc2, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = RATE, group = NAME, color = NAME)) +
    scale_y_continuous(y1lab, sec.axis = sec_axis(trans = ~(./ax2.trans)+1, name = y2lab)) +
    scale_color_manual(name = "Light curve", values = colours) +
    labs(x = xlab, title = plt.title) + theme_bw() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18, color = colours[1]),
          axis.title.y.right = element_text(size = 18, color = colours[2]),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          axis.ticks.length = unit(-3, "pt"),
          axis.text.x = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          axis.text.y = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          axis.text.y.right = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  if (bg1 == TRUE) {
    plt <- plt + geom_linerange(data = lc1, aes(x = TIME, ymin = BACKV-BACKE, ymax = BACKV+BACKE), color = colours[3])
    plt <- plt + geom_linerange(data = lc1, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = BACKV), color = colours[3])
  }
  if (bg2 == TRUE) {
    plt <- plt + geom_linerange(data = lc2, aes(x = TIME, ymin = BACKV-BACKE, ymax = BACKV+BACKE), color = colours[4])
    plt <- plt + geom_linerange(data = lc2, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = BACKV), color = colours[4])
  }
  return(plt)
}

#' @title Plot Light Curve Error
#' @description Plots the distribution of measurement error for a lightcurve
#' @author Derek Blue
#' @param lc Light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param bin.width optional: Histogram bin width
#' @param xlab optional: X axis label
#' @param ylab optional: Y axis label
#' @return Plot of the source and background light curve
#' @examples \dontrun{
#' plt <- lc.plot(lightcurve)
#' plot(plt)
#' }
#' @import ggplot2
#' @export
lc.plot.error <- function(lc, bin.width = 0.01, xlab = "Rate Error [count/s]", ylab = "Density") {
  plt <- ggplot(data = lc, aes(x = ERROR)) +
    geom_histogram(aes(y=..density..), binwidth = bin.width, color = "black", fill = "grey") +
    geom_density(alpha=.2, fill="#FF6666") +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          axis.ticks.length = unit(-3, "pt"),
          axis.text.x = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          axis.text.y = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(plt)
}

#' @title Plot Hard and Soft Light Curves
#' @description Plots the source and background of the hard and soft light curves
#' @author Derek Blue
#' @param hlc 2 - 10 keV light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param slc 0.3 - 1 keV light curve data frame with structure: TIME, TIMED, RATE, ERROR, BACKV, BACKE
#' @param xlab optional: X axis label
#' @param ylab optional: Y axis label
#' @param plt.title optional: Plot title
#' @return Plot of the source and background light curve
#' @examples \dontrun{
#' plt <- hs.plot(lightcurve)
#' plot(plt)
#' }
#' @import ggplot2
#' @export
hs.plot <- function(slc, hlc, xlab = "Time [s]", ylab = "Rate [count/s]", plt.title = "Hard and Soft Lightcurves") {
  plt <- ggplot() +
    geom_linerange(data = slc, aes(x = TIME, ymin = RATE-ERROR, ymax = RATE+ERROR), color = "black") +
    geom_linerange(data = slc, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = RATE), color = "black") +
    geom_linerange(data = slc, aes(x = TIME, ymin = BACKV-BACKE, ymax = BACKV+BACKE), color = "red") +
    geom_linerange(data = slc, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = BACKV), color = "red") +
    geom_linerange(data = hlc, aes(x = TIME, ymin = RATE-ERROR, ymax = RATE+ERROR), color = "blue") +
    geom_linerange(data = hlc, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = RATE), color = "blue") +
    geom_linerange(data = hlc, aes(x = TIME, ymin = BACKV-BACKE, ymax = BACKV+BACKE), color = "green") +
    geom_linerange(data = hlc, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = BACKV), color = "green") +
    labs(x = xlab, y = ylab, title = plt.title) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          axis.ticks.length = unit(-3, "pt"),
          axis.text.x = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          axis.text.y = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(plt)
}

#' @title Plot Hardness Ratio
#' @description Plots the source and background of a light curve
#' @author Derek Blue
#' @param hr.df Hardness Ratio data frame with structure: TIME, RATIO, ERROR
#' @param xlab optional: X axis label
#' @param ylab optional: Y axis label
#' @param plt.title optional: Plot title
#' @return Plot of the source and background light curve
#' @examples \dontrun{
#' plt <- hr.plot(lightcurve)
#' plot(plt)
#' }
#' @import ggplot2
#' @export
hr.plot <- function(hr.df, xlab = "Time [s]", ylab = "Ratio", plt.title = "Hardness Ratio", show.origin = FALSE) {
  plt <- ggplot() +
    geom_linerange(data = hr.df, aes(x = TIME, ymin = RATIO-ERROR, ymax = RATIO+ERROR), color = "black") +
    geom_linerange(data = hr.df, aes(xmin = TIME-TIMED, xmax = TIME+TIMED, y = RATIO), color = "black") +
    labs(x = xlab, y = ylab, title = plt.title) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          axis.ticks.length = unit(-3, "pt"),
          axis.text.x = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          axis.text.y = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  if (show.origin == TRUE) {
    plt <- plt + geom_hline(yintercept = 0)
  }
  return(plt)
}

#' @title Plot Flux Flux
#' @description Plots the hard band flux vs soft band flux
#' @author Derek Blue
#' @param ff.df Flux flux data frame with structure: SOFT.RATE, SOFT.ERROR, HARD.RATE, HARD.ERROR
#' @param xlab optional: X axis label
#' @param ylab optional: Y axis label
#' @param plt.title optional: Plot title
#' @return Plot of the hard band flux vs soft band flux
#' @examples \dontrun{
#' plt <- ff.plot(ff.df)
#' plot(plt)
#' }
#' @import ggplot2
#' @export
ff.plot <- function(ff.df, xlab = "Soft Rate [count/s]", ylab = "Hard Rate [count/s]", plt.title = "Flux-Flux Plot") {
  plt <- ggplot() +
    geom_linerange(data = ff.df, aes(x = SOFT.RATE, ymin = HARD.RATE-HARD.ERROR, ymax = HARD.RATE+HARD.ERROR), color = "black") +
    geom_linerange(data = ff.df, aes(xmin = SOFT.RATE-SOFT.ERROR, xmax = SOFT.RATE+SOFT.ERROR, y = HARD.RATE), color = "black") +
    scale_y_log10() + scale_x_log10() +
    labs(x = xlab, y = ylab, title = plt.title) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          axis.ticks.length = unit(-3, "pt"),
          axis.text.x = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          axis.text.y = element_text(margin = margin(6,6,10,6,"pt"), size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(plt)
}

#' @title Plot DCF
#' @description Plots the DCF
#' @author Derek Blue
#' @param dcf.df DCF data frame with strcture: tau, dcf, p90, p95, p99, n90, n95, n99
#' @param contours optional: boolean to include or exclude confidence contours
#' @param ylab optional: X axis label
#' @param ylab optional: Y axis label
#' @param plt.title optional: Plot title
#' @return Plot of the DCF
#' @examples \dontrun{
#' plt <- dcf.plot(dcf.df)
#' plot(plt)
#' }
#' @import ggplot2
#' @export
dcf.plot <- function(dcf.df, contours = FALSE, xlab = "tau [days]", ylab = "dcf", plt.title = "Broadband UV DCF") {
  colors <- c("DCF" = "black","99.99%" = "blue", "95%" = "orange", "90%" = "red")
  dcf.plt <- ggplot() +
    geom_hline(yintercept = 0, color = "grey") +
    geom_vline(xintercept = 0, color = "grey") +
    geom_line(data = dcf.df, aes(x = tau, y = dcf), size = 1, color = "black")
  if (contours) {
    dcf.plt <- dcf.plt +
      geom_line(data = dcf.df, aes(x = tau, y = p99, color = "99.99%"), linetype = "dashed", size = 1) +
      geom_line(data = dcf.df, aes(x = tau, y = n99, color = "99.99%"), linetype = "dashed", size = 1) +
      geom_line(data = dcf.df, aes(x = tau, y = p95, color = "95%"), linetype = "dotdash", size = 1) +
      geom_line(data = dcf.df, aes(x = tau, y = n95, color = "95%"), linetype = "dotdash", size = 1) +
      geom_line(data = dcf.df, aes(x = tau, y = p90, color = "90%"), linetype = "dotted", size = 1) +
      geom_line(data = dcf.df, aes(x = tau, y = n90, color = "90%"), linetype = "dotted", size = 1)
  }
  dcf.plt <- dcf.plt +
    ylim(-1, 1) +
    scale_color_manual(name = "Confidence", values = colors) +
    labs(x = xlab, y = ylab, title = plt.title) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.ticks.length = unit(-3, "pt"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          axis.text.x = element_text(margin=margin(6,6,10,6,"pt"), size=14),
          axis.text.y = element_text(margin=margin(6,6,10,6,"pt"), size=14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(dcf.plt)
}
