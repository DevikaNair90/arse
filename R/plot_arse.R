#' Plots the area of resilience to stress event.
#'
#' This function takes a series of 'x,y' coordinates and a specified 'y'
#' baseline value and plots them on an x- and y-axis.
#'
#' @param xcoord A specified selection of the x-coordinate values within
#' a vector. The first x-coordinate value should correspond to the baseline
#' input of 'y'.
#' @param ycoord A specified selection of the y-coordinate values within a
#' vector. The first y-coordinate value should correspond to the baseline value
#' of 'y'. The second y-coordinate value should be the first measure of 'y' after
#' the intrusion of a stress event. The last value of 'y' should correspond to
#' the last measurement of 'y' over the measured timeframe.
#' @param ybase A specified selection of the baseline of the 'y' measured variable.
#' Users are advised to place baseline as the first instance of the y-coordinate
#' values. Function defaults to use the first y-coordinate value in the series.
#' @param ll Sets the lower limit of the y-axis scale. Recommended to be the lowest
#' possible value of your 'y' measure.
#' @param ul Sets the upper limit of the y-axis scale. Recommended to be the highest
#' possible value of your 'y' measure.
#' @param xlab Labels the x-axis. Defaults to "Time (X)".
#' @param ylab Labels the y-axis. Defaults to "Outcome (Y)".
#' @return Returns a graph with 'x,y' coordinates and baseline.
#' @import dplyr
#' @import pracma
#' @importFrom graphics abline lines plot
#' @export
#' @examples
#' xc <- t(c(1,2,3,4,5,6,7,8,9,10))
#' yc <- t(c(75,53,37,25,27,35,50,75,75,75))
#' dataset1 <- data.frame(xc, yc)
#' plot_arse(xcoord = as.numeric(dataset1[,1:10]),
#' ycoord = as.numeric(dataset1[,11:20]), ll = 0, ul = 100)
plot_arse <- function(xcoord, ycoord, ybase = NA, ll, ul, xlab = NA, ylab = NA) {
  if(is.na(ybase[1]) == TRUE){ybase <- ycoord[1]} #baseline defaults to first column of ycoord
  if(is.na(xlab)){xlab <- "Time (X)"}
  if(is.na(ylab)){ylab <- "Outcome (Y)"}
  plot(xcoord, ycoord, ylim = c(ll, ul), xlab = xlab, ylab = ylab)
  lines(xcoord, ycoord)
  abline(h = ybase, lty = 3)
}
