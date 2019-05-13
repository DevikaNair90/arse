#' Calculates a count of the time beneath the baseline.
#'
#' This function takes a series of y-coordinates and returns a count
#' of y-coordinates that fall beneath the specified baseline after
#' the intrusion of a stress event (i.e., rapidity).
#'
#' @param data A dataframe with x- and y-coordinates in wide format.
#' @param ycoord A specified selection of the y-coordinate values within the
#' dataframe. The first y-coordinate value should correspond to the baseline value
#' of 'y'. The second y-coordinate value should be the first measure of 'y' after
#' the intrusion of a stress event. The last value of 'y' should correspond to
#' the last measurement of 'y' over the measured timeframe.
#' @param ybase A specified selection of the baseline of the 'y' measured variable.
#' Users are advised to place baseline as the first instance of the y-coordinate
#' values. Function defaults to use the first y-coordinate value in the series.
#' @param yinvert Specifies whether resilience occurs above or below the baseline
#' depending on the meaning of high and low 'y' values. When parameter 'yinvert'
#' is set to 'FALSE' (the default), it is assumed that higher numbers are indicative
#' of positive (i.e., desired) 'y' values (e.g., exam grade). When 'yinvert' is set
#' to 'TRUE', it is assumed that lower numbers are indicative of positive
#' (i.e., desired) 'y' values (e.g., blood pressure).
#' @param saveout When the parameter 'saveout' is set to 'FALSE' (the default),
#' a vector of calculated count_rapidity values are given for each case. When 'saveout' is
#' set to 'TRUE', a dataframe of the original inputted dataset is returned with a
#' new column of calculated count_rapidity values.
#' @return When the parameter 'saveout' is set to 'FALSE', a vector of calculated
#' count_rapidity values are given for each case. When 'saveout' is set to 'TRUE', a
#' dataframe of the original inputted dataset is returned with a new column of
#' calculated count_rapidity values.
#' @import dplyr
#' @import pracma
#' @export
#' @examples
#' xc <- t(c(1,2,3,4,5,6,7,8,9,10))
#' yc <- t(c(75,53,37,25,27,35,50,75,75,75))
#' dataset1 <- data.frame(xc, yc)
#' count_rapidity(data = dataset1, ycoord = dataset1[,11:20], saveout = TRUE)
count_rapidity <- function(data, ycoord, ybase = NA, yinvert = FALSE, saveout = FALSE) {
  if(is.na(ybase[1]) == TRUE){ybase <- ycoord[,1]}
  if(is.null(data)){
    print("Need data entry")}
  count_rapidity <- vector(length = nrow(ycoord))
  if (yinvert == TRUE) {
    for (i in 1:nrow(ycoord)) {
      yn <- as.integer(ycoord[i,])
      yb <- ybase[i]
      count_rapidity[i] = sum(yn > yb)
    }
  } else {
    for (i in 1:nrow(ycoord)) {
      yn <- as.integer(ycoord[i,])
      yb <- ybase[i]
      count_rapidity[i] = sum(yn < yb)
    }
  }
  count_rapidity
  if(saveout == FALSE) return(count_rapidity)
  if(saveout == TRUE) data.frame(data) %>% mutate(count_rapidity = count_rapidity)
}
