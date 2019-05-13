#' Calculates the nadir point of measured 'y' values.
#'
#' This function takes a series of y-coordinates and returns the
#' lowest value.
#'
#' @param data A dataframe with x- and y-coordinates in wide format.
#' @param ycoord A specified selection of the y-coordinate values within the
#' dataframe.
#' @param saveout When the parameter 'saveout' is set to 'FALSE' (the default),
#' a vector of calculated ynadir values are given for each case. When 'saveout' is
#' set to 'TRUE', a dataframe of the original inputted dataset is returned with a
#' new column of calculated ynadir values.
#' @param yinvert Specifies whether resilience occurs above or below the baseline
#' depending on the meaning of high and low 'y' values. When parameter 'yinvert'
#' is set to 'FALSE' (the default), it is assumed that higher numbers are indicative
#' of positive (i.e., desired) 'y' values (e.g., exam grade). When 'yinvert' is set
#' to 'TRUE', it is assumed that lower numbers are indicative of positive
#' (i.e., desired) 'y' values (e.g., blood pressure).
#' @return When the parameter 'saveout' is set to 'FALSE', a vector of calculated
#' ynadir values are given for each case. When 'saveout' is set to 'TRUE', a
#' dataframe of the original inputted dataset is returned with a new column of
#' calculated ynadir values.
#' @import dplyr
#' @import pracma
#' @export
#' @examples
#' yc <- t(c(75,53,37,25,27,35,50,75,75,75))
#' dataset1 <- data.frame(yc)
#' ynadir(data = dataset1, ycoord = yc, saveout = TRUE)
ynadir <- function(data, ycoord, yinvert = FALSE, saveout = FALSE) {
  if(is.null(data)){
    print("Need data entry")}
  if (yinvert == TRUE) {
    ynadir <- vector(length = nrow(ycoord))
    for (i in 1:nrow(ycoord)) {
      yn <- ycoord[i,]
      ynadir[i] = max(yn)
    }
  } else {
    ynadir <- vector(length = nrow(ycoord))
    for (i in 1:nrow(ycoord)) {
      yn <- ycoord[i,]
      ynadir[i] = min(yn)
    }
  }
  ynadir #calculates the lowest point of y
  if(saveout == FALSE) return(ynadir)
  if(saveout == TRUE) as.data.frame(data) %>% mutate(ynadir = ynadir)
}

