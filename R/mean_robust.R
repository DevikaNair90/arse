#' Calculates the mean robustness after a stress event.
#'
#' This function takes a series of y-coordinates and returns the
#' average deviation from the specified baseline after the intrusion
#' of a stress event (i.e., robustness). Magnitude is indicated by how
#' large the mean_robust value becomes. Direction of the robustness is
#' indicated by a positive or negative value; negative numbers indicate
#' a departure below the baseline while positive numbers indicate a
#' departure above the baseline.
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
#' a vector of calculated mean_robust values are given for each case. When 'saveout' is
#' set to 'TRUE', a dataframe of the original inputted dataset is returned with a
#' new column of calculated mean_robust values.
#' @return When the parameter 'saveout' is set to 'FALSE', a vector of calculated
#' mean_robust values are given for each case. When 'saveout' is set to 'TRUE', a
#' dataframe of the original inputted dataset is returned with a new column of
#' calculated mean_robust values.
#' @import dplyr
#' @import pracma
#' @export
#' @examples
#' yc <- t(c(75,53,37,25,27,35,50,75,75,75))
#' dataset1 <- data.frame(yc)
#' mean_robust(data = dataset1, ycoord = yc, saveout = TRUE)
mean_robust <- function(data, ycoord, ybase = NA, yinvert = FALSE, saveout = FALSE) {
  if(is.na(ybase[1]) == TRUE){ybase <- ycoord[,1]}
  if(is.null(data)){
    print("Need data entry")}
  mean_robust <- vector(length = nrow(ycoord))
  if (yinvert == TRUE) {
    for (i in 1:nrow(ycoord)) {
      yn <- as.integer(ycoord[i,])
      yb <- ybase[i]
      mean_robust[i] = mean(yb - yn)
    }
  } else {
    for (i in 1:nrow(ycoord)) {
      yn <- as.integer(ycoord[i,])
      yb <- ybase[i]
      mean_robust[i] = mean(yn - yb)
    }
  }
  mean_robust #calcualates average deviation from the baseline of y
  if(saveout == FALSE) return(mean_robust)
  if(saveout == TRUE) as.data.frame(data) %>% mutate(mean_robust = mean_robust)
}
