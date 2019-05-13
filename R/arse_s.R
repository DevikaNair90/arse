#' Calculates the area of resilience to stress event below a baseline
#' with scaling for end state growth or non-resilience.
#'
#' This function takes a series of 'x,y' coordinates and a specified 'y'
#' baseline value. For a given set of x-coordinates over time and repeated
#' measures of a 'y' variable, this function calculates the area of
#' resilience to a stress event (arse) that is formed below the specified
#' baseline value of 'y' using an implementation of the shoelace formula
#' (Gauss's area formula) for the area of irregular polygons. With arse
#' calculated, the arse is scaled in reference to the end state value
#' by multiplying the arse value by the division of baseline value over
#' the end state value (i.e., last measured value of 'y') or
#' \eqn{arse value * (baseline value/end state value)}. Smaller arse_s values
#' are indicative of better resilience on the measured variable.
#'
#' @param data A dataframe with x- and y-coordinates in wide format.
#' @param xcoord A specified selection of the x-coordinate values within
#' the dataframe. The first x-coordinate value should correspond to the baseline
#' input of 'y'.
#' @param ycoord A specified selection of the y-coordinate values within the
#' dataframe. The first y-coordinate value should correspond to the baseline value
#' of 'y'. The second y-coordinate value should be the first measure of 'y' after
#' the intrusion of a stress event. The last value of 'y' should correspond to
#' the last measurement of 'y' over the measured timeframe.
#' @param ybase A specified selection of the baseline of the 'y' measured variable.
#' Users are advised to place baseline as the first instance of the y-coordinate
#' values. Function defaults to use the first y-coordinate value in the series.
#' @param yend A specified selection of the end state of the 'y' measured variable.
#' Users are advised to place end state as the last instance of the y-coordinate
#' values. Function defaults to use the last y-coordinate value in the series.
#' @param yinvert Specifies whether resilience occurs above or below the baseline
#' depending on the meaning of high and low 'y' values. When parameter 'yinvert'
#' is set to 'FALSE' (the default), it is assumed that higher numbers are indicative
#' of positive (i.e., desired) 'y' values (e.g., exam grade). When 'yinvert' is set
#' to 'TRUE', it is assumed that lower numbers are indicative of positive
#' (i.e., desired) 'y' values (e.g., blood pressure).
#' @param saveout When the parameter 'saveout' is set to 'FALSE' (the default),
#' a vector of calculated arse_s values are given for each case. When 'saveout' is
#' set to 'TRUE', a dataframe of the original inputted dataset is returned with a
#' new column of calculated arse_s values.
#' @return When the parameter 'saveout' is set to 'FALSE', a vector of calculated
#' arse_s values are given for each case. When 'saveout' is set to 'TRUE', a
#' dataframe of the original inputted dataset is returned with a new column of
#' calculated arse_s values.
#' @import dplyr
#' @import pracma
#' @export
#' @examples
#' xc <- t(c(1,2,3,4,5,6,7,8,9,10))
#' yc <- t(c(75,53,37,25,27,35,46,49,49,51))
#' dataset1 <- data.frame(xc, yc)
#' arse_s(data = dataset1, xcoord = dataset1[,1:10], ycoord = dataset1[,11:20], saveout = TRUE)
arse_s <- function(data, xcoord, ycoord, ybase = NA, yend = NA, yinvert = FALSE, saveout = FALSE ){
  if(is.na(ybase[1]) == TRUE){ybase <- ycoord[,1]}
  if(is.na(yend)){yend <- ycoord[,ncol(ycoord)]}
  if(is.null(data)){
    print("Need data entry")
  }
  #Identifies intersection points of cases in which growth might occur and adds them as interpolated points
  getintersect <- function(x, y, ybase = NA) {
    if(is.na(ybase[1]) == TRUE){ybase <- (rep(y[,1], length(y)))} #baseline defaults to first column of ycoord
    # Find points where x1 is above x2.
    above <- ybase > y
    # Points always intersect when above = TRUE, then FALSE or reverse
    intersect.points <- which(diff(above)!= 0)
    # Find the slopes for each line segment.
    Y1.slopes <- rep(0,length(y))
    Y2.slopes <- (y[intersect.points + 1] - y[intersect.points]) /
      (x[intersect.points + 1] - x[intersect.points])
    # Find the intersection for each segment
    X.points <- x[intersect.points] + ((y[intersect.points] - ybase[intersect.points]) / (Y1.slopes - Y2.slopes))
    Y.points <- ybase[intersect.points] + (Y1.slopes*(X.points - intersect.points))
    combined <- bind_rows(   # combine rows from...
      tibble(x, y),         # table of original, plus
      tibble(x = X.points,
             y = Y.points)) %>%  # table of interpolations
      distinct() %>%         # and drop any repeated rows
      arrange(x)             # and sort by X
    combined
  }
  #Creates interpolation points for 'y' datapoints that intersect baseline using 'getintersect' function
  yycoord <- list()
  ybase2 <- as.data.frame(ybase)[rep(names(as.data.frame(ybase)), length(ycoord))]
  for (g in 1:nrow(ycoord)) {
    yycoord[[g]] <- suppressWarnings(getintersect(x = as.numeric(xcoord[g,]), y = as.numeric(ycoord[g,]), ybase = as.numeric(ybase2[g,]))[,2])[[1]]
  }
  yycoord_mat <- matrix(NA, nrow = length(yycoord), ncol = max(sapply(yycoord,length)))
  for (m in 1:nrow(yycoord_mat)) {
    yycoord_mat[m,1:length(yycoord[[m]])] <- yycoord[[m]]
    yycoord_mat[m,is.na(yycoord_mat[m,])] <- ybase[m]
  }
  #Creates interpolation points for 'x' datapoints that intersect baseline using 'getintersect' function
  xxcoord <- list()
  ybase2 <- as.data.frame(ybase)[rep(names(as.data.frame(ybase)), length(ycoord))]
  for (g in 1:nrow(xcoord)) {
    xxcoord[[g]] <- suppressWarnings(getintersect(x = as.numeric(xcoord[g,]), y = as.numeric(ycoord[g,]), ybase = as.numeric(ybase2[g,]))[,1])[[1]]
  }
  xxcoord_mat <- matrix(NA, nrow = length(xxcoord), ncol = max(sapply(xxcoord,length)))
  for (m in 1:nrow(xxcoord_mat)) {
    xxcoord_mat[m,1:length(xxcoord[[m]])] <- xxcoord[[m]]
    xxcoord_mat[m,is.na(xxcoord_mat[m,])] <- max(xxcoord[[m]])
  }
  if (yinvert == TRUE) {
    #Creates matrix of y-coordinates and adds a final y-coordinate at the baseline
    tyr <-  as.data.frame(yycoord_mat) %>% mutate(baseline = ybase)
    for (i in 1:nrow(tyr)) {
      tyr[i,] <- pmax(tyr$baseline[i],tyr[i,])
    }
    tyr <- as.matrix(tyr)
    #Creates matrix of x-coordinates and adds a final x-coordinate
    txr <- as.matrix(cbind(xxcoord_mat, xxcoord_mat[,ncol(xxcoord_mat)]))
  } else {
    #Creates matrix of y-coordinates and adds a final y-coordinate at the baseline
    tyr <-  as.data.frame(yycoord_mat) %>% mutate(baseline = ybase)
    for (i in 1:nrow(tyr)) {
      tyr[i,] <- pmin(tyr$baseline[i],tyr[i,])
    }
    tyr <- as.matrix(tyr)
    #Creates matrix of x-coordinates and adds a final x-coordinate
    txr <- as.matrix(cbind(xxcoord_mat, xxcoord_mat[,ncol(xxcoord_mat)]))
  }
  #Calculates ARSE for every row x,y
  arse <- vector(length = nrow(tyr))
  for (i in 1:nrow(tyr)) {
    x <- txr[i,]
    y <- tyr[i,]
    arse[i] = abs(polyarea(x, y))
  }
  arse_s <- arse * (ybase/yend)
  if(saveout == FALSE) return(arse_s)
  if(saveout == TRUE) as.data.frame(data) %>% mutate(arse_s = arse_s)
}
