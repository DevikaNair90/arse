#' Calculates x-coordinate interpolation points across a baseline.
#'
#' This function takes a series of 'x,y' coordinates and a specified 'y'
#' baseline value. For any two sequential x-coordinate points, where the
#' 'y' baseline value is crossed in between them, the function calculates
#' the predicted x-coordinate value and interpolates that point with the
#' matching 'y' baseline value. The new points are then added to the original
#' set of 'x,y' coordinates. Note that all the prefixed arse functions
#' automatically perform this operation within their respective functions.
#'
#' @param data A dataframe with x- and y-coordinates in wide format.
#' @param xcoord A specified selection of the x-coordinate values within
#' the dataframe.
#' @param ycoord A specified selection of the y-coordinate values within
#' the dataframe.
#' @param ybase A specified selection of the baseline of the 'y' measured
#' variable.
#' Users are advised to place baseline as the first instance of the
#' y-coordinate values. Function defaults to use the first y-coordinate value
#' in the series.
#' @return A dataframe of old and newly-interpolated x-coordinate values
#' at the specified baseline.
#' @import dplyr
#' @import pracma
#' @export
#' @examples
#' xc <- t(c(1,2,3,4,5,6,7,8,9,10))
#' yc <- t(c(75,53,37,25,95,35,50,75,75,75))
#' dataset1 <- data.frame(xc, yc)
#' getintersectx(data = dataset1, xcoord = dataset1[,1:10], ycoord = dataset1[,11:20])
getintersectx <- function(data, xcoord, ycoord, ybase = NA){
  if(is.na(ybase[1]) == TRUE){ybase <- ycoord[,1]} #baseline defaults to first column of ycoord
  if(is.null(data)){
    print("ERROR: Need data entry")
  }
  #Identifies intersection points of cases in which growth might occur and adds them as interpolated points
  interpol <- function(x, y, ybase = NA) {
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
  #Creates interpolation points for 'y' datapoints that intersect baseline using 'interpol' function
  yycoord <- list()
  ybase2 <- as.data.frame(ybase)[rep(names(as.data.frame(ybase)), length(ycoord))]
  for (g in 1:nrow(ycoord)) {
    yycoord[[g]] <- suppressWarnings(interpol(x = as.numeric(xcoord[g,]), y = as.numeric(ycoord[g,]), ybase = as.numeric(ybase2[g,]))[,2])[[1]]
  }
  yycoord_mat <- matrix(NA, nrow = length(yycoord), ncol = max(sapply(yycoord,length)))
  for (m in 1:nrow(yycoord_mat)) {
    yycoord_mat[m,1:length(yycoord[[m]])] <- yycoord[[m]]
    yycoord_mat[m,is.na(yycoord_mat[m,])] <- ybase[m]
  }
  #Creates interpolation points for 'x' datapoints that intersect baseline using 'interpol' function
  xxcoord <- list()
  ybase2 <- as.data.frame(ybase)[rep(names(as.data.frame(ybase)), length(ycoord))]
  for (g in 1:nrow(xcoord)) {
    xxcoord[[g]] <- suppressWarnings(interpol(x = as.numeric(xcoord[g,]), y = as.numeric(ycoord[g,]), ybase = as.numeric(ybase2[g,]))[,1])[[1]]
  }
  xxcoord_mat <- matrix(NA, nrow = length(xxcoord), ncol = max(sapply(xxcoord,length)))
  for (m in 1:nrow(xxcoord_mat)) {
    xxcoord_mat[m,1:length(xxcoord[[m]])] <- xxcoord[[m]]
    xxcoord_mat[m,is.na(xxcoord_mat[m,])] <- max(xxcoord[[m]])
  }
  combine2 <- data.frame(data.frame(xxcoord_mat), data.frame(yycoord_mat))
  combine2
}
