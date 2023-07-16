#' DrawDice
#'
#' Draws a dice face given an integer from 1 to 6.
#'
#' @param n integer from 1-6
#' @param pointsize override for the size of the points
#'
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics points
#'
#' @return plot
#' @export DrawDice
#'
#' @examples
#'
#' DrawDice(5)
#' box()
#'
DrawDice <- function(n, pointsize = 0.5) {
  # Returns a plot of a "Dice Face" corresponding to the number n between 1-6

  # Raise error if n is not between 1-6
  if (!(n %in% 1:6)) stop("n not an integer between 1 and 6!")

  plot.new()
  plot.window(xlim = c(1,5), ylim = c(1,5))
  if (n == 1) {
    points(3,3, pch = 19, cex = pointsize)
  }
  else if (n == 2) {
    points(c(2,4),c(2,4), pch = 19, cex = pointsize)
  }
  else if (n == 3) {
    points(2:4,2:4, pch = 19, cex = pointsize)
  }
  else if (n == 4) {
    points(c(2,2,4,4),c(2,4,2,4), pch = 19, cex = pointsize)
  }
  else if (n == 5) {
    points(c(2,2,4,4,3),c(2,4,2,4,3), pch = 19, cex = pointsize)
  }
  else if (n == 6) {
    points(rep(c(2,4),3),rep(2:4, each = 2), pch = 19, cex = pointsize)
  }
  box()
}
