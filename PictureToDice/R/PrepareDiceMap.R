#' PrepareDiceMap
#'
#' @param image gsimage
#' @param exposure.increase Increase/Decrease the values of the dice
#'
#' @return gsimage.dice
#' @export PrepareDiceMap
#'
#' @examples
#'
#' image = ImportPicture(system.file("pictures", "pngexample.png", package="PictureToDice"))
#' image.dice = PrepareDiceMap(image)
#' table(image.dice)
#'
PrepareDiceMap <- function(image, exposure.increase = 0) {
  # This function maps a grey scale image matrix to a matrix of integers from
  # 1 to 6 with 1 being white and 6 being black.
  # These numbers will correspond with the numbers on the dice.
  # image = image pixel matrix
  # exposure = integer value increase.

  # Super simple formula.
  image = 6 - round(5 * image) - exposure.increase

  # Round if there are values out of bounds.
  image[image > 6] = 6
  image[image < 1] = 1
  attr(image, "diceplot.ready") = TRUE

  image
}
