#' PlotImage
#'
#' Plots a grey scale image (gsimage)
#'
#' @param image gsimage
#'
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics points
#'
#' @return plot
#' @export PlotImage
#'
#' @examples
#'
#' image = ImportPicture(system.file("pictures", "pngexample.png", package="PictureToDice"))
#' image.conv = ConvolutePixels(image,20)
#' PlotImage(image)
#'
PlotImage <- function(image) {
  # Takes a matrix of elements from 0 to 1 with black being 0 and 1 being white.
  # Image = 2x2 matrix of values between 0 and 1
  # convolute = convolute pixels together in an area of convolute^2

  # Raise error if not gsimage
  if (class(image) != 'gsimage') stop("image is not of class gsimage!")

  plot.new()
  # par(mar = rep(0,4))
  dims = dim(image)
  plot.window(xlim = c(1,dims[1]),
              ylim = c(1,dims[2]))

  for (r in 1:dim(image)[2]) {
    points(rep(r, dims[1]), dims[1]:1, pch=15, col = ToHexGrey(image[,r]))
  }
}
