#' PlotDiceMap
#'
#' Takes a gsimage matrix image bitmap image object and returns a dice map plot.
#'
#' @param image gsimage object
#' @param pointsize pointsize override
#'
#' @importFrom graphics par
#'
#' @return plot
#' @export PlotDiceMap
#'
#' @examples
#'
#' file = system.file("pictures", "pngexample.png", package="PictureToDice")
#' image = ImportPicture(file)
#' image.conv = ConvolutePixels(image,20)
#' image.prepped = PrepareDiceMap(image.conv, exposure.increase = 1)
#' PlotDiceMap(image.prepped, 0.2)
#'
#'
PlotDiceMap <- function(image, pointsize = 0.5) {
  # Plots the resulting Dice Map Image

  # Throw error if not a gsimage or is not dicemap.ready
  if (class(image) != 'gsimage' | !attr(image, "diceplot.ready")) stop("Not correct gsimage type!")

  op <- par('mfrow','mar')
  on.exit(par(op))
  par(
    mfrow = dim(image),
    mar = rep(0,4)
  )
  for (pxl in t(image)) {
    DrawDice(pxl, pointsize)
  }
}
