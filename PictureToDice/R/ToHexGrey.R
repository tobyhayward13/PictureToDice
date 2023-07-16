#' ToHexGrey
#'
#' Take a number x on the scale of 0-1 (black-white) and convert to hexcode
#'
#' @param x numeric between 0 and 1
#'
#' @return character hexcode
#' @export ToHexGrey
#'
#' @examples
#'
#' # Black
#' ToHexGrey(0)
#' # White
#' ToHexGrey(1)
#'
ToHexGrey <- function(x) {
  # Take a number x on the scale of 0-1 (black-white) and convert to a string of 6 digits
  # preceded by a '#' corresponding to the hex code.
  vals = sapply(
    as.character(
      sapply(
        c(0:9, LETTERS[1:6]),
        function(x) paste0(x,c(0:9, LETTERS[1:6])))
      ),
    function(x) paste0("#", paste0(rep(x, 3), collapse = ''))
  )
  unname(vals)
  vals[round(255*x)+1]
}
