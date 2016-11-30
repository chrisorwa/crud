#' Modues of distribution
#'
#' All frequency distributions are not normally distributed and it is therefore not a good idea to use the mean.
#' Rather use the median (halfway) or mode (most frequent).
#' Source: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#' @param vector of any type
#' @return mode
#' @export
#' @examples
#' modus(mtcars$cyl)  # most frequent type of cars: 8 cylinders


modus <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
