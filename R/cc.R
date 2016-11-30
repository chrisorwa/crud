#' concatenate without quotes (fast c replacement)
#'
#'This is a utility that avoids the quotes that have to surround every element in a vector. It works only with elements that are valid names (no spaces etc).
#'
#' @param character strings
#' @export character vector
#' @examples
#' myvec = cc(one, two, three, "here you have to add quotes because there are spaces...")
#' @export

cc = function(...) {
  as.character(sys.call())[-1]
}
