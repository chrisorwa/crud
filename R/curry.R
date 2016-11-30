#' Curry a function
#'
#' Curry a function. Comes from library functional.
#' @param function 1, 2, i
#' @return curried function
#' @export
#' @examples
#' f = function(a, b) {c = a * b; return(c)}
#' f(2,3)
#' g = curry(f, b = 10)
#' f(2)

curry = function (FUN, ...)
{
  .orig = list(...)
  function(...) do.call(FUN, c(.orig, list(...)))
}
