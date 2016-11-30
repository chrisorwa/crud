#' Get unique values from factor variables
#'
#' Data table is screened for factor variables.
#' The unique values of each variable is obtained.
#'
#' @param data data.table object
#' @return vals list of unique values
#' @export
#' @examples
#' checkval(data)[]

getvalues = function(mydf) {

  # check whether this is a data.table
  if (!is.data.table(mydf)) {
    warning("sorry, I want a data.table, converting...")
    data = setDT(mydf)
  }

  # get factor values
  vars = xray(mydf)[class == "factor", variable]

  if (length(vars) == 0) {
    warning("sorry, no factor variables found.")
  } else {

    # get individual values
    vals = foreach(i = 1:length(vars)) %do% {
      y = mydf[, eval(vars[i]), with = F]
      setorder(y)
      z = unique(y, by = NULL)
    }

    names(vals) = vars

    return(vals)
  }

}
