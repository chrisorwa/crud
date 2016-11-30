#' Remove duplicates
#'
#' @param mydf a data.table. Will be converted into a data.table if it isn't one.
#' @return dups list of duplicates - both originals and duplicates are returned.
#' @export
#' @examples
#' find_dups = dedup(mydf)
#' clean_df = find_dups[[1]]
#' dups = find_dups[[2]]


dedup = function(mydf) {

  if(!is.data.table(mydf)) {
    warning("Sorry, I want a data.table, and converted your data.table.")
    setDT(mydf, keep.rownames = T)
  }

  dups1 = duplicated(mydf, by = NULL)
  dups2 = duplicated(mydf, by = NULL, fromLast = T)
  dups = dups1 | dups2

  if (any(dups)) {
    warning("Found duplicates. Creating dup object and deduplicated your data.table. ")
    dup = mydf[dups, ]
    mydf = unique(mydf, by = NULL)
  }else{
    dup = NULL
    mydf = mydf
  }

  output = list(mydf, dup)

  return(output)

}

