#' unpack a list
#'
#' This function is from library PBSmodelling.
#' Atomizes a list into its constituents.
#' @param mylist : a list object with data.tables, objects etc
#' @return items from list
#' @export
#' @examples
#' l = list(a = "a", b = "b", c = "c")
#' unpackList(l)

unpackList = function(mylist) {

  foreach(i = 1:length(mylist)) %do% {
    assign(names(mylist)[i], mylist[[i]], envir = .GlobalEnv)
  }

}
