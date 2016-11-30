#' Find and remove empty columns and/or rows
#'
#' @param mydf data.table; if it is a data.frame it will be converted into a data.table
#' @return data.table without empty rows and columns
#' @export
#' @examples
#' dempty(mydf)

dempty = function(mydf) {

  if(!is.data.table(mydf)) {
    warning("Sorry, I want a data.table, and converted your data.frame.")
    setDT(mydf, keep.rownames = T)
  }

  # identify and remove empty columns
  junkcol = which(colSums(is.na(mydf)) == nrow(mydf))

  if (length(junkcol) > 0) {
    warning("Found empty columns and removed them.")
    mydf = mydf[, !junkcol, with = F]
  }else{
    message("No empty columns found.")
  }


  # identify and remove empty rows
  junkrow = which(rowSums(is.na(mydf)) == ncol(mydf))

  if (length(junkrow) > 0) {
    warning("Found empty columns and removed them.")
    mydf = mydf[, !junkrow, with = F]
  }else{
    message("No empty columns found.")
  }

  return(mydf)

}


