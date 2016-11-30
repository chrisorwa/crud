#' Convert table to data.table
#'
#' Takes a table and converts it into a matrix. The name of the table is used to name the row headers.
#' @param tab a table
#' @param labels.cols should each column be labeled with the table name?
#' @return data.table a data.table object with the rownames header taken from the tab name
#' @examples
#' data(mtcars)
#' mytab = with(mtcars, table(mpg, cyl))
#' table2dt(mytab)
#' @export

table2dt = function(tab, label.cols = F) {
  dt = tab %>% as.data.frame.matrix %>% setDT(keep.rownames = T)
  rn = deparse(substitute(tab))
  setnames(dt, "rn", rn)

  if (label.cols) {

    oldnames = names(dt)
    newnames = paste(oldnames, rn, sep = "_")
    newnames[1] = oldnames[1]
    setnames(dt, newnames)
  }

  return(dt)
}
