#' Clean character and factor vectors
#'
#' Takes character and factor vectors. Factors are first converted into characters.
#' Text strings are validated, converted to lower case, trimmed (trailing, leading), excessive whitespace is removed,
#' non printable characters are removed, encoding is set to UTF-8.
#' Strings "null", "NA" and " " and "  " are converted to NA.
#'
#' factors are lumped to a number corresponding to n.factors.
#' NA values are converted to an explicit value ("..missing..").
#' All character vectors are converted to factors.
#'
#' @param datatable if you supply a data.frame it will be converted to a data.table.
#' @param n.factors how many values should the factor have?
#' @return datatable
#' @export
#' @examples
#' data(msleep, package = "ggplot2)
#'
#'

cleanchar = function(mydf, n.factors = 25) {

  # this function is only for factors and character vectors
  x = xray(mydf)
  dfx = copy(mydf)


  # define variable vectors:  factor, character, other
  factor.cols = x[class == "factor", variable]
  char.cols = x[class == "character", variable]
  convert.cols = c(factor.cols, char.cols)
  other.cols = x[variable %nin% convert.cols, variable]


  # if there are no character or factor variables then there is nothing to do
  if (length(convert.cols) == 0) {
    stop("there are no character or factor variables to convert.")
  }


  # clean up character vectors
  dfx = dfx[, lapply(.SD, function(x) {
    x = as.character(x)
    x = stri_enc_toutf8(x, validate = T)
    x = str_to_lower(x)
    x = str_trim(x, side = "both")
    x = make.names(x)
    x = str_replace_all(x, "\\.+", " ")
    x = str_replace_all(x, "U 00E3 U 00AF U 00E2 U 00BF U 00E2 U 00BD", "")
    x = str_replace_all(x, "  ", " ")
  }), .SDcols = convert.cols]


  # find and convert NA values
  dfx = dfx[, lapply(.SD, function(x) {
    x = ifelse(x == "na", NA, x)
    x = ifelse(x == "null", NA, x)
    x = ifelse(x == "missing", NA, x)
    x = ifelse(x == "^\\s+$", NA, x)
  }), .SDcols = convert.cols]


  # convert to factors
  if (length(convert.cols) > 0) {
    dfx = dfx[, lapply(.SD, factor), .SDcols = convert.cols]
  }


  # clean up factors
  if (length(convert.cols) > 0) {
    dfx = dfx[, lapply(.SD, function(x) fct_explicit_na(x, na_level = "not available")), .SDcols = convert.cols]
    dfx = dfx[, lapply(.SD, function(x) fct_lump(x, n.factors, ties.method = "first", other_level = "other")), .SDcols = convert.cols]
  }


  # close out
  dfx.other = mydf[, names(mydf) %in% other.cols, with = F]
  dfx.all = cbind(dfx, dfx.other)
  setcolorder(dfx.all, x$colidx)

  print(xray(dfx.all))

  message("...done.")


  return(dfx.all)

}
