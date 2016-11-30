#' Clean column names of a dataset
#'
#' Variable names in a dataset normally need cleaning in order to:
#' Remove excessive whitespace (trailing, leading, double spaces)
#' Non alpha numeric characters. The characters are replaced by dots.
#' Leading and trailing dots are removed.
#' Encodings are set to UTF-8.
#' Names are set to lowercase.
#'
#'
#' @param mydf data.table or data.frame. If a data.frame is supplied it will be converted to a data.table
#' @return mydf data.table with cleaned column names
#' @export
#' @examples
#' mydf = cleanheader(mydf)

cleanheader = function(mydf) {
  header = names(mydf)
  newheader = header %>%
    make.names(unique = T) %>%
    str_replace("\\.+$", "") %>%
    str_replace("^\\.+", "") %>%
    str_replace("\\.{2,4}", ".") %>%
    str_to_lower %>%
    str_conv("UTF-8")

  setnames(mydf, newheader)

  return(mydf)
}
