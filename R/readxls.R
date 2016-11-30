#' Read bunch of Excel files and unmess
#' 
#' Set a path to a folder with Excel files. 
#' This function will then read them and clean the first mess (empty columns, empty rows, illegal column names).
#' 
#' @param xls.files a vector with xls files
#' @return list object with cleaned xls files
#' @export
#' @examples
#' xls.files = dir(pattern = "xlsx$)
#' l = readxls(xls.files)
#' rbindlist(l)

readxls = function(xlsfiles) {
  
  l = foreach(i = 1:length(xls.files)) %do% {
    
    # read files
    f = read_excel(xls.files[i], skip = 3)
    setDT(f)
    
    # dedup columns
    f = f[, unique(names(f), fromLast = T), with = FALSE]

    # remove empty rows and columns
    f = dempty(f)
    
    # label with filenames
    f[, label := xls.files[i]]
    
  }
  
  sanitized.names = make.names(xls.files) %>% str_replace("\\.\\.\\.", "\\.")
  names(l) = sanitized.names
  
  return(l)
  
}

