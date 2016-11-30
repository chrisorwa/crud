#' Get overview of dataset
#'
#' List all characteristics of data, split out by class.
#' For numeric values: mean, median, infinites, NaNs, negatives, zeros
#' for factors: levels and unused levels;
#' all variables: class, blanks.
#'
#'
#'
#' @param data data.table object
#' @return xray data.table with the fields {variable name, class, blanks, infinites, NaNs, zeros, means, medians, most frequent values, fraction most frequent values}.
#' @examples
#' data(ggplot2::msleep)
#' msleep = as.data.table(msleep, keep.rownames = T)
#' xray(msleep)[]
#' @export


xray = function(mydf) {

  if (!is.data.table(mydf)) {
    warning("Sorry, I want a data.table, and converted your data.frame.")
    setDT(mydf, keep.rownames = T)
    }

  setkey(mydf)

  # collect stats
  NAs = mydf[, lapply(.SD, function(x) sum(is.na(x)))]
  Infs = mydf[, lapply(.SD, function(x) sum(is.infinite(x)))]
  NaNs = mydf[, lapply(.SD, function(x) sum(is.nan(x)))]
  negs = mydf[, lapply(.SD, function(x) ifelse(class(x) == "numeric" | class(x) == "integer", sum(x < 0, na.rm = T), 0))]
  zeros = mydf[, lapply(.SD, function(x) ifelse(class(x) == "numeric" | class(x) == "integer", sum(x == 0, na.rm = T), 0))]
  means = mydf[, lapply(.SD, function(x) ifelse(class(x) == "numeric" | class(x) == "integer", mean(x, na.rm = T), 0))]
  medians = mydf[, lapply(.SD, function(x) ifelse(class(x) == "numeric" | class(x) == "integer", median(x, na.rm = T), 0))]

  nlevel1 = mydf[, lapply(.SD, function(x) ifelse(class(x) == "factor", nlevels(x), 0))]
  mydf = droplevels(mydf)
  nlevel2 = mydf[, lapply(.SD, function(x) ifelse(class(x) == "factor", nlevels(x), 0))]

  unique.vals = mydf[, lapply(.SD, uniqueN)]
  classes = mydf[, lapply(.SD, class)][1, ]

  # bind in one table
  xrays = list(class = classes,
                NAs = NAs,
                Infs = Infs,
                NaNs = NaNs,
                negs = negs,
                zeros = zeros,
                means = means,
                medians = medians,
                nlevel1 = nlevel1,
                nlevel2 = nlevel2,
                unique.vals = unique.vals)

  xray = rbindlist(xrays, idcol = T)
  xray = unique(xray, by = NULL)

  # get second row of nlevel duplicates, which are the rows that you want
  mylevels = xray[duplicated(.id)]
  mylevelnames = mylevels[, .id]
  xray = xray[.id %nin% mylevelnames, ]
  xray = rbind(xray, mylevels)

  # clean up
  xray = xray %>% t %>% as.data.table(keep.rownames = T)
  newnames = xray[1, ] %>% as.character
  newnames[1] = "var"
  setnames(xray, newnames)
  xray = xray[-1,]

  xray[, `:=`(
    means = means %>% as.numeric %>% round(2),
    medians = medians %>% as.numeric %>% round(2),
    nlevel1 = as.integer(nlevel1),
    nlevel2 = as.integer(nlevel2)
  )]

  xray[, unused.levels := nlevel1 - nlevel2]
  setnames(xray, c("var", "nlevel1"), c("variable", "levels"))

  xray[, variable := tolower(variable)]
  xray[, colidx := .I]

  mynums = c("NAs", "Infs", "NaNs", "negs", "zeros", "medians", "levels", "unused.levels", "unique.vals")
  xray[, (mynums) := lapply(.SD, as.numeric), .SDcol = mynums]

  xray = xray[, .(colidx, variable, class, NAs, Infs, NaNs, negs, zeros, means, medians, levels, unused.levels, unique.vals)]
  setorder(xray, variable)


  # find fields with (almost) no variance
  xray[, `:=` (
    no.var = {
      blank.rate = (NAs + Infs + NaNs) / nrow(mydf)
      junk = ifelse(unique.vals == 1 |
                      blank.rate > 0.99 |
                      unique.vals == nrow(mydf),
                    "*", ".")
    }
  )]

  cat("Number of rows (records) = ", nrow(mydf), "\n")
  cat("Number of columns (variables) = ", ncol(mydf), "\n\n")

  cat("Number of empty rows (all variables empty over records) = ", sum(mydf[, rowSums(is.na(mydf))] == ncol(mydf)), "\n")
  cat("Number of empty columns (all records empty over variable) = ", sum(colSums(is.na(mydf)) == nrow(mydf)), "\n")

  a = nrow(mydf)
  b = unique(mydf, by = NULL) %>% nrow

  cat("Duplicated records = ", a - b, "\n\n")

  cat("Variable is marked with '*' if: 99% blank values (NA, Inf, NaN), 1 unique value or number of values == nrows.\n\n")

  return(xray)
}
