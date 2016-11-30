#' Take a stratified sample of your dataset
#'
#' Takes a character or factor variable and calculates the frequencies.
#' @param data.table object
#' @param var The variable to base your stratified sample on
#' @param fraction The fraction of the nrows of the dataset to sample
#' @param split Split dataset into a training set and a test set? Default = T
#' @return stratified sample of {data, var}, size nrow * fraction
#' @examples
#' data(mtcars)
#' mtcars = as.data.table(mtcars, keep.rownames = T)
#' stratsamp(mtcars, "cyl", 0.3, split = F)[]
#' @export


stratsamp = function(data, var, fraction = 0.8, split = T) {

  if (!is.data.table(data)) {
    message("Sorry, I want a data.table, converting...")
    setDT(data, keep.rownames = T)
  }

  data = copy(data)
  data[, idx := .I, by = var]
  samplesize = data[, .(s = as.integer(.N * fraction)), by = var]

  x = foreach(i = 1:length(samplesize$s), .combine = c) %do% {
    data[var == samplesize[i, var], sample(idx, samplesize[i, s])]
  }

  train = data[idx %in% x]
  test = data[idx %nin% x]

  train[, idx := NULL]
  test[, idx := NULL]

  return(list(
    train = train,
    test = test))

}
