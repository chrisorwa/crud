#' Find association between ordinal variables
#'
#' Finds categorical values, calculates pairwise Cramer V and filters for p < 0.05.
#' @param data data.table object
#' @param cutoff value for P cutoff, default = 0.05
#' @export
#' @examples
#' catassoc(df)


catassoc = function(data, cutoff = 0.05) {

  if (!is.data.table(data)) {
    warning("Sorry, I want a data.table, converting...")
    setDT(data)
  }

  # find categorical variables
  classes = lapply(data, class)
  classes = (classes == "factor" | classes == "character")
  classes = classes[classes == T] %>% names  # names of factor or character vars

  data.fac = data[, classes, with = F]

  # calculate Cramer V for each pair
  g = expand.grid(classes, classes) %>% setDT

  message("Calculating strength of association ...")

  l = foreach(i = 1:nrow(g)) %do% {

    message(paste("...calculating association strength for pair", i))
    arg1 = data[, eval(g[i, Var1]), with = F] %>% unlist
    arg2 = data[, eval(g[i, Var2]), with = F] %>% unlist
    tab = table(arg1, arg2) %>% assocstats
    cramer = tab$cramer
    p = tab$chisq_tests[1,][3]

    output = list(
      cramer = cramer,
      p = p
    )

  }

  # cleanup, filter for signif p
  h = cbind(g, rbindlist(l))[p <= cutoff & Var1 != Var2,]
  h[, cramer := round(cramer, 4)]
  h = h[!duplicated(h, by = "cramer")][cramer > 0.5,][order(cramer)]
  h[, varpair := paste(Var1, Var2, sep = " - ") %>% factor %>% fct_inorder ]

  # plot
  message("...plotting")

  catassoc.plot = ggplot(h, aes(varpair, cramer, sep = " -")) +
    geom_point() +
    coord_flip() +
    labs(title = "Strength of association",
         subtitle = "Calculated Cramer V by nominal variable pair,
cutoff for P = 0.05",
         x = "", y = "Cramer V association",
         caption = "")
  print(catassoc.plot)

  # close out
  # h[, varpair := NULL]

  message("done.")

  return(list(
    h = h,
    catassoc.plot = catassoc.plot
  ))

}

