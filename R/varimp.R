#' Variable importance
#'
#' The Boruta algorithm is a wrapper for randomForest.
#' It shuffles variable values (shadows) and then compares whether the randomForest prediction improves the predictive value compared to that shadow value.
#' A variable importance plot is produced including the shadow variables.
#'
#' @param mydf data.table
#' @return variable importance plot
#' @export
#' @examples
#' varimp(df, "myclass")


varimp = function(mydf, predictor) {

  x = mydf[, names(mydf) != predictor, with = F]
  y = mydf[, status]

  varimp = Boruta(x, y, doTrace = 2)

  varimp$finalDecision %>%
    as.data.table(keep.rownames = T) %>%
    setnames(c("variable", "decision")) %>%
    setorder(variable) %>%
    print

  vars = varimp$ImpHistory %>% as.data.table

  m = vars[, idx := .I] %>%
    melt(id.var = "idx") %>%
    setorder(variable, value)

  mf = m[, mean(value), by = variable][order(V1)][, factor(variable) %>% fct_inorder]
  m[, variable := factor(variable, levels = mf )]

  varimp.plot = ggplot(m) +
    geom_boxplot(aes(variable, value)) +
    coord_flip() +
    labs(x = "", y = "Z score",
         title = "Variable importance",
         subtitle = "The importance of variables for the prediction of functional status of water wells, sorted by importance.")

  plot(varimp.plot)

  return(varimp.plot)


}

