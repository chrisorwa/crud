#' Confusion matrix
#'
#' Present accuracy statistics of trues against predictions.
#' Based on http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
#' @details
#' acc = accuracy = (TP + TN) / total
#'
#' err = error rate = 1 - accuracy
#'
#' tpr = true positive rate = TP / (TP + FN)
#'
#' tnr = true negative rate = TN / (TN + FP)
#'
#' fpr = false positive rate = FP / (FP + TN)
#'
#' fnr = false negative rate = FN / (FN + TN)
#'
#' ppv = positive predictive value = TP / (TP + FP)
#'
#' npv = negative predictive value = TN / (TN + FN)
#'
#'
#' Synonyms:
#' true positive rate: {sensitivity, recall}
#'
#' true negative rate: {specificity}
#'
#' false positive rate: {fallout}
#'
#' false negative rate: {miss}
#'
#' positive predictive value: {precision}
#' @examples
#' set.seed(0)
#' actual = c('a','b','c')[runif(100, 1,4)] # actual labels
#' predicted = actual # predicted labels
#' predicted[runif(30,1,100)] = actual[runif(30,1,100)]  # introduce incorrect predictions
#' cm = as.matrix(table(Actual = actual, Predicted = predicted)) # create the confusion matrix
#' conf(actual, predicted)
#' @param trues actual outcomes
#' @param preds predicted outcomes
#' @return conf confusion matrix and stats
#' @export
#'
confmat = function(trues, preds) {

  # define basic variables

  # confusion matrix
  cm = table(trues, preds)

  # basic stats
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes

  # metrics
  accuracy = sum(diag) / n

  precision = diag / colsums
  recall = diag / rowsums
  f1 = 2 * precision * recall / (precision + recall)

  # macro metrics
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)

  data.frame(macroPrecision, macroRecall, macroF1)

  # kappa metric
  expAccuracy = sum(p*q)
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)

  # prevalence
  prevalence = prop.table(cm) %>% rowSums

  # close out
  perclass.metrics = rbind(precision, recall, f1, prevalence) %>% round(2)
  macro.metrics = rbind(macroPrecision, macroRecall, macroF1, accuracy, expAccuracy, kappa) %>% round(2)
  colnames(macro.metrics) = "value"

  return(list(confusion.matrix = cm,
              per.class = perclass.metrics,
              macro.metrics = macro.metrics))

}
