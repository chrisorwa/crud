#' Timeplot using dygraph library
#'
#' This function is wrapper for the dygraphs library.
#' The purpose is to avoid code repeats for options that you always want to have anyway, such as range selectors etc.
#'
#' @param mydf a time series, data.frame or data.table. The first column must hold the x-axis values. All other columns will be considered to be y values.
#' @param ylabel the label that you want to use for your y-axis
#' @param mainlabel the title of your plot
#' @param roll rolling average, default = 1 (no rolling average).
#' @export
#' @return dygraph time plot
#' @examples
#' lungDeaths = cbind(mdeaths, fdeaths)
#' dyplot(lungDeaths, ylabel = "Number of deaths", mainlabel = "Lung deaths", roll = 7)



dyplot = function(mydf, ylabel = "count", mainlabel = "timeplot", roll = 1) {

  require(dygraphs)

  dyp = dygraph(mydf, ylab = ylabel, main = mainlabel) %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE) %>%
    dyRangeSelector() %>%
    dyRoller(rollPeriod = roll) %>%
    dyOptions(strokeWidth = 1.2)

  return(dyp)
}
