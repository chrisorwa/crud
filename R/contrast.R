#' Contrast calculate and visualise measures of inequality and concentration
#'
#' @param x a numerical vector
#' @return Lorentz curve plot and inequality/concentration measures
#' @examples
#' data(mtcars)
#' contrast(mtcars$hp)
#' @export
#'
#' @section Explanation:
#' For a supplied vectors of numbers, the following measures are calculated
#' \itemize{
#'     \item Lorentz curve
#'     \item Pen's parade
#'     \item Gini index
#'     \item Theil index
#'     \item Herfindahl
#'     \item Hoover index
#' }
#'
#' @section Details:
#' The \href{https://en.wikipedia.org/wiki/Lorenz_curve}{Lorentz curve} shows the cumulative percentage of a variable against the cumulative percentage of the population.
#' E.g. 20 percent of the population has 10 percent of the total income.
#' A larger 'sag' in the line means larger inequality.
#'
#' In \href{http://www.theatlantic.com/magazine/archive/2006/09/the-height-of-inequality/305089/}{Pen's parade} the values are sorted from low to high.
#' The plot shows the share of the population against the average for the value.
#' The average is indicated by a dotted line.
#' It can be visualised as a parade of people which lasts one hour, starting with the smallest values (e.g. incomes).
#'
#' The \href{https://en.wikipedia.org/wiki/Gini_coefficient}{Gini index} summarises the inequality that is visualised in the Lorentz curve.
#' The index is calculated by dividing the area between the dotted line (perfect equality) and the Lorentz line by the total area under the dotted line.
#'
#' The \href{https://en.wikipedia.org/wiki/Income_inequality_metrics}{Hoover index} is defined as the maximum vertical distance between the dotted line and the Lorentz curve.
#' It is shown as a dotted line in the Lorentz curve (percentages).
#' The Hoover index is the proportion of all income which would have to be redistributed to achieve a state of perfect equality.
#'
#' The \href{https://en.wikipedia.org/wiki/Income_inequality_metrics}{Palma ratio} is the ratio of the 40 percent quantile and the 10 percent quantile.
#' The Palma ratio is thought to overcome some weaknesses of the Gini index (insensitivity for top and bottom end of the line).
#'
#' The \href{https://en.wikipedia.org/wiki/Theil_index}{Theil index} is the maximum possible entropy of the data minus the observed entropy.
#'
#' The \href{https://en.wikipedia.org/wiki/Herfindahl_index}{Herfindahl index} is a measure of concentration.
#' In its original context the Herfindahl index measures the competition between firms, with a value of larger than 0.25 considered as critical (US).

contrast = function(x) {

  if (class(x) == "factor") {
    warning("converting factor variable to numeric.")
    x = as.numeric(x)
  }

  if (class(x) == "character") {
    x = factor(x) %>% as.numeric
    warning("converting character variable to numeric via factor.")
  }

  if (class(x) == "logical") {
    stop("sorry, can't do anything with class logical.")
  }

  if (class(x) == "POSIX.ct") {
    stop("sorry, can't do anything with dates.")
  }


  # set up for plotting =======

  theme_set(theme_bw())

  theme_update(plot.title = element_text(size = 12),
               plot.subtitle = element_text(size = 10, color = "grey40"),
               plot.caption = element_text(size = 8, hjust = 0, color = "grey40"))

  update_geom_defaults("step", list(colour = "steelblue"))
  update_geom_defaults("line", list(colour = "steelblue"))

  # Lorentz curve ============

  lorentz = Lc(x)

  dfl = data.table(
    p = lorentz$p,
    L = lorentz$L,
    L.general = lorentz$L.general
  )


  # Measures of concentration ==============

  # hoover index -------------
  dfl[, delta := p - L]
  hoover.index = dfl[, max(p-L)]
  position = dfl[delta == hoover.index, p]
  hoover.idx = round(hoover.index, 3)

  gini.idx = Gini(x, na.rm = T) %>% round(3)
  theil.idx = Theil(x, na.rm = T) %>% round(3)
  herfindahl.idx = Herfindahl(x, na.rm = T) %>% round(3)

  palma.ratio = (quantile(x, probs = 0.1, names = F, na.rm = T) / quantile(x, probs = 0.4, names = F, na.rm = T) ) %>% round(3)


  lorentz1 = ggplot(dfl) +
    geom_line(aes(p, L)) +
    geom_abline(slope = 1, linetype = "dashed", col = "grey50") +
    geom_vline(xintercept = position, linetype = "dotted", col = "grey50") +
    scale_x_continuous(label = percent) +
    scale_y_continuous(label = percent) +
    labs(x = "cumulative percentage of population",
         y = "cumulative percentage of variable",
         title = "Lorentz curve, percentages",
         subtitle = paste("Visualises inequality: the more depressed the line is, the larger the inequality.
Hoover index (vertical dotted line):", hoover.idx, "Gini index:", gini.idx, "Palma ratio:", palma.ratio))


  raw = data.table(x)[order(x),]
  raw[, idx := .I]

  cumul = ggplot(raw) +
    stat_ecdf(aes(x)) +
    geom_rug(aes(x), col = "grey50") +
    geom_vline(xintercept = mean(raw$x), linetype = "dotted", col = "grey50") +
    scale_y_continuous(label = percent) +
    labs(x = "value", y = "cumulative percentage",
         title = "Cumulative distribution",
         subtitle = paste("Vertical dashes at x-axis show individual observations.
Mean (vertical dotted line):", mean(x, na.rm = T) %>% round(3), "median:", median(x, na.rm = T) %>% round(3)))


  # Pen's parade ============================

  p = as.data.table(x)
  p = p[order(x), ]
  p[, idx := .I]
  p[, `:=` (
    i.n = idx/max(idx),
    xi.x = x / mean(x)
  )]
  p[, minutes := scales::rescale(idx, to = c(1, 60))]

  subtitle = "The parade takes 1 hour and starts with the lowest values.
The spectator is thought to have the average (height) (dotted line)"

  pen1 = ggplot(p) +
    geom_step(aes(i.n, xi.x)) +
    geom_hline(yintercept = 1, linetype = "dashed", col = "grey50") +
    scale_x_continuous(label = percent) +
    labs(x = "percentage of population",
         y = "value / average value",
         title = "Pen's parade, percentages",
         subtitle = subtitle)

  pen2 = ggplot(p) +
    geom_step(aes(minutes, x)) +
    geom_hline(yintercept = mean(x, na.rm = T), linetype = "dashed", col = "grey50") +
    scale_x_continuous(breaks = seq(0, 60, 15)) +
    labs(x = "minutes (hypothetical)",
         y = "value",
         title = "Pen's parade, raw",
         subtitle = subtitle)

  plots = arrangeGrob(lorentz1, cumul, pen1, pen2)
  plot(plots)

}
