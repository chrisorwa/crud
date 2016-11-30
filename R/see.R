#' Plot summary of numerical vectors
#'
#' Numerical vectors in a data.table are plotted in 3 different ways: density, violin and cumulative.
#' The plots are then combined.
#' This function is inspired on the DescTools package. The motivation for this function is to avoid base plots in R as I find them clunky.
#' @param mydf a data.table object. If it isn't a data.table, it will be converted into one.
#' @return plots: density, box and cumulative empirical distribution.
#' @export
#' @examples
#' data(mtcars)
#' see(mtcars)


see = function(mydf) {

  # check whether this is a data.table
  if (!is.data.table(mydf)) {
    warning("sorry, I want a data.table, converting...")
    mydf = as.data.table(mydf, keep.rownames = T)
    message("plotting summaries of numerical vectors...")
  }else{
    message("plotting summaries of numerical vectors...")
  }

  theme_set(theme_bw())

  # find numerical class
  myclasses = mydf[, lapply(.SD, class)] %>% t %>% as.data.table(keep.rownames = T)
  numcols = myclasses[V1 == "numeric" | V1 == "integer", rn]

  foreach(i = 1:length(numcols)) %do% {

    x = mydf[, numcols[i], with = FALSE]
    setnames(x, "param")

    mymedian = median(x[, param])
    mymean = mean(x[, param])
    mymodus = modus(x[, param])
    mymin = min(x, na.rm = T) %>% floor
    mymax = max(x, na.rm = T) %>% ceiling

    dens = ggplot(x, aes(x = param, y = ..density..)) +
      geom_density(col = NA) +
      geom_line(stat = "density", col = "steelblue") +
      xlim(mymin, mymax) +
      geom_vline(xintercept = mymedian, linetype = "dotted", col = "steelblue") +
      geom_vline(xintercept = mymean, linetype = "dashed", col = "tomato2") +
      labs(x = "", y = "", title = numcols[i], subtitle = "dotted blue line = median, dashed red line = mean")

    box = ggplot(x, aes(as.factor("a"), y = param)) +
      geom_boxplot(col = "steelblue", alpha = 0.4, width = 0.3) +
      geom_rug(col = "grey50") +
      geom_hline(yintercept = mymean, col = "tomato2", linetype = "dashed") +
      labs(x = "", y = "") +
      ylim(mymin, mymax) +
      theme(axis.text.y = element_blank() ) +
      coord_flip()

    cumul = ggplot(x, aes(param)) +
      stat_ecdf(col = "steelblue") +
      geom_rug(col = "grey50") +
      labs(x = "", y = "") +
      xlim(mymin, mymax) +
      scale_y_continuous(labels = percent)

    strip = ggplot(x, aes(as.factor("a"), param)) +
      geom_jitter(shape = 1, col = "steelblue", width = 0.1) +
      theme(axis.text.y = element_blank()) +
      labs(x = "", y = "") +
      ylim(mymin, mymax) +
      coord_flip()


    gA = ggplotGrob(dens)
    gB = ggplotGrob(box)
    gC = ggplotGrob(cumul)
    gD = ggplotGrob(strip)
    grid::grid.newpage()
    grid::grid.draw(rbind(gA, gD, gB, gC))


    }


}
