#' Set up environment
#'
#' Detect cores and set up cluster.
#' Sets locale.
#' Sets ggplot2 defaults.
#' @param none no parameters required
#' @return none
#' @export
#' @examples
#' setup()

setup = function(trunk = trunk, create.dirs = T) {

  if (!exists("trunk")) {
    stop("define path (trunk) first.")
  }

  setwd(trunk)
  in.dir = paste(trunk, "in", sep = "/")
  out.dir = paste(trunk, "out", sep = "/")
  r.dir = paste(trunk, "R", sep = "/")
  admin = paste(trunk, "admin", sep = "/")
  docs = paste(trunk, "docs", sep = "/")

  if (!dir.exists(r.dir) & create.dirs) {
    dir.create(in.dir)
    dir.create(out.dir)
    dir.create(r.dir)
    dir.create(admin)
    dir.create(docs)
  }else{
    message("in.dir, out.dir and r.dir already available.")
  }


  # set locale to avoid encoding issues
  message("...setting environment")
  Sys.setlocale("LC_ALL", "C")


  # setup parallel backend
  message("...detecting cores
          and setting up parallel backend")
  cl = makeCluster(4)
  registerDoParallel(cl)


  # ggplot2 defaults
  message("...setting up ggplot defaults")
  theme_set(theme_bw())

  theme_update(plot.title = element_text(size = 12),
               plot.subtitle = element_text(size = 10, color = "grey40"),
               plot.caption = element_text(size = 8, hjust = 0, color = "grey40"))

  update_geom_defaults("line", list(colour = "steelblue"))
  update_geom_defaults("boxplot", list(colour = "steelblue", notch = TRUE, alpha = 0.5, width = 0.5))
  update_geom_defaults("point", list(colour = "steelblue"))
  update_geom_defaults("density", list(colour = "grey", fill = "steelblue", alpha = 0.3))
  update_geom_defaults("bar", list(colour = "grey50", fill = "steelblue", alpha = 0.5, width = 0.5))
  update_geom_defaults("step", list(colour = "steelblue"))

  output = list(
    in.dir = in.dir,
    out.dir = out.dir,
    r.dir = r.dir
  )

  return(output)

}
