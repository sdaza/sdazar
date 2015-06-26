#' @title Create posts from Rmd files for blogging.
#' @description Create posts from .Rmd files for blogging.
#' @param input Path of Rmd file.
KnitPost <- function(input, base.url="/") {
  opts_knit$set(base.url = base.url)
  
  fig.path <- paste0("figs/", sub(".Rmd$", "", basename(input)), "/")
  opts_chunk$set(fig.path = fig.path)
  opts_chunk$set(fig.cap = "center")

  render_jekyll()
  knit(input, envir = parent.frame())
}