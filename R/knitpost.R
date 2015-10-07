#' @title Create posts from Rmd files for blogging.
#' @description Create posts from .Rmd files for blogging.
#' @param input Path of Rmd file.
KnitPost <- function(input, base.url="", fig.path = NULL) {
  opts_knit$set(base.url = base.url)

  opts_chunk$set(comment = NA)
  opts_chunk$set(fig.path = fig.path)
  opts_chunk$set(fig.cap = "center")

  render_jekyll()
  knit(input, envir = parent.frame())
}
