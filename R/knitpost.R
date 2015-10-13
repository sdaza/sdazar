#' @title Create posts from Rmd files for blogging.
#' @description Create posts from .Rmd files for blogging.
#' @param input Path of Rmd file.
KnitPost <- function(input, base.url="", fig.path = NULL) {
  knitr::opts_knit$set(base.url = base.url)

  # knitr::opts_chunk$set(comment = NA)
  knitr::opts_chunk$set(fig.path = fig.path)
  knitr::opts_chunk$set(fig.cap = "center")

  knitr::render_jekyll()
  knitr::knit(input, envir = parent.frame())
}
