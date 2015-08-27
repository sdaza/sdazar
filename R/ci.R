#' @title Computes confidence interval (CI)
#' @description Computes upper and lower confidence intervals of a variable
#' @param x A numeric vector.
#' @param type `upper` or `lower` limit of the confidence interval (default = `upper`).
#' @param cl Confidence level (default = 0.95)
#' @return Returns one of the limits of the confidence interval.
#' @examples
#' x <- runif(100, 0)
#' ci(x)
#' ci(x, type = "lower")
ci <- function(x, type = "upper", cl = 0.95) {

  (n <- length(na.omit(x)))

  (stderr <- sqrt( var(x) / n ))
  (smean <- sdazar::Mean(x))
  (pp <- cl + (1 - cl) / 2 )
  (df <- n - 1)

  if ( type == "upper") { fci <- smean + qt(pp, df = df) * stderr }
  if ( type == "lower") { fci <- smean - qt(pp, df = df) * stderr }

  return(fci)
}

