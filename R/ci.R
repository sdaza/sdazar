#' @title Computes confidence intervals (function for tables)
#' @description Upper and lower limit of a confidence interval.
#' @param x Numeric vector.
#' @param cl Level of confidence,
#' @examples
#' x <- runif(100, 0)
#' uci(x)
#' lci(x)
#' @name ci
NULL
#> NULL


#' @rdname ci
uci <-  function(x, cl = 0.95) {

  (n <- length(na.omit(x)))

  stderr <- sqrt( var(x, na.rm = TRUE) / n )
  smean <- sdazar::Mean(x)
  pp <- cl + (1 - cl) / 2
  df <- n - 1

  fci <- smean + qt(pp, df = df) * stderr
  return(fci)
}


#' @rdname ci
lci <-  function(x, cl = 0.95) {

  (n <- length(na.omit(x)))

  stderr <- sqrt( var(x, na.rm = TRUE) / n )
  smean <- sdazar::Mean(x)
  pp <- cl + (1 - cl) / 2
  df <- n - 1

  fci <- smean - qt(pp, df = df) * stderr
  return(fci)
}
