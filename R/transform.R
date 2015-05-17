#' @title Wrapped functions for transformation of variables.
#' @description Wrapped functions for transformation of variables.
#' @param dat Data.table or data.frame object.
#' @param var Character or vector of characters with variables names to transform.
#' @return Data.table object with transformed variables. 
#' @examples
#' factors(dat, c("var1", "var2"))
factors <- function(dat, cnames) {
  dat <- dat[ , cnames := lapply(dat[ , cnames, with=FALSE], as.factor), with=FALSE]
}

numerics <- function(dat, cnames) {
  dat <- dat[ , cnames := lapply(dat[ , cnames, with=FALSE], as.numeric), with=FALSE]
}

