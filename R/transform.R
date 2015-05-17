#' @title Wrapped functions for transformation of variables.
#' @description Wrapped functions for transformation of variables.
#' @param dat Data.table or data.frame object.
#' @param vars Character or vector of characters with variables names to transform.
#' @return Data.table object with transformed variables. 
#' @examples
#' factors(dat, c("var1", "var2"))
#' @name transform
NULL
#> NULL

#' @rdname transform
factors <- function(dat, vars) {
  dat <- dat[ , vars := lapply(dat[ , vars, with=FALSE], as.factor), with=FALSE]
}

#' @rdname transform
numerics <- function(dat, vars) {
  dat <- dat[ , vars := lapply(dat[ , vars, with=FALSE], as.numeric), with=FALSE]
}