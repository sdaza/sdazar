#' @title Create dummy variables.
#' @description Create dummy variables from a data.table.
#' @param dat `data.table` object.
#' @param vars Character vector with name of variables.
#' @return data.table with dummy variables.
#' @examples
#' a <- c(NA,2,3,4, NA)
#' b <- c(2,3,4,NA, 3)
#' c <- c(1,2,3,NA, 1)
#' dat <- data.table(a,b,c)
#' vars <- names(dat)
#' getdummies(dat, "a")
getdummies <- function(dat, vars){
  stopifnot(is.data.table(dat))
  stopifnot(vars %in% names(dat))
  factors(dat, vars)

  dat[, paste0(vars, "." , levels(get(vars)))] -> new.names
  dat[, (new.names) := transpose(lapply(get(vars),
                                        FUN = function(x) { as.numeric(x == levels(get(vars)))})) ]
  cat(paste("\nNew variables: ", paste0(new.names, collapse = ", ")))
  return(dat)
}
