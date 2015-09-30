#' @title Create missing data indicators
#' @description Create missing data indicators for varaibles with missing data.
#' @param dat `data.table` object.
#' @param vars Character vector with name of variables. 
#' @return `data.table` with indicator variables. Missing data are replaced by the mean (numeric and interger) and the mode (factors).
#' @examples
#' a <- c(NA,2,3,4, NA)
#' b <- c(2,3,4,NA, 3)
#' c <- c(1,2,3,NA, 1)
#' dat <- data.table(a,b,c)
#' vars <- names(dat)
#' mi_indicator(dat, vars)
mi_indicator <- function(dat, vars = NULL) {

  if (is.null(vars)) {
    vars <- names(dat)
  }

  dat <- data.table(dat)
  nvars <- names(countmis(dat, vars, exclude.complete = TRUE))
  mivars <- paste0("mi_", nvars) 

  for (i in 1:length(nvars)) {

    dat[, mivars[i] := ifelse(is.na(dat[, nvars[i], with = FALSE]), 1, 0)]

  }

  return(dat)

  }
