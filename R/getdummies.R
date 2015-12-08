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
getdummies <- function(dat, vars) {

  # checks
  stopifnot(is.data.table(dat))
  stopifnot(vars %in% names(dat))

  # create lists for loop
  inds <- list()
  nvars <- list()

  for (i in 1:length(vars)) {
    inds[[i]] <- na.omit(unique(dat[, vars[i], with = FALSE]))[[1]]
    nvars[[i]] <- paste0(vars[i], "_", inds[[i]])
    dat[, (nvars[[i]]) := lapply(inds[[i]], function(x) as.numeric(get(vars[i]) == x))]
  }

  # print variables names to check
  print(nvars)
  return(dat)

}
