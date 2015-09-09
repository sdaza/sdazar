#' @title Create missing data indicators
#' @description Create scores: mean or if any of the variables have specific values.
#' @param dat `data.table` object.
#' @param vars Character vector with name of variables. 
#' @return `data.table` with indicator variables. Missing data are replaced by the mean (numeric and interger) and the mode (factors).
#' @examples
#' a <- c(NA,2,3,4, NA)
#' b <- c(2,3,4,NA, 3)
#' c <- c(1,2,3,NA, 1)
#' dat <- data.table(a,b,c)
#' vars <- names(dat)
#' mi(dat, vars)
mi <- function(dat, vars) {

dat <- data.table(dat)
ivars <- paste0("i_", vars) 
mivars <- paste0("mi_", vars) 


for (i in 1:length(vars)) {

if ( is.numeric(dat[, vars[i], with = FALSE][[1]]) ) {

  dat[, mivars[i] := ifelse(is.na(dat[, vars[i], with = FALSE]), 1, 0)]
  varmean <- sdazar::Mean(dat[, vars[i], with = FALSE][[1]])
  dat[, ivars[i] :=  dat[, vars[i], with = FALSE]]
  dat[eval(parse(text = paste0(mivars[i], " == 1"))), ivars[i] := varmean]

}

if ( is.integer(dat[, vars[i], with = FALSE][[1]]) ) {

  dat[, mivars[i] := ifelse(is.na(dat[, vars[i], with = FALSE]), 1, 0)]
  varmean <- round(sdazar::Mean(dat[, vars[i], with = FALSE][[1]]), 0)
  dat[, ivars[i] :=  dat[, vars[i], with = FALSE]]
  dat[eval(parse(text = paste0(mivars[i], " == 1"))), ivars[i] := varmean]

}

 if ( is.factor(dat[, vars[i], with = FALSE][[1]]) ) {

  dat[, mivars[i] := ifelse(is.na(dat[, vars[i], with = FALSE]), 1, 0)]
  v <- dat[, vars[i], with = FALSE][[1]]
  mode <- as.numeric(names(table(v))[which.max(table(v))]) 
  dat[, ivars[i] :=  dat[, vars[i], with = FALSE]]
  dat[eval(parse(text = paste0(mivars[i], " == 1"))), ivars[i] := as.character(mode)]

}}

return(dat)

}
