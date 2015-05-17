#' @title Recode variables.
#' @description Recode variables.
#' @param dat Datatable object.
#' @param var Character or vector of characters with variable names.
#' @param newvar Character or vector of characters with new variable names.
#' @param old Old values.
#' @param new New values (1 to 1 recoding).
#' @return Data frame with recoded variables.
#' @details Avoid define the first value as missing, put missing values at the end of the old/new specification
#' @examples
#' lrecode(dat, c("var1", "var2"), c("nvar1", "nvar2"), c(1,2,3,4), c(4,3,2,1))
lrecode <- function(dat, var, newvar, old, new) {

if (length(var) == length(newvar) & length(old) == length(new)) {

for (i in 1:length(var)) {

  for (h in 1:length(old)) { 

  a <- paste0(var[i], "==", old[h]) 
  b <- paste0(newvar[i], ":= ", new[h])
  dat[eval(parse(text=a)), eval(parse(text=b))]
    
    }
 }
 return(dat)
} 

else {
  stop("var or value list doesn't have the same size")

}
}