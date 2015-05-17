#' @title Look variables in a dataframe using regular expressions.
#' @description The \code{getvars} function gets ACS variable names from text containing formulas with addition, substraction, and/or division operators.
#' @param dat data.frame object.  Character or vector of characters with formulas using ACS variable names.
#' @param varnames Character or vector with regular expressions containing variables names. 
#' @return Returns the variables names of the variables found.
#' @examples
#' lookvar(dat, "gender")
lookvar  <- function(dat, varnames) {
  n  <- names(dat)
  nn  <- list()
    for (i in 1:length(varnames)) {
      nn[[i]]  <- grep(varnames[i],n)
    }
  
  nn  <- unlist(nn)

  if ( length(nn) >0 )
  {
    r  <- n[nn]
    return(r)
  }
  else
  { return("No variables found")}
}
