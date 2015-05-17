#' @title Formulas for linear models.
#' @description Formulas for linear models.
#' @param var1 Dependent variable (character).
#' @param vars2 Independent variables (character).
#' @param sym1 First operator (between dependent and independent variables)
#' @param sym2 Second operator (between independent variables)
#' @return Formula object.
#' @examples
#' # formulas(c("var1", "var2", "~", "+"))
formulas <- function(var1, vars2, sym1 = "~", sym2 = "+") {
f <- as.formula(paste0( paste0(var1, sym1), paste0(vars2, collapse = sym2)))
return(f)
}
