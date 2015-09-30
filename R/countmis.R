#' @title Count missing data.
#' @description Function to count missing data.
#' @param dat Data frame. 
#' @param vars Vector with name of variables. 
#' @param pct Logical. If this is \code{TRUE} will show percentage of missing data by variable.
#' @param exclude.complete Logical. Exclude complete variables from the output.
#' @examples
#' a <- c(NA,2,3,4, NA)
#' b <- c(2,3,4,NA, 3)
#' c <- c(1,2,3,1, 1)
#' dat <- data.table(a,b,c)
#' countmis(dat, pct = FALSE)
countmis  <- function(dat, vars = NULL, pct = TRUE, exclude.complete = TRUE) {

if (is.null(vars)) {
  vars <- names(dat)
}

mis <- sort( sapply(dat[, vars, with = FALSE], function(x) sum(is.na(x))), decreasing = TRUE)

if (exclude.complete == TRUE) {
mis <- mis[ mis > 0]
}

if (pct == FALSE) 
  { return(mis) } 

else if ( pct == TRUE ) 
  { return( round(mis / nrow(dat), 3)) } 

return(mis)

}
