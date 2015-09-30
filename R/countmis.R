#' @title Count missing data.
#' @description Function to count missing data.
#' @param dat Data frame. 
#' @param pct Logical. If this is \code{TRUE} will show percentage of missing data by variable.
#' @param exclude.complete Logical. Exclude complete variables from the output.
#' @examples
#' countmis(dat, pct = FALSE)
countmis  <- function(dat, pct = TRUE, exclude.complete = TRUE) {
  
mis <- sort( sapply(dat, function(x) sum(is.na(x))), decreasing = TRUE)

if (exclude.complete == TRUE) {
mis <- mis[mis>0]
}

if (pct == FALSE) { return(mis) } 
else if ( pct == TRUE ) { return( round(mis / nrow(dat), 3)) } 

}

