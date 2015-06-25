#' @title Identify outliers.
#' @description Function to identify outliers
#' @param c Threshold value.
#' @param values Logical. If it is \code{TRUE}, will show outlier values.
#' @examples
#' outliers(x = c(1,2,3,4,100,21000,20021, -122332), values = TRUE)
outliers <- function(x, c = 1.5, values = FALSE) {
  x <- na.omit(x)
  q1 <- quantile(x, .25)
  q3 <- quantile(x, .75)
  iqd <- q3 - q1
  val <- x[ (x <= (q1 - c * iqd))  | (x >= ( q3 + c * iqd))]
  ind <- which((x <= (q1 - c * iqd)) | (x >= ( q3 + c * iqd)))

  if (values == TRUE) {
    return(val)
  }
  else {
    return(ind)
  }
}
