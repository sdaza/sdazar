#' @title Arithmetic and statistiscal functions but dealing with missing data.
#' @description Arithmetic and statistiscal functions but dealing with missing data.
#' @param x Numeric vector.
#' @examples
#' Mean(c(1,2,4, NA))
#' @name operations
NULL
#> NULL

#' @rdname operations
Mean <- function(x) as.numeric(mean(x, na.rm=TRUE))

#' @rdname operations
Sd <- function(x) as.numeric(sd(x, na.rm=TRUE))

#' @rdname operations
Sum <- function(x) sum(x, na.rm=TRUE)

#' @rdname operations
Sums <- function(x) {

if (sum(is.na(x))==length(x)) {
    y <- NA
    }

else {y <- sum(x, na.rm=TRUE)}

return(as.numeric(y))
  }

#' @rdname operations
Max  <- function(x) {
  if (class(x) == "integer") {
  ifelse(all(is.na(x)), as.integer(NA) , max(x, na.rm = TRUE))
  }
  else {
  ifelse(all(is.na(x)), as.numeric(NA) , max(x, na.rm = TRUE))
  }
}

#' @rdname operations
Min  <- function(x) {
  if (class(x) == "integer") {
  ifelse(all(is.na(x)), as.integer(NA) , min(x, na.rm = TRUE))
  }
  else {
  ifelse(all(is.na(x)), as.numeric(NA) , min(x, na.rm = TRUE))
  }
}
