#' @title Create scores
#' @description Create scores: mean or if any of the variables have specific values.
#' @param dat \code{data.table} or \code{data.frame}.
#' @param vars Character vector with name of variables.
#' @param type Type of score (\code{mean}, \code{any}, \code{parcel}). \code{parcel} selects items based on \code{p}. Once the items are selected the average with the available items is computed.
#' @param p  Proportion of items with valid values to compute the average.
#' @param nitems Number of items with valid values to compute the average.
#' @param values Values used to define \code{any} index
#' @return Returns a vector with scores.
#' @examples
#' a <- c(NA,2,3,4, NA)
#' b <- c(2,3,4,NA, 3)
#' c <- c(1,2,3,NA, 1)
#' dat <- data.table(a,b,c)
#' vars <- names(dat)
#' rowscore(dat, vars)
rowscore <- function (dat, vars, type = "mean", p = 1/2, nitems = NULL, val = NULL) {

temp <- data.table(dat)
temp <- temp[, vars, with = FALSE]

  if (is.null(val) & type == "any") {
    stop("`val` need to be specified wheh using `any`")
  }
  if (!is.character(vars)) {
    stop("`vars` has to be a character vector")
  }
  if (!is.na(p) & is.null(nitems)) {
  temp[, z := apply(temp, 1, function(x) sum(!is.na(x)) / length(x))]
}

  if (!is.null(nitems)) {
   temp[, z := apply(temp, 1, function(x) sum(!is.na(x)))]
   p <-  nitems
   }

  if (type == "mean") {
    out <- temp[, ifelse( z >= p, apply( temp[, vars, with = FALSE], 1, mean, na.rm = TRUE), NA)]
    # remove this residual variable
     }

  if (type == "any") {
    out <- temp[, ifelse( apply(temp[, vars, with = FALSE], 1,
                               function (x) all( is.na(x) ) ), NA, apply(temp, 1, function(x) as.numeric( any(x %in% val)) ))]

  }

    if (type == "parcel") {

    nitems <- round( length(vars) * p, 0)
    # select items with less missing data
    pnames <- names(rev(countmis(temp, vars, exclude.complete = FALSE))[1:nitems])
    rnames <- as.character(na.omit(vars[- which(vars %in% pnames)][1:nitems]))

    temp <- temp[, tid := 1:.N]
    numerics(temp, vars)
    mtemp <- melt(temp, id = "tid", measure = vars)
    setkey(mtemp, tid)

    # complete cases computation
    out1 <- mtemp[variable %in% pnames, .(mvalue = mean(value, na.rm = TRUE)), by = tid]
    ids <- out1[is.na(mvalue), tid]
    out2 <-     mtemp[tid %in% ids & variable %in% rnames, .(mvalue = mean(value, na.rm = TRUE)), by = tid]
    
    setkey(out1, tid)
    setkey(out2, tid)
    summary(out1)
    summary(out2)
    out <- out1[is.na(mvalue), mvalue := out2$mvalue][, .(mvalue)]

}

  return(out)

}
