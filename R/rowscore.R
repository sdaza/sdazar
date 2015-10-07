#' @title Create scores
#' @description Create scores: mean or if any of the variables have specific values.
#' @param dat `data.table` object.
#' @param vars Character vector with name of variables.
#' @param type Type of score (`mean`, `any`, `parcel`). `parcel` selects the items with less missing data based on `p` (once the items are define a mean is computed using complete cases).
#' @param p  Proportion of items with valid values to compute mean.
#' @param nitems Number of items with valid values to compute mean.
#' @param values Values used to define `any` index.
#' @return Returns a vector with scores.
#' @examples
#' a <- c(NA,2,3,4, NA)
#' b <- c(2,3,4,NA, 3)
#' c <- c(1,2,3,NA, 1)
#' dat <- data.table(a,b,c)
#' vars <- names(dat)
#' rowscore(dat, vars)
rowscore <- function (dat, vars, type = "mean", p = 0.5, nitems = NULL, val = NULL)
{
  if (is.null(val) & type == "any") {
    stop("`val` need to be specified wheh using `any`")
  }
  if (!is.character(vars)) {
    stop("`vars` has to be a character vector")
  }
  if (!is.na(p) & is.null(nitems)) {
  dat[, z := apply(dat[, vars, with = FALSE], 1, function(x) sum(!is.na(x)) / length(x))]
}

  if (!is.null(nitems)) {
 dat[, z := apply(dat[, vars, with = FALSE], 1, function(x) sum(!is.na(x)))]
  p <-  nitems
   }

  if (type == "mean") {
    out <- dat[, ifelse( z >= p, apply( dat[, vars, with = FALSE], 1, mean, na.rm = TRUE), NA)]
    # remove this residual variable
     dat[, z := NULL]
     }

  if (type == "any") {
    out <- dat[, ifelse( apply(dat[, vars, with = FALSE], 1,
                               function (x) all( is.na(x) ) ), NA, apply(dat[, vars, with = FALSE], 1, function(x) as.numeric( any(x %in% val)) ))][, out]

  }
    if (type == "parcel") {

    nitems <- trunc( length(vars) * p)
    # select items with less missing data
    pnames <- names(rev(countmis(w1, vars, exclude.complete = FALSE))[1:nitems])

    temp <- dat[, tid := 1:.N][, c(" tid", vars), with = FALSE]
    numerics(temp, vars)
    mtemp <- melt(temp, id = "tid", measure = vars)
    setkey(mtemp, tid)

    # remove this residual variable
    dat[, tid := NULL]

    # complete cases computation
    out <- mtemp[variable %in% pnames, .(mvalue = mean(value)), by = tid][, mvalue]

  }


  return(out)

}
