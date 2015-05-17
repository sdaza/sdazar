#' @title Reverse escale.
#' @description Reverse escale.
#' @param dat Data.table or data.frame object.
#' @param var Character or vector of character with variables names to transform.
#' @param newvar Character or vector of character with the name of new variables.
#' @return Data.table object with transformed variables.
#' @examples
#' revscale(dat, "var", "newvar")
revscale  <- function(dat, var, nvar) {
  if (sum(class(dat) %in% c("data.frame", "data.table"))>0) {

  pkgTest <- function(x) {

    is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

    if (!is.installed(x))
    {
      install.packages(x)
      if(!require(x , character.only = TRUE)) stop("Package not found")
    }
    else {
      require(x, character.only = TRUE)
    }
  }

  pkgTest("data.table")

  dt  <- data.table(dat)

  if (length(var)==length(nvar)) {

    for(n in 1:length(var)) {
      maxvalue  <- max(dt[, var[n], with=FALSE], na.rm=TRUE) + 1
      char <- paste0(nvar[n], ' := ', maxvalue, ' - ', var[n])
      dt[, eval(parse(text=char))]

    }
    if (class(dat)[1]=="data.table") {
      return(r <- dt)
    }

    else if  (class(dat)[1]=="data.frame") {
      return(r  <- data.frame(dt))
    }
}
  else {
    stop("Number of old and new variables is not same")
  }
}
  else{
    stop("The first object is not a data.frame or a data.table")
  }
}
