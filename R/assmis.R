#' @title Assign NA to missing data codes.
#' @description Assign NA to missing data codes.
#' @param dat data.table object.
#' @param var List of variable names.
#' @param codes List of missing data codes.
#' @return Returns date.table with transformed missing data codes.
#' @examples
#' dat <- assmis(dat, list(dvar), list(c(9,99)))

assmis  <- function(dat, var, codes) {

  if (sum(class(dat) %in% c("data.frame", "data.table")) > 0) {

  if (class(var) %in% "list" & class(codes) %in%  "list"
      & (length(var) == length(codes) ))

  {

    dt  <- data.table(dat)

    for (i  in 1:length(var)) {

      for(j in 1:length(var[[i]])) {
        chari <-  paste0(var[[i]][j], ' %in% ' , 'codes[[', i, ']]')
        charj <-  paste0(var[[i]][j], ':= NA')
        dt[eval(parse(text = chari)), eval(parse(text = charj))]

      }
    }

    if (class(dat)[1] == "data.table") {
      return(r <- dt)
    }

    else if  (class(dat)[1] == "data.frame") {
      return(r  <- data.frame(dt))
    }

  }
  else {
    stop("Variables or codes are not defined as lists, or lists' elements are not the same")
  }
  }
      else{
        stop("The first object is not a data.frame or data.table")
      }
  print(". . . . . .  Missing data assignment done!")
}
