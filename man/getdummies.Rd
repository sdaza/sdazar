% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getdummies.R
\name{getdummies}
\alias{getdummies}
\title{Create dummy variables.}
\usage{
getdummies(dat, vars)
}
\arguments{
\item{dat}{`data.table` object.}

\item{vars}{Character vector with name of variables.}
}
\value{
data.table with dummy variables.
}
\description{
Create dummy variables from a data.table.
}
\examples{
a <- c(NA,2,3,4, NA)
b <- c(2,3,4,NA, 3)
c <- c(1,2,3,NA, 1)
dat <- data.table(a,b,c)
vars <- names(dat)
getdummies(dat, "a")
}
