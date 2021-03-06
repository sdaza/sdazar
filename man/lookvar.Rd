% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookvar.R
\name{lookvar}
\alias{lookvar}
\title{Look variables in a dataframe using regular expressions.}
\usage{
lookvar(dat, varnames)
}
\arguments{
\item{dat}{data.frame object.  Character or vector of characters with formulas using ACS variable names.}

\item{varnames}{Character or vector with regular expressions containing variables names.}
}
\value{
Returns the variables names of the variables found.
}
\description{
The \code{getvars} function gets ACS variable names from text containing formulas with addition, substraction, and/or division operators.
}
\examples{
lookvar(dat, "gender")
}
