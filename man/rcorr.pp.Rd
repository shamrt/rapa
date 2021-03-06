% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/tables.R
\name{rcorr.pp}
\alias{rcorr.pp}
\title{Beautiful correlation matrices}
\usage{
rcorr.pp(rcorr, short.names = TRUE, lower = TRUE)
}
\arguments{
\item{rcorr}{An \code{rcorr} matrix}

\item{short.names}{Name the columns 1 .. N to save space}

\item{lower}{Only show the lower triangular matrix and remove the last column}
}
\description{
Takes an \code{\link[Hmisc]{rcorr}} matrix and returns a pretty printed
data frame for APA publishing, complete with appropriate asterisks for
significance and a stripped upper triangle.
}
\examples{
iris.rcorr <- Hmisc::rcorr(as.matrix(iris[, 1:4]))
rcorr.pp(iris.rcorr)
rcorr.pp(iris.rcorr, short.names = FALSE, lower = FALSE)
}
\seealso{
\code{\link[Hmisc]{rcorr}}
}

