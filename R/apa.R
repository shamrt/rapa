#' Helper function for generating APA 6-styled output from R objects
#'
#' \code{apa} takes an R object, finds an applicable \code{rapa} function, and
#' returns a string of APA 6-styled text, if an appropriate function is
#' available; otherwise, returns an empty string.
#'
#' @param x An R object.
#' @param ... Arguments to be passed on to the applicable \code{rapa} function.

#' @examples
#' library(Hmisc)
#' library(xtable)
#'
#' iris.rcorr <- rcorr(as.matrix(iris[, 1:4]))
#' iris.rcorr.pp <- rcorr.pp(iris.rcorr)
#' iris.xtable <- xtable(iris.rcorr.pp)
#'
#' # Return correlation
#' apa(iris.rcorr, 'Sepal.Length', 'Sepal.Width')
#'
#' # Return xtable
#' apa(iris.xtable)
#'
#' @include errors.R
#' @export
apa <- function(x, ...) {
  switch(class(x)[1],
         xtable = apa.xtable(x, ...),
         lm = apa.stargazer(x, ...),
         rcorr = apa.rcorr.inline(x, ...),
         anova = apa.anova(x, ...),
         ""
         )
}
