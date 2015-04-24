#' Report correlation between two variables
#'
#' Displays the correlation between two variables within an
#' \code{\link[Hmisc]{rcorr}} matrix table according to APA 6 guidelines.
#'
#' @param rcorr An \code{rcorr} matrix
#' @param var1 Name of first variable of interest (string/quoted object)
#' @param var2 Name of second variable of interest (string/quoted object)
#' @param type The method of correlation used to generate the \code{rcorr}
#'   matrix
#'
#' @examples
#' iris.rcorr <- Hmisc::rcorr(as.matrix(iris[, 1:4]))
#' apa.rcorr.inline(iris.rcorr, 'Sepal.Length', 'Sepal.Width')
#'
#' @include errors.R
#' @export
apa.rcorr.inline <- function(rcorr, var1, var2, type = "pearson") {
  # error handling
  if (class(rcorr)[1] != "rcorr") {
    return(cat(.type.error('rcorr', "rcorr")))
  }

  .error.present <- "\n"

  if (!exists('var1')) {
    .error.present <- c(.error.present, .missing.error('var1'))
  } else {
    if (!is.character(var1)) {
      .error.present <- c(.error.present, .type.error('var1', 'character'))
    }
  }
  if (!exists('var2')) {
    .error.present <- c(.error.present, .missing.error('var2'))
  } else {
    if (!is.character(var2)) {
      .error.present <- c(.error.present, .type.error('var2', 'character'))
    }
  }

  # continue only if no errors
  if (length(.error.present) == 1) {
    .r <- weights::rd(rcorr$r[var1, var2], digits = 2)
    .p <- apa.p.value(rcorr$P[var1, var2])

    # set method string
    .method_str <- switch(type,
                          "pearson" = "_r_",
                          "spearman" = "_$r_s$_",
                          "kendall" = "_W_"
    )

    output <- paste0(.method_str, " = ", .r, ", ", .p)
  } else {
    output <- cat(.error.present)
  }

  return(output)
}


#' Report ANOVA
#'
#' Displays the results of an ANOVA (F values) in accordance with APA 6
#' guidelines.
#'
#' @param anova An \code{\link[stats]{anova}} object
#'
#' @examples
#' iris.lm.1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#' iris.lm.2 <- update(iris.lm.1, .~. + Petal.Length + Petal.Width)
#' iris.anova <- anova(iris.lm.1, iris.lm.2)
#' apa.anova(iris.anova)
#'
#' @include errors.R
#' @export
apa.anova <- function(anova) {
  # error handling
  if (class(anova)[1] != "anova") {
    return(.type.error('anova', "anova"))
  }

  .df <- anova$Df[2]
  .n <- anova$Res.Df[2]
  .f <- round(anova$F[2], 2)
  .p <- apa.p.value(anova[["Pr(>F)"]][2])

  output <- paste0("*F*(", .df, ", ", .n, ") = ", .f, ", ", .p)

  return(output)
}

#' Report a p-value
#'
#' Formats and reports a p-value according to APA 6 guidelines. Returns a
#' string.
#'
#' This function is primarily intended to be used by other helper functions, so
#' by default p-values are pretty-printed (e.g., "_p_ = .152") instead of simply
#' returning the correctly rounded p-value (e.g., ".152")
#'
#' @param p A number representing a p-value (must be less than 1).
#' @param pretty.print An optional dichotomous indicator for whether to prepend
#'   the p-value abbreviation and appropriate sign to the returned number (see
#'   details).
#'
#' @include errors.R
#' @export
apa.p.value <- function(p, pretty.print = TRUE) {
  # error handling
  .error.present <- "\n"

  if (!is.null(p)) {
    if (!is.numeric(p)) {
      .error.present <- c(.error.present, .type.error('p', 'numeric'))
    }
    if (p > 1) {
      .error.present <- c(.error.present, .value.error('note', "greater than 1"))
    }
  }

  # continue only if no errors
  if (length(.error.present) == 1) {
    # calculate displayed p-value
    if (p > .001) {
      p <- weights::rd(p, digits = 3, add = FALSE)
      p.pp <- paste("_p_", "=", p)
    } else {
      p <- "< .001"
      p.pp <- paste("_p_", p)
    }

    output <- ifelse(pretty.print, p.pp, p)
  } else {
    output <- cat(.error.present)
  }

  return(output)
}
