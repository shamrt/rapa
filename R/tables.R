#' Create APA 6 export tables
#'
#' Function extending \code{\link[xtable]{xtable}}, which converts an R object
#' to an \code{xtable} object in APA 6 format, which can then be printed as a
#' LaTeX table. It uses the same arguments as the original \code{xtable}
#' function, as well as additional arguments, which are sent to the
#' \code{print.xtable} function. Only arguments that are different from
#' \code{xtable} are listed below.
#'
#' This function calls both \code{\link[xtable]{xtable}} and
#' \code{\link[xtable]{print.xtable}} because it is meant to make the creation
#' of printable APA 6-styled LaTeX tables as simple as possible.
#'
#' @param caption Character vector of length 1 containing the table's title.
#' @param note Character vector of length 1 containing the table's notes.
#'
#' @seealso \code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}}
apa.xtable <- function(x, caption = NULL, label = NULL, align = NULL,
                       digits = NULL, note = NULL, wrap.text = FALSE, ...) {
  # error handling
  .error.present <- "\n"

  for (arg in c(caption, note)) {
    if (!is.null(arg)) {
      if (!is.character(arg)) {
        .error.present <- c(.error.present, type.error(arg, 'character'))
      }
      if (length(arg) > 1) {
        .error.present <- c(.error.present, length.error(arg, 1))
      }
    }
  }

  # ----------------

  # continue only if no errors
  if (length(error.present) == 1) {
    # create xtable
    .xtable <- xtable(x, label = label, caption = caption, align = align,
                      digits = digits)

    .print.args <- list(.xtable, caption.placement = "top")

    if (!is.null(note)) {
      .print.args$hline.after = c(-1, 0)
      .print.args$add.to.row = .xtable.note(x, note)
    }
    if (wrap.text) {
      .print.args$tabular.environment="tabularx"
      .print.args$width="\\textwidth"
    }

    # print xtable
    .output <- do.call(print, c(.print.args, ...))
  } else {
    cat(error.present, sep="")
    .output <- error.present
  }

  return(.output)
}

# Add notes to xtables
.xtable.note <- function(xtable, text) {
  .note          <- list()
  .note$pos      <- list()
  .note$pos[[1]] <- c(nrow(xtable))
  .note$command  <- c(paste("\\hline \n",
                            "\\multicolumn{", ncol(xtable)+1, "}{p{\\linewidth}}{",
                            "\\textit{Notes:}",
                            text, "} \n"))

  return(.note)
}


length.error <- function(object, length) {
  paste0("% Error: Argument '", object, "' must have length of ", length, ".\n")
}
type.error <- function(object, type) {
  paste0("% Error: Argument '", object, "' must be of type '", type, "'\n")
}
