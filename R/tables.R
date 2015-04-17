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

#' Beautiful correlation matrices
#'
#' Takes an \code{\link[Hmisc]{rcorr}} matrix and returns a pretty printed
#' data frame for APA publishing, complete with appropriate asterisks for
#' significance and a stripped upper triangle.
#'
#' @param x \code{rcorr} matrix
#' @param short.names Name the columns 1 .. N to save space
#' @param lower Only show the lower triangular matrix and remove the last column
#'
#' @seealso \code{\link[Hmisc]{rcorr}}
rcorr.pp <- function(x, short.names = TRUE, lower = TRUE) {
  # error handling
  if (class(x) != "rcorr") {
    return(print(type.error(x, "rcorr")))
  }

  # define table values
  R <- weights::rd(x$r, 2)
  p <- x$P

  # define notions for significance levels
  stars <- apply(cor.matrix$P, 1:2, weights::starmaker,
                 p.levels=c(.001, .01, .05), symbols=c("***", "**", "*"))

  # build a new matrix that includes the correlations with appropriate stars
  pp.matrix <- matrix(paste0(R, stars), ncol = ncol(R))
  diag(pp.matrix) <- "---"

  # apply row and column names
  if (short.names) {
    rownames(pp.matrix) <- paste0(1:ncol(R), ". ", colnames(R))
    colnames(pp.matrix) <- 1:ncol(R)
  } else {
    rownames(pp.matrix) <- colnames(R)
    colnames(pp.matrix) <- colnames(R)
  }

  if (lower) {
    # remove upper triangle
    pp.matrix[upper.tri(pp.matrix)] <- ""
  }

  pp.matrix <- as.data.frame(pp.matrix)

  if (lower) {
    # remove last column and return the matrix (which is now a data frame)
    pp.matrix <- cbind(pp.matrix[1:length(pp.matrix)-1])
  }

  return(pp.matrix)
}

# -----------------------------

length.error <- function(object, length) {
  paste0("% Error: Argument '", object, "' must have length of ", length, ".\n")
}
type.error <- function(object, type) {
  paste0("% Error: Argument '", object, "' must be of type '", type, "'\n")
}
