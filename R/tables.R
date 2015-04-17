#' Create APA 6 export tables
#'
#' Function that take an \code{\link[xtable]{xtable}} object and prints a LaTeX
#' table in APA 6 format. It uses the same arguments as the original
#' \code{\link[xtable]{xtable.print}} function as well as an additional
#' argument for creating table notes.
#'
#' @param note Character vector of length 1 containing the table's notes.
#'
#' @seealso \code{\link[xtable]{xtable}}, \code{\link[xtable]{print.xtable}}
apa.xtable <- function(xtable, note = NULL, wrap.text = FALSE, ...) {
  # error handling
  .error.present <- "\n"

  if (!is.null(note)) {
    if (!is.character(note)) {
      .error.present <- c(.error.present, type.error(note, 'character'))
    }
    if (length(note) > 1) {
      .error.present <- c(.error.present, length.error(note, 1))
    }
  }

  # ----------------

  # continue only if no errors
  if (length(error.present) == 1) {
    # create xtable

    .print.args <- list(xtable, caption.placement = "top")

    if (!is.null(note)) {
      .print.args$hline.after = c(-1, 0)
      .print.args$add.to.row = .xtable.note(x, note)
    }

    # wrap wide tables
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
