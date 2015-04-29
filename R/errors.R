.length.error <- function(x, length) {
  paste0("Argument '", x, "' must have length of ", length, ".\n")
}
.type.error <- function(x, type) {
  paste0("Argument '", x, "' must be of class/type '", type, "'\n")
}
.value.error <- function(x, value) {
  paste0("Argument '", x, "' must have a value of ", value, "\n")
}
.missing.error <- function(x) {
  paste0("Argument '", x, "' is missing\n")
}
