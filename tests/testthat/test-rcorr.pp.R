library(rapa)
context("rcorr.pp")

# setup dummy variables
iris.rcorr <- Hmisc::rcorr(as.matrix(iris[, 1:4]))
iris.rcorr.pp <- rcorr.pp(iris.rcorr)

test_that("rcorr.pp only accepts rcorr objects as first argument", {
  # doesn't accept strings…
  expect_error(rcorr.pp("foo"), "type")

  # or concatenated lists…
  expect_error(rcorr.pp(c("foo", "bar")), "type")

  # or atomic lists…
  expect_error(rcorr.pp(list(foo = "bar")), "type")

  # but it DOES accept rcorr objects…
  expect_equal(class(iris.rcorr), 'rcorr')  # check iris.rcorr class
  expect_equal(class(iris.rcorr.pp), 'data.frame')
})

test_that("rcorr.pp returns cells with asterisks", {
  # note: highly correlated variables in the iris data set
  expect_output(iris.rcorr.pp, "\\*\\*\\*")
})


test_that("rcorr.pp 'short.names' argument works", {
  # note: short names on by default
  expect_equal(names(iris.rcorr.pp), c("1", "2", "3"))
  expect_equal(row.names(iris.rcorr.pp), c("1. Sepal.Length", "2. Sepal.Width",
                                            "3. Petal.Length", "4. Petal.Width"))

  short.names.off <- rcorr.pp(iris.rcorr, short.names = FALSE)
  iris.column.names <- c("Sepal.Length", "Sepal.Width", "Petal.Length")
  expect_equal(names(short.names.off), iris.column.names)
  expect_equal(row.names(short.names.off), c(iris.column.names, "Petal.Width"))
})


test_that("rcorr.pp 'lower' argument works", {
  # note: on by default
  expect_equal(length(names(iris.rcorr.pp)), 3)  # last column removed
  expect_output(iris.rcorr.pp[1, 3], "")  # upper triangle removed

  lower.off <- rcorr.pp(iris.rcorr, lower = FALSE)
  expect_equal(length(names(lower.off)), 4)
  expect_output(lower.off[1, 4], "82")  # last column, first row
})
