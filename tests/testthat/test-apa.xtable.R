library(rapa)
library(xtable)
context("apa.xtable")

# setup dummy variables
iris.rcorr <- Hmisc::rcorr(as.matrix(iris[, 1:4]))
iris.rcorr.pp <- rcorr.pp(iris.rcorr)
iris.xtable <- xtable(iris.rcorr.pp)

test_that("apa.xtable only accepts xtable objects as first argument", {
  # doesn't accept strings…
  expect_error(apa.xtable("foo"), "type")

  # or combined lists…
  expect_error(apa.xtable(c("foo", "bar")), "type")

  # or lists…
  expect_error(apa.xtable(list(foo = "bar")), "type")

  # or rcorr objects…
  expect_equal(class(iris.rcorr), 'rcorr')  # check iris.rcorr class
  expect_error(apa.xtable(iris.rcorr), "type")

  # or data frames…
  expect_equal(class(iris.rcorr.pp), 'data.frame')  # check iris.rcorr class
  expect_error(apa.xtable(iris.rcorr.pp), "type")

  # but xtable object ARE accepted!
  expect_equal(class(iris.xtable)[1], 'xtable')  # check iris.xtable class
  expect_output(apa.xtable(iris.xtable), "tabular")
})


test_that("apa.xtable prints notes, which can only be characters and must have length of 1", {
  expect_error(apa.xtable(iris.xtable, note = list(foo = "bar")), "type")

  expect_error(apa.xtable(iris.xtable, note = 1:10), "type")

  expect_output(apa.xtable(iris.xtable, note = "It works!"), "It works!")

  expect_error(apa.xtable(iris.xtable, note = c("Should", "not", "work")), "length")
})


test_that("apa.xtable changes to tabularx LaTeX environment when wrap enabled", {
  expect_output(apa.xtable(iris.xtable, wrap.text = TRUE), "tabularx")
})
