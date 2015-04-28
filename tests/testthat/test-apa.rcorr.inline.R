library(rapa)
context("apa.rcorr.inline")

# setup dummy variables
iris.rcorr <- Hmisc::rcorr(as.matrix(iris[, 1:4]))

test_that("apa.rcorr.inline only accepts rcorr objects as first argument", {
  # doesn't accept strings…
  expect_error(apa.rcorr.inline("foo"), "type")

  # or concatenated lists…
  expect_error(apa.rcorr.inline(c("foo", "bar")), "type")

  # or atomic lists…
  expect_error(apa.rcorr.inline(list(foo = "bar")), "type")

  # but it DOES accept rcorr objects…
  expect_equal(
    class(apa.rcorr.inline(iris.rcorr, 'Sepal.Length', 'Sepal.Width')),
    'character'
  )
})

test_that("apa.rcorr.inline returns expected string", {
  test.rcorr <- apa.rcorr.inline(iris.rcorr, 'Sepal.Length', 'Sepal.Width')

  expect_equal(typeof(test.rcorr), "character")
  expect_output(test.rcorr, "_r_ = -.12, _p_ = .152")
})
