library(rapa)
context("apa.p.value")

# setup dummy variables
test.p.insig <- .10
test.p.insig.2 <- .15201
test.p.1.sig <- .05
test.p.2.sig <- .01
test.p.3.sig <- .001
test.p.3.sig.sci <- .152e-2
test.p.tiny <- .00001

test_that("apa.p.value only accepts numeric objects, returns strings", {
  expect_error(apa.p.value("foo"), "must be.*numeric")

  expect_output(typeof(apa.p.value(test.p.insig)), "character")
})

test_that("apa.p.value returns expected string", {
  # insignificant
  expect_output(apa.p.value(test.p.insig), "_p_ = .10")
  expect_output(apa.p.value(test.p.insig.2), "_p_ = .152")

  # 1 sigma
  expect_equal(apa.p.value(test.p.1.sig), "_p_ = .05")

  # 2 sigma
  expect_equal(apa.p.value(test.p.2.sig), "_p_ = .01")

  # 3 sigma, plus scientific notation! 
  expect_equal(apa.p.value(test.p.3.sig), "_p_ = .001")
  expect_equal(apa.p.value(test.p.3.sig.sci), "_p_ = .002")

  # very small
  expect_equal(apa.p.value(test.p.tiny), "_p_ < .001")
})

test_that("apa.p.value does not accept numbers greater than 1", {
  expect_output(apa.p.value(1), "_p_ = 1")

  expect_error(apa.p.value(1.1), "less than or equal to 1")
})

test_that("apa.p.value does not accept numbers less than 0", {
  expect_output(apa.p.value(0), "_p_ < .001")

  expect_error(apa.p.value(-0.1), "greater than or equal to 0")
})
