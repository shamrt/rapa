library(rapa)
context("apa.anova")

# setup dummy variables
iris.lm.1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
iris.lm.2 <- update(iris.lm.1, .~. + Petal.Length + Petal.Width)
iris.anova <- anova(iris.lm.1, iris.lm.2)

test_that("apa.anova only accepts 'anova'-type objects", {
  expect_error(apa.anova("foo"), "must be.*anova")

  expect_error(apa.anova(list(foo = "bar")))

  expect_equal(typeof(apa.anova(iris.anova)), "character")
})

test_that("apa.anova returns expected string", {
  expect_output(apa.anova(iris.anova), "*F*\\(2, 146\\) = 436.17, _p_ < .001")
})
