library(rapa)
library(stargazer, quietly = TRUE)
context("apa.stargazer")

iris.lm.1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
iris.lm.2 <- update(iris.lm.1, .~. + Petal.Length + Petal.Width)

test_that("apa.stargazer returns stargazer output", {
  expected_output <- "table"

  expect_output(apa.stargazer(iris.lm.1, iris.lm.2), expected_output)
  expect_output(apa(iris.lm.1, iris.lm.2), expected_output)
})
