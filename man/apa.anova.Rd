% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/inline-text.R
\name{apa.anova}
\alias{apa.anova}
\title{Report ANOVA}
\usage{
apa.anova(anova)
}
\arguments{
\item{anova}{An \code{\link[stats]{anova}} object}
}
\description{
Displays the results of an ANOVA (F values) in accordance with APA 6
guidelines.
}
\examples{
iris.lm.1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
iris.lm.2 <- update(iris.lm.1, .~. + Petal.Length + Petal.Width)
iris.anova <- anova(iris.lm.1, iris.lm.2)
apa.anova(iris.anova)
}

