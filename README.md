# RAPA: Report tables and statistics in APA 6 format

`rapa` allows you to dynamically inject R objects (especially tables) into your manuscript and does its best to style them in accordance with APA 6 guidelines.
The helper functions in this package return text in either LaTeX or pandoc-style markdown format, making them especially handy when used in combination with [RMarkdown](http://rmarkdown.rstudio.com).


## Installation

Use Hadley Wickham's `devtools` package:

    install.packages("devtools")

    library(devtools)

    install_github("shamrt/rapa")


## Details

The primary reason `rapa` was created is because, while there are packages that do a good job of creating nice LaTeX tables from R objects (e.g., [`stargazer`](http://cran.r-project.org/web/packages/stargazer) and [`xtable`](http://cran.r-project.org/web/packages/xtable), the markup generated by those packages do not easily comply with APA 6 guidelines.
Tables generated by other packages, such as [`psych`](http://cran.r-project.org/web/packages/psych), not only do not comply with the requirements necessary for manuscript publication, they also lack the features and flexibility of more mature table packages.
This package provides a thin layer on top of `stargazer` and `xtable` such that their respective outputs are formatted as closely to guidelines from the APA 6 manual as (reasonably) possible.

Also included are a few other helper functions that format in-line statistics (e.g., correlations, GLM model-based ANOVAs).
Additionally, there is a function (`rcorr.pp`) that pretty-prints correlation matrices generated by the `rcorr` function in the [Hmisc](http://cran.r-project.org/web/packages/Hmisc) package.

Ultimately, the aim of the `rapa` package is to eliminate the need to report any statistics in APA format by-hand.
In its current form, the helper functions provided by `rapa` barely scratch the surface in terms of the kinds of statistics that psychologists use on a regular basis.
Please contact me if additional helper functions are desired.
