library(knitr)

print(getwd())
knitr::purl("../Mini-Challenge_2.Rmd", output = "/testthat/test_mc2.R")
