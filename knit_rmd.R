library(knitr)

print(getwd())
knitr::purl("Mini-Challenge_2.Rmd", output = "tests/testthat/test-mc2.R")
