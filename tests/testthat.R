library(testthat)
library(knitr)

runAllChunks <- function(rmd, envir=globalenv()){
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(rmd, output=tempR)
  testthat::test_file(tempR)
}

runAllChunks("Mini-Challenge_2.Rmd")