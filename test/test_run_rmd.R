library(knitr)

runAllChunks <- function(rmd, envir=globalenv()){
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(rmd, output=tempR)
  sys.source(tempR, envir=envir)
}

runAllChunks("Mini-Challenge_2.Rmd")
