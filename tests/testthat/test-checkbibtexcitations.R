context("check bibtex citations")

test_that("error occurs when file is NOT BibTeX", {
  expect_error(check_bibtex_citations(citation("tidyverse")$textVersion, "tidyverse"))
})

test_that("error occurs when package does not exist", {
  expect_error(check_bibtex_citations(toBibtex(citation("tidyverse")), "MissingPackage"))
})
