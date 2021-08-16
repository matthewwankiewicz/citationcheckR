context("check bibtex file")

test_that("error occurs when packages argument is missing",{
  expect_error(check_bibtex_file("test-file.bib"))
})

test_that("error occurs when file is missing", {
  expect_error(check_bibtex_file(, "tidyverse"))
})

test_that("error occurs when file is NOT BibTeX", {
  expect_error(check_bibtex_file("test-file.bib", citation("tidyverse")$textVersion))
})
