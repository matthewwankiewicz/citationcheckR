context("check citation file")

test_that("error occurs when file is missing",{
  expect_error(check_citation_file(, "citationcheckR"))
})


test_that("error occurs when file does not exist",{
  expect_error(check_citation_file("filedoesnotexist.bib", "citationcheckR"))
})


test_that("error occurs when package does not exist",{
  expect_error(check_citation_file("test-file.bib", "testpackagename"))
})


