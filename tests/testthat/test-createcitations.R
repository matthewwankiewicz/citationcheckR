context("create citations")

test_that("error occurs when unknown package is used",{
  expect_error(create_citations("totallyrealpackage"))
})


test_that("error occurs when unknown package is used",{
  expect_error(create_citation_file("totallyrealpackage"))
})
