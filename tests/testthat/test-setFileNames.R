test_that("handouts are padded and live in their own directory", {
  fi = setFileNames(2, "/notes")
  expect_s3_class(fi, "fileInfo")
  expect_equal(fi$part, "H02")
  expect_equal(fi$path, "/notes/H02/")
  expect_equal(fi$fullName, "/notes/H02/H02.Rnw")
  expect_equal(setFileNames("10", "/notes")$part, "H10")
})

test_that("handout numbers are validated", {
  for(i in list(0, 21, 1.5, "x", NA, 1:2)){
    expect_error(setFileNames(i, "/notes"), "needs to be in")
  }
})

test_that("other files use the knitr flag for their extension", {
  expect_equal(setFileNames("cs1", "/notes", handout = FALSE)$fullName, "/notes/cs1.Rmd")
  expect_equal(setFileNames("cs1", "/notes", handout = FALSE, knitr = TRUE)$fName, "cs1.Rnw")
})

test_that("a trailing slash on root makes no difference", {
  expect_equal(setFileNames(2, "/notes/")$fullName, "/notes/H02/H02.Rnw")
})
