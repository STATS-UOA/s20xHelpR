test_that("rebuild runs in the file's directory and returns afterwards", {
  fi = makeFile("text")
  start = getwd()
  seen = NULL
  local_mocked_bindings(knit2pdf = function(input, ...){
    seen <<- c(getwd(), input)
    "demo.pdf"
  }, .package = "knitr")

  quietly(rebuild(fi))
  expect_equal(normalizePath(seen[1]), normalizePath(fi$path))
  expect_equal(seen[2], fi$fName)
  expect_equal(getwd(), start)
})

test_that("the working directory is restored after a failure", {
  fi = makeFile("text")
  start = getwd()
  local_mocked_bindings(knit2pdf = function(input, ...) stop("boom"), .package = "knitr")

  expect_error(quietly(rebuild(fi)), "boom")
  expect_equal(getwd(), start)
})
