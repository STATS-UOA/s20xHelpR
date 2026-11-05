test_that("contractions are reported and possessives are not", {
  fi = makeFile(c("It's fine", "no apostrophe", "don't", "the student's mark",
                  "we'll see", "they've gone", "that's it", "the data's mean"))
  expect_equal(quietly(findContract(fi)), c(1L, 3L, 5L, 6L, 7L))
})

test_that("each match shows its line number", {
  expect_output(findContract(makeFile(c("x", "can't"))), "2: can't")
})
