test_that("plots without a pdf are reported with their line in the file", {
  fi = makeFile(c("a", "b", "c", "<<x>>=", "plot(1)", "@"))
  expect_output(result <- findPlotChunks(fi), "demo.Rnw 5:plot\\(1\\)")
  expect_equal(result, 5L)
})

test_that("several matches in a chunk print one per line", {
  fi = makeFile(c("<<x>>=", "plot(1)", "hist(1)", "@"))
  expect_equal(quietly(findPlotChunks(fi)), 2:3)
})

test_that("chunks writing a pdf and eval=FALSE chunks are not reported", {
  fi = makeFile(c("<<a>>=", "pdf(\"a.pdf\")", "plot(1)", "@",
                  "<<b>>=", "trimPlot(x, fileName = \"b.pdf\", plotCommand = hist)", "@",
                  "<<c, eval=FALSE>>=", "plot(1)", "@"))
  expect_length(quietly(findPlotChunks(fi)), 0)
})

test_that("names that only contain a plot command are not reported", {
  fi = makeFile(c("<<a>>=", "lm.plot = 1", "histogram(x)", "my.plot(1)", "plotted = 2", "@"))
  expect_length(quietly(findPlotChunks(fi)), 0)
})

test_that("files without chunks and stray @ lines are fine", {
  expect_length(quietly(findPlotChunks(makeFile("text"))), 0)
  fi = makeFile(c("<<a>>=", "plot(1)", "@", "text", "@"))
  expect_equal(quietly(findPlotChunks(fi)), 2L)
})
