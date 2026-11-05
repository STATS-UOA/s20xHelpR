plotFile = function(...){
  makeFile(c(...), env = parent.frame())
}

test_that("the file name and include line follow the chunk label", {
  fi = plotFile("<<RC-H02-005, echo=FALSE>>=",
                "trimPlot(x,",
                "         fileName = \"figure/old.pdf\",",
                "         fig.width = 7)",
                "@",
                "\\includegraphics[width=3in]{figure/old}")
  quietly(renumPlotFilenames(fi, dummyRun = FALSE))
  result = readLines(fi$fullName)
  expect_equal(result[3], "         fileName = \"figure/RC-H02-005.pdf\",")
  expect_equal(result[6], "\\includegraphics[width=3in]{figure/RC-H02-005}")
})

test_that("a pdf() call on one line is handled", {
  fi = plotFile("<<RC-1>>=", "pdf(\"figure/old.pdf\", width = 3)", "plot(1)", "dev.off()", "@",
                "\\includegraphics{figure/old}")
  quietly(renumPlotFilenames(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[2], "pdf(\"figure/RC-1.pdf\", width = 3)")
})

test_that("single quotes, extensions and text after the include work", {
  fi = plotFile("<<RC-1>>=", "pdf('old.pdf')", "@", "\\includegraphics{old.pdf}\\\\")
  quietly(renumPlotFilenames(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[c(2, 4)], c("pdf('RC-1.pdf')", "\\includegraphics{RC-1.pdf}\\\\"))
})

test_that("dots in labels become underscores", {
  fi = plotFile("<<a.b>>=", "pdf(\"a.pdf\")", "@", "\\includegraphics{a}")
  quietly(renumPlotFilenames(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[4], "\\includegraphics{a_b}")
})

test_that("swapped names do not collide across chunks", {
  fi = plotFile("<<RC-2>>=", "trimPlot(x, fileName = \"figure/RC-3.pdf\")", "@",
                "<<RC-3>>=", "trimPlot(x, fileName = \"figure/RC-2.pdf\")", "@",
                "\\includegraphics{figure/RC-3}", "\\includegraphics{figure/RC-2}")
  quietly(renumPlotFilenames(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[7:8],
               c("\\includegraphics{figure/RC-2}", "\\includegraphics{figure/RC-3}"))
})

test_that("a chunk with several PDF files gets lettered names", {
  fi = plotFile("<<RC-H13-003, echo=FALSE>>=",
                "trimPlot(x, fileName = \"figure/first.pdf\")",
                "trimPlot(y, fileName = \"figure/second.pdf\")",
                "@",
                "\\includegraphics[width=2in]{figure/first}",
                "\\includegraphics[width=2in]{figure/second}")
  quietly(renumPlotFilenames(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[c(2, 3, 5, 6)],
               c("trimPlot(x, fileName = \"figure/RC-H13-003A.pdf\")",
                 "trimPlot(y, fileName = \"figure/RC-H13-003B.pdf\")",
                 "\\includegraphics[width=2in]{figure/RC-H13-003A}",
                 "\\includegraphics[width=2in]{figure/RC-H13-003B}"))
})

test_that("lettered names that are already correct do not change", {
  Lines = c("<<RC-1>>=", "pdf(\"RC-1A.pdf\")", "pdf(\"RC-1B.pdf\")", "@",
            "\\includegraphics{RC-1A}", "\\includegraphics{RC-1B}")
  fi = plotFile(Lines)
  quietly(renumPlotFilenames(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName), Lines)
})

test_that("a missing include is reported without stopping", {
  fi = plotFile("<<RC-1>>=", "pdf(\"old.pdf\")", "@")
  expect_output(renumPlotFilenames(fi, backup = FALSE), "No matching includegraphics for old")
})

test_that("eval=FALSE chunks, comments and other pdf text are skipped", {
  Lines = c("<<RC-1, eval=FALSE>>=", "pdf(\"old.pdf\")", "@",
            "<<RC-2>>=", "# pdf(\"old.pdf\")", "x = \"pdf\"", "@")
  fi = plotFile(Lines)
  quietly(renumPlotFilenames(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName), Lines)
})

test_that("a dummy run leaves the file alone", {
  Lines = c("<<RC-1>>=", "pdf(\"old.pdf\")", "@", "\\includegraphics{old}")
  fi = plotFile(Lines)
  expect_output(renumPlotFilenames(fi), "New 2")
  expect_equal(readLines(fi$fullName), Lines)
})

test_that("files without chunks are fine", {
  expect_silent(quietly(renumPlotFilenames(plotFile("text"), backup = FALSE)))
})

test_that("problems are errors with useful messages", {
  expect_error(renumPlotFilenames(plotFile("<<echo=FALSE>>=", "pdf(\"a.pdf\")", "@"), backup = FALSE),
               "no label")
  expect_error(renumPlotFilenames(plotFile("<<a>>=", "1"), backup = FALSE), "Unmatched")
  expect_error(renumPlotFilenames(makeFile("x", knitr = FALSE)), "only works with .Rnw")
})
