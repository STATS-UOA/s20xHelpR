test_that("knitr chunks are labelled in order and options are kept", {
  fi = makeFile(c("<<old, echo=FALSE>>=", "1", "@",
                  "<<RC-H01-009, fig.height=3, echo=FALSE>>=", "2", "@",
                  "<<>>=", "3", "@",
                  "<<echo=FALSE>>=", "4", "@"))
  quietly(renumChunks(fi, dummyRun = FALSE))
  expect_equal(grep("<<", readLines(fi$fullName), value = TRUE),
               c("<<RC-DEMO-000, echo=FALSE>>=",
                 "<<RC-DEMO-001, fig.height=3, echo=FALSE>>=",
                 "<<RC-DEMO-002>>=",
                 "<<RC-DEMO-003, echo=FALSE>>="))
})

test_that("handouts use the handout number in the label", {
  root = withr::local_tempdir()
  dir.create(file.path(root, "H02"))
  fi = setFileNames(2, root)
  writeLines(c("<<a>>=", "@"), fi$fullName)
  quietly(renumChunks(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[1], "<<RC-H02-000>>=")
})

test_that("option values that look like labels are left alone", {
  fi = makeFile(c("<<a, fig.cap=\"Results\", R.options=list()>>=", "@"))
  quietly(renumChunks(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[1], "<<RC-DEMO-000, fig.cap=\"Results\", R.options=list()>>=")
})

test_that("indented knitr chunks are renumbered", {
  fi = makeFile(c("  <<a>>=", "@"))
  quietly(renumChunks(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[1], "  <<RC-DEMO-000>>=")
})

test_that("R Markdown chunks are renumbered without stray commas", {
  fi = makeFile(c("```{r}", "```", "```{r, echo=FALSE}", "```", "```{r foo}", "```",
                  "```{rcpp}", "```"), knitr = FALSE)
  quietly(renumChunks(fi, dummyRun = FALSE, labelStub = "rc-cs1-"))
  expect_equal(grep("^```\\{", readLines(fi$fullName), value = TRUE),
               c("```{r RC-CS1-000}", "```{r RC-CS1-001, echo=FALSE}", "```{r RC-CS1-002}",
                 "```{rcpp}"))
})

test_that("renumbering twice changes nothing", {
  for(knitr in c(TRUE, FALSE)){
    Lines = if(knitr) c("<<a, echo=FALSE>>=", "@") else c("```{r a, echo=FALSE}", "```")
    fi = makeFile(Lines, knitr = knitr)
    quietly(renumChunks(fi, dummyRun = FALSE))
    once = readLines(fi$fullName)
    quietly(renumChunks(fi, dummyRun = FALSE))
    expect_equal(readLines(fi$fullName), once)
  }
})

test_that("labels stacked by earlier versions are repaired", {
  fi = makeFile(c("```{r RC-CS1-000,  RC-CS1-000, , echo=FALSE}", "```"), knitr = FALSE)
  quietly(renumChunks(fi, dummyRun = FALSE))
  expect_equal(readLines(fi$fullName)[1], "```{r RC-DEMO-000, echo=FALSE}")
})

test_that("the chunk style follows the file extension by default", {
  fi = makeFile(c("```{r}", "```"), knitr = FALSE)
  expect_output(renumChunks(fi, backup = FALSE), "```\\{r RC-DEMO-000\\}")
})

test_that("a dummy run leaves the file alone and makes no backup", {
  Lines = c("<<a>>=  ", "@")
  fi = makeFile(Lines)
  expect_output(renumChunks(fi), "RC-DEMO-000")
  expect_equal(readLines(fi$fullName), Lines)
  expect_equal(list.files(fi$path), fi$fName)
})

test_that("a real run makes a backup of the original", {
  fi = makeFile(c("<<a>>=", "@"))
  quietly(renumChunks(fi, dummyRun = FALSE))
  expect_equal(readLines(paste0(fi$fullName, ".bak1"))[1], "<<a>>=")
})
