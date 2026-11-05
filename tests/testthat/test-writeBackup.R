test_that("backups are numbered and never overwritten", {
  fi = makeFile("one")
  first = quietly(writeBackup(fi$fName, fi$path))
  writeLines("two", fi$fullName)
  second = quietly(writeBackup(fi$fName, fi$path))

  expect_match(first, "demo\\.Rnw\\.bak1$")
  expect_match(second, "demo\\.Rnw\\.bak2$")
  expect_equal(readLines(first), "one")
  expect_equal(readLines(second), "two")
})

test_that("R Markdown backups are numbered too", {
  fi = makeFile("one", knitr = FALSE)
  quietly(writeBackup(fi$fName, fi$path))
  expect_match(quietly(writeBackup(fi$fName, fi$path)), "demo\\.Rmd\\.bak2$")
})

test_that("numbering is separate for each file", {
  fi = makeFile("one")
  writeLines("other", paste0(fi$path, "demo2.Rnw.bak7"))
  expect_match(quietly(writeBackup(fi$fName, fi$path)), "demo\\.Rnw\\.bak1$")
})

test_that("a dummy run writes nothing", {
  fi = makeFile("one")
  quietly(writeBackup(fi$fName, fi$path, dummyRun = TRUE))
  expect_equal(list.files(fi$path), fi$fName)
})

test_that("paths containing quotes work", {
  root = file.path(withr::local_tempdir(), "o'brien")
  dir.create(root)
  writeLines("one", file.path(root, "demo.Rnw"))
  expect_true(file.exists(quietly(writeBackup("demo.Rnw", root))))
})

test_that("a failed copy is an error", {
  expect_error(suppressWarnings(quietly(writeBackup("missing.Rnw", withr::local_tempdir()))),
               "Could not back up")
})
