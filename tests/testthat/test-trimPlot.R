pdfFile = function(env = parent.frame()){
  withr::local_tempfile(fileext = ".pdf", .local_envir = env)
}

test_that("the file name must end in .pdf", {
  expect_error(trimPlot(1:3, fileName = "x.png"), "Invalid filename")
})

test_that("each supported kind of plot writes a file", {
  fit = lm(mpg ~ wt, data = mtcars)
  checks = list(
    function(f) trimPlot(mpg ~ wt, data = mtcars, fileName = f),
    function(f) trimPlot(mtcars$mpg, fileName = f, plotCommand = hist),
    function(f) trimPlot(mpg ~ cyl, data = mtcars, fileName = f, plotCommand = boxplot),
    function(f) trimPlot(mpg ~ cyl, data = mtcars, fileName = f, plotCommand = boxplot, horizontal = TRUE),
    function(f) trimPlot(c(a = 1, b = 2), fileName = f, plotCommand = barplot),
    function(f) trimPlot(fit, fileName = f, which = 1),
    function(f) trimPlot(fit, fileName = f, plotCommand = s20x::normcheck),
    function(f) trimPlot(fit, fileName = f, plotCommand = s20x::cooks20x),
    function(f) trimPlot(fit, fileName = f, plotCommand = s20x::eovcheck),
    function(f) trimPlot(mpg ~ wt, data = mtcars, fileName = f, plotCommand = s20x::trendscatter),
    function(f) trimPlot(mtcars[, 1:3], fileName = f, plotCommand = s20x::pairs20x)
  )

  for(check in checks){
    f = pdfFile()
    check(f)
    expect_gt(file.size(f), 1000)
  }
})

test_that("normcheck gets a page twice the size, however it is named", {
  fit = lm(mpg ~ wt, data = mtcars)
  pageWidth = function(f){
    text = readLines(f, warn = FALSE, encoding = "latin1")
    as.numeric(sub(".*MediaBox \\[0 0 ([0-9]+) .*", "\\1", grep("MediaBox", text, value = TRUE, useBytes = TRUE)[1]))
  }
  f = pdfFile()
  trimPlot(fit, fileName = f, plotCommand = s20x::normcheck, fig.width = 5, fig.height = 4)
  expect_equal(pageWidth(f), 720)
})

test_that("a single axis.size.cex is used for both axes", {
  f = pdfFile()
  expect_silent(trimPlot(mpg ~ wt, data = mtcars, fileName = f, axis.size.cex = 0.5))
})

test_that("addElements runs after the plot exists", {
  f = pdfFile()
  drawn = FALSE
  trimPlot(mpg ~ wt, data = mtcars, fileName = f,
           addElements = list({drawn <- length(dev.list()) > 0; abline(h = 20)}))
  expect_true(drawn)
})

test_that("quoted calls in addElements are drawn", {
  plain = pdfFile()
  extra = pdfFile()
  trimPlot(mpg ~ wt, data = mtcars, fileName = plain)
  trimPlot(mpg ~ wt, data = mtcars, fileName = extra, addElements = list(quote(abline(h = 20))))
  expect_gt(file.size(extra), file.size(plain))
})

test_that("other graphics devices stay open and active", {
  other = pdfFile()
  pdf(other)
  device = dev.cur()
  on.exit(dev.off(device))
  trimPlot(1:3, fileName = pdfFile())
  expect_equal(dev.cur(), device)
})

test_that("the device is closed after an error", {
  devices = dev.list()
  expect_error(trimPlot(1:3, fileName = pdfFile(), addElements = list(stop("boom"))), "boom")
  expect_equal(dev.list(), devices)
})

test_that("plotting failures name the plot and give the reason", {
  expect_error(trimPlot(mpg ~ wt, data = mtcars, fileName = pdfFile(), plotCommand = noSuchFunction),
               "could not draw.*noSuchFunction.*could not find function")
  fit = lm(mpg ~ wt, data = mtcars)
  expect_error(trimPlot(fit, fileName = pdfFile(), plotCommand = noSuchFunction), "could not draw")
})

test_that("objects local to the calling function can be plotted", {
  drawLocal = function(f){
    localData = data.frame(a = 1:5, b = c(2, 4, 3, 5, 7))
    localVector = localData$b
    localFit = lm(b ~ a, data = localData)
    localPlot = function(x, ...) plot(x, ...)

    trimPlot(b ~ a, data = localData, fileName = f)
    trimPlot(localVector, fileName = f, plotCommand = hist)
    trimPlot(localFit, fileName = f, which = 1)
    trimPlot(localVector, fileName = f, plotCommand = localPlot)
  }

  f = pdfFile()
  drawLocal(f)
  expect_gt(file.size(f), 1000)
})

test_that("arguments in ... are evaluated with the data", {
  f = pdfFile()
  expect_silent(trimPlot(mpg ~ wt, data = mtcars, fileName = f, pch = substr(row.names(mtcars), 1, 1)))
})

test_that("nothing is returned visibly", {
  expect_invisible(trimPlot(1:3, fileName = pdfFile()))
})
