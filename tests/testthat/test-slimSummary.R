printed = function(...){
  utils::capture.output(slimSummary(...))
}

test_that("lm output has the coefficients and model summary only", {
  out = printed(lm(mpg ~ wt, data = mtcars))
  expect_true(any(grepl("^Coefficients:", out)))
  expect_true(any(grepl("^F-statistic", out)))
  expect_false(any(grepl("^Call:|^Residuals:|^Signif", out)))
})

test_that("glm output has the call and deviance only", {
  out = printed(glm(am ~ wt, family = binomial, data = mtcars))
  expect_true(any(grepl("^Call:", out)))
  expect_true(any(grepl("^Residual deviance", out)))
  expect_false(any(grepl("^F-statistic", out)))
})

test_that("constant mean models work", {
  out = printed(lm(mpg ~ 1, data = mtcars))
  expect_true(any(grepl("^Residual standard error", out)))
})

test_that("models printed without stars work", {
  withr::local_options(show.signif.stars = FALSE)
  out = printed(lm(mpg ~ wt, data = mtcars))
  expect_true(any(grepl("^wt", out)))
  expect_true(any(grepl("^F-statistic", out)))
})

test_that("calls spanning several lines are shown in full", {
  fit = glm(am ~ wt + hp + disp + drat + qsec + cyl + gear + carb, family = poisson,
            data = mtcars, control = glm.control(epsilon = 1e-07))
  out = printed(fit, showCall = TRUE)
  callLines = out[seq_len(which(out == "")[1] - 1)]
  expect_gt(length(callLines), 2)
  expect_true(any(grepl("control", callLines)))
})

test_that("coefficient names that start with lm or Call stay out of the call", {
  d = transform(mtcars, lmx = wt, Caller = hp)
  out = printed(lm(mpg ~ lmx + Caller, data = d), showCall = TRUE)
  expect_equal(sum(grepl("^lmx", out)), 1)
})

test_that("the residuals and legend can be turned on", {
  out = printed(lm(mpg ~ wt, data = mtcars), showResiduals = TRUE, showSignif = TRUE)
  expect_true(any(grepl("^Residuals:", out)))
  expect_true(any(grepl("^Signif", out)))
  i = grep("^Residuals:", out)
  expect_match(out[i + 1], "^ *Min +1Q")
  expect_equal(nchar(out[i + 1]), nchar(out[i + 2]))
})

test_that("sections can all be turned off", {
  out = printed(lm(mpg ~ wt, data = mtcars), showCall = FALSE, showAOVsummary = FALSE,
                showGLMsummary = FALSE)
  expect_equal(out[1], "Coefficients:")
  expect_false(any(grepl("^Residual", out)))
})

test_that("arguments are passed on to summary", {
  out = printed(lm(mpg ~ wt, data = mtcars), correlation = FALSE)
  expect_true(any(grepl("^wt", out)))
})

test_that("unsupported models give a clear error", {
  expect_error(slimSummary(aov(mpg ~ factor(cyl), data = mtcars)), "does not support.*aov")
  expect_error(slimSummary(lm(cbind(mpg, hp) ~ wt, data = mtcars)), "does not support.*mlm")
})

test_that("nothing is returned visibly", {
  expect_invisible(quietly(slimSummary(lm(mpg ~ wt, data = mtcars))))
})
