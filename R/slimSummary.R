#' Print a shortened model summary
#'
#' Prints selected sections of \code{summary(lm.obj)}, always including the
#' coefficient table, so that model output fits on a slide or a page.
#'
#' The sections are taken from the printed output of \code{summary()}, which
#' keeps the formatting identical to R's own. Multiple response models and
#' \code{aov} objects print differently and are not supported.
#'
#' @param lm.obj a fitted model of class \code{lm} or \code{glm}.
#' @param showCall if \code{TRUE} then show the model call.
#' @param showResiduals if \code{TRUE} then show the summary of the residuals.
#' @param showSignif if \code{TRUE} then show the significance codes legend.
#' @param showAOVsummary if \code{TRUE} then show the block from the residual
#'   standard error to the F-statistic. The default is \code{TRUE} for \code{lm}
#'   objects.
#' @param showGLMsummary if \code{TRUE} then show the block from the dispersion
#'   parameter to the residual deviance. The default is \code{TRUE} for
#'   \code{glm} objects.
#' @param ... other arguments passed to \code{\link{summary}}.
#'
#' @return \code{NULL}, invisibly. The selected output is printed.
#'
#' @examples
#' slimSummary(lm(mpg ~ wt, data = mtcars))
#' slimSummary(glm(am ~ wt, family = binomial, data = mtcars))
#'
#' @export
slimSummary = function(lm.obj,
                       showCall = !showAOVsummary,
                       showResiduals = FALSE,
                       showSignif = FALSE,
                       showAOVsummary = class(lm.obj)[1] == "lm",
                       showGLMsummary = !showAOVsummary,
                       ...){

  if(inherits(lm.obj, c("mlm", "aov"))){
    stop("slimSummary does not support objects of class: ", class(lm.obj)[1], "\n")
  }

  Lines = capture.output(summary(lm.obj, ...))

  if(showCall){
    cat(paste0(summaryBlock(Lines, "^Call:", includeEnd = FALSE, section = "call"),
               collapse = "\n"))
    cat("\n\n")
  }

  if(showResiduals){
    cat(paste0(summaryBlock(Lines, "^(Weighted )?Residuals:", includeEnd = FALSE,
                            section = "residuals"),
               collapse = "\n"))
    cat("\n\n")
  }

  ## Without significance stars there is no --- line, so the table then ends at
  ## the first blank line.
  cat(paste0(summaryBlock(Lines, "^Coefficients:", "^---$", section = "coefficients"),
             collapse = "\n"))
  cat("\n")

  if(showSignif){
    cat(paste0(grep("^Signif.*$", Lines, value = TRUE), "\n"))
    cat("\n")
  }

  if(showAOVsummary){
    cat(paste0(summaryBlock(Lines, "^Residual standard.*$", "^F-statistic.*$",
                            section = "residual standard error"),
               collapse = "\n"))
  }

  if(showGLMsummary){
    cat(paste0(summaryBlock(Lines, "^\\(Dispersion parameter.*$", "^Residual deviance.*$",
                            section = "dispersion parameter"),
               collapse = "\n"))
  }

  invisible(NULL)
}
