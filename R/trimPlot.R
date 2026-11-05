#' Write a plot to a PDF file with reduced margins
#'
#' Draws a plot in a new PDF file with small margins, tight axes and small axis
#' text, for use with \code{\\includegraphics} in the notes. The file is always
#' closed, including after an error, and any graphics device that was open before
#' the call stays open and active.
#'
#' Pass \code{plotCommand} by name, for example \code{plotCommand = normcheck},
#' because the function name selects how the plot is drawn. \code{normcheck} and
#' \code{cooks20x} get a PDF twice the size of \code{fig.width} and
#' \code{fig.height}. \code{normcheck}, \code{cooks20x}, \code{pairs20x} and
#' \code{barplot} manage their own axes and labels, so \code{x.lab}, \code{y.lab}
#' and the other axis arguments do not apply to them, except that \code{barplot}
#' uses \code{axis.size.cex}, \code{mgpy} and \code{lasy}.
#'
#' The plot is first drawn with axes and labels turned off, then with a simpler
#' call if that fails. An error is raised if every attempt fails.
#'
#' @param x the object to plot, for example a formula, a vector or a fitted model.
#' @param data a data frame for use when \code{x} is a formula.
#' @param fileName the name of the PDF file, which must end in \code{.pdf}. The
#'   directory must already exist.
#' @param plotCommand the plotting function.
#' @param x.lab,y.lab the axis labels.
#' @param axes if \code{TRUE} then \code{plotCommand} draws its own axes.
#' @param mai the margin sizes in inches, passed to \code{\link[graphics]{par}}.
#' @param mgpx,mgpy the margin lines for the axes, as for \code{mgp} in
#'   \code{\link[graphics]{par}}.
#' @param axis.size.cex the size of the axis numbers for the x and y axes. A
#'   single value is used for both.
#' @param axis.lab.cex the size of the axis labels.
#' @param linex,liney the margin lines for the axis labels.
#' @param lasy the orientation of the y axis numbers, as for \code{las} in
#'   \code{\link[graphics]{par}}.
#' @param fig.width,fig.height the size of the PDF file in inches.
#' @param .at,.labels the tick positions and labels for the categorical axis of a
#'   boxplot.
#' @param addElements a list of calls that add to the plot, for example
#'   \code{list(abline(h = 0), points(1, 1))}. They are written directly in the
#'   call to \code{trimPlot}, and run after the plot has been drawn.
#' @param ... other arguments passed to \code{plotCommand}.
#'
#' @return \code{NULL}, invisibly.
#'
#' @examples
#' fileName = tempfile(fileext = ".pdf")
#' trimPlot(mpg ~ wt, data = mtcars, fileName = fileName,
#'          x.lab = "Weight", y.lab = "MPG",
#'          addElements = list(abline(h = 20, lty = 2)))
#'
#' @export
trimPlot = function(x, data = NULL, fileName, plotCommand = plot, x.lab = "", y.lab = "", axes = FALSE, mai = c(0.5, 0.5, 0.1, 0.1),
                    mgpx = c(3, .3, 0), mgpy = c(3, .6, 0), axis.size.cex = c(0.7, 0.7), axis.lab.cex = 0.7, linex = 1.2, liney = 1.8,
                    lasy = 1, fig.width = 10, fig.height = 7, .at = NULL, .labels = NULL, addElements = list(), ...){

  if(!grepl("^.*\\.pdf$", fileName)){
    stop(paste("Invalid filename:", fileName, "\n"))
  }

  ## The plot call is rebuilt from the expressions in the original call, so it
  ## must be evaluated where those expressions were written.
  callerEnv <- parent.frame()
  plotName = sub("^.*::", "", deparse(substitute(plotCommand))[1])
  selfContained = plotName %in% c("normcheck", "cooks20x", "pairs20x", "barplot")
  axis.size.cex = rep_len(axis.size.cex, 2)

  if (plotName %in% c("normcheck", "cooks20x")) {
    fig.width <- fig.width*2
    fig.height <- fig.height*2
  }

  plotFailed = function(result) {
    inherits(result, "try-error")
  }

  stopPlot = function(result) {
    stop("trimPlot could not draw ", fileName, " with ", plotName, ": ",
         conditionMessage(attr(result, "condition")), call. = FALSE)
  }

  previousDevice <- dev.cur()
  pdf(fileName, width = fig.width, height = fig.height)
  thisDevice <- dev.cur()

  on.exit({
    if (thisDevice %in% dev.list()) {
      dev.off(thisDevice)
    }
    if (previousDevice %in% dev.list()) {
      dev.set(previousDevice)
    }
  })

  par(mai = mai)

  if (inherits(x, "lm")) {
    tryEvalData <- try(eval(substitute(plotCommand(x, xlab = "", ylab = "", axes = axes, ...)), callerEnv), silent = TRUE)

    if (plotFailed(tryEvalData)) {
      if (plotName == "plot") {
        plot(x, caption = list("", "", "", "", "", ""), xaxt = ifelse(!axes, "n", "s"), yaxt = ifelse(!axes, "n", "s"), cex.id = 0.5, ...)
      } else if (plotName == "normcheck") {
        normcheck(x, ...)
      } else if (plotName == "cooks20x") {
        cooks20x(x, main = "", line = c(0.5, 1.5, 2), ...)
      } else if (plotName == "eovcheck") {
        eovcheck(x, ...)
      } else {
        stopPlot(tryEvalData)
      }
    }

  } else if (plotName == "pairs20x") {
    pairs20x(x, main = "", ...)
  } else if (plotName == "barplot") {
    barplot(x, cex.names = axis.size.cex[1], cex.axis = axis.size.cex[2], mgp = mgpy, las = lasy, ...)
  } else {
    tryEvalData <- try(eval(substitute(plotCommand(x, data = data, xlab = "", ylab = "", axes = axes, ...)), callerEnv), silent = TRUE)

    if (plotFailed(tryEvalData)) {
      tryEvalData <- try(eval(substitute(plotCommand(x, xlab = "", ylab = "", axes = axes, ...)), callerEnv), silent = TRUE)

      if (plotFailed(tryEvalData)) {
        stopPlot(tryEvalData)
      }
    }
  }

  if (!axes && !selfContained) {
    horizontalTest = isTRUE(eval(match.call()$horizontal, callerEnv))

    if (plotName == "boxplot" && !horizontalTest) {
      axis(1, mgp = mgpx, cex.axis = axis.size.cex[1], at = .at, labels = .labels)
    } else {
      axis(1, mgp = mgpx, cex.axis = axis.size.cex[1])
    }
    if (plotName == "boxplot" && horizontalTest) {
      axis(2, mgp = mgpy, cex.axis = axis.size.cex[2], las = lasy, at = .at, labels = .labels)
    } else {
      axis(2, mgp = mgpy, cex.axis = axis.size.cex[2], las = lasy)
    }

  }

  if (!selfContained) {
    title(xlab = x.lab, cex.lab = axis.lab.cex, line = linex)
    title(ylab = y.lab, cex.lab = axis.lab.cex, line = liney)
    box()
  }

  lapply(addElements, function(x) eval(x))

  invisible(NULL)
}
