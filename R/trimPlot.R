#' To be documented
#'
#' @export
trimPlot = function(x, data = NULL, fileName, plotCommand = plot, x.lab = "", y.lab = "", axes = FALSE, mai = c(0.5, 0.5, 0.1, 0.1),
                    mgpx = c(3, .3, 0), mgpy = c(3, .6, 0), axis.size.cex = c(0.7, 0.7), axis.lab.cex = 0.7, linex = 1.2, liney = 1.8,
                    lasy = 1, fig.width = 10, fig.height = 7, .at = NULL, .labels = NULL, addElements = list(), ...){

  if(!grepl("^.*\\.pdf$", fileName)){
    stop(paste("Invalid filename:", fileName, "\n"))
  }


  if (substitute(plotCommand)  == "normcheck" || substitute(plotCommand)  =="cooks20x") {
    fig.width <- fig.width*2
    fig.height <- fig.height*2
  }

  pdf(fileName, width = fig.width, height = fig.height)
  par(mai = mai)

  # print(as.list(match.call(expand.dots=FALSE)))
  if (any(class(x) == "lm")) {
    tryEvalData <- try(eval(substitute(plotCommand(x, xlab = "", ylab = "", axes = axes, ...))), silent = TRUE)

    if (class(tryEvalData) == "try-error" && substitute(plotCommand) == "plot") {
      plot(x, caption = list("", "", "", "", "", ""), xaxt = ifelse(!axes, "n", "s"), yaxt = ifelse(!axes, "n", "s"), cex.id = 0.5, ...)
    }

    if (class(tryEvalData) == "try-error" && substitute(plotCommand) == "normcheck") {
      normcheck(x, ...)
    }

    if (class(tryEvalData) == "try-error" && substitute(plotCommand) == "cooks20x") {
      cooks20x(x, main = "", line = c(0.5, 1.5, 2), ...)
    }

    if (class(tryEvalData) == "try-error" && substitute(plotCommand) == "eovcheck") {
      eovcheck(x, main = "", ...)
    }

  } else if (substitute(plotCommand) == "pairs20x") {
    pairs20x(x, main = "", ...)
  } else if (substitute(plotCommand) == "barplot") {
    barplot(x, cex.names = axis.size.cex[1], cex.axis = axis.size.cex[2], mgp = mgpy, las = lasy, ...)
  } else {
    tryEvalData <- try(eval(substitute(plotCommand(x, data = data, xlab = "", ylab = "", axes = axes, ...))), silent = TRUE)
    if (class(tryEvalData) == "try-error") {
      try(eval(substitute(plotCommand(x, xlab = "", ylab = "", axes = axes, ...))), silent = TRUE)
    }
  }

  if(!axes && substitute(plotCommand) != "normcheck" && substitute(plotCommand) != "cooks20x" && substitute(plotCommand) != "pairs20x" && substitute(plotCommand) != "barplot"){
    if (is.null(as.list(match.call())$horizontal)) {
      horizontalTest = FALSE
    } else {
      horizontalTest = as.list(match.call())$horizontal
    }

    if (substitute(plotCommand) == "boxplot" && !horizontalTest) {
      axis(1, mgp = mgpx, cex.axis = axis.size.cex[1], at = .at, labels = .labels)
    } else {
      axis(1, mgp = mgpx, cex.axis = axis.size.cex[1])
    }
    if (substitute(plotCommand) == "boxplot" && horizontalTest) {
      axis(2, mgp = mgpy, cex.axis = axis.size.cex[2], las = lasy, at = .at, labels = .labels)
    } else {
      axis(2, mgp = mgpy, cex.axis = axis.size.cex[2], las = lasy)
    }

  }

  if(substitute(plotCommand) != "barplot" && substitute(plotCommand) != "normcheck" && substitute(plotCommand) != "cooks20x" && substitute(plotCommand) != "pairs20x"){
    title(xlab = x.lab, cex.lab = axis.lab.cex, line = linex)
    title(ylab = y.lab, cex.lab = axis.lab.cex, line = liney)
    box()
  }

  lapply(addElements, function(x) eval(x))

  graphics.off()
}

### Examples of how this is supposed to work - uncomment to test
# library(s20x)
# data(course.df)
# examtest.fit <- lm(Exam ~ Test, data = course.df)
# trimPlot(Exam ~ Test, data = course.df, fileName = "figure/test.pdf", pch = substr(Attend, 1, 1), cex = 0.7,
#          addElements = list(
#            points(course.df$Test[21],fitted(examtest.fit)[21], col="blue", pch=19, cex=0.7),
#            points(course.df$Test[21],course.df$Exam[21], col="red", pch=19, cex=0.7)
#          ))
#
# I want this to produce a PDF with reduced margins, and the scaling set to the scaling provided.
