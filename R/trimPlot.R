trimPlot = function(x, data = NULL, fileName, plotCommand = plot, xlab = "", ylab = "", axes = FALSE, mai = c(0.5, 0.5, 0.1, 0.1),
                    mgpx = c(3, .3, 0), mgpy = c(3, .5, 0), axis.size.cex = 0.7, axis.lab.cex = 0.7, linex = 1.2, liney = 1.8,
                    lasy = 1, fig.width = 10, fig.height = 7, addElements = list(), ...){
  
  if(!grepl("^.*\\.pdf$", fileName)){
    stop(paste("Invalid filename:", fileName, "\n"))
  }

  pdf(fileName, width = fig.width, height = fig.height)
  par(mai = mai)

  eval(substitute(plotCommand(x, data = data, xlab = "", ylab = "", axes = axes, ...)))
  
  if(!axes){
    axis(1, mgp = mgpx, cex.axis = axis.size.cex)
    axis(2, mgp = mgpy, cex.axis = axis.size.cex)
  }

  title(xlab = xlab, cex.lab = axis.lab.cex, line = linex)
  title(ylab = ylab, cex.lab = axis.lab.cex, line = liney, las = lasy)

  lapply(addElements, function(x) eval(parse(text = x)))
  
  box()
  graphics.off()
}

### Examples of how this is supposed to work - uncomment to test
# library(s20x)
# data(course.df)
# trimPlot(Exam ~ Test, data = course.df, fileName = "figure/test.pdf", pch = substr(Attend, 1, 1), cex = 0.7)
# 
# I want this to produce a PDF with reduced margins, and the scaling set to the scaling provided.
