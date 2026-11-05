#' s20xHelpR: helper functions for the STATS 20x course notes
#'
#' Tools for maintaining the knitr and R Markdown source of the STATS 20x
#' notes, and for producing compact output and figures inside them.
#'
#' Start with \code{\link{setFileNames}}, which describes a file, then pass the
#' result to \code{\link{renumChunks}}, \code{\link{renumPlotFilenames}},
#' \code{\link{findPlotChunks}}, \code{\link{findContract}} or
#' \code{\link{rebuild}}. \code{\link{trimPlot}} and
#' \code{\link{slimSummary}} are called from inside the notes.
#'
#' @importFrom grDevices dev.cur dev.list dev.off dev.set pdf
#' @importFrom graphics axis barplot box par title
#' @importFrom s20x cooks20x eovcheck normcheck pairs20x trendscatter
#' @importFrom utils capture.output
#' @keywords internal
"_PACKAGE"
