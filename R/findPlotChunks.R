#' Find plotting chunks that do not write to a PDF file
#'
#' Reports code chunks that call a plotting function but neither open a
#' \code{pdf()} device nor use \code{\link{trimPlot}}. Chunks with
#' \code{eval=FALSE} are skipped.
#'
#' @param fi an object of class \code{fileInfo} describing an \code{.Rnw} file.
#'   See \code{\link{setFileNames}}.
#'
#' @return The matching line numbers in the file, invisibly. Each match is
#'   printed as the file name, line number and line. The file is not changed.
#'
#' @examples
#' root = tempfile()
#' dir.create(root)
#' writeLines(c("<<a>>=", "plot(1:3)", "@"), file.path(root, "demo.Rnw"))
#' findPlotChunks(setFileNames("demo", root, handout = FALSE, knitr = TRUE))
#'
#' @export
findPlotChunks = function(fi){
  Lines = readLines(fi$fullName)
  chunks = findChunks(Lines, fi$fName)

  plotCommands = paste0("(^|[^[:alnum:]._])",
                        "((box|qq|bar)?plot|cooks20x|(norm|eov)check|hist|modcheck|pairs(20x)?|trendscatter)",
                        "[[:space:]]*\\(")
  found = integer(0)

  for(row in seq_len(nrow(chunks))){
    cp = chunks[row, ]
    chunkHeader = Lines[cp$chunkStart]

    if(!grepl("eval *[=] *FALSE", chunkHeader)){
      chunkLines = Lines[cp$chunkStart:cp$chunkEnd]

      if(!any(grepl("(pdf|trimPlot)\\(", chunkLines))){
        plotLines = grep(plotCommands, chunkLines)
        fileLines = cp$chunkStart + plotLines - 1
        found = c(found, fileLines)

        for(line in fileLines){
          cat(paste0(fi$fName, " ", line, ":", Lines[line], "\n"))
        }
      }
    }
  }

  invisible(found)
}
