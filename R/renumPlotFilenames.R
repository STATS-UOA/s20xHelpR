#' Renumber all the PDF file names in an Sweave/Knitr file
#'
#' Makes sure the file
#'
#' @param fi an object of class \code{fileInfo}. See \code{\link{setFileNames}}.
#' @param dummyRun if \code{FALSE} then the changes will be committed to disk as well as being displayed on screen. It is a good idea to do a dummy run first.
#' @param backup if \code{TRUE} and \code{dummyRun} is \code{FALSE} then a backup file will be created with sequential numbering to stop over-writing existing backups.
#'
#' @export
renumPlotFilenames = function(fi, dummyRun = TRUE, backup = TRUE){
  Lines = readLines(fi$fullName)

  if(backup){
    writeBackup(src = fi$fName, path = fi$path, dummyRun = dummyRun)
  }

  chunks = data.frame(chunkStart = grep("^ *<<[^>]*>>= *$", Lines),
                      chunkEnd = grep(" *^[@] *$", Lines))

  if(length(chunks$chunkStart) != length(chunks$chunkEnd)){
    stop(paste0("Unmatched chunks in : ", fi$fName, "\n"))
  }

  numChunks = nrow(chunks)

  plotCommands = "(boxplot|cooks20x|eovcheck|hist|modcheck|pairs(20x)*|plot|trendscatter)"

  for(row in 1:numChunks){
    cp = chunks[row,]
    chunkHeader = Lines[cp$chunkStart]
    chunkLines = Lines[(cp$chunkStart):(cp$chunkEnd)]

    if(!grepl('eval *[=] *FALSE', chunkHeader)){ ## got to be echo=FALSE in chunk because we do want to evaluate it but we don't want the students to see it.
      if(any(grepl('pdf\\(', chunkLines))){
        chunkLabel = gsub("\\.", "_", gsub("^<<([^,>]+).*>>=$", "\\1", chunkHeader))
        pdfLine = chunkLines[grep('pdf\\(', chunkLines)]
        cat(paste(pdfLine, "\n"))
      }
    }
  }

  if(!dummyRun){
    writeLines(Lines, fi$fullName)
  }
}
