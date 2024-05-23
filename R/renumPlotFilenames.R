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
  Lines = trimws(readLines(fi$fullName), which = "right")

  if(backup){
    writeBackup(src = fi$fName, path = fi$path, dummyRun = dummyRun)
  }

  chunks = data.frame(chunkStart = grep("^ *<<[^>]*>>= *$", Lines),
                      chunkEnd = grep(" *^[@] *$", Lines))

  if(length(chunks$chunkStart) != length(chunks$chunkEnd)){
    stop(paste0("Unmatched chunks in : ", fi$fName, "\n"))
  }

  numChunks = nrow(chunks)

  plotCommands = "((box|qq|trim)?plot|cooks20x|(norm|eov)check|hist|modcheck|pairs(20x)*|trendscatter)"

  for(row in 1:numChunks){
    cp = chunks[row,]
    chunkHeader = Lines[cp$chunkStart]
    chunkLines = Lines[(cp$chunkStart):(cp$chunkEnd)]

    if(!grepl('eval *[=] *FALSE', chunkHeader)){ ## got to be echo=FALSE in chunk because we do want to evaluate it but we don't want the students to see it.
      pdfFileLine = grep('pdf', chunkLines)

      if(length(pdfFileLine) > 1){
        stop(paste0("There is more than one pdf line in chunck ", chunkHeader, "\n"))
      }

      if(length(pdfFileLine) == 1){
        chunkLabel = gsub("\\.", "_", gsub("^<<([^,>]+).*>>=$", "\\1", chunkHeader))
        pdfFileName = gsub('^[^"]*"(figure/)?([^.]*)\\.pdf".*$', '\\2', chunkLines[pdfFileLine])

        ## first replace the file name line

        ## We need the location of the file name line in the original file
        pfnNum = grep(chunkLines[pdfFileLine], Lines)

        if(length(pfnNum) != 1) ## we didn't find it which would be odd but might happen
          stop(paste0("Could not find the line '",
                      chunkLines[pdfFileLine],
                      "'\n"))

        # browser()
        cat("\n")
        cat(paste0('Old ', pfnNum, ': ', Lines[pfnNum], '\n'))
        Lines[pfnNum] = gsub(paste0('(figure\\/)?', pdfFileName, '.pdf'),
                             paste0(chunkLabel, '.pdf'),
                             Lines[pfnNum])

        cat(paste0('New ', pfnNum, ': ', Lines[pfnNum], '\n'))


        ## Are there any matching includegraphics statements?
        igLine = grep(paste0('includegraphics(\\[[^]*\\])?\\{(figure\\/)?', pdfFileName),
                      Lines)

        if(length(igLine > 0)){
          cat(paste0('Old ', igLine, ': ', Lines[igLine], '\n'))
          Lines[igLine] = gsub(paste0('(figure\\/)?', pdfFileName),
                               chunkLabel,
                               Lines[igLine])
          cat(paste0('New ', igLine, ': ', Lines[igLine], '\n'))
          cat("\n")
        }else{
          cat(paste0("No matching includegraphics for ", pdfFileName, "\n"))
        }
        #cat(paste0(chunkLines[pdfFileLine], "\n"))
      }
    }
  }

  if(!dummyRun){
    writeLines(Lines, fi$fullName)
  }
}
