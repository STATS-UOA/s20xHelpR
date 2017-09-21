findPlotChunks = function(fi){
  Lines = readLines(fi$fullName)
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

    if(!grepl('eval *[=] *FALSE', chunkHeader)){
      chunkLines = Lines[(cp$chunkStart):(cp$chunkEnd)]

      if(!any(grepl('pdf\\(', chunkLines))){
        plotLines = grep(plotCommands, chunkLines)

        if(length(plotLines) > 0){
          cat(paste0(fi$fName, " ", plotLines, ":", chunkLines[plotLines], "\n"))
        }
      }
    }
  }
}

