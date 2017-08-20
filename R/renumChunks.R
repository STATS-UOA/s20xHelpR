renumChunks = function(fi, dummyRun = TRUE, backup = TRUE){
  Lines = readLines(fi$fullName)

  if(backup){
    writeBackup(src = fi$fName, path = fi$path, dummyRun = dummyRun)
  }

  chunkLines = grep("^<<([^>]*)>>=$", Lines)

  chunkCounter = 0

  handoutNumber = gsub("^(H(0[1-9]|1[0-9]|20))\\.Rnw$", "\\1", fi$fName)

  for(lineNum in chunkLines){
    line = Lines[lineNum]
    chunkOptions = gsub("^<<([^>]*)>>=$", "\\1", line)

    chunkLabel = paste0("RC-", handoutNumber, "-", if(chunkCounter < 10){
                                                      "00"
                                                    }else if (chunkCounter >=10 && chunkCounter < 100){
                                                      "0"
                                                    }else{
                                                      ""
                                                    }, chunkCounter)
    chunkCounter = chunkCounter + 1
    ## remove any existing chunk label
    pattern ="(RC-(0[1-9]|1[0-9]|20)-[0-9]{3}|R[^,>]+)(, )*"
    if(grepl(pattern, chunkOptions)){
      r = regexpr(pattern, chunkOptions)
      tmp = regmatches(chunkOptions, r)
      start = r + nchar(tmp)
      chunkOptions = if(start >= nchar(chunkOptions)){
        ""
      }else{
        substr(chunkOptions, start, nchar(chunkOptions))
      }
    }

    chunkOptions = if(nchar(chunkOptions) > 0){
                     paste0(chunkLabel, ", ", chunkOptions)
                   }else{
                     chunkLabel
                   }

    replacementLine = paste0("<<", chunkOptions, ">>=")
    cat(paste0(lineNum,": ", replacementLine, "\n"))

    if(!dummyRun){
      Lines[lineNum] = replacementLine
    }
  }

  if(!dummyRun){
    writeLines(Lines, fi$fullName)
  }
}
