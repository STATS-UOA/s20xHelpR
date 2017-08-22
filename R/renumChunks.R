renumChunks = function(fi, dummyRun = TRUE, backup = TRUE, knitr = TRUE, labelStub = NULL){
  Lines = readLines(fi$fullName)

  if(backup){
    writeBackup(src = fi$fName, path = fi$path, dummyRun = dummyRun)
  }

  chunkLines = if(knitr){
    grep("^<<([^>]*)>>=$", Lines)
  }else{
    grep("^[`]{3}\\{r[^}]*\\}$", Lines)
  }

  chunkCounter = 0

  handoutNumber = gsub("^(H(0[1-9]|1[0-9]|20))\\.Rnw$", "\\1", fi$fName)

  for(lineNum in chunkLines){
    line = Lines[lineNum]
    chunkOptions = if(knitr){
      gsub("^<<([^>]*)>>=$", "\\1", line)
    }else{
      gsub("^[`]{3}\\{r([^}]*)\\}$", "\\1", line)
    }

    if(is.null(labelStub)){
      labelStub = paste0("RC-", handoutNumber, "-")
    }
    chunkLabel = paste0(labelStub, str_pad(as.character(chunkCounter), width = 3, side = "left", pad = "0"))
    chunkCounter = chunkCounter + 1
    ## remove any existing chunk label
    pattern = if(knitr){
      "(RC-(0[1-9]|1[0-9]|20)-[0-9]{3}|R[^,>]+)(, )*"
    }else{
      "(rc-(cs[0-9]+)-[0-9]{3})(, )*"
    }
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
                     paste0(toupper(chunkLabel), ", ", chunkOptions)
                   }else{
                     toupper(chunkLabel)
                   }

    replacementLine = if(knitr){
      paste0("<<", chunkOptions, ">>=")
    }else{
      paste0("```{r ", chunkOptions, "}")
    }
    cat(paste0(lineNum,": ", replacementLine, "\n"))

    if(!dummyRun){
      Lines[lineNum] = replacementLine
    }
  }

  if(!dummyRun){
    writeLines(Lines, fi$fullName)
  }
}
