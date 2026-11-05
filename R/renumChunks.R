#' Renumber all the code chunks in an Sweave/knitr or R Markdown file
#'
#' Gives every chunk a sequential unique label, numbered from 000. The existing
#' label, which is the leading unnamed chunk option, is replaced and all other
#' chunk options are kept. Trailing whitespace is removed from every line when
#' the file is written.
#'
#' @param fi an object of class \code{fileInfo}. See \code{\link{setFileNames}}.
#' @param dummyRun if \code{FALSE} then the changes will be committed to disk as
#'   well as being displayed on screen. It is a good idea to do a dummy run first.
#' @param backup if \code{TRUE} and \code{dummyRun} is \code{FALSE} then a backup
#'   file will be created with sequential numbering to stop over-writing existing
#'   backups. See \code{\link{writeBackup}}.
#' @param knitr if \code{TRUE} then knitr style code chunks are expected otherwise
#'   R Markdown code chunks are expected. By default this follows the file
#'   extension.
#' @param labelStub used for specifying a bespoke label stub for each label. If it
#'   is not specified (default) then \code{RC-} and the \code{part} of \code{fi}
#'   are used, for example \code{RC-H02-}. Labels are written in upper case.
#'
#' @return \code{NULL}, invisibly. Each new chunk header is printed.
#'
#' @examples
#' root = tempfile()
#' dir.create(root)
#' writeLines(c("<<old, echo=FALSE>>=", "1", "@"), file.path(root, "demo.Rnw"))
#' fi = setFileNames("demo", root, handout = FALSE, knitr = TRUE)
#' renumChunks(fi)
#'
#' @export
renumChunks = function(fi, dummyRun = TRUE, backup = TRUE,
                       knitr = grepl("\\.Rnw$", fi$fName), labelStub = NULL){
  Lines = trimws(readLines(fi$fullName), which = "right")

  if(backup){
    writeBackup(src = fi$fName, path = fi$path, dummyRun = dummyRun)
  }

  chunkPattern = if(knitr){
    "^([[:space:]]*)<<([^>]*)>>=$"
  }else{
    "^([[:space:]]*)[`]{3}\\{r([[:space:],][^}]*)?\\}$"
  }

  chunkLines = grep(chunkPattern, Lines)

  if(is.null(labelStub)){
    labelStub = paste0("RC-", fi$part, "-")
  }

  chunkCounter = 0

  for(lineNum in chunkLines){
    line = Lines[lineNum]
    indent = sub(chunkPattern, "\\1", line)
    chunkOptions = parseChunkOptions(sub(chunkPattern, "\\2", line))$options

    chunkLabel = toupper(sprintf("%s%03d", labelStub, chunkCounter))
    chunkCounter = chunkCounter + 1

    chunkOptions = if(nchar(chunkOptions) > 0){
      paste0(chunkLabel, ", ", chunkOptions)
    }else{
      chunkLabel
    }

    replacementLine = if(knitr){
      paste0(indent, "<<", chunkOptions, ">>=")
    }else{
      paste0(indent, "```{r ", chunkOptions, "}")
    }

    cat(paste0(lineNum, ": ", replacementLine, "\n"))

    if(!dummyRun){
      Lines[lineNum] = replacementLine
    }
  }

  if(!dummyRun){
    writeLines(Lines, fi$fullName)
  }

  invisible(NULL)
}
