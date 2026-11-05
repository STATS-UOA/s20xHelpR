#' Rename the PDF files in an Sweave/knitr file to match their chunk labels
#'
#' Renames the PDF file written in each chunk after the chunk's label, with any
#' \code{.} replaced by \code{_}, and updates the \code{\\includegraphics} lines
#' that follow the chunk. Any directory in a path, such as \code{figure/}, is
#' kept. Run \code{\link{renumChunks}} first so that the labels are final.
#'
#' A chunk is processed when a line in it contains a quoted file name ending in
#' \code{.pdf}. Commented lines and chunks with \code{eval=FALSE} are ignored. A
#' chunk naming several PDF files gets the suffixes \code{A}, \code{B}, ... in
#' the order they appear, for example \code{RC-H13-003A}. It is an error for a
#' chunk with a PDF file to have no label. A plot with no matching \code{\\includegraphics} line is reported. PDF files
#' already on disk are not renamed.
#'
#' @param fi an object of class \code{fileInfo} describing an \code{.Rnw} file.
#'   See \code{\link{setFileNames}}.
#' @param dummyRun if \code{FALSE} then the changes will be committed to disk as
#'   well as being displayed on screen. It is a good idea to do a dummy run first.
#' @param backup if \code{TRUE} and \code{dummyRun} is \code{FALSE} then a backup
#'   file will be created with sequential numbering to stop over-writing existing
#'   backups. See \code{\link{writeBackup}}.
#'
#' @return \code{NULL}, invisibly. Each changed line is printed.
#'
#' @examples
#' root = tempfile()
#' dir.create(root)
#' writeLines(c("<<RC-H02-005>>=",
#'              "trimPlot(1:3, fileName = \"figure/old.pdf\")",
#'              "@",
#'              "\\includegraphics{figure/old}"),
#'            file.path(root, "demo.Rnw"))
#' fi = setFileNames("demo", root, handout = FALSE, knitr = TRUE)
#' renumPlotFilenames(fi)
#'
#' @export
renumPlotFilenames = function(fi, dummyRun = TRUE, backup = TRUE){
  if(!grepl("\\.Rnw$", fi$fName)){
    stop(paste0("renumPlotFilenames only works with .Rnw files, not: ", fi$fName, "\n"))
  }

  Lines = trimws(readLines(fi$fullName), which = "right")

  if(backup){
    writeBackup(src = fi$fName, path = fi$path, dummyRun = dummyRun)
  }

  chunks = findChunks(Lines, fi$fName)

  ## Include lines are searched for in the unedited text. Otherwise a name given
  ## to one chunk could be mistaken for the old name of a later chunk.
  originalLines = Lines

  for(row in seq_len(nrow(chunks))){
    cp = chunks[row, ]
    chunkHeader = Lines[cp$chunkStart]
    chunkLines = Lines[cp$chunkStart:cp$chunkEnd]

    ## Chunks hidden from students use echo=FALSE, they must still be evaluated
    if(!grepl("eval *[=] *FALSE", chunkHeader)){
      pdfFileLine = setdiff(grep("[.]pdf[\"']", chunkLines),
                            grep("^[[:space:]]*#", chunkLines))

      if(length(pdfFileLine) > 0){
        headerOptions = sub("^[[:space:]]*<<([^>]*)>>=$", "\\1", chunkHeader)
        chunkLabel = gsub(".", "_", parseChunkOptions(headerOptions)$label, fixed = TRUE)

        if(!nzchar(chunkLabel)){
          stop(paste0("Chunk has no label: ", chunkHeader, ". Run renumChunks first.\n"))
        }

        if(length(pdfFileLine) > length(LETTERS)){
          stop(paste0("There are too many pdf lines in chunk ", chunkHeader, "\n"))
        }

        ## A chunk writing several files uses the suffixes A, B, ...
        suffixes = if(length(pdfFileLine) > 1) LETTERS[seq_along(pdfFileLine)] else ""

        for(i in seq_along(pdfFileLine)){
          newName = paste0(chunkLabel, suffixes[i])
          pdfLine = chunkLines[pdfFileLine[i]]
          pdfFileName = regmatches(pdfLine, regexpr("[^\"'/]+(?=\\.pdf[\"'])", pdfLine, perl = TRUE))

          ## First replace the file name line
          pfnNum = cp$chunkStart + pdfFileLine[i] - 1

          cat("\n")
          cat(paste0("Old ", pfnNum, ": ", Lines[pfnNum], "\n"))
          Lines[pfnNum] = sub(paste0(pdfFileName, ".pdf"), paste0(newName, ".pdf"),
                              Lines[pfnNum], fixed = TRUE)
          cat(paste0("New ", pfnNum, ": ", Lines[pfnNum], "\n"))

          ## Are there any matching includegraphics statements?
          before = "(includegraphics(\\[[^]]*\\])?\\{([^}]*/)?)"
          after = "((\\.pdf)?\\})"
          igPattern = paste0(before, escapeRegex(pdfFileName), after)
          igLine = grep(igPattern, originalLines)
          igLine = igLine[igLine > cp$chunkStart]

          if(length(igLine) > 0){
            cat(paste0("Old ", igLine, ": ", Lines[igLine], "\n"))
            Lines[igLine] = gsub(igPattern, paste0("\\1", newName, "\\4"), Lines[igLine])
            cat(paste0("New ", igLine, ": ", Lines[igLine], "\n"))
            cat("\n")
          }else{
            cat(paste0("No matching includegraphics for ", pdfFileName, "\n"))
          }
        }
      }
    }
  }

  if(!dummyRun){
    writeLines(Lines, fi$fullName)
  }

  invisible(NULL)
}
