## Internal helpers shared by the functions that read Sweave/knitr files.

## Escape a string so that it matches literally inside a regular expression
escapeRegex = function(x){
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

## Locate the code chunks in the lines of an .Rnw file. Each chunk header is
## paired with the next line holding only @, so an @ outside a chunk is ignored.
findChunks = function(Lines, fName){
  chunkStart = grep("^[[:space:]]*<<[^>]*>>=[[:space:]]*$", Lines)
  chunkMarkers = grep("^[[:space:]]*@[[:space:]]*$", Lines)

  chunkEnd = vapply(seq_along(chunkStart), function(i){
    ends = chunkMarkers[chunkMarkers > chunkStart[i]]
    nextStart = if(i < length(chunkStart)) chunkStart[i + 1] else Inf

    if(length(ends) == 0 || ends[1] > nextStart){
      stop(paste0("Unmatched chunks in : ", fName, " at line ", chunkStart[i], "\n"))
    }

    ends[1]
  }, integer(1))

  data.frame(chunkStart = chunkStart, chunkEnd = chunkEnd)
}

## Split the text inside a chunk header into its label and its other options.
## The label is the leading unnamed option. Any further leading unnamed options
## are dropped because knitr only allows one, and earlier versions of
## renumChunks() could leave several behind.
parseChunkOptions = function(chunkOptions){
  options = trimws(chunkOptions)
  label = ""

  repeat{
    options = sub("^[[:space:],]+", "", options)
    token = sub(",.*$", "", options)

    if(!nzchar(token) || grepl("=", token, fixed = TRUE)){
      break
    }

    if(!nzchar(label)){
      label = trimws(token)
    }

    options = substring(options, nchar(token) + 1)
  }

  list(label = label, options = options)
}

## Extract the lines from the first match of startPattern to the line matching
## endPattern. Without an endPattern match the block runs to the next blank
## line. The closing line is included when includeEnd is TRUE.
summaryBlock = function(Lines, startPattern, endPattern = NULL, includeEnd = TRUE,
                        section = startPattern){
  start = grep(startPattern, Lines)[1]

  if(is.na(start)){
    stop(paste0("Could not find the ", section, " section of the summary output\n"))
  }

  following = seq_along(Lines) > start
  end = NA

  if(!is.null(endPattern)){
    end = which(following & grepl(endPattern, Lines))[1]
  }

  if(is.na(end)){
    end = which(following & nchar(Lines) == 0)[1]
  }

  if(is.na(end)){
    end = length(Lines)
  }else if(!includeEnd){
    end = end - 1
  }

  Lines[start:end]
}
