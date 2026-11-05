#' Create an object of class fileInfo
#'
#' Create a variable which stores more detailed information about a file. The
#' other file functions in this package take this object as their first argument.
#'
#' A handout \code{i} is found at \code{<root>/H0i/H0i.Rnw}, for example
#' \code{<root>/H02/H02.Rnw}. Any other file is found directly in \code{root}.
#' The file does not have to exist yet.
#'
#' @param i either a handout number or the name of the file you wish to work with,
#'   without its extension.
#' @param root the fully qualified path of the directory the file resides in. For
#'   a handout, this is the directory containing the \code{H01}, \code{H02}, ...
#'   directories.
#' @param handout if \code{TRUE} then this function thinks you're working with a
#'   handout, and \code{i} must be a whole number from 1 to 20.
#' @param knitr if \code{TRUE} then the file is a .Rnw file not a .Rmd file. This
#'   is ignored for handouts, which are always .Rnw files.
#'
#' @return A list of class \code{fileInfo} with elements \code{part} (the name
#'   without its extension), \code{path} (the directory, ending in \code{/}),
#'   \code{fName} (the file name) and \code{fullName} (the path and file name).
#'
#' @examples
#' setFileNames(2, "~/notes")$fullName
#' setFileNames("casestudy", "~/notes", handout = FALSE)$fullName
#'
#' @export
setFileNames = function(i, root, handout = TRUE, knitr = FALSE){
  root = sub("/+$", "", root)

  if(handout){
    i = suppressWarnings(as.numeric(i))

    if(length(i) != 1 || is.na(i) || !(i %in% 1:20)){
      stop("i needs to be in {1, 2, ...,  20}")
    }

    part = sprintf("H%02d", i)
    path = paste0(root, "/", part, "/")
    fName = paste0(part, ".Rnw")
  }else{
    part = i
    path = paste0(root, "/")
    fName = paste0(part, ifelse(knitr, ".Rnw", ".Rmd"))
  }

  l = list(part = part, path = path, fName = fName,
           fullName = paste0(path, fName))
  class(l) = "fileInfo"

  return(l)
}
