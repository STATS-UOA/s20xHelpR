#' Create an object of class fileInfo
#'
#' Create a variable which stores more detailed information about a file.
#'
#' @param i either a handout number or the name of the file you wish to work with.
#' @param root the fully qualified path of the directory the file resides in.
#' @param handout if \code{TRUE} then this function thinks you're working with a handout.
#' @param knitr if \code{TRUE} then the file is a .Rnw file not a .Rmd file
#'
#' @export
setFileNames = function(i, root, handout = TRUE, knitr = FALSE){
  part = path = fName = NULL

  if(handout){
    i = as.numeric(match.arg(as.character(i), as.character(c(1:20))))

    if(is.na(i)){
      stop("i needs to be in {1, 2, ...,  20}")
    }

    part = paste0(ifelse(i < 10, "H0", "H"), i)
    path = paste0(root, "/", part, "/")
    fName = paste0(part, ".Rnw")


    if(is.null(fName)){
      stop(paste0("Input: ", i, " is invalid\n"))
    }
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
