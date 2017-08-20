setFileNames = function(i, root){
  part = path = fName = NULL

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

  l = list(part = part, path = path, fName = fName,
           fullName = paste0(path, fName))
  class(l) = "fileInfo"

  return(l)
}
