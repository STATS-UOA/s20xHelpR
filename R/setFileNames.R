setFileNames = function(i, root){
  part = path = fName = NULL

  i = match.arg(i, c(1:17))

  if(i %in% as.character(1:17)){
    i = as.numeric(i)
    part = paste0(ifelse(i < 10, "H0", "H"), i)
    path = paste0(root, "/", part, "/")
    fName = paste0(part, ".Rnw")
  }

  if(is.null(fName)){
    stop(paste0("Input: ", i, " is invalid\n"))
  }

  l = list(part = part, path = path, fName = fName,
           fullName = paste0(path, fName))
  class(l) = "fileInfo"

  return(l)
}
