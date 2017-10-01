#' To be documented
#'
#' @export
rebuild = function(fi){
  curPath = getwd()
  setwd(fi$path)
  cmd = paste0("knit2pdf(input = \"", fi$fName, "\")")
  cat(cmd)
  tryCatch(eval(parse(text = cmd)), finally = setwd(curPath))
}
