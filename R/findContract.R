#' To be documented
#'
#' @export
findContract = function(fi){
  Lines = readLines(fi$fullName)
  matchLines = grep("[a-z]'[a-z]", Lines, perl = TRUE)
  for(line in matchLines){
    if(grepl("it's", Lines[line]) | grepl("", Lines[line]))
    cat(paste0(line, ": ", Lines[line], "\n"))
  }
}
