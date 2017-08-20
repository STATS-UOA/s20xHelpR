findContract = function(fi){
  Lines = readLines(f1$fullName)
  matchLines = grep("[a-z]'[a-z]", Lines, perl = TRUE)
  for(line in matchLines){
    if(grepl("it's", Lines[line]) | grepl(""))
    cat(paste0(line, ": ", Lines[line], "\n"))
  }
}
