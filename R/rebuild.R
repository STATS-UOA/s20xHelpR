#' Rebuild the PDF for a notes file
#'
#' Runs \code{\link[knitr]{knit2pdf}} in the directory containing the file, then
#' returns to the current working directory, even if the build fails.
#'
#' @param fi an object of class \code{fileInfo}. See \code{\link{setFileNames}}.
#'
#' @return The value of \code{\link[knitr]{knit2pdf}}, the path of the PDF file.
#'
#' @examples
#' \dontrun{
#' rebuild(setFileNames(2, "~/notes"))
#' }
#'
#' @export
rebuild = function(fi){
  curPath = getwd()
  on.exit(setwd(curPath))
  setwd(fi$path)
  cat(paste0("knit2pdf(input = \"", fi$fName, "\")\n"))
  knitr::knit2pdf(input = fi$fName)
}
