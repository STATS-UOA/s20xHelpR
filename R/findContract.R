#' Find contractions in a notes file
#'
#' Prints each line containing a contraction such as \emph{it's}, \emph{don't}
#' or \emph{we'll}, so they can be removed from formal writing. Possessives such
#' as \emph{the student's mark} are not reported.
#'
#' @param fi an object of class \code{fileInfo}. See \code{\link{setFileNames}}.
#'
#' @return The matching line numbers, invisibly. The file is not changed.
#'
#' @examples
#' root = tempfile()
#' dir.create(root)
#' writeLines(c("It's a fit.", "The student's mark."), file.path(root, "demo.Rnw"))
#' findContract(setFileNames("demo", root, handout = FALSE, knitr = TRUE))
#'
#' @export
findContract = function(fi){
  Lines = readLines(fi$fullName)

  pattern = paste0("\\b(?:it|that|there|here|what|who|he|she|let|how)['\u2019]s\\b",
                   "|n['\u2019]t\\b",
                   "|['\u2019](?:re|ve|ll|d|m)\\b")
  matchLines = grep(pattern, Lines, perl = TRUE, ignore.case = TRUE)

  for(line in matchLines){
    cat(paste0(line, ": ", Lines[line], "\n"))
  }

  invisible(matchLines)
}
