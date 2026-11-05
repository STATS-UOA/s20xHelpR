#' Make a numbered backup of a file
#'
#' Copies \code{src} to \code{src.bak<n>} in the same directory, where \code{n}
#' is one more than the highest existing backup of that file, so that backups are
#' never overwritten.
#'
#' @param src the name of the file, without its directory.
#' @param path the directory containing the file.
#' @param dummyRun if \code{TRUE} then the copy is displayed on screen but not made.
#'
#' @return The path of the backup file, invisibly.
#'
#' @examples
#' root = tempfile()
#' dir.create(root)
#' writeLines("text", file.path(root, "demo.Rnw"))
#' writeBackup("demo.Rnw", root)
#' list.files(root)
#'
#' @export
writeBackup = function(src, path, dummyRun = FALSE){
  path = paste0(sub("/+$", "", path), "/")

  ## get all the backup files
  bkupPattern = paste0("^", escapeRegex(src), "\\.bak([0-9]+)$")
  bkupFiles = list.files(path = path, pattern = bkupPattern)
  bkupNum = 1

  if(length(bkupFiles) > 0){
    bkupNum = max(as.numeric(sub(bkupPattern, "\\1", bkupFiles))) + 1
  }

  dest = paste0(path, src, ".bak", bkupNum)
  srcFile = paste0(path, src)
  cat(paste0("cp '", srcFile, "' '", dest, "'\n"))

  if(!dummyRun && !file.copy(srcFile, dest)){
    stop(paste0("Could not back up ", srcFile, " to ", dest, "\n"))
  }

  invisible(dest)
}
