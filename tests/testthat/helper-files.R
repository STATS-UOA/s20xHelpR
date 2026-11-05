## Write lines to a new .Rnw or .Rmd file and describe it with a fileInfo object
makeFile = function(Lines, env = parent.frame(), name = "demo", knitr = TRUE){
  root = withr::local_tempdir(.local_envir = env)
  extension = if(knitr) ".Rnw" else ".Rmd"
  writeLines(Lines, file.path(root, paste0(name, extension)))
  setFileNames(name, root, handout = FALSE, knitr = knitr)
}

quietly = function(expr){
  utils::capture.output(result <- expr)
  invisible(result)
}
