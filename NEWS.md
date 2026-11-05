# s20xHelpR 0.2.0

The interfaces of `trimPlot()` and `slimSummary()` are unchanged, so existing
calls in the notes keep working. `s20x` and `knitr` are now installed with this
package. `library(stringr)` is no longer attached by it.

## Bug fixes

* `trimPlot()` no longer closes every open graphics device. It closes its own
  PDF, including after an error, and leaves other devices open and active.
* `trimPlot()` now raises an error naming the plot and the cause when a plot
  cannot be drawn. Failures were previously hidden, or surfaced as an unrelated
  error from `axis()`.
* `trimPlot()` can now see objects in the calling environment, not just the
  global environment. `rebuild()` knits inside a function, so plots using
  objects created in the notes previously failed there.
* `trimPlot()` recognises functions written with a namespace, such as
  `s20x::normcheck`, accepts a single value for `axis.size.cex`, and works with
  `eovcheck` on a fitted model with the current `s20x`.
* `slimSummary()` shows calls that span several lines in full, aligns the
  residual output, and handles weighted models. Coefficients with names starting
  `lm` or `glm` are no longer printed as part of the call.
* `slimSummary()` gives a clear error for `aov` and multiple response models.
* `renumChunks()` no longer stacks labels when rerun on R Markdown files, no
  longer writes `, ,` into chunk headers, and now replaces any existing label.
  Previously labels that did not start with `R` were kept as a second label.
  Options that start with `R` are no longer at risk of being removed.
* `renumChunks()` renumbers indented chunks and ignores other languages in
  R Markdown, such as `{rcpp}`.
* `renumPlotFilenames()` now works when the PDF name is on the same line as a
  `pdf(` or `trimPlot(` call.
* `renumPlotFilenames()` keeps the `figure/` directory in `\includegraphics`
  lines, handles single quotes, `.pdf` extensions and text after the closing
  brace, and no longer drops into `browser()`.
* `renumPlotFilenames()` handles chunks that write several PDF files, naming them
  with the suffixes `A`, `B`, ... after the chunk label.
* `renumPlotFilenames()` no longer confuses a new name with the old name of a
  later chunk, or treats any line containing "pdf" as a file name.
* `findContract()` now reports contractions only. It previously reported every
  apostrophe between letters, including possessives.
* `findPlotChunks()` reports line numbers in the file rather than in the chunk,
  does not match names such as `lm.plot` or `histogram`, and does not report
  `trimPlot()` calls, which write their own PDF.
* `findPlotChunks()` and `renumPlotFilenames()` handle files with no chunks, and
  ignore `@` lines outside chunks.
* `writeBackup()` numbers backups of any file type. `.Rmd` backups previously
  overwrote each other. It also works with quotes in paths, and raises an error
  if the copy fails. Numbering is now separate for each file.
* `rebuild()` works without `knitr` attached.
* `setFileNames()` accepts a `root` with a trailing slash and gives a clear error
  for an invalid handout number.

## Other changes

* `renumChunks()` now chooses knitr or R Markdown chunks from the file extension
  by default. Its default label stub uses the file's `part`, so files that are
  not handouts get labels such as `RC-CS1-000`.
* Every function is documented, with examples, and the package has a test suite.
* The stale `hello()` help page and the merge conflict markers in the help pages
  have been removed.
