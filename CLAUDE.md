# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`s20xHelpR` is a small R package of helpers for producing the University of Auckland STATS 20x course notes. The notes are not in this repo. They are knitr `.Rnw` handouts in `~/Dropbox/Teaching/2027/STATS20x/CourseNotes/2023Sem2` (GitHub: `STATS-UOA/STATS20x`), which load this package through `s20xNotesHelper.R`.

## Commands

The R package workflow in the parent `CLAUDE.md` applies. Tests are in `tests/testthat`, and `s20x` must be installed to run them.

To try a change, use a throwaway copy of a file, never the real notes. `dummyRun = TRUE` shows the proposed edits without writing them:

```r
devtools::load_all()
fi <- setFileNames(1, "/tmp/notes")                  # /tmp/notes/H01/H01.Rnw
renumChunks(fi)                                      # dummy run
renumChunks(fi, dummyRun = FALSE)                    # writes the file, after a backup
```

For changes to `trimPlot()` or `slimSummary()`, knit a copy of the notes with the old and new versions and compare the `.tex` output and PDFs.

## How it fits together

Most functions take a `fileInfo` object from `setFileNames()`, a list of `part`, `path`, `fName` and `fullName`. `writeBackup()` is the exception: it takes `src` and `path` directly.

- **Rewrite the source file**: `renumChunks()` and `renumPlotFilenames()`. Both trim trailing whitespace from every line, default to `dummyRun = TRUE`, and call `writeBackup()` first. `writeBackup()` itself defaults to `dummyRun = FALSE`.
- **Report only**: `findPlotChunks()` and `findContract()`.
- **Used from inside the notes**: `trimPlot()` and `slimSummary()`. `rebuild()` runs `knit2pdf()` in the file's directory.

`R/utils.R` holds the internal helpers: `findChunks()` pairs chunk headers with their `@` lines, `parseChunkOptions()` separates a label from its options, and `summaryBlock()` extracts sections of printed output.

## Compatibility with the notes

The notes call `trimPlot()` 206 times and `slimSummary()` 66 times, so keep their argument names and defaults. Three behaviours look odd but are relied on:

- **`addElements` must stay lazily evaluated**: the notes write `addElements = list(abline(...))`, and that `list()` must not be forced until after the plot is drawn.
- **`trimPlot()` depends on `substitute()`**: `plotCommand` is identified by the name written in the call, and `...` reaches the plotting call unevaluated, so `pch = substr(Attend, 1, 1)` can see columns of `data`. The notes rely on arguments such as `which`, `main` and `cex` passing through `...`.
- **`slimSummary()` parses printed output on purpose**: this keeps the formatting identical to `summary()`, but a change to R's print methods can break it.

Plot PDFs are named after chunk labels, so run `renumChunks()` before `renumPlotFilenames()`. A chunk naming several PDFs gets suffixes such as `RC-H13-003A`. Hidden plotting chunks use `echo=FALSE`, not `eval=FALSE`.
