# s20xHelpR

Helper functions for producing the University of Auckland STATS 20x course
notes. The package maintains the knitr and R Markdown source of the notes, and
provides the compact figures and model output used inside them.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("STATS-UOA/s20xHelpR")
```

## Inside the notes

`trimPlot()` writes a plot to a PDF with small margins, ready for
`\includegraphics`:

``` r
trimPlot(Exam ~ Test, data = course.df,
         fileName = "figure/RC-H02-005.pdf",
         x.lab = "Test", y.lab = "Exam",
         fig.width = 5, fig.height = 3.5,
         addElements = list(abline(examtest.fit, lty = 2)))
```

`slimSummary()` prints a shortened model summary. By default that is the
coefficients and overall fit for an `lm`, and the call, coefficients and
deviance for a `glm`.

``` r
slimSummary(lm(mpg ~ wt, data = mtcars))
```

## Maintaining the source files

`setFileNames()` describes a file, and the other functions take the result.
Handout 2 is expected at `<root>/H02/H02.Rnw`.

``` r
fi <- setFileNames(2, "~/notes")

renumChunks(fi)                          # shows the new chunk labels
renumChunks(fi, dummyRun = FALSE)        # writes them, after making a backup

renumPlotFilenames(fi)                   # names each PDF after its chunk
renumPlotFilenames(fi, dummyRun = FALSE)

findPlotChunks(fi)                       # plots that aren't written to a PDF
findContract(fi)                         # contractions in the text
rebuild(fi)                              # runs knit2pdf() on the file
```

Functions that change a file only show the changes unless `dummyRun = FALSE`.
Run `renumChunks()` before `renumPlotFilenames()`, because plot files are named
after their chunk labels.

`renumChunks()` also works on R Markdown files. Use
`setFileNames("casestudy", "~/notes", handout = FALSE)` for those, and for `.Rnw`
files that aren't handouts add `knitr = TRUE`.
