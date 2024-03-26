
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# recycle <img src="man/figures/logo.png" align="right" height="139" alt="" />

{recycle} provides a way to set a hook on garbage collection. This is
useful for instance if you want to run a R process periodically. Use
cases might be:

- Notifications
- Logs
- Diagnostics
- Updates

You can’t pick when garbage collection occurs but it is frequent, so the
next best thing is to pick a minimum delta between calls to the hook,
and this is what we do (1 sec by default).

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/recycle")
```

## Example

``` r
log <- function() message(sprintf("This R session currently takes %s", capture.output(pryr::mem_used())))

# set the hook and triggers it a first time, use a 1 sec delta
recycle::recycle(log) # delta = 1 is the default
#> This R session currently takes 48.8 MB

# doesn't trigger the hook, too soon!
invisible(gc())

Sys.sleep(2)

some_object <- sample(1e6, 1e6)
# triggers the hook again
invisible(gc())
#> This R session currently takes 53.1 MB
```

We can also run the code in the background in another session by setting
`background = TRUE`. Since it’s called in another session and we don’t
wait for the output we cannot print to the console with this strategy so
we’ll use another example.

Let’s have a purposeless progress bar widget fill up on garbage
collection.

By definition a function run in parallel cannot print to the console,
the following example shows a spinner for 3 sec on garbage collection.

Run the following example in your session and you should see that your
session is still active while the progress bar is running

``` r
fill_pb <- function() {
  pb <- tcltk::tkProgressBar()
  for(i in 1:100) {
    Sys.sleep(0.05)
    tcltk::setTkProgressBar(pb, i/100, sprintf("test (%d%% done)", i))
  }
}
recycle::recycle(fill_pb, background = TRUE)
x <- 1 # the progress bar is still going on
x
```

If you call `recycle::recycle(new_hook)` your new hook will replace the
old, if you call `recycle::recycle(NULL)` the old hook will be removed.
