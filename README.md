
<!-- README.md is generated from README.Rmd. Please edit that file -->

# recycle

Experimental!

{recycle} provides a way to set a hook on garbage collection. This is
useful for instance if you want to run a R process periodically to log
some information about the state (time, working directory, git branch,
call stack, …) or automatically download some data to update a local
database etc…

You don’t pick when it happens but garbage collection is frequent, and
you can set a minimum delta not to call the hook too frequently, by
default set to 1 sec.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/recycle")
```

## Example

``` r
log <- function() message(sprintf("Hey there it's %s", Sys.time()))

# set the hook and triggers it a first time, use a 1 sec delta
recycle::recycle(log, delta = 1) # delta = 1 is the default
#> Hey there it's 2024-02-21 14:08:57

# doesn't trigger the hook, too soon!
invisible(gc())

Sys.sleep(2)

# triggers the hook again
invisible(gc())
#> Hey there it's 2024-02-21 14:08:59
```

Note that your hook is run in the current session so if might takes
time, consider the {callr} package to run a call in a new R process.

Something like this :

``` r
download_hook <- function() callr::r(function(arg) download_somthing(arg), args = list(some_global_var))
# download new data every 30 min, but don't freeze the session
recycle::recycle(download_hook, delta = 1800)
```

If you call `recycle::recycle()` another time your new hook will replace
the old, if you call `recycle::recycle(NULL)` the old hook will be
removed.
