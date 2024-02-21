
<!-- README.md is generated from README.Rmd. Please edit that file -->

# recycle

{recycle} provides a way to set a hook on garbage collection. This is
useful for instance if you want to run a R process periodically to log
some informations about the state (time, working directory, git branch…)
or automatically download some data to update a local database etc…

You don’t pick when it happens but garbage collection is frequent, and
you can set a minimum delta not to call the hook too frequently.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/recycle")
```

## Example

Log time to console

``` r
log <- function() message(sprintf("Hey there it's %s", Sys.time()))
recycle::recycle(log, delta = 1)
#> NULL
# triggers the hook
gc()
#>          used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
#> Ncells 471948 25.3    1009637   54         NA   673000 36.0
#> Vcells 899529  6.9    8388608   64      16384  1858040 14.2
# doesn't trigger the hook, too soon!
gc()
#>          used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
#> Ncells 474004 25.4    1009637   54         NA   673000 36.0
#> Vcells 881044  6.8    8388608   64      16384  1858040 14.2
Sys.sleep(2)
# triggers the hook again
gc()
#>           used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
#> Ncells  474041 25.4    1009637   54         NA   673000 36.0
#> Vcells 1436128 11.0    8388608   64      16384  1858040 14.2
```
