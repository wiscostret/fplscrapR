# fplscrapR

This package enables those interested in [Fantasy Premier League](https://fantasy.premierleague.com) to perform detailed data analysis of the game, using the FPL's JSON API. The fplscrapR functions help R users collect and parse data from the Official Fantasy Premier League website.

## Installation

You can install the released version of fplscrapR from [GitHub](https://github.com/wiscostret/fplscrapR/) with:

``` r
remotes::install_github("wiscostret/fplscrapR")

library(fplscrapR)
```
## Examples

On the [fplscrapR website](https://wiscostret.github.io/fplscrapR/), you can find examples for alle the functions available in fplscrapR, showing you how you can easily use the package to perform simple yet interesting FPL analysis.

## News

Key changes to fplscrapR v0.2.6:

* All functions updated to align with FPL’s updated API for the 21/22 season.

* Added new functions get_player_ids(), allowing you to easily pull all player ids for combining datasets, e.g. from get_player_info() and get_player_details().

* Added access to key FPL data from the 20/21 season. There is now full coverage going back to 16/17.

* Vignettes updated

* Various bug fixes

## Questions

If you have an questions or wish to collaborate, etc., find me on [GitHub](https://github.com/wiscostret) or [Twitter](https://www.twitter.com/fplscrapR) 
