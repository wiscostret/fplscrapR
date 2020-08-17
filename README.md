# fplscrapR

This package enables those interested in [Fantasy Premier League](https://fantasy.premierleague.com) to perform detailed data analysis of the game, using the FPL's JSON API. The fplscrapR functions help R users collect and parse data from the Official Fantasy Premier League website.

## Installation

You can install the released version of fplscrapR from [GitHub](https://github.com/wiscostret/fplscrapR/) with:

``` r
if (!require(remotes)) {
  install.packages("remotes") 
}

remotes::install_github("wiscostret/fplscrapR")

library(fplscrapR)
```
## Examples

On the [fplscrapR website](https://wiscostret.github.io/fplscrapR/), you can find examples for alle the functions available in fplscrapR, showing you how you can easily use the package to perform simple yet interesting FPL analysis.

## News

August 2020 update: 

19/20 season data has been added. Remember you don´t need to update the package to grab the data from the new FPL season, or from prior seasons. Use any function to grab current season (20/21) data. Or apply the ´season´ parameter to get data from prior seasons.

---

Two new functions added in July 2020:

* get_entry_captain() - allowing you to pull captain info for any entry(s) and gw(s) as a dataframe.

* get_entry_player_picks() - allowing you to grab team selections for any entry(s) and gw(s) as a dataframe.

This information was already available through get_entry_picks() but required data manipulation to access given the structure of the nested list return. This should be more targeted, straightforward and user-friendly.

---

Key changes to fplscrapR v0.2.0:

* All functions updated to align with FPL's restructured API for the '19/20 season.

* Access to key FPL data from prior seasons on players and fixtures.

* Improved error messaging

* Access to full list of mini-league entries (with get_league_entries)

* Player and entry functions now default to grab all available player data if no name/id is supplied.

* Name parameter added to get_player_details to avoid confusion of finding player ID's.

* Vignettes updated

* Logo added

* Various bug fixes

## Questions

If you have an questions or wish to collaborate, etc., find me on [GitHub](https://github.com/wiscostret) or [Twitter](https://www.twitter.com/fplscrapR) 
