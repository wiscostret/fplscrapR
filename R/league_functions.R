# get_league()

#' get_league
#'
#' This function fetches statistics for a Fantasy Premier League mini-league given the league ID and type. IMPORTANT NOTE: FPL generally asks scrapers NOT to abuse the /league sites (see: https://fantasy.premierleague.com/robots.txt).
#' @param leagueid The league ID(s). Can be found on the FPL website.
#' @param leaguetype The league type: 'classic' or 'h2h'.
#' @keywords league
#' @export
#' @examples
#' get_league(441,"classic")

get_league <- function(leagueid = NULL,leaguetype = "classic"){
  ifelse(
    is.null(leagueid),
    return(print("You'll need to input at least one entry ID, mate.")),
    {
      league <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/leagues-",leaguetype,"/",leagueid,"/standings/",sep=""))
      return(league)
    }
  )
}



jsonlite::fromJSON("https://fantasy.premierleague.com/api/leagues-classic/4552/standings/")

paste("https://fantasy.premierleague.com/api/leagues-","classic","/",4552,"/standings/",sep="")
