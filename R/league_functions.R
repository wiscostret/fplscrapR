# get_league()

#' get_league
#'
#' This function fetches basic information for a Fantasy Premier League mini-league given the league ID and type.
#' @param leagueid The league ID(s). Can be found on the FPL website.
#' @param leaguetype The league type: 'classic' or 'h2h'.
#' @keywords league
#' @export

get_league <- function(leagueid = NULL, leaguetype = "classic"){
  if(is.null(leagueid)) stop("You'll need to input a league ID, mate.")
  if(length(leagueid) != 1) stop("One league at a time, please.")
  {
    league <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/leagues-",leaguetype,"/",leagueid,"/standings/",sep=""))
    return(league)
  }
}

# get_league_entries()

#' get_league_entries
#'
#' This function fetches a list of entries in the standings for a Fantasy Premier League mini-league given the league ID and type.
#' @param leagueid The league ID(s). Can be found on the FPL website.
#' @param leaguetype The league type: 'classic' or 'h2h'.
#' @param pages The number of pages of entries to fetch (there are 50 entries on each page)
#' @keywords league
#' @export

get_league_entries <- function(leagueid = NULL, leaguetype = "classic", pages = 1){
  if(is.null(leagueid)) stop("You'll need to input a league ID, mate.")
  if(length(leagueid) != 1) stop("One league at a time, please.")
  if(pages %% 1 != 0) stop("The number of pages needs to be a whole number.")

  {
    entries <- data.frame()

    for (i in 1:pages){

      standings <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/leagues-",leaguetype,"/",leagueid,"/standings/?","page_standings=",i,sep=""))

      entries <- rbind(entries,standings$standings$results)

    }

    return(entries)
  }
}
