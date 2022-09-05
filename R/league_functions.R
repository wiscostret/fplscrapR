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
#' @param pages The number of pages of entries to fetch (there are 50 entries on each page). Can also be a list of pages if specificpage = TRUE.
#' @param specificpage Logical to specify whether a specific page is requested or not
#' @keywords league
#' @export

get_league_entries <- function(leagueid = NULL,
                               leaguetype = "classic",
                               pages = 1,
                               specificpage = FALSE){
  if(is.null(leagueid)) stop("You'll need to input a league ID, mate.")
  if(length(leagueid) != 1) stop("One league at a time, please.")
  if(is.list(pages) & isFALSE(specificpage)) stop("Can only supply a list if specificpage == TRUE")
  if(!is.list(pages)) if(pages %% 1 != 0) stop(
    "The number of pages needs to be a whole number, or a list of numbers when specificpage == TRUE."
  )
  if(!is.logical(specificpage)) stop("specificpage can only be TRUE/FALSE")
  
  {
    entries <- data.frame()
    if(specificpage == FALSE) {
      for (i in 1:pages){
        
        standings <- jsonlite::fromJSON(
          paste(
            "https://fantasy.premierleague.com/api/leagues-",
            leaguetype,
            "/",
            leagueid,
            "/standings/?page_standings=",
            i,
            sep = "")
        )
        
        entries <- rbind(entries, standings$standings$results)
        
      }
    } else if(specificpage == TRUE) {
      for(i in pages) {
        standings <- jsonlite::fromJSON(
          paste(
            "https://fantasy.premierleague.com/api/leagues-",
            leaguetype,
            "/",
            leagueid,
            "/standings/?page_standings=",
            i,
            sep = "")
        )
        
        entries <- rbind(entries,standings$standings$results)
      }
    }
    return(entries)
  }
}

# get_points_for_rank()

#' get_points_for_rank
#'
#' This function fetches the points required for an overall rank
#' @param ranks_of_intrest Number of a list of numbers for the rank(s)
#' @keywords league
#' @export

get_points_for_rank <- function(ranks_of_interest) {
  list_of_pages <- ceiling(ranks_of_interest/50) %>%
    as.list()
# Overall standings is league number 314  
  ranks <- get_league_entries(leagueid = 314,
                              pages = list_of_pages,
                              specificpage = TRUE)
  
  ranks <- ranks %>%
    dplyr::filter(rank_sort %in% ranks_of_interest) %>%
    dplyr::select(rank = rank_sort, total)
}