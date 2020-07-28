# get_entry()

#' get_entry
#'
#' This function fetches detailed information for a Fantasy Premier League entry given the entry ID(s).
#' @param entryid The entry ID(s). Can be found on the FPL website under 'Gameweek history' in the URL - https://fantasy.premierleague.com/a/entry/XXXXXX/history.
#' @keywords entry
#' @export
#' @examples
#' get_entry(1076)

get_entry <- function(entryid = NULL){
  if(is.null(entryid)) stop("You'll need to input at least one entry ID, mate.")
  if(length(entryid) != 1) stop("One at a time, please")
   {
     entry <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid,"/",sep=""))
     return(entry)
   }
}

# get_entry_hist()

#' get_entry_hist
#'
#' This function fetches historical statistics for a Fantasy Premier League entry given the entry ID(s).
#' @param entryid The entry ID(s). Can be found on the FPL website under 'Gameweek history' in the URL - https://fantasy.premierleague.com/a/entry/XXXXXX/history. You can list multiple, e.g. with c().
#' @keywords entry
#' @export
#' @examples
#' get_entry_hist(1076)

get_entry_hist <- function(entryid = NULL){
  if(is.null(entryid)) stop("You'll need to input at least one entry ID, mate.")
    {
      entryhistory2 <- data.frame()
      for (i in 1:length(entryid)){
        entry <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid[i],"/",sep=""))
        entryhistory <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid[i],"/history","/",sep=""))
        if(length(entryhistory$past)==0) {entryhistory$past <- data.frame(season_name="",total_points="",rank="")}
        entryhistory2 <- rbind(entryhistory2,data.frame(entryhistory$past,name=paste(entry$player_first_name,entry$player_last_name)))
      }
      return(entryhistory2)
    }
}

# get_entry_season()

#' get_entry_season
#'
#' This function fetches in-season statistics for a Fantasy Premier League entry given the entry ID(s).
#' @param entryid The entry ID(s). Can be found on the FPL website under 'Gameweek history' in the URL - https://fantasy.premierleague.com/a/entry/XXXXXX/history. You can list multiple, e.g. with c().
#' @keywords entry
#' @export
#' @examples
#' get_entry_season(1076)

get_entry_season <- function(entryid = NULL){
  if(is.null(entryid)) stop("You'll need to input at least one entry ID, mate.")
    {
      entryseason2 <- data.frame()
      for (i in 1:length(entryid)){
        entry <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid[i],"/",sep=""))
        entryseason <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid[i],"/history","/",sep=""))
        entryseason2 <- rbind(entryseason2,data.frame(entryseason$current,name=paste(entry$player_first_name,entry$player_last_name)))
      }
      return(entryseason2)
    }
}

# get_entry_picks()

#' get_entry_picks
#'
#' This function fetches player picks for a Fantasy Premier League entry given the entry ID and GW.
#' @param entryid The entry ID(s). Can be found on the FPL website under 'Gameweek history' in the URL - https://fantasy.premierleague.com/a/entry/XXXXXX/history.
#' @param gw The GW for which player picks is requested.
#' @keywords entry
#' @export
#' @examples
#'
#' get_entry_picks(1076,1)

get_entry_picks <- function(entryid = NULL, gw = NULL){
  if(is.null(entryid)) stop("You'll need to input an entry ID, mate.")
  if(is.null(gw)) stop("You'll need to input a gameweek, mate.")

  roundinfo <- get_round_info()

  picks <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid,"/event/",gw,"/picks","/",sep=""))

  return(picks)
}

