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

  picks <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid,"/event/",gw,"/picks","/",sep=""))

  return(picks)
}

# get_entry_player_picks()

#' get_entry_player_picks
#'
#' This function fetches detailed information for a Fantasy Premier League entry given the entry ID(s) and GW(s).
#' @param entryid The entry ID(s). Can be found on the FPL website under 'Gameweek history' in the URL - https://fantasy.premierleague.com/a/entry/XXXXXX/history.
#' @param gw The GW(s) for which player picks is requested.
#' @keywords entry
#' @return A data.frame of player picks for the requested entry ID(s) and GW(s).
#' @export
#' @examples
#'
#' get_entry_player_picks(c(1076, 1077), 1:10)

get_entry_player_picks <- function(entryid = NULL, gw = NULL){
  if(is.null(entryid)) stop("You'll need to input an entry ID, mate.")
  if(is.null(gw)) stop("You'll need to input a gameweek, mate.")
  
  picks3 <- data.frame()
  for (j in 1:length(entryid)) {
    picks2 <- data.frame()
    gw_started <- get_entry(entryid[j])[["started_event"]]
    gw_thisentry <- gw[gw >= gw_started]
    for (i in 1:length(gw_thisentry)) {
      picks_list <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid[j],"/event/",gw_thisentry[i],"/picks","/",sep=""))
      picks <- picks_list$picks
    
      picks$event <- gw_thisentry[i]
      picks2 <- rbind(picks2, picks)
    }
    picks2$entry <- entryid[j]
    picks3 <- rbind(picks3, picks2)
  }
  
  player_names <- get_player_name(unique(picks3$element))
  picks3 <- merge(player_names, picks3, by.x = "id", by.y = "element")
  names(picks3) <- c("id", "playername", "position", "multiplier", "is_captain", "is_vice_captain", "event", "entry")
  
  picks3 <- picks3[order(picks3$entry, picks3$event), ]
  
  return(picks3)
}

# get_entry_captain()

#' get_entry_captain
#'
#' This function fetches the captain for a Fantasy Premier League entry given the entry ID(s) and GW(s).
#' @param entryid The entry ID(s). Can be found on the FPL website under 'Gameweek history' in the URL - https://fantasy.premierleague.com/a/entry/XXXXXX/history.
#' @param gw The GW(s) for which player picks is requested.
#' @keywords entry
#' @return A data.frame of captain(s) for the requested entry ID(s) and GW(s). If the selected captain was benched, the vice captain is returned. If both captain picks are benched, the intended captain is returned.
#' @export
#' @examples
#' get_entry_captain(c(1076, 1077), 1:10)

get_entry_captain <- function(entryid = NULL, gw = NULL){
  if(is.null(entryid)) stop("You'll need to input an entry ID, mate.")
  if(is.null(gw)) stop("You'll need to input a gameweek, mate.")
  
  captain3 <- data.frame()
  for (j in 1:length(entryid)) {
    captain2 <- data.frame()
    gw_started <- get_entry(entryid[j])[["started_event"]]
    gw_thisentry <- gw[gw >= gw_started]
    for (i in 1:length(gw_thisentry)) {
      picks <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/",entryid[j],"/event/",gw_thisentry[i],"/picks","/",sep=""))
      captain <- data.frame("id" = picks$picks[picks$picks$multiplier %in% c(2, 3), "element"])
      if(nrow(captain) == 0) {
        captain <- data.frame("id" = picks$picks[picks$picks$is_captain == TRUE, "element"])
      }
      captain$event <- gw_thisentry[i]
      captain2 <- rbind(captain2, captain)
    }
    captain2$entry <- entryid[j]
    captain3 <- rbind(captain3, captain2)
  }
  
  player_names <- get_player_name(unique(captain3$captain))
  captain3 <- merge(captain3, player_names, by = "id")
  captain3 <- captain3[c(1, 4, 3, 2)]
  names(captain3) <- c("id", "playername", "entry", "event")
  captain3 <- captain3[order(captain3$entry,captain3$event), ]
  
  return(captain3)
}
