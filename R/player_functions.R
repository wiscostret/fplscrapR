# imports
#' @import jsonlite
#' @import dplyr

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# get_player_id

#' get_player_id
#'
#' This function fetches the FPL ID of a player given the player's full name.
#' @param name The player's full name, as listed on the official Fantasy Premier League site (for instance: "Richarlison de Andrade", not "Richarlison". You can list multiple, e.g. with c().
#' @keywords player
#' @export
#' @examples
#' get_player_id(name="Aleksandar Mitrovic")
#'
#' get_player_id(name=c("Petr Cech","Bernd Leno"))

get_player_id <- function(name = NULL){
  ifelse(
    is.null(name),
    return(print("You'll need to input a player name, mate.")),
    {
      elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static")$elements
      elements$playername <- paste(elements$first_name,elements$second_name)
      return(elements %>% dplyr::filter(playername %in% name) %>% dplyr::select(playername,id))
    }
  )
}

# get_player_name

#' get_player_name
#'
#' This function fetches a player's full name given FPL ID.
#' @param playerid The player's FPL ID. You can list multiple, e.g. with c().
#' @keywords player
#' @export
#' @examples
#' get_player_name(playerid=300)
#'
#' get_player_name(playerid=150:160)

get_player_name <- function(playerid = NULL){
  ifelse(
    is.null(playerid),
    return(print("You'll need to input an ID, mate.")),
    {
      elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static")$elements
      elements$playername <- paste(elements$first_name,elements$second_name)
      return(elements %>% dplyr::filter(id %in% playerid) %>% dplyr::select(playername,id))
    }
  )
}

# get_player_info

#' get_player_info
#'
#' This function fetches selected summary information on selected players given full player name(s).
#' @param name The player's full name, as listed on the official Fantasy Premier League site (for instance: "Richarlison de Andrade", not "Richarlison"). If blank, the function fetches all players.
#' @keywords player
#' @export
#' @examples
#' get_player_info(name=c("Jesse Lingard","Aleksandar Mitrovic"))

get_player_info <- function(name = NULL){
  elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static")$elements
  elements$playername <- paste(elements$first_name,elements$second_name)
  ifelse(
    is.null(name),
    return(elements),
    return(elements %>% dplyr::filter(playername %in% name))
  )
}

# get_player_hist

#' get_player_hist
#'
#' This function fetches historical summary information on selected players given player ID(s).
#' @param playerid The player's ID. Can be found using get_player_id(). You can list multiple , e.g. with c(). If blank, the function fetches all current players - note that takes a while to load.
#' @keywords player
#' @export
#' @examples
#' get_player_hist(playerid=300)
#'
#' get_player_hist(playerid=get_player_id("Eden Hazard"))

get_player_hist <- function(playerid = NULL){
  elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static")$elements
  elements$playername <- paste(elements$first_name,elements$second_name)
  ifelse(
    is.null(playerid),
    {
      histinfo <- data.frame()
      for (i in 1:nrow(elements)){
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",i,sep="")))$history_past
        histinfo <- rbind(histinfo,data.frame(fplboot,playername=rep(elements$playername[which(elements$id==i)],length(unique(fplboot$season_name)))))}
      return(histinfo)
    },
    {
      histinfo <- data.frame()
      for (i in 1:length(playerid)){
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",playerid[i],sep="")))$history_past
        histinfo <- rbind(histinfo,data.frame(fplboot,playername=rep(elements$playername[which(elements$id==playerid[i])],length(unique(fplboot$season_name)))))}
      return(histinfo)})
}

# get_player_details

#' get_player_details
#'
#' This function fetches detailed, gameweek-by-gameweek information for the current season on selected players given player ID(s).
#' @param playerid The player's ID. Can be found using get_player_id(). You can list multiple , e.g. with c(). If blank, the function fetches all current players - note that takes a while to load.
#' @keywords player
#' @export
#' @examples
#' get_player_details(300)

get_player_details <- function(playerid = NULL){
  elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static")$elements
  elements$playername <- paste(elements$first_name,elements$second_name)
  ifelse(
    is.null(playerid),
    {
      detinfo <- data.frame()
      for (i in 1:nrow(elements)){
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",i,sep="")))$history
        detinfo <- rbind(detinfo,data.frame(fplboot,playername=rep(elements$playername[which(elements$id==i)],length(unique(fplboot$id)))))}
      return(detinfo)
    },
    {
      detinfo <- data.frame()
      for (i in 1:length(playerid)){
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",playerid[i],sep="")))$history
        detinfo <- rbind(detinfo,data.frame(fplboot,playername=rep(elements$playername[which(elements$id==playerid[i])],length(unique(fplboot$id)))))}
      return(detinfo)})
}

