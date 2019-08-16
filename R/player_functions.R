# imports
#' @import jsonlite
#' @import dplyr
#' @import curl
#' @import getPass

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# get_player_id

#' get_player_id
#'
#' This function fetches the FPL ID of a player given the player's full name. If blank, the function fetches all current players.
#' @param name The player's full name, as listed on the official Fantasy Premier League site (for instance: "Richarlison de Andrade", not "Richarlison". You can list multiple, e.g. with c().
#' @keywords player
#' @export
#' @examples
#' get_player_id(name="Virgil van Dijk")

get_player_id <- function(name = NULL){
  ifelse(
    is.null(name),
      {elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
      elements$playername <- paste(elements$first_name,elements$second_name)
      return(elements %>% dplyr::select(playername,id))}
    ,
    {
      elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
      elements$playername <- paste(elements$first_name,elements$second_name)
      return(elements %>% dplyr::filter(playername %in% name) %>% dplyr::select(playername,id))
    }
  )
}

# get_player_name

#' get_player_name
#'
#' This function fetches a player's full name given FPL ID. If blank, the function fetches all current players.
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
    {
      elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
      elements$playername <- paste(elements$first_name,elements$second_name)
      return(elements %>% dplyr::select(playername,id))
    }
    ,
    {
      elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
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
#' @param season To retrieve prior season data, enter a 2-digit year corresponding to the start of the historical FPL season of interest (e.g. '17' for the 2017/2018 season).
#' @keywords player
#' @export
#' @examples
#' get_player_info(name=c("Jesse Lingard","Virgil van Dijk"))

get_player_info <- function(name = NULL, season = NULL){
  ifelse(
    is.null(season),
    {
      elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
      elements$playername <- paste(elements$first_name,elements$second_name)
    },
    {
      elements <- read.csv(paste0("https://raw.githubusercontent.com/wiscostret/histfpldata/master/getplayerinfo",season,".csv"),encoding="UTF-8")
    })

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
#' get_player_hist(playerid=200)
#'
#' get_player_hist(playerid=get_player_id("Sead Kolasinac")$id)

get_player_hist <- function(playerid = NULL){
  elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
  elements$playername <- paste(elements$first_name,elements$second_name)
  ifelse(
    is.null(playerid),
    {
      histinfo <- data.frame()
      for (i in 1:nrow(elements)){
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",elements$id[i],"/",sep="")))$history_past
        histinfo <- rbind(histinfo,data.frame(fplboot,playername=rep(elements$playername[which(elements$id==elements$id[i])],length(unique(fplboot$season_name)))))}
      return(histinfo)
    },
    {
      histinfo <- data.frame()
      for (i in 1:length(playerid)){
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",playerid[i],"/",sep="")))$history_past
        histinfo <- rbind(histinfo,data.frame(fplboot,playername=rep(elements$playername[which(elements$id==playerid[i])],length(unique(fplboot$season_name)))))}
      return(histinfo)})
}

# get_player_details

#' get_player_details
#'
#' This function fetches detailed, gameweek-by-gameweek information for the current season on selected players. If parameters left blank, the function fetches all current players - note that takes a while to load.
#' @param playerid The player's ID. Can be found using get_player_id(). You can list multiple, e.g. with c(). Won't work with prior season's data.
#' @param name Alternatively, the player's full name, as listed on the official Fantasy Premier League site (for instance: "Richarlison de Andrade", not "Richarlison"). If blank, the function fetches all players.
#' @param season To retrieve prior season data, enter a 2-digit year corresponding to the start of the historical FPL season of interest (e.g. '17' for the 2017/2018 season).
#' @keywords player
#' @export
#' @examples
#' get_player_details(300)
#'
#' get_player_details(name="Virgil van Dijk")

get_player_details <- function(playerid = NULL, name = NULL, season = NULL){
  if(!is.null(playerid) & !is.null(name)) stop("Please only supply playerid OR name, not both.")

  ifelse(
    !is.null(season),
    {
      detinfo <- read.csv(paste0("https://raw.githubusercontent.com/wiscostret/histfpldata/master/getplayerdetails",season,".csv"),encoding="UTF-8")

      ifelse(
        is.null(playerid),
        ifelse(
          is.null(name),
          return(detinfo),
          return(detinfo %>% dplyr::filter(playername %in% name))),
        return(detinfo %>% dplyr::filter(playerid %in% id))
      )
    },
    {
      elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
      elements$playername <- paste(elements$first_name,elements$second_name)

      ifelse(
        is.null(playerid),
        ifelse(
          is.null(name),
          {
            detinfo <- data.frame()
            for (i in 1:nrow(elements)){
              fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",elements$id[i],"/",sep="")))$history
              detinfo <- rbind(detinfo,data.frame(fplboot,playername=elements$playername[which(elements$id==elements$id[i])]))}
            return(detinfo)
          },
          {
            detinfo <- data.frame()
            selection <- elements %>% dplyr::filter(playername %in% name) %>% dplyr::select(id) %>% unlist() %>% as.numeric()
            for (i in 1:length(selection)){
              fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",selection[i],"/",sep="")))$history
              detinfo <- rbind(detinfo,data.frame(fplboot,playername=elements$playername[which(elements$id==selection[i])]))}
            return(detinfo)
          }),
        {
          detinfo <- data.frame()
          for (i in 1:length(playerid)){
            fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/",playerid[i],"/",sep="")))$history
            detinfo <- rbind(detinfo,data.frame(fplboot,playername=elements$playername[which(elements$id==playerid[i])]))}
          return(detinfo)
        }
      )
    }
  )
}

