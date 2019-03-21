# getPlayerID

#' getPlayerID
#'
#' This function fetches the FPL ID of a player given the player's full name.
#' @param name The player's full name, as listed on the official Fantasy Premier League site (for instance: "Richarlison de Andrade", not "Richarlison". You can list multiple rounds, e.g. with c().
#' @keywords player
#' @export
#' @examples
#' getPlayerID()

getPlayerID <- function(name=NULL){
  ifelse(
    is.null(name),
    return(print("You'll need to input a player name, mate.")),
    {
      elements <- fromJSON("https://fantasy.premierleague.com/drf/elements")
      elements$playername <- paste(elements$first_name,elements$second_name)
      return(elements %>% filter(playername %in% name) %>% select(playername,id))
    }
  )
}

# getPlayerName

#' getPlayerName
#'
#' This function fetches a player's full name given FPL ID.
#' @param playerid The player's FPL ID. You can list multiple rounds, e.g. with c().
#' @keywords player
#' @export
#' @examples
#' getPlayerName()

getPlayerName <- function(playerid=NULL){
  ifelse(
    is.null(playerid),
    return(print("You'll need to input an ID, mate.")),
    {
      elements <- fromJSON("https://fantasy.premierleague.com/drf/elements")
      elements$playername <- paste(elements$first_name,elements$second_name)
      return(elements %>% filter(id %in% playerid) %>% select(playername,id))
    }
  )
}

# getPlayerInfo

#' getPlayerInfo
#'
#' This function fetches selected summary information on selected players given full player name(s).
#' @param name The player's full name, as listed on the official Fantasy Premier League site (for instance: "Richarlison de Andrade", not "Richarlison"). If blank, the function fetches all players.
#' @keywords player
#' @export
#' @examples
#' getPlayerInfo()

getPlayerInfo <- function(name=NULL){
  elements <- fromJSON("https://fantasy.premierleague.com/drf/elements")
  elements$playername <- paste(elements$first_name,elements$second_name)
  ifelse(
    is.null(name),
    return(elements),
    return(elements %>% filter(playername %in% name))
  )
}
