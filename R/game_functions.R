# get_game_list

#' get_game_list
#'
#' This function fetches a basic list of games.
#' @keywords game
#' @export
#' @examples
#' get_game_list()

get_game_list <- function(){
  fixtures <- jsonlite::fromJSON("https://fantasy.premierleague.com/drf/fixtures/")
  data.frame(
    "GW"=fixtures$event,
    "id"=fixtures$id,
    "home"=fixtures$team_h,
    "away"=fixtures$team_a,
    "finished"=fixtures$finished,
    "kickoff"=fixtures$kickoff_time) %>%
    dplyr::mutate(home=dplyr::recode(home,"1"="ARS","2"="BOU","3"="BHA","4"="BUR","5"="CAR","6"="CHE","7"="CRY","8"="EVE","9"="FUL","10"="HUD","11"="LEI","12"="LIV","13"="MCI","14"="MUN","15"="NEW","16"="SOU","17"="TOT","18"="WAT","19"="WHU","20"="WOL")) %>%
    dplyr::mutate(away=dplyr::recode(away,"1"="ARS","2"="BOU","3"="BHA","4"="BUR","5"="CAR","6"="CHE","7"="CRY","8"="EVE","9"="FUL","10"="HUD","11"="LEI","12"="LIV","13"="MCI","14"="MUN","15"="NEW","16"="SOU","17"="TOT","18"="WAT","19"="WHU","20"="WOL"))
}

# get_game_stats

#' get_game_stats
#'
#' This function fetches detailed statistics for a game given the game ID. It fetches ten variables that are accessed as lists with home and away players, e.g. with '$': goals_scored, assists, own_goals, penalties_saved, yellow_cards, red_cards, saves, bonus, and bps.
#' @param gameid The game ID. Can be found with get_game_list().
#' @keywords game
#' @export
#' @examples
#' get_game_stats(20)

get_game_stats <- function(gameid = NULL){
  ifelse(
    is.null(gameid),
    return(print("You'll need to input a game ID, mate.")),
    ifelse(length(gameid) != 1,"One at a time, please",
           {
             fixtures <- jsonlite::fromJSON("https://fantasy.premierleague.com/drf/fixtures")
             return((fixtures %>% dplyr::filter(id %in% gameid))$stats[[1]])
           }
    ))
}
