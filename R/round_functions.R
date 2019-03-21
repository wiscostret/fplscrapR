# get_round_info

#' get_round_info
#'
#' This function fetches basic summary information on FPL Gameweeks.
#' @param round The FPL Gameweek number for which basic summary information is requested, e.g. 20. You can list multiple rounds, e.g. with c(). If blank, the function fetches all Gameweeks.
#' @keywords round
#' @export
#' @examples
#' get_round_info(2:4)

get_round_info <- function(round = NULL){
  events <- jsonlite::fromJSON("https://fantasy.premierleague.com/drf/events")
  ifelse(
    is.null(round),
    return(events),
    return(events %>% dplyr::filter(id %in% round)))
}

