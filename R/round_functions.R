# get_round_info

#' get_round_info
#'
#' This function fetches basic summary information on FPL Gameweeks.
#' @param round The FPL Gameweek number for which basic summary information is requested, e.g. 20. You can list multiple rounds, e.g. with c(). If blank, the function fetches all Gameweeks.
#' @param season To retrieve prior season data, enter a 2-digit year corresponding to the start of the historical FPL season of interest (e.g. '18' for the 2018/2019 season).
#' @keywords round
#' @export
#' @examples
#' get_round_info(2:4)

get_round_info <- function(round = NULL, season = NULL){
  ifelse(
    !is.null(season),
    {
      events <- read.csv(paste0("https://raw.githubusercontent.com/wiscostret/histfpldata/master/getroundinfo",season,".csv"),encoding="UTF-8")
    },
    {
      events <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$events
    })

  ifelse(
    is.null(round),
    {
      return(events)
    },
    {
      return(events %>% dplyr::filter(id %in% round))
    }
  )
}
