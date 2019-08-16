# get_fdr()

#' get_fdr
#'
#' This function fetches FPL's 'team strength' measures, which form the basis of the 'FDR' (Fixture Difficulty Rating)
#' @keywords teams
#' @export
#' @examples
#' get_fdr()

get_fdr <- function(){
  teams <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$teams
  return(teams)
}
