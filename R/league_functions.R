# get_league()

#' get_league
#'
#' This function fetches basic information for a Fantasy Premier League mini-league given the league ID and type. In the new FPL API for 19/20, this information requires authentication - so you'll be prompted to post your FPL login and password to retrieve the data.
#' @param leagueid The league ID(s). Can be found on the FPL website.
#' @param leaguetype The league type: 'classic' or 'h2h'.
#' @keywords league
#' @export

get_league <- function(leagueid = NULL, leaguetype = "classic"){
  if(is.null(leagueid)) stop("You'll need to input a league ID, mate.")
  if(length(leagueid) != 1) stop("One league at a time, please.")
  {
    fplfetchhandle <- curl::new_handle()
    curl::handle_setform(fplfetchhandle,
      login=readline("Please enter your FPL login email: "),
      password=getPass::getPass(msg="Please enter your FPL password:"),
      redirect_uri="https://fantasy.premierleague.com/a/login",
      app="plfpl-web")
    fplfetchmemory <- curl::curl_fetch_memory("https://users.premierleague.com/accounts/login/", handle = fplfetchhandle)
  }

  if(fplfetchmemory$url != "https://fantasy.premierleague.com/a/login?state=success") stop("The authentication didn't work. You've most likely entered an incorrect FPL email and/or password.")

  {
    league <- jsonlite::fromJSON(curl::curl(paste("https://fantasy.premierleague.com/api/leagues-",leaguetype,"/",leagueid,"/standings/",sep=""), handle = fplfetchhandle))
    return(league)
  }
}

# get_league_entries()

#' get_league_entries
#'
#' This function fetches a list of entries in the standings for a Fantasy Premier League mini-league given the league ID and type. In the new FPL API for 19/20, this information requires authentication - so you'll be prompted to post your FPL login and password to retrieve the data.
#' @param leagueid The league ID(s). Can be found on the FPL website.
#' @param leaguetype The league type: 'classic' or 'h2h'.
#' @param pages The number of pages of entries to fetch (there are 50 entries on each page)
#' @keywords league
#' @export

get_league_entries <- function(leagueid = NULL, leaguetype = "classic", pages = 1){
  if(is.null(leagueid)) stop("You'll need to input a league ID, mate.")
  if(length(leagueid) != 1) stop("One league at a time, please.")
  if(pages %% 1 != 0) stop("The number of pages needs to be a whole number.")
  {
    fplfetchhandle <- curl::new_handle()
    curl::handle_setform(fplfetchhandle,
      login=readline("Please enter your FPL login email: "),
      password=getPass::getPass(msg="Please enter your FPL password:"),
      redirect_uri="https://fantasy.premierleague.com/a/login",
      app="plfpl-web")
    fplfetchmemory <- curl::curl_fetch_memory("https://users.premierleague.com/accounts/login/", handle = fplfetchhandle)
    }

  if(fplfetchmemory$url != "https://fantasy.premierleague.com/a/login?state=success") stop("The authentication didn't work. You've most likely entered an incorrect FPL email and/or password.")

  {
    entries <- data.frame()

    for (i in 1:pages){

      standings <- jsonlite::fromJSON(curl::curl(paste("https://fantasy.premierleague.com/api/leagues-",leaguetype,"/",leagueid,"/standings/?","page_standings=",i,sep=""), handle = fplfetchhandle))

      entries <- rbind(entries,standings$standings$results)

    }

    return(entries)
  }
}
