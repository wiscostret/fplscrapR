% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entry_functions.R
\name{get_entry_captain}
\alias{get_entry_captain}
\title{get_entry_captain}
\usage{
get_entry_captain(entryid = NULL, gw = NULL)
}
\arguments{
\item{entryid}{The entry ID(s). Can be found on the FPL website under 'Gameweek history' in the URL - https://fantasy.premierleague.com/entry/XXXXXX/history.}

\item{gw}{The gameweek(s) for which captain is requested.}
}
\description{
This function fetches captain information for a Fantasy Premier League entry given the entry ID(s) and GW(s).
}
\examples{
get_entry_captain(1076,1)
}
\keyword{entry}
