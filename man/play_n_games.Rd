% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{play_n_games}
\alias{play_n_games}
\title{Mounty Hall Game outcomes.}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{...}{for loop wasused for repeating the game n times.}
}
\value{
The function returns the game results if the contestant switched what would
be the result (win or lose) and if contestant stayed what would
be the result (win or lose) n times and stack the results in a table.
}
\description{
\code{play_n_game()} is a function that collects the game's outcomes and results n times.
}
\details{
It's a wrapper function that makes it easier to repeat the game n times and collect the
game results each time and put in a table (stay/switch and win/lost)
}
\examples{
  play_n_games <- function( n=100 )
  
}
