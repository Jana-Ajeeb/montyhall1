#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title
#'   Select a random door.
#' 
#' @description
#'  `select_door()` picks a random door that have either a goat or car behind.
#' 
#' @details
#'  The contestant selects a door (picked randomly here) and he/she cannot see what's behind.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 1 numeric vector
#'   indicating the door that the contestant choose.
#'   
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a goat door.
#' 
#' @description
#'   `open_goat_door()` picks a door with a goat behind. 
#' 
#' @details
#'   The Host chooses a door with a goat behind that is different from the door picked
#'   by the contestant and of course not having a car behind.
#'  
#' @param ... 2 arguments used: first if the contestant selected car above then the host can choose
#'   any of the 2 goats, and second if the contestant selected car above.
#'  
#' @return The function returns the game, initial door number picked, opened door number by 
#'   the host(goat).
#'  
#' @examples
#'   this.game <- create_game()
#'   this.game
#'   my.initial.pick <- select_door()
#'   my.initial.pick
#'   open_goat_door( this.game, my.initial.pick )

#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'   Stay/Switch door
#' 
#' @description
#'   `change_door()` contestant chooses to switch his/her initial selection or stay.
#' 
#' @details
#'   The contestant chooses to stay on his/her initial selection, or to switch to the other
#'   door that is different from the opened one and from their initial pick. 
#' 
#' @param ... arguments used: first if the contestant chooses to stay their final selection will
#'   equal to their initial selection, second if the contestant chooses to change to the
#'   remaining other door their final pick will be different than their initial selection.
#'  
#' @return The function returns the game, initial door number picked, final door number picked.
#' 
#' @examples
#'   opened.door <- open_goat_door( this.game, my.initial.pick )
#'   change_door( stay=T, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   change_door( stay=F, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   my.final.pick <- change_door( stay=F, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   this.game
#'   my.initial.pick
#'   my.final.pick

#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3) 
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ] 
  }
  
  return( final.pick )  # number between 1 and 3
}



#'@title
#'   Determining the winner.
#'  
#' @description
#'   `determine_winner()` see if the contestant has won or lost.
#' 
#' @details
#'   Check the final pick of the contestant if it's a car he/she wins, otherwise he/she loses.
#' 
#' @param ... arguments used: first if the final pick turns out a car the contestant will win, and if the 
#'   final pick turns out to be a goat the contestant will loose.
#' 
#' @return The function returns the game, initial door number picked, final door number picked.
#' 
#' @examples
#'   my.initial.pick
#'   my.final.pick <- change_door( stay=T, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   determine_winner( final.pick=my.final.pick, 
#'   game=this.game )
#'   my.final.pick <- change_door( stay=F, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   determine_winner( final.pick=my.final.pick, 
#'   game=this.game )
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}



#'@title
#'   Mounty Hall Game.
#' 
#' @description
#'   `play_game()` is a function including all game steps together. 
#' 
#' @details
#'   It's a wrapper function that makes it easier to control and repeat the steps of the game 
#'   in your simulation
#'  
#' @param ... no arguments are used by the function. 
#' 
#' @return The function returns the game results if the contestant switched what would 
#'   be the result (win or lose) and if contestant stayed what would 
#'   be the result (win or lose)
#' 
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#'@title
#'   Mounty Hall Game outcomes.
#' 
#' @description
#'   `play_n_game()` is a function that collects the game's outcomes and results n times. 
#' 
#' @details
#'   It's a wrapper function that makes it easier to repeat the game n times and collect the
#'   game results each time and put in a table (stay/switch and win/lost)
#'  
#' @param ... for loop wasused for repeating the game n times. 
#' 
#' @return The function returns the game results if the contestant switched what would 
#'   be the result (win or lose) and if contestant stayed what would 
#'   be the result (win or lose) n times and stack the results in a table.
#' 
#' @examples
#'   play_n_games <- function( n=100 )
#'   
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}
