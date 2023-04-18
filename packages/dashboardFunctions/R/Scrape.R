#' Scrape
#' 
#' @param url url of a game.

#' @return data frame of game
#' @examples
#' Scrape(url)
#' @export

Scrape <- function(url){
  library(tidyverse)
  ScrapeGame(url)
  
  OneGame <- read_csv("OneGameEvents.csv")
  OneGamePlayers <- read_csv("OneGamePlayers.csv")
  OneGamePlayers <- OneGamePlayers[,-1]
  OneGame <- ConvertIDOneGame(OneGame,OneGamePlayers)
  return(OneGame)
} 
