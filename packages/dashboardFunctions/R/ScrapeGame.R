#' Scrape a game
#' 
#' @param url A url from whoscored

#' @return df of game 
#' @examples
#' ScrapeGame(url)
#' @export

ScrapeGame <- function(url){
  path2script = "EventsOneGame.py"
  args = url
  allArgs = c(path2script, args)
  command = "python"
  system2(command, args=allArgs, stdout=TRUE)
  
  Sys.sleep(5)
  
  path2script = "PlayersOneGame.py"
  allArgs = c(path2script, args)
  command = "python"
  system2(command, args=allArgs, stdout=TRUE)
  
}

ScrapeGameTemp <- function(url){
  path2script = "EventsOneGameTemp.py"
  args = url
  allArgs = c(path2script, args)
  command = "python"
  system2(command, args=allArgs, stdout=TRUE)
  
  
  
  path2script = "PlayersOneGame.py"
  allArgs = c(path2script, args)
  command = "python"
  system2(command, args=allArgs, stdout=TRUE)
  
}
