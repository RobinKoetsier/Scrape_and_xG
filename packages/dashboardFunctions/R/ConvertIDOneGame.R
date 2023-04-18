#' change PlayerId, TeamId and relevantPlayerId to names
#' 
#' @param DATA A data frame
#' @param OneGamePlayers Date frame with player names and id's.
#' @return `DATA` with the names in it
#' @examples
#' ConvertIDOneGame(DATA,OneGamePlayers)
#' @export

ConvertIDOneGame <- function(DATA,OneGamePlayers){
  for(i in 1:length(TeamCodes$X1)){
    
    DATA$TeamId[DATA$TeamId == TeamCodes$X1[i]] <- as.character(TeamCodes$X2[i])
    DATA$HomeTeam[DATA$HomeTeam == TeamCodes$X1[i]] <- as.character(TeamCodes$X2[i])
    DATA$AwayTeam[DATA$AwayTeam == TeamCodes$X1[i]] <- as.character(TeamCodes$X2[i])
  }
  
  for(i in 1:length(OneGamePlayers$ID)){
    #gsub(PECFCU$teamId,TeamCodes$X1[i],TeamCodes$X2[i])
    DATA$PlayerId[DATA$PlayerId == OneGamePlayers$ID[i]] <- as.character(OneGamePlayers$Name[i])
  }
  # DATA$PlayerId <-  sub(".*? ", "", DATA$PlayerId)
  #DATA$relatedPlayerId <- gsub("\\..*","",DATA$relatedPlayerId)
  for(i in 1:length(OneGamePlayers$ID)){
    
    DATA$relatedPlayerId[DATA$relatedPlayerId == OneGamePlayers$ID[i]] <- as.character(OneGamePlayers$Name[i])
  }
  #  DATA$relatedPlayerId <-  sub(".*? ", "", DATA$relatedPlayerId)
  #print(unique(DATA$PlayerId))
  DATA$x <- as.numeric(DATA$x)
  DATA$y <- as.numeric(DATA$y)
  DATA$endX <- as.numeric(DATA$endX)
  DATA$endY <- as.numeric(DATA$endY)
  return(DATA)
}
