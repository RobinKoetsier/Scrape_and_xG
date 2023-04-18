#' Add xG and xA
#' 
#' @param df A dataframe of the game.

#' @return data frame with the values.
#' @examples
#' AddxGenxA(df)
#' @export

AddxGenxA <- function(df){
  Penals <- df  %>% 
    group_by(PlayerId, TeamId)%>% filter(Type_of_play == "Penalty") %>%
    dplyr::summarise(Strafschoppen =  dplyr::n(),StrafGoal = sum(Goal),StrafxG=sum(xG))
  
  Shots <- df %>% 
    group_by(PlayerId, TeamId) %>%  dplyr::summarise(Shots= dplyr::n(),Goals=sum(Goal),xG=sum(xG))
  Shots <- left_join(Shots,Penals, by=c("PlayerId","TeamId"))
  Shots$Strafschoppen[is.na(Shots$Strafschoppen)]  <- 0
  Shots$StrafGoal[is.na(Shots$StrafGoal)]  <- 0
  Shots$StrafxG[is.na(Shots$StrafxG)]  <- 0
  Assists <- df %>% group_by(relatedPlayerId, TeamId) %>% 
    dplyr::summarise(SchotAssists= dplyr::n(),Assists=sum(Goal),xA=sum(xG))
  SandA <- full_join(Shots,Assists, by=c("PlayerId"="relatedPlayerId","TeamId"))
  SandA <- SandA %>% drop_na(PlayerId)
  SandA[is.na(SandA)] <- 0
  return(SandA)
}



