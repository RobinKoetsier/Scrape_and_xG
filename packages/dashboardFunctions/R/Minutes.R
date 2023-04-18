#' Get minutes played
#' 
#' @param dfcsv A data frame
#' @return summarise of players and minutes, added red card
#' @examples
#' Minutes(dfcsv)
#' @export

Minutes <- function(dfcsv){
  OneGame <- dfcsv
  fulltime <- max(OneGame$expandedMinute) - 1
  setDF(OneGame)
  df <- unique(OneGame[c("PlayerId", "TeamId")])
  df <- df[complete.cases(df), ]
  
  
  Off <- OneGame %>% filter(`type/displayName` == "SubstitutionOff") %>% select(PlayerId, TeamId,expandedMinute) %>% 
    mutate(Wissel = "Off")
  On <- OneGame %>% filter(`type/displayName` == "SubstitutionOn") %>% select(PlayerId, TeamId,expandedMinute) %>% 
    mutate(Wissel = "On") %>% mutate(expandedMinute = fulltime+1 - expandedMinute)
  red <-  OneGame %>% filter(cardType == "{u'displayName': u'Red', u'value': 33}") %>% select(PlayerId, TeamId,expandedMinute) %>% 
    mutate(Wissel = "Off")
  
  dfwissel <- rbind(On,Off,red)
  
  Players <-left_join(df,dfwissel, by=c("PlayerId","TeamId"))
  Players$expandedMinute[is.na(Players$expandedMinute)] <- fulltime
  
  df <- OneGame %>% filter(isShot == "TRUE") %>% group_by(PlayerId) %>%  dplyr::mutate(Shots =  dplyr::n()) %>%
    select(PlayerId,TeamId,Shots) 
  
  ShotAssists <- OneGame %>% filter(isShot == "TRUE") %>% select(relatedPlayerId)
  #ShotAssists <- ShotAssists[complete.cases(ShotAssists), ]
  ShotAssists <- ShotAssists %>% 
    group_by(relatedPlayerId) %>%  
    dplyr::summarise(Schotassists =  dplyr::n())
  Assists <- OneGame %>% filter(isGoal == "TRUE") %>% group_by(relatedPlayerId) %>%  dplyr::summarise(Assists =  dplyr::n())
  colnames(ShotAssists)[1]<- "PlayerId"
  colnames(Assists)[1] <- "PlayerId"
  df <- left_join(Players,df, by=c("PlayerId","TeamId"))
  df <- left_join(df,Assists, by=c("PlayerId"))
  df <- left_join(df,ShotAssists, by=c("PlayerId"))
  df <- unique(df)
  #XG en XA toevoegen
  return(df)
}
