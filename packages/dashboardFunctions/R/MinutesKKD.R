#' Get minutes played
#' 
#' @param dfcsv A data frame
#' @return summarise of players and minutes, added red card
#' @examples
#' Minutes(dfcsv)
#' @export

MinutesKKD <- function(df_rel){
  df <- df_rel
  maxmin <- df %>%
    filter(periodId == 1) %>%
    select(timeMin,periodId) %>%
    group_by(periodId) %>%
    summarise_each(funs(max)) %>%
    select(timeMin)
  
  OneGame <- df %>%
    mutate(PlayerId = playerName) %>% 
    mutate(expandedMinute = ifelse(periodId == 2, timeMin - 45 +maxmin$timeMin,timeMin )) 
  
  
  fulltime <- max(OneGame$expandedMinute) - 1
  setDF(OneGame)
  df <- unique(OneGame[c("PlayerId", "TeamId")]) %>%
    drop_na()
  df <- df[complete.cases(df), ]
  
  
  Off <- OneGame %>% filter(eventName == "Player off ") %>% select(PlayerId, TeamId,expandedMinute) %>% 
    mutate(Wissel = "Off")
  On <- OneGame %>% filter(eventName == "Player on ") %>% select(PlayerId, TeamId,expandedMinute) %>% 
    mutate(Wissel = "On") %>% mutate(expandedMinute = fulltime+1 - expandedMinute)
  red <-  OneGame%>% filter(rowSums(across(everything(), ~grepl("\\bRed card\\b", .x))) > 0) %>%
          select(PlayerId, TeamId,expandedMinute) %>% 
          mutate(Wissel = "Off")
  yellow <-  OneGame%>% filter(rowSums(across(everything(), ~grepl("\\bSecond yellow\\b", .x))) > 0) %>%
    select(PlayerId, TeamId,expandedMinute) %>% 
    mutate(Wissel = "Off")
  
  dfwissel <- rbind(On,Off,red,yellow)
  
  if(length(dfwissel$PlayerId) == length(unique(dfwissel$PlayerId))){
    print('ok')
    }else{
    onOff <- dfwissel %>%
        filter(Wissel == "Off") %>%
        left_join( dfwissel %>%
                     filter(Wissel == "On"),
                   by=c("PlayerId","TeamId")) %>%
        drop_na() %>%
        mutate(expandedMinute = expandedMinute.x-(fulltime - expandedMinute.y)) %>%
        select(PlayerId,TeamId,expandedMinute) %>%
      mutate(Wissel = "On")
      dfwissel <- dfwissel %>%
      filter(PlayerId != onOff$PlayerId) %>%
      rbind(onOff)
      }
  
  
  
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
