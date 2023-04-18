#' Function that prepares a dataframe for xG
#'makes a shotmap of shots against Eredivisie Team
#' @param df event data
#' @keywords shotmap
#' @examples
#' PrepAGameKKD(df)
#' @export


PrepAGameKKD <- function(df_rel){
  library(zoo)
  library(tidyverse)
  library(ade4)
  library(data.table)
  library(caret)
  #OneGame <- dfcsv %>% filter(isShot =="TRUE")
  OneGame <- df_rel %>% filter(isShot == TRUE) %>%
    mutate(PlayerId = playerName,
           id = timeStamp)
  maxmin <- df_rel %>%
    filter(periodId == 1) %>%
    filter(eventName != "Deleted event ") %>%
    select(timeMin,periodId) %>%
    group_by(periodId) %>%
    summarise_each(funs(max)) %>%
    select(timeMin)
  #isIntentionalAssist <- OneGame %>% filter_all(any_vars(. %in% "IntentionalAssist")) %>% mutate(isIntentionalAssist = "Yes") %>% select(PlayerId,id,isIntentionalAssist)
  isIntentionalAssist <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bIntentional assist\\b", .x))) > 0) %>% mutate(isIntentionalAssist = "Yes") %>% select(PlayerId,id,isIntentionalAssist)
  
  #OneGame$relatedPlayerId[is.na(OneGame$relatedPlayerId)] <- "No"
  isAssistedShot <- OneGame %>%filter(rowSums(across(everything(), ~grepl("\\bAssisted\\b", .x))) > 0) %>%
    mutate(isAssistedShot = "Yes") %>% select(PlayerId,id,isAssistedShot)
  
  Assisted <- left_join(isAssistedShot,isIntentionalAssist,by=c("PlayerId","id"))
  IsAssist <- left_join(OneGame,Assisted, by=c("PlayerId", "id")) 
  
  #Headers <- OneGame %>% filter_all(any_vars(. %in% "Head")) %>% mutate(Bodypart = "Head") %>% select(PlayerId,id,Bodypart)
  #Left <- OneGame %>% filter_all(any_vars(. %in% "LeftFoot")) %>% mutate(Bodypart = "LeftFoot")  %>% select(PlayerId,id,Bodypart)
  #Right <- OneGame %>% filter_all(any_vars(. %in% "RightFoot")) %>% mutate(Bodypart = "RightFoot")  %>% select(PlayerId,id,Bodypart)
  #Other <- OneGame %>% filter_all(any_vars(. %in% "OtherBodyPart")) %>% mutate(Bodypart = "OtherBP")  %>% select(PlayerId,id,Bodypart)
  
  Headers <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bHead\\b", .x))) > 0) %>% mutate(Bodypart = "Head") %>% select(PlayerId,id,Bodypart)
  Left <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bLeft footed\\b", .x))) > 0)  %>% mutate(Bodypart = "LeftFoot")  %>% select(PlayerId,id,Bodypart)
  Right <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bRight footed\\b", .x))) > 0) %>% mutate(Bodypart = "RightFoot")  %>% select(PlayerId,id,Bodypart)
  Other <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bOther body part\\b", .x))) > 0)  %>% mutate(Bodypart = "OtherBP")  %>% select(PlayerId,id,Bodypart)
  
  
  Bodyparts <- rbind(Headers,Left,Right,Other)
  
  test <-left_join(IsAssist,Bodyparts, by=c("PlayerId", "id")) 
  
  
  BigChance <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bBig Chance\\b", .x))) > 0) %>% mutate(isBigChance = "Yes") %>% select(PlayerId,id,isBigChance)
  Penalty <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bPenalty\\b", .x))) > 0) %>% mutate(isPenalty = "Yes") %>% select(PlayerId,id,isPenalty)
  
  OwnGoal <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bOwn goal\\b", .x))) > 0) %>% mutate(isOwnGoal = TRUE) %>% select(PlayerId,id,isOwnGoal)
  
  FromCorner <- OneGame %>% filter(rowSums(across(everything(), ~grepl("From corner", .x))) > 0) %>% mutate(Type_of_play = "FromCorner") %>% select(PlayerId,id,Type_of_play)          
  RegularPlay <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Regular play", .x))) > 0) %>% mutate(Type_of_play = "RegularPlay") %>% select(PlayerId,id,Type_of_play)          
  DirectFreekick <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Free kick", .x))) > 0) %>% mutate(Type_of_play = "DirectFreekick") %>% select(PlayerId,id,Type_of_play)          
  SetPiece <- OneGame %>% filter(rowSums(across(everything(), ~grepl("\\bSet piece\\b", .x))) > 0) %>% mutate(Type_of_play = "SetPiece") %>% select(PlayerId,id,Type_of_play)          
  FastBreak <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Fast break", .x))) > 0) %>% mutate(Type_of_play = "FastBreak") %>% select(PlayerId,id,Type_of_play)          
  ThrowinSetPiece <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Throw-in set piece", .x))) > 0) %>% mutate(Type_of_play = "ThrowinSetPiece") %>% select(PlayerId,id,Type_of_play)          
  Penalty <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Penalty", .x))) > 0) %>% mutate(Type_of_play = "Penalty") %>% select(PlayerId,id,Type_of_play)          
  
  
  Types_of_play <- rbind(FromCorner,RegularPlay,DirectFreekick,SetPiece,FastBreak,ThrowinSetPiece,Penalty)
  
  type <- left_join(test,Types_of_play,by=c("PlayerId", "id")) 
  
  #test2 <- left_join(type,Zone,by=c("PlayerId", "id")) 
  test2 = type
  situation <- left_join(BigChance,OwnGoal,by=c("PlayerId", "id"))
  #test3 <- left_join(test2,situation,by=c("PlayerId", "id")) # %>%
  test3 <- left_join(test2,BigChance,by=c("PlayerId", "id"))   %>%
    left_join(OwnGoal,by=c("PlayerId", "id"))
   # left_join(situation,by=c("PlayerId", "id")) 
  colnames(test3)
  
  test3$isOwnGoal[is.na(test3$isOwnGoal)] <- FALSE
  test3$TeamId <- ifelse((test3$isOwnGoal == TRUE|test3$isOwnGoal == TRUE) & test3$TeamId == test3$HomeTeam,test3$AwayTeam,
                         ifelse((test3$isOwnGoal == TRUE|test3$isOwnGoal == TRUE) & test3$TeamId == test3$AwayTeam,test3$HomeTeam,test3$TeamId))
  df <- test3 %>% mutate(teamone = HomeTeam,teamtwo = AwayTeam) %>%
    rowwise() %>%
    mutate(onescore= ifelse(isGoal==TRUE& TeamId== teamone,1,0)) %>%
    mutate(twoscore= ifelse(isGoal==TRUE & TeamId== teamtwo,1,0)) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(Goal =  ifelse(isGoal==TRUE,1,0))
  df$Goal[is.na(df$Goal)] <- 0
  df$sumone<- cumsum(df$onescore) - df$onescore
  df$sumtwo<- cumsum(df$twoscore) - df$twoscore
  
  
  
  df <-
    df %>% mutate(diff = sumone - sumtwo) %>%
    mutate(GamestateOne = case_when((diff)>0 ~ "Leading",
                                    (diff) == 0~ "Draw",
                                    (diff) <0 ~ "Trailing")) %>%
    
    mutate(GamestateTwo = case_when((diff)<0 ~ "Leading",
                                    (diff) == 0~ "Draw",
                                    (diff) >0 ~ "Trailing"))
  
  

  
  
  
  
  colnames(df)
  df$Date <- df$timeStamp
  
  
  shots_ext <- df %>% 
    mutate(expandedMinute = ifelse(periodId == 2, timeMin - 45 +maxmin$timeMin,timeMin )) %>%
    mutate(distance = dashboardFunctions::distanceToGoal(x, y)) %>% # distance from goal mid-point
    mutate(angle =   dashboardFunctions::goal_angle(x, y)) %>%# based on available goal mouth
    select(Date,playerName,TeamId,HomeTeam,AwayTeam,timeMin,timeSec,x,y,angle,distance,Type_of_play,relatedPlayerId,
           #GamestateOne,GamestateTwo,zone,Bodypart,TeamId,isGoal,isBigChance,relatedPlayerId,
           GamestateOne,GamestateTwo,Bodypart,TeamId,isGoal,isBigChance,
           isIntentionalAssist,isAssistedShot,isOwnGoal,expandedMinute) %>%
    mutate(Goal = ifelse(isGoal == TRUE,1,0)) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(Time_in_sec = as.numeric(timeMin) * 60 + as.numeric(timeSec))
  
# NEED TO ADD ASSISTER!!!!!!!!

  shots_ext <-shots_ext %>%
    mutate(Gamestate = ifelse(TeamId == HomeTeam,GamestateOne,GamestateTwo))
  
  shots_ext$isBigChance[is.na(shots_ext$isBigChance)] <- "No"
  shots_ext$isAssistedShot[is.na(shots_ext$isAssistedShot)] <- "No"
  shots_ext$isIntentionalAssist[is.na(shots_ext$isIntentionalAssist)] <- "No"
  return(shots_ext)
  
}

