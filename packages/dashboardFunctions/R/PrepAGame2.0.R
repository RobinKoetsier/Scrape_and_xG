#' Function that prepares a dataframe for xG
#'makes a shotmap of shots against Eredivisie Team
#' @param df event data
#' @keywords shotmap
#' @examples
#' PrepAGame2.0(df)
#' @export


PrepAGame2.0 <- function(df){
  library(zoo)
  library(tidyverse)
  library(ade4)
  library(data.table)
  library(caret)
 
  OneGame <- df %>% filter(isShot =="TRUE")
  
  isIntentionalAssist <- OneGame %>% filter_all(any_vars(. %in% "IntentionalAssist")) %>% mutate(isIntentionalAssist = "Yes") %>% select(PlayerId,id,isIntentionalAssist)
  isIntentionalAssist <- OneGame %>% filter(rowSums(across(everything(), ~grepl("IntentionalAssist", .x))) > 0) %>% mutate(isIntentionalAssist = "Yes") %>% select(PlayerId,id,isIntentionalAssist)
  
  OneGame$relatedPlayerId[is.na(OneGame$relatedPlayerId)] <- "No"
  isAssistedShot <- OneGame %>% filter(relatedPlayerId != "No") %>% mutate(isAssistedShot = "Yes") %>% select(PlayerId,id,isAssistedShot)
  
  Assisted <- left_join(isAssistedShot,isIntentionalAssist,by=c("PlayerId","id"))
  IsAssist <- left_join(OneGame,Assisted, by=c("PlayerId", "id")) 
  
  Headers <- OneGame %>% filter_all(any_vars(. %in% "Head")) %>% mutate(Bodypart = "Head") %>% select(PlayerId,id,Bodypart)
  Left <- OneGame %>% filter_all(any_vars(. %in% "LeftFoot")) %>% mutate(Bodypart = "LeftFoot")  %>% select(PlayerId,id,Bodypart)
  Right <- OneGame %>% filter_all(any_vars(. %in% "RightFoot")) %>% mutate(Bodypart = "RightFoot")  %>% select(PlayerId,id,Bodypart)
  Other <- OneGame %>% filter_all(any_vars(. %in% "OtherBodyPart")) %>% mutate(Bodypart = "OtherBP")  %>% select(PlayerId,id,Bodypart)
  

  Bodyparts <- rbind(Headers,Left,Right,Other)
  
  test <-left_join(IsAssist,Bodyparts, by=c("PlayerId", "id")) 
  
  OutofBoxCentre <- OneGame %>% filter_all(any_vars(. %in% "OutOfBoxCentre")) %>% mutate(zone = "OutOfBoxCentre") %>% select(PlayerId,id,zone)
  OutofBoxLeft <- OneGame %>% filter_all(any_vars(. %in% "OutofBoxLeft")) %>% mutate(zone = "OutofBoxLeft") %>% select(PlayerId,id,zone)
  OutofBoxRight <- OneGame %>% filter_all(any_vars(. %in% "OutofBoxRight")) %>% mutate(zone = "OutofBoxRight") %>% select(PlayerId,id,zone)
  
  
  
  BoxLeft <- OneGame %>% filter_all(any_vars(. %in% "BoxLeft")) %>% mutate(zone = "BoxLeft") %>% select(PlayerId,id,zone)
  BoxRight <- OneGame %>% filter_all(any_vars(. %in% "BoxRight")) %>% mutate(zone = "BoxRight") %>% select(PlayerId,id,zone)
  BoxCentre <- OneGame %>% filter_all(any_vars(. %in% "BoxCentre")) %>% mutate(zone = "BoxCentre") %>% select(PlayerId,id,zone)
  
  
  DeepBoxLeft <- OneGame %>% filter_all(any_vars(. %in% "DeepBoxLeft")) %>% mutate(zone = "DeepBoxLeft") %>% select(PlayerId,id,zone)
  DeepBoxRight <- OneGame %>% filter_all(any_vars(. %in% "DeepBoxRight")) %>% mutate(zone = "DeepBoxRight") %>% select(PlayerId,id,zone)
  DeepBoxCentre <- OneGame %>% filter_all(any_vars(. %in% "DeepBoxCentre")) %>% mutate(zone = "DeepBoxCentre") %>% select(PlayerId,id,zone)
  
  SmallBoxLeft <- OneGame %>% filter_all(any_vars(. %in% "SmallBoxLeft")) %>% mutate(zone = "SmallBoxLeft") %>% select(PlayerId,id,zone)
  SmallBoxRight <- OneGame %>% filter_all(any_vars(. %in% "SmallBoxRight")) %>% mutate(zone = "SmallBoxRight") %>% select(PlayerId,id,zone)
  SmallBoxCentre <- OneGame %>% filter_all(any_vars(. %in% "SmallBoxCentre")) %>% mutate(zone = "SmallBoxCentre") %>% select(PlayerId,id,zone)
  
  ThirtyFivePlusLeft <- OneGame %>% filter_all(any_vars(. %in% "ThirtyFivePlusLeft")) %>% mutate(zone = "ThirtyFivePlusLeft") %>% select(PlayerId,id,zone)
  ThirtyFivePlusRight <- OneGame %>% filter_all(any_vars(. %in% "ThirtyFivePlusRight")) %>% mutate(zone = "ThirtyFivePlusRight") %>% select(PlayerId,id,zone)
  ThirtyFivePlusCentre <- OneGame %>% filter_all(any_vars(. %in% "ThirtyFivePlusCentre")) %>% mutate(zone = "ThirtyFivePlusCentre") %>% select(PlayerId,id,zone)
  
  BigChance <- OneGame %>% filter_all(any_vars(. %in% "BigChance")) %>% mutate(isBigChance = "Yes") %>% select(PlayerId,id,isBigChance)
  Penalty <- OneGame %>% filter_all(any_vars(. %in% "Penalty")) %>% mutate(isPenalty = "Yes") %>% select(PlayerId,id,isPenalty)
  
  OwnGoal <- OneGame %>% filter(isOwnGoal == "TRUE") %>% mutate(IsOwnGoal = "Yes") %>% select(PlayerId,id,isOwnGoal)
  Zone <- rbind(OutofBoxCentre,OutofBoxLeft, OutofBoxRight,BoxLeft,BoxRight,BoxCentre,
                DeepBoxLeft,DeepBoxRight,DeepBoxCentre)
  
  FromCorner <- OneGame %>% filter_all(any_vars(. %in% "FromCorner")) %>% mutate(Type_of_play = "FromCorner") %>% select(PlayerId,id,Type_of_play)          
  RegularPlay <- OneGame %>% filter_all(any_vars(. %in% "RegularPlay")) %>% mutate(Type_of_play = "RegularPlay") %>% select(PlayerId,id,Type_of_play)          
  DirectFreekick <- OneGame %>% filter_all(any_vars(. %in% "DirectFreekick")) %>% mutate(Type_of_play = "DirectFreekick") %>% select(PlayerId,id,Type_of_play)          
  SetPiece <- OneGame %>% filter_all(any_vars(. %in% "SetPiece")) %>% mutate(Type_of_play = "SetPiece") %>% select(PlayerId,id,Type_of_play)          
  FastBreak <- OneGame %>% filter_all(any_vars(. %in% "FastBreak")) %>% mutate(Type_of_play = "FastBreak") %>% select(PlayerId,id,Type_of_play)          
  ThrowinSetPiece <- OneGame %>% filter_all(any_vars(. %in% "ThrowinSetPiece")) %>% mutate(Type_of_play = "ThrowinSetPiece") %>% select(PlayerId,id,Type_of_play)          
  Penalty <- OneGame %>% filter_all(any_vars(. %in% "Penalty")) %>% mutate(Type_of_play = "Penalty") %>% select(PlayerId,id,Type_of_play)          
  
  
  Type_of_play <- rbind(FromCorner,RegularPlay,DirectFreekick,SetPiece,FastBreak,ThrowinSetPiece,Penalty)
  
  type <- left_join(test,Type_of_play,by=c("PlayerId", "id")) 
  
  test2 <- left_join(type,Zone,by=c("PlayerId", "id")) 
  situation <- left_join(BigChance,OwnGoal,by=c("PlayerId", "id"))
  test3 <- left_join(test2,BigChance,by=c("PlayerId", "id")) 
  
  colnames(test3)
  
  test3$isOwnGoal[is.na(test3$isOwnGoal)] <- "FALSE"
  test3$TeamId <- ifelse((test3$isOwnGoal == "TRUE"|test3$isOwnGoal == TRUE) & test3$TeamId == test3$HomeTeam,test3$AwayTeam,
                         ifelse((test3$isOwnGoal == "TRUE"|test3$isOwnGoal == TRUE) & test3$TeamId == test3$AwayTeam,test3$HomeTeam,test3$TeamId))
  df <- test3 %>% mutate(teamone = HomeTeam,teamtwo = AwayTeam) %>%
    rowwise() %>%
    mutate(onescore= ifelse(isGoal=="TRUE" & TeamId== teamone,1,0)) %>%
    mutate(twoscore= ifelse(isGoal=="TRUE" & TeamId== teamtwo,1,0)) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(Goal =  ifelse(isGoal=="TRUE",1,0))
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
  shots_ext <- df %>% 
    mutate(distance = distance(x, y)) %>% 
    mutate(angle = goal_angle(x, y)) %>%
    select(Date,PlayerId,TeamId,HomeTeam,AwayTeam,minute,second,x,y,angle,distance,Type_of_play,
           GamestateOne,GamestateTwo,zone,Bodypart,TeamId,isGoal,isBigChance,relatedPlayerId,
           isIntentionalAssist,isAssistedShot,isOwnGoal, goalMouthY, goalMouthZ, zone,expandedMinute) %>%
    mutate(Goal = ifelse(isGoal == "TRUE",1,0)) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(Time_in_sec = as.numeric(minute) * 60 + as.numeric(second))
  
  
  
  shots_ext <-shots_ext %>%
    mutate(Gamestate = ifelse(TeamId == HomeTeam,GamestateOne,GamestateTwo))
  
  shots_ext$isBigChance[is.na(shots_ext$isBigChance)] <- "No"
  shots_ext$isAssistedShot[is.na(shots_ext$isAssistedShot)] <- "No"
  shots_ext$isIntentionalAssist[is.na(shots_ext$isIntentionalAssist)] <- "No"
  return(shots_ext)
  
}

