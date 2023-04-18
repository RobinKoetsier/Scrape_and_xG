#' Predict
#' 
#' @param OneGame A game.

#' @return df of game with xG column
#' @examples
#' Predict(OneGame)
#' @export

Predict <- function(OneGame){
  library(ggtext)
  library(tidyverse)
  library(xgboost)
  library(Matrix)
  df <- OneGame
  df <- PrepAGame2.0(OneGame)
  df_for_pred <- df %>% select(Goal,x,y,Bodypart,Type_of_play,isIntentionalAssist,isAssistedShot,isBigChance,Gamestate,Time_in_sec,distance,angle)
  #dummy <- makeDummy(test)
  dummy[2,1] <- 1 
  df_for_pred$Type_of_play[is.na(df_for_pred$Type_of_play)] <- "RegularPlay"
  df_for_pred <- rbind(dummy,df_for_pred)
  df_for_pred$x <- ifelse(df_for_pred$Type_of_play == "Penalty",88.5,df_for_pred$x)
  df_for_pred$y <- ifelse(df_for_pred$Type_of_play == "Penalty",50,df_for_pred$y)
  df_for_pred$Goal[is.na(df_for_pred$Goal)] <- 0
  
  pred <- PredictxG(df_for_pred)
  
  xG <- as.data.frame(pred[-c(1:15)])
  Wedstrijd <- cbind(xG,df)
  
  
  colnames(Wedstrijd)[1]<- "xG"
  Wedstrijd$Type_of_play[is.na(Wedstrijd$Type_of_play)] <- "RegularPlay"
  Wedstrijd$xG <- ifelse(Wedstrijd$Type_of_play == "Penalty",0.79,Wedstrijd$xG)
  Wedstrijd$xG <- ifelse(Wedstrijd$isOwnGoal == "TRUE",0,Wedstrijd$xG)
  
  return(Wedstrijd)
}
