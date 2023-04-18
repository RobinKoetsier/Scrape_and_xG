#' Make the ready for dashboard
#' 
#' @param jsonfile path to json file of match

#' @return png of dashboard
#' @examples
#' doEverything(json_file, HomeTeam, AwayTeam)
#' @export
#' 

doEverything <- function(json_file, HomeTeam, AwayTeam){
  df_list <- list()
  events2 <- jsonlite::read_json(json_file)
  df2 <- events2$event
  bst_model2.0 <- xgb.load("~/Documents/R_with_git/Football/xgb.model")
  load('~/Documents/ScraperWhoScored/TDLXG/dummy.rda') 
  matches <- read.csv("~/Documents/ScraperWhoScored/KKD/KKD Matches.csv") %>%
    select(matchInfo.contestant.0.id,matchInfo.contestant.0.officialName) %>%
    unique() %>%
    `colnames<-`(c("contestantId","TeamId")) %>%
    mutate(TeamId= 
             stringi::stri_replace_all_regex(TeamId,
                                             pattern=c('SV Roda', 'SC ', 'Eindhoven II','Alkmaar Zaanstreek','AFC ', 'HFC ','BV '),
                                             replacement=c('Roda', '', 'II','AZ','','',''),
                                             vectorize=FALSE))
  
  game2 <- data.table::rbindlist(df2,fill=TRUE) %>%
    left_join(matches)
  game_wide2 <- unnest_wider(game2, qualifier, names_sep = "_") 
  game_wide2 <- game_wide2 %>%
    mutate(match =
             glue::glue("{HomeTeam} - {AwayTeam}")) %>%
    mutate(match= 
             stringi::stri_replace_all_regex(match,
                                             pattern=c('SV Roda', 'SC ', 'Eindhoven II','Alkmaar Zaanstreek','AFC ', 'HFC ','BV '),
                                             replacement=c('Roda', '', 'II','AZ','','',''),
                                             vectorize=FALSE))
  game_wide2 <- 
    game_wide2 %>% separate(match, c("HomeTeam", "AwayTeam"), sep=" - ")
  
 
  
  With_names <- game_wide2 %>%
    left_join( #read.csv("https://raw.githubusercontent.com/tomh05/football-scores/master/data/reference/opta-qualifiers.csv",header = FALSE) %>%
      read.csv("opta-qualifiers.csv",header=FALSE) %>%
        select(V1, V3) %>%
        rename( "Qualifier_name" = "V3"),
      by = c("qualifier_qualifierId" = "V1")) %>%
    left_join(#read.csv("https://raw.githubusercontent.com/tomh05/football-scores/master/data/reference/opta-events.csv",header = FALSE) %>%
      read.csv("opta-events.csv",header=FALSE) %>%
        select(V1,V2), by = c("typeId" = "V1")) %>%
    rename(eventName = V2) %>%
    select(-c(id,lastModified,qualifier_id,playerId)) %>%
    filter(eventId > 1) %>%
    mutate(isShot = ifelse(eventName %in% c("Goal ","Attempt Saved ", "Miss ","Post "), TRUE,FALSE )) %>%
    mutate(isGoal = ifelse(eventName == "Goal ", TRUE,FALSE)) 
  
  df <- With_names |>
    unique() %>%
    select(eventId,outcome,periodId,timeMin,timeSec,HomeTeam,AwayTeam,playerName,TeamId,isShot,isGoal,x,y, timeStamp,eventName,Qualifier_name ) %>%
    group_by(eventId,outcome,periodId,timeMin,timeSec,HomeTeam,AwayTeam,playerName, TeamId,x,y,timeStamp,eventName,isShot,isGoal) |>
    mutate(row = seq(n())) |>
    ungroup() |>
    pivot_wider(names_from = "row", values_from = "Qualifier_name") 
  
  df_rel <- getRelatedKKD(With_names,df)
  
  df1 <- PrepAGameKKD(df_rel)
  
  df_for_pred <- df1 %>% select(Goal,x,y,Bodypart,Type_of_play,isIntentionalAssist,isAssistedShot,isBigChance,Gamestate,Time_in_sec,distance,angle)
  
  df_for_pred <- rbind(dummy,df_for_pred)
  df_for_pred$Goal[is.na(df_for_pred$Goal)] <- 0
  df_for_pred$Type_of_play[is.na(df_for_pred$Type_of_play)] <- "RegularPlay"
  
  df_for_pred$Bodypart[is.na(df_for_pred$Bodypart)] <- "RightFoot"
  nc <- 2
  pred <- PredictxG(df_for_pred)
  xG <- as.data.frame(pred[-c(1:15)])
  
  Wedstrijd <- cbind(xG,df1)
  colnames(Wedstrijd)[1]<- "xG"
  Wedstrijd$xG <- ifelse(Wedstrijd$isOwnGoal == TRUE,0,Wedstrijd$xG)
  Wedstrijd$xG <- ifelse(Wedstrijd$Type_of_play == "Penalty",0.79,Wedstrijd$xG)
  Wedstrijd <- Wedstrijd %>%
    rename(PlayerId = playerName)
  df_list[[1]] <- Wedstrijd
  df_list[[2]] <- data.frame(df_rel)
  df_list[[3]] <- data.frame(With_names)
  return(df_list)
}
