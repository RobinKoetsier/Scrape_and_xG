#' Add type of play
#' 
#' @param OneGame A game.

#' @return df of game with Typ_of_play column
#' @examples
#' typeOfPlay(OneGame)
#' @export


typeOfPlayKKD <- function(OneGame){
  
  CornerTaken <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Corner taken", .x))) > 0) %>% mutate(Type_of_play = "CornerTaken") %>% select(PlayerId,eventId,TeamId,Type_of_play)
  FreekickTaken <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Free kick taken", .x))) > 0)  %>% mutate(Type_of_play = "FreekickTaken") %>% select(PlayerId,eventId,TeamId,Type_of_play)
  RegularPlay <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Regular play", .x))) > 0)  %>% mutate(Type_of_play = "RegularPlay") %>% select(PlayerId,eventId,TeamId,Type_of_play)
  Throwin <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Throw-in", .x))) > 0) %>% mutate(Type_of_play = "ThrowIn") %>% select(PlayerId,eventId,TeamId,Type_of_play)
  Penalty <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Penalty", .x))) > 0) %>% mutate(Type_of_play = "Penalty") %>% select(PlayerId,eventId,TeamId,Type_of_play)
  Goalkick <- OneGame %>% filter(rowSums(across(everything(), ~grepl("Goal Kick", .x))) > 0) %>% mutate(Type_of_play = "Goalkick") %>% select(PlayerId,eventId,TeamId,Type_of_play)
  
  
  Type_of_play <- rbind(CornerTaken,FreekickTaken,RegularPlay,Throwin,Penalty,Goalkick)
  
  df <- left_join(OneGame,Type_of_play,by=c("PlayerId", "eventId","TeamId")) %>%
    mutate(Type_of_play = replace_na(Type_of_play,"RegularPlay"))
  return(df)
}
