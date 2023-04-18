#' Add type of play
#' 
#' @param OneGame A game.

#' @return df of game with Typ_of_play column
#' @examples
#' typeOfPlay(OneGame)
#' @export


typeOfPlay <- function(OneGame){
  
  CornerTaken <- OneGame %>% filter_all(any_vars(. %in% "CornerTaken")) %>% mutate(Type_of_play = "CornerTaken") %>% select(PlayerId,id,Type_of_play)
  FreekickTaken <- OneGame %>% filter_all(any_vars(. %in% "FreekickTaken")) %>% mutate(Type_of_play = "FreekickTaken") %>% select(PlayerId,id,Type_of_play)
  RegularPlay <- OneGame %>% filter_all(any_vars(. %in% "RegularPlay")) %>% mutate(Type_of_play = "RegularPlay") %>% select(PlayerId,id,Type_of_play)
  Throwin <- OneGame %>% filter_all(any_vars(. %in% "ThrowIn")) %>% mutate(Type_of_play = "ThrowIn") %>% select(PlayerId,id,Type_of_play)
  Penalty <- OneGame %>% filter_all(any_vars(. %in% "Penalty")) %>% mutate(Type_of_play = "Penalty") %>% select(PlayerId,id,Type_of_play)
  
  
  Typ_of_play <- rbind(CornerTaken,FreekickTaken,RegularPlay,Throwin,Penalty)
  
  df <- left_join(OneGame,Typ_of_play,by=c("PlayerId", "id")) 
  return(df)
}
