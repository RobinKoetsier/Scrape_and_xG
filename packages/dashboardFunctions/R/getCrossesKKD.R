#' Function that adds cross = 1 or cross = 0
#'for xT
#' @param df event data
#' @keywords crosses
#' @examples
#' getCrosses(df)
#' @export 


getCrossesKKD <- function(df){
  crosses <- df %>% filter(rowSums(across(everything(), ~grepl("\\bCross\\b", .x))) > 0) %>%
    mutate(Cross = 1) %>%
    select(x,y,PlayerId,eventId,TeamId,Cross)
  
  all_passes <- left_join(df,crosses) %>%
    mutate(Cross=replace_na(Cross,0)) 
  
  
  return(all_passes)
}