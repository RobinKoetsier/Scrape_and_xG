#' Get related player for shots
#' 
#' @param df A data frame
#' @return df with relatedPlayerId added
#' @examples
#' getRelatedKKD(dfcsv)
#' @export

getRelatedKKD <- function(With_names,df){
  
  assisted <- With_names %>%
    
    filter(Qualifier_name == "Related event ID") %>%
    filter(isShot == TRUE) %>%
    mutate(`Related event ID` = qualifier_value) %>%
    select(eventId,playerName, `Related event ID`,TeamId) %>%
    #select(-eventId) %>%
    #rename(eventId = `Related event ID`)%>%
    mutate(`Related event ID` = as.numeric(`Related event ID`))
  
  
  temp <-  df %>%
    select(eventId, TeamId, playerName) %>%
    rename(relatedPlayerId = playerName) %>%
    mutate(eventId = as.numeric(eventId)) %>%
    left_join(assisted, by = c(  "eventId" = "Related event ID",
                                 "TeamId" = "TeamId")) %>%
    drop_na() %>%
    rename(relatedEventId = eventId,
           eventId = eventId.y)
  
  withRel <- df %>% left_join(temp)
  return(withRel)
}
