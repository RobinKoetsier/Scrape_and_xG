#' Function that adds end locations
#'for xT
#' @param df event data
#' @keywords crosses
#' @examples
#' getEndKKD(df)
#' @export 


getEndKKD <- function(With_name){
  endxpasses <-   With_names %>%
    filter(Qualifier_name == "Pass End X") %>%
    #select(timeStamp, playerName, Qualifier_name, qualifier_value) %>%
    mutate(endX = as.numeric(qualifier_value)) %>%
    select(eventId,TeamId,playerName, endX) %>%
    left_join(
      With_names %>%
        filter(Qualifier_name == "Pass End Y") %>%
        #select(timeStamp, playerName, Qualifier_name, qualifier_value) %>%
        mutate(endY = as.numeric(qualifier_value)) %>%
        select(eventId,TeamId,playerName, endY))
  
  
  return(endxpasses)
}
