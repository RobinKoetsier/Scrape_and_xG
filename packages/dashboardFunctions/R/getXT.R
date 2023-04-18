#' Add xThreat
#' 
#' @param OneGame A game.

#' @return df of game with xT column
#' @examples
#' getXT(OneGame)
#' @export

getXT <- function(OneGame){
  grid <- read.csv("https://raw.githubusercontent.com/RobinKoetsier/temp/master/xT.csv", 
                   header = FALSE, sep = ";")
  grid <- apply(grid, 2, rev)
  df <- typeOfPlay(OneGame)
  df$Type_of_play[is.na(df$Type_of_play)] <- "RegularPlay"
  data <- df %>% filter(`type/displayName` == "Pass") %>% 
    filter(`outcomeType/displayName` == "Successful") %>%
    filter(Type_of_play == "RegularPlay")
  colnames(grid)<-round(seq(0,93.75,(100/16)),4)
  rownames(grid)<-round(seq(0,(100-(100/12)),(100/12)),4)
  
  #rownames(grid)<-round(seq(from=(100),to=(100/12),by=-(100/12)),4)
  grid[ order(row.names(grid)), ]
  
  data$xTStart <- sapply(seq(nrow(data)), function(i) {
    grid[rev(which(as.numeric(rownames(grid)) <= data$y[i]))[1],
         rev(which(as.numeric(colnames(grid)) <= data$x[i]))[1]]
  })
  
  data$xTEnd <- sapply(seq(nrow(data)), function(i) {
    grid[rev(which(as.numeric(rownames(grid)) <= data$endY[i]))[1],
         rev(which(as.numeric(colnames(grid)) <= data$endX[i]))[1]]
  })
  
  data$net <- as.numeric(unlist(data$xTEnd)) - as.numeric(unlist(data$xTStart))
  data$side <- ifelse(data$TeamId==data$HomeTeam, "Home", "Away")
  df <- data %>% group_by(PlayerId,TeamId,side) %>% summarise(total = sum(net),passes=n(),pp=total/passes) %>%
    ungroup()
  # df <- top_n(df,5,total) 
  return(df)
}
