#' Get xT values for carries only.
#'gives you the start, end en net xT value of a carry
#' @param dataframe Filter the data frame beforhand so it only contains successful carries with dribble_start_x,dribble_start_y,dribble_end_x & dribble_end_y
#' @keywords xThreat
#' @export
#' @examples
#' getXTPerPass(all_data)
#' 

getXTPerCarry <- function(OneGame){
  grid <- read.csv("https://raw.githubusercontent.com/RobinKoetsier/temp/master/xT.csv",header=FALSE, sep=";")
  grid <- apply(grid, 2, rev) # flip grid because y is reversed in opta data. 
  data <- OneGame
  
  colnames(grid)<-round(seq(0,93.75,(100/16)),4)
  rownames(grid)<-round(seq(0,(100-(100/12)),(100/12)),4)
  
  #rownames(grid)<-round(seq(from=(100),to=(100/12),by=-(100/12)),4)
  grid[ order(row.names(grid)), ]
  
  data$xTStart <- sapply(seq(nrow(data)), function(i) {
    grid[rev(which(as.numeric(rownames(grid)) <= data$dribble_start_y[i]))[1],
         rev(which(as.numeric(colnames(grid)) <= data$dribble_start_x[i]))[1]]
  })
  
  data$xTEnd <- sapply(seq(nrow(data)), function(i) {
    grid[rev(which(as.numeric(rownames(grid)) <= data$dribble_end_y[i]))[1],
         rev(which(as.numeric(colnames(grid)) <= data$dribble_end_x[i]))[1]]
  })
  
  data$net <- as.numeric(unlist(data$xTEnd)) - as.numeric(unlist(data$xTStart))
  
  return(data)
}
