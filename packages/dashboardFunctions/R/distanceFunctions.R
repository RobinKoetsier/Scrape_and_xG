#' @title distance
#'
#' @description Function that calculates distance to goal
#'
#' @param x_pos x
#' @param y_pos y

#'
#' @return distance in meters.
#' @examples
#' distance(10,10)

distance <- function(x_pos, y_pos){
  x_meters <- 105
  y_meters <- 68
  
  x_shift <- (100 - x_pos)*x_meters/100
  y_shift <- abs(50 - y_pos)*y_meters/100
  
  distance <- sqrt(x_shift*x_shift + y_shift*y_shift)
}

#' @title distanceToGoal
#'
#' @description Function that calculates distance to goal
#'
#' @param x_pos x
#' @param y_pos y

#'
#' @return distance in meters.
#' @examples
#' distanceToGoal(10,10)
#' @export

distanceToGoal <- function(x_pos, y_pos){
  x_meters <- 105
  y_meters <- 68
  
  x_shift <- (100 - x_pos)*x_meters/100
  y_shift <- abs(50 - y_pos)*y_meters/100
  
  distance <- sqrt(x_shift*x_shift + y_shift*y_shift)
}

#' @title distanceCarry
#'
#' @description Function that calculates distance of carry
#'
#' @param x_start x
#' @param y_start y
#' @param x_end endX
#' @param y_end endY

#'
#' @return distance in meters.
#' @examples
#' distanceCarry(75,10,75,5)
#' @export

distanceCarry <- function(x_start, y_start,x_end,y_end){
  x_meters <- 1.05
  y_meters <- .68
  
  x_shift <- abs(x_start-x_end)*x_meters
  y_shift <- abs(y_start-y_end)*y_meters
  
  distance <- sqrt(x_shift*x_shift + y_shift*y_shift)
  return(distance)
}

#' @title goal_angle
#'
#' @description Function that calculates angle to goal
#'
#' @param x_pos x
#' @param y_pos y
#'
#' @return angle in degrees
#' @examples
#' goal_angle(10,10)
#' @export

goal_angle <- function(x_pos, y_pos){
  x_meters <- 120
  y_meters <- 74
  
  x_shift <- (100 - x_pos)*x_meters/100
  y_shift <- (50 - y_pos)*y_meters/100
  
  angle <- atan((7.32*x_shift)/(x_shift*x_shift + y_shift*y_shift - (7.32/2)*(7.32/2)))
  angle <- ifelse(angle < 0, angle + pi, angle)
  
  angle_degrees <- angle*180/pi
}

