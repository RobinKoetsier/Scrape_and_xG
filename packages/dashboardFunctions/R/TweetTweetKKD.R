#' Add together two numbers
#' 
#' @param Wedstrijd A df from the game or empty
#' @param number what do we tweet?.
#' @return it tweets!
#' @examples
#' TweetTweet(Wedstrijd, 1)
#' 1 = game
#' 2 = xRank
#' 3 = topscorers
#' @export

TweetTweetKKD <- function(Wedstrijd = data.frame(1,1),number){
  library(twitteR)
  consumerKey = "EMrF9z7lcwbNFrj3CM0lelsR9"
  consumerSecret = "lACU90dZDurcnZfbt6zyLlRqALBisXV0OcfZ3fQtj0rZ8L8Dx0"
  accessToken = "234746783-cLdroBPF86mPFZvWZ1qUEPdF5YjHbAlbdlxtvBzk"
  accessSecret = "rDELanbzRq9jtTMadpIDME6UzNFwqzqhZMcfcOujqWOHe"
  
  setup_twitter_oauth(consumerKey,
                      consumerSecret,
                      accessToken,
                      accessSecret)
  
  if(number == 1){
    HashHome <- gsub(" ","",Wedstrijd$HomeTeam[1])
    HashAway <- gsub(" ","",Wedstrijd$AwayTeam[1])
    HashHome <- GetHashtag(Wedstrijd$HomeTeam[1])
    HashAway <- GetHashtag(Wedstrijd$AwayTeam[1])
    
    tweet(glue::glue("Match Dashboard {Wedstrijd$HomeTeam[1]} - {Wedstrijd$AwayTeam[1]}
  #{HashHome}{HashAway} #KKD #xG"), 
          mediaPath = "graphs/dashboard.png")
  }else if(number ==2){
    tweet(glue::glue("Expected Rank KKD
interactive: gettingbluefingers.com/rankKKD

#KKD #xG"), 
          mediaPath = "plots/xRankKKD.png")
  }else{
    tweet(glue::glue("Topscorers KKD
    all players: gettingbluefingers.com/shotsKKD

#KKD #xG"), 
          mediaPath = "plots/TopscorersKKD.png")
  }
}

