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

TweetTweet <- function(Wedstrijd = data.frame(1,1),number){
  library(twitteR)
  consumerKey = "pmVG7CdbDlVX6AEmCZe0wE7VI"
  consumerSecret = "RE48lbsIvIQGY0nLc89zzjbDBXO5H5RsuI965pX2n4nGjhaD1Y"
  accessToken = "1418489210907107330-nPdpjW5ZFtBPiiN1RZwmRGWNwDFgvb"
  accessSecret = "av62qr9QmJ8TTZGIW3ggqyHuL7zo0wRFw1hPyWAZWPMbj"
  
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
  #{HashHome}{HashAway} #Eredivisie #xG"), 
          mediaPath = "graphs/dashboard.png")
  }else if(number ==2){
    tweet(glue::glue("Expected Rank Eredivisie
interactive: gettingbluefingers.com/rank

#Eredivisie #xG"), 
          mediaPath = "plots/xRank.png")
  }else{
    tweet(glue::glue("Topscorers Eredivisie
    all players: gettingbluefingers.com/shots

#Eredivisie #xG"), 
          mediaPath = "plots/Topscorers.png")
  }
}

