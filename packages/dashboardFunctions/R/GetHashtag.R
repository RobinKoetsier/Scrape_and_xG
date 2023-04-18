#' Add hashtag for twitter
#' 
#' @param team A team of the game.

#' @return hastag.
#' @examples
#' GetHashtag(Team)
#' @export


GetHashtag <- function(Team){
  hastag <- case_when(Team == "Ajax" ~ "Aja",
                      Team == "AZ Alkmaar" ~ "AZ",
                      Team == "Cambuur" ~ "Cam",
                      Team == "FC Groningen" ~ "Gro",
                      Team == "FC Utrecht" ~ "Utr",
                      Team == "Feyenoord" ~ "Fey",
                      Team == "Fortuna Sittard" ~ "For",
                      Team == "Go Ahead Eagles" ~ "gae",
                      Team == "Heracles" ~ "Her",
                      Team == "NEC Nijmegen" ~ "NEC",
                      Team == "PEC Zwolle" ~"PEC",
                      Team == "PSV Eindhoven" ~ "PSV",
                      Team == "RKC Waalwijk" ~ "RKC",
                      Team == "SC Heerenveen" ~ "Hee",
                      Team == "Sparta Rotterdam" ~ "Spa",
                      Team == "Twente" ~ "Twe",
                      Team == "Vitesse" ~ "Vit",
                      Team == "Willem II" ~ "Wil",
                      Team == "Excelsior" ~ "Exc",
                      Team == "FC Emmen" ~ "Emm",
                      Team == "FC Volendam" ~ "Vol",
                      Team == "FC Utrecht II" ~ "Utr",
                      Team == "Ajax II" ~ "Aja",
                      Team == "AZ II" ~ "AZ",
                      Team == "PSV II" ~ "PSV",
                      Team == "Heracles Almelo" ~ "Her",
                      Team == "Almere City FC" ~ "Alm",
                      Team == "Roda JC" ~ "ROD",
                      Team == "FC Eindhoven" ~ "Ein",
                      Team == "De Graafschap" ~ "Gra",
                      Team == "VVV Venlo" ~ "VVV",
                      Team == "Telstar" ~ "Tel",
                      Team == "ADO Den Haag" ~ "ADO",
                      Team == "Helmond Sport" ~ "Hel",
                      Team == "NAC Breda" ~ "NAC",
                      Team == "MVV" ~ "MVV",
                      Team == "FC Dordrecht" ~ "Dor",
                      Team == "FC Den Bosch" ~ "DBO",
                      Team == "TOP Oss" ~ "TOP")
}


