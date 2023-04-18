#' A plot function
#'
#' This function allows you to express your love of cats.
#' @param name_of_player Name of the player defined in the excel file, default = Dusan Tadic.
#' @param showstats show the xG and means. default = FALSE
#' @keywords shotmap
#' @export
#' @examples
#' create_shotmap_twitter(name_of_player = "Dusan Tadic", showstats = FALSE)
#' @save
#' ggsave("name.png",dpi=300,width=8,height=7,device=png)

create_shotmap_twitter <- function(name_of_player = "Dusan Tadic", showstats = FALSE){
  df <- readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                           sheet = "Shots")%>%
    filter(OwnGoal == FALSE) %>%
    filter(playername %in% name_of_player) %>%
    filter(Type_of_play != "Penalty") %>%
    mutate(Goal = replace_na(Goal,0))
  
  df_text <-  df %>%
    filter(Type_of_play != "Penalty") %>%
    filter(OwnGoal == FALSE) %>%
    mutate(Goal = replace_na(Goal,0)) %>%
    filter(playername %in% name_of_player) %>%
    
    group_by(playername) %>%
    summarise(Goals = sum(Goal),
              Shots = n(),
              xG = sum(xG),
              avg = xG/Shots)  
  
  p <- ggplot(df,aes(x,y)) +
    ggsoccer::annotate_pitch(fill = "#120E41") +
    geom_point(aes(x,y, size= xG,fill=as.factor(Goal),colour=as.factor(Goal)),shape=21) +
    coord_flip(xlim=c(50,100),
               ylim=c(100,0))+
    scale_fill_manual(values=c("#120E41","pink"),labels=c("Miss","Goal")) +
    scale_color_manual(values=c("pink","#120E41"),labels=c("Miss","Goal")) +
    
    guides(size=guide_legend(override.aes=list(colour = "pink")))+
    # stat_density_2d(aes(fill = ..level..), geom = "polygon")+
    
    
    themes::theme_twitter_pitch()# +
  theme(panel.background = element_rect(colour="#120E41"),
        axis.text = element_blank(),
        panel.grid.major = element_blank())
  
  
  if(showstats == TRUE){
    p <- p+ 
      geom_text(data = df_text, aes(70,80, label= glue::glue("Goals")),colour="white",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(65,80, label= glue::glue("{round(Goals)}")),colour="white",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(70,60, label= glue::glue("NPxG")),colour="white",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(65,60, label= glue::glue("{round(xG,2)}")),colour="white",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(70,40, label= glue::glue("Shots")),colour="white",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(65,40, label= glue::glue("{round(Shots,2)}")),colour="white",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(70,20, label= glue::glue("NPxG/Shot")),colour="white",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(65,20, label= glue::glue("{round(avg,2)}")),colour="white",family="Spartan-Medium",size=3)+
      labs(x="",
           y="",
           fill = " ",
           colour=" ",
           title = glue::glue("Shotmap {name_of_player} Eredivisie 21/22"),
           subtitle = "Excluding penalty's",
           caption = "@RobinWilhelmus") 
    return(p)
  } else {
    
    p <- p +
      labs(x="",
           y="",
           fill = " ",
           colour=" ",
           title = glue::glue("Shotmap {name_of_player} Eredivisie 21/22"),
           subtitle = "Excluding penalty's",
           caption = "@RobinWilhelmus") 
    return(p)
  }
  
  
}
