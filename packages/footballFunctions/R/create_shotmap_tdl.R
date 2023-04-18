#' A plot function
#'
#' This function creates a shotmap of a player for Tussen de Linies
#' @param name_of_player Name of the player defined in the excel file, default = Dusan Tadic.
#' @param showstats show the xG and means. default = FALSE
#' @keywords shotmap
#' @export
#' @examples
#' create_shotmap_tdl(name_of_player = "Dusan Tadic", showstats = FALSE)
#' ggsave("name.png",dpi=300,width=8,height=7,device=png)
#' @save
#' ggsave("name.png",dpi=300,width=8,height=7,device=png)

create_shotmap_tdl <- function(name_of_player = "Dusan Tadic", showstats = FALSE){
  df <- readxl::read_excel("Export_TDL_NED_2223.xlsx", 
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
    ggsoccer::annotate_pitch(fill="#f2f4f5")+
    geom_point(aes(size=xG,fill=as.factor(Goal),shape=as.factor(Goal)),
               colour="black") +
    guides(size = guide_legend(order = 2,override.aes = list(color="green4")),
           fill = guide_legend(order = 1),
           shape = guide_legend(order = 1))+
    scale_shape_manual(name = " ",
                       values=c("1" = 21,
                                "0"= 21),
                       
                       labels = c("Mis", "Goal")) +
    scale_fill_manual(name = " ",
                      values=c("1" = "green4",
                               "0"= "transparent"),
                      
                      labels = c("Mis", "Goal")) +
    coord_flip(xlim=c(65,101),
               ylim=c(105,-5))+
    labs(title = glue::glue("Schoten {df$playername[1]} 22/23 (excl. penalty's)"),
         subtitle = glue::glue("{length(df$Goal)} schoten ~ {round(sum(df$xG),2)} xG ~ {sum(df$Goal)} goals"),
         caption = "@Tussendelinies",
         
         size = "xG") +
    #guides(fill = "none") +
    theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
          panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
          
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = ggtext::element_markdown(family="Spartan-Light"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right",
          plot.title = element_text(hjust=0.5,family="Spartan-Bold",color="green4",size=12),
          plot.subtitle = element_text(hjust=0.5,family="Spartan-Medium",color="green4"),
          plot.caption = element_text(size=10,family="Spartan-Medium",color="green4"),
          legend.text =  element_text(family="Spartan-Medium"),
          legend.background = element_rect(fill = "#f2f4f5")) 
  
  
  if(showstats == TRUE){
    p <- p+ 
      geom_text(data = df_text, aes(70,80, label= glue::glue("Goals")),colour="green4",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(65,80, label= glue::glue("{round(Goals)}")),colour="green4",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(70,60, label= glue::glue("NPxG")),colour="green4",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(65,60, label= glue::glue("{round(xG,2)}")),colour="green4",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(70,40, label= glue::glue("Shots")),colour="green4",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(65,40, label= glue::glue("{round(Shots,2)}")),colour="green4",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(70,20, label= glue::glue("NPxG/Shot")),colour="green4",family="Spartan-Medium",size=3)+
      geom_text(data = df_text, aes(65,20, label= glue::glue("{round(avg,2)}")),colour="green4",family="Spartan-Medium",size=3)+
      labs(x="",
           y="",
           fill = " ",
           colour=" ",
           title = glue::glue("Shotmap {name_of_player} Eredivisie 22/23"),
           subtitle = "Exclusief penalty's",
           caption = "@Tussendelinies") 
    return(p)
  } else {
    
    p <- p +
      labs(x="",
           y="",
           fill = " ",
           colour=" ",
           title = glue::glue("Shotmap {name_of_player} Eredivisie 21/22"),
           subtitle = "Exclusief penalties",
           caption = "@Tussendelinies") 
    return(p)
  }
  
  
}
