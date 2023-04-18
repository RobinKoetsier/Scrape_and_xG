#' A TDL Theme
#'
#' This theme is for Twitter
#' @param  nothing
#' @keywords twitter, tdl
#' @export
#' @examples
#' use theme() afterwards

theme_tdl <-  function(){
  theme(plot.background = element_rect(fill="#f2f4f5", colour="#f2f4f5"),
        panel.background = element_rect(fill="#f2f4f5", colour="green4"),
        text = element_text(family = "Spartan-Medium", colour="green4"),
        plot.title = ggtext::element_markdown(family = "Spartan-Medium", colour="green4",size=12),
        plot.subtitle = ggtext::element_markdown(family = "Spartan-Medium", colour="green4"),
        panel.grid.major = element_line(size=.05),
        panel.grid.minor = element_blank(),
        axis.text =element_text(family = "Spartan-Medium", colour="green4"),
        #axis.text = element_blank(),
        legend.background = element_rect(fill="#f2f4f5"),
        legend.key  = element_rect(fill="#f2f4f5"),
        strip.background =element_rect(fill="#f2f4f5"),
        strip.text = element_text(colour = 'green4'))
}


#' A Twitter Theme
#'
#' This theme is for Twitter
#' @param love 
#' @keywords twitter
#' @export
#' @examples
#' use theme() afterwards

theme_tdl_pitch <-  function(){
  theme(plot.background = element_rect(fill="#f2f4f5", colour="#f2f4f5"),
        panel.background = element_rect(fill="#f2f4f5", colour="#f2f4f5"),
        text = element_text(family = "Spartan-Medium", colour="green4"),
        plot.title = ggtext::element_markdown(family = "Spartan-Medium", colour="green4",size=12),
        plot.subtitle = ggtext::element_markdown(family = "Spartan-Medium", colour="#f2f4f5"),
        panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank(),
        axis.text =element_text(family = "Spartan-Medium", colour="#f2f4f5"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text = element_blank(),
        legend.background = element_rect(fill="#f2f4f5"),
        legend.key  = element_rect(fill="#f2f4f5"),
        legend.text =  element_text(family="Spartan-Medium"),
        strip.background =element_rect(fill="#f2f4f5"),
        strip.text = element_text(colour = 'green4'))
}

