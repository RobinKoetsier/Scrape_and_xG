#' A Twitter Theme
#'
#' This theme is for Twitter
#' @param love 
#' @keywords twitter
#' @export
#' @examples
#' use theme() afterwards

theme_twitter <-  function(){
  theme(plot.background = element_rect(fill="#120E41", colour="#120E41"),
        panel.background = element_rect(fill="#120E41", colour="#4CACE2"),
        text = element_text(family = "Spartan-Medium", colour="#4CACE2"),
        plot.title = ggtext::element_markdown(family = "Spartan-Medium", colour="white",size=12),
        plot.subtitle = ggtext::element_markdown(family = "Spartan-Medium", colour="#4CACE2"),
        panel.grid.major = element_line(size=.05),
        panel.grid.minor = element_blank(),
        axis.text =element_text(family = "Spartan-Medium", colour="#4CACE2"),
        #axis.text = element_blank(),
        legend.background = element_rect(fill="#120E41"),
        legend.key  = element_rect(fill="#120E41"),
        strip.background =element_rect(fill="#79CFDB"),
        strip.text = element_text(colour = 'black'))
}
