#' @title TopscoreTable
#'
#' @description makes topscore table and saves png
#'
#' @return png file
#' @export

TopscoreTable <- function(){
  library(gt)
  df <- readxl::read_excel("Export_TDL_NED_2223.xlsx", 
                           sheet = "Player xG") %>% arrange(-Goals, -NP_Goals,-xG) %>%
    top_n(10,Goals) %>%
    select(2,1,3:6) %>%
    `names<-`(c("Club", "Player", "Goals", "NP Goals", "xG", "NPxG")) %>%
    mutate(xG = round(xG,1),
           NPxG = round(NPxG,1),
           Club = glue::glue("~/Documents/ScraperWhoScored/TDLXG/Clubs/{Club}.png")) 
  
  
  
  
  df %>%
    slice_head(n=10) %>%
    gt() %>%
    
    data_color(
      columns = vars(xG,NPxG,Goals, `NP Goals`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "left",
          color = "black",
          weight = px(1)
        ),
        cell_text(
          font = "Roboto",
          align = "center"
        )
      ),
      locations = list(
        cells_body(
          columns = vars(Goals, `NP Goals`,xG, NPxG)
        )
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "left",
          color = "black",
          weight = px(3)
          
        )
      ),
      locations = list(
        cells_body(
          columns = vars(xG)
        )
      )
    ) %>%
    text_transform(
      locations = cells_body(vars(Club)),
      fn = function(Club) {
        #  The key is here.
        lapply(Club, local_image)
      }
    ) %>%
    cols_label(
      Club =" ",
      Player = " "
      
    ) %>%
    cols_width(
      vars(Goals, `NP Goals`,xG,NPxG) ~ px(50)
    ) %>%
    cols_align(columns = c(4,5),
               align = "center"
    ) %>%
    tab_options(
      #  column_labels.border.top.color = "white",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(3),
      #    table_body.hlines.color = "white",
      table.border.bottom.color = "black",
      table.border.bottom.width = px(3)
    ) %>% 
    tab_header(
      title = md("**Topscorers Eredivisie 2022/2023**"),
      subtitle = "Sorted on goals and xG"
    )%>%
    tab_source_note(md("**Twitter**: @EredivisiePlots | **Site**: EredivisiePlots.com")
    )  %>%
    opt_table_font(font = "Roboto Mono"
    ) %>%
    gtsave(filename = "plots/Topscorers.png",vwidth = 1000, zoom = 6)
  
}
