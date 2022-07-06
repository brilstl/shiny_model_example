# libraries ----
library(tidyverse)
library(arrow)
library(geoarrow)
library(ggiraph)

# css ----

tooltip_css <- "background-color:#D33F49;color:white;font-weight:500;font-family:'Corbel';padding:10px;border-radius:5px;"


# create map function ----


map_fun <- function(){
  
  # arrow query ----
  
  x <- 
    open_dataset("data/bbga_geo.parquet") %>%
    filter(variabele == "BEVWOONDUUR" & jaar == 2022) %>%
    mutate(waarde = as.numeric(waarde)) %>%
    geoarrow_collect_sf()
  
  # graphic ----
  
  p1 <- x %>% 
    ggplot(aes(fill = waarde,
               geometry = geometry,
               data_id = naam,
               tooltip = glue::glue(
                 "{label}: {waarde} jr.<br>neigbourhood: {naam}"
               ))) +
    geom_sf_interactive(colour = "white") +
    scale_fill_gradient2(low = "#004699", high = "#ec0000") + 
    theme_void() +
    guides(fill = guide_colourbar(barwidth = 14, barheight = .2)) + 
    theme(
      legend.position = "bottom",
      legend.text = element_text(family = "Corbel", size = 15, lineheight = 1.2),
      plot.caption = ggtext::element_markdown(family = "Corbel", size = 15, lineheight = 1.2),
      legend.title = element_text(family = "Corbel", size = 18, lineheight = 1.2),
      strip.background = element_blank(),
      title = element_text(family = "sans", size = 18, face = "bold"),
      strip.text = ggtext::element_textbox(
        size = 22,
        family = "Corbel",
        color = "black", 
        fill = "transparent", 
        box.color = "transparent",
        halign = 0.5, 
        linetype = 1, 
        r = unit(5, "pt"), 
        width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0), 
        margin = margin(3, 3, 3, 3)
      )) +
    coord_sf(default_crs = 4326) +
    labs(title = "average duration  of residence in years", fill = "number of\nyears")
  
  girafe(ggobj = p1,
         #width_svg = 9, 
         #height_svg = 5,
         options = list(opts_tooltip(css = tooltip_css),
                        opts_hover(css = "fill:#77BA99;"),
                        opts_selection(type = "single",
                                       css = "fill:black;stroke:gray;")))
  
  
}