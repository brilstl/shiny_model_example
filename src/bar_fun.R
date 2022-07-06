# libraries ----
library(dplyr)
library(arrow)
library(geoarrow)

bar_fun <- function(selection){
  
  x <- 
    open_dataset("data/bbga_geo.parquet") %>%
    filter(grepl("^W", variabele) & jaar == 2019 & naam == selection) %>%
    mutate(waarde = as.numeric(waarde)/100) %>%
    collect() %>%
    mutate(label = str_remove_all(label, "Property: % "))
  
  p1 <- 
    x %>%
    ggplot(
      aes(x = waarde,
          y = label,
          fill = label,
          tooltip = glue::glue("{label}<br>share:  {scales::percent(waarde)}"))
    ) +
    geom_col_interactive() +
    scale_x_continuous(labels = scales::percent) +
    guides(fill = guide_legend(nrow = 3, reverse = TRUE, byrow = TRUE)) +
    scale_fill_manual(values = c("#323232", "#767676", "#B4B4B4")) +
    labs(x = NULL, y = NULL, fill = NULL, title = "type of property registration") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text = element_text(
        family = 'sans',
        size = 20,
        face = "bold"
      ),
      plot.caption = element_text(
        family = 'sans',
        size = 14,
        face = "bold"
      ),
      axis.title = element_text(
        family = 'sans',
        hjust = 1,
        size = 14
      ),
      plot.subtitle = element_text(family = 'sans',
                                   size = 20),
      plot.title = element_text(
        family = 'sans',
        lineheight = 1.5,
        size = 21,
        face = "bold"
      ),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      panel.grid.major.y = element_line(size = 1.2),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.justification = "center",
      panel.border = element_rect(fill = "transparent",
                                  color = NA),
      legend.text = element_text(family = 'sans',
                                 size = 20),
      legend.title = element_text(
        family = 'sans',
        size = 20,
        face = "bold"
      ),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent",
                                     color = NA),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent",
                                           colour = "transparent"),
      strip.text = element_text(
        family = 'sans',
        size = 20,
        face = "bold"
      )
    )
  
  gp1 <- 
    girafe(ggobj = p1,
           width_svg = 10, 
           height_svg = 2,
           options = list(opts_tooltip(css = tooltip_css),
                          opts_hover(css = "fill:#77BA99;"),
                          opts_selection(type = "single",
                                         css = "fill:pink;stroke:gray;")))
  
  if(nrow(x) == 0){
    return(NULL)
  }else{
    return(gp1)
  }
  
}
