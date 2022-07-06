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
    ggois::theme_ois() +
    scale_fill_manual(values = rev(unlist(unname(ggois::os_colours)))) +
    labs(x = NULL, y = NULL, fill = NULL, title = "type of property registration") +
    theme(
      legend.position = "none"
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
