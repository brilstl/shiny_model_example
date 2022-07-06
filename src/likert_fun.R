# libraries ----
library(dplyr)
library(arrow)
library(geoarrow)

likert_fun <- function(selection){
  
  x <- 
    open_dataset("data/bbga_geo.parquet") %>%
    filter(grepl("IINKQ", variabele) & jaar == 2019 & naam == selection) %>%
    mutate(waarde = as.numeric(waarde)/100) %>%
    collect() 
  
  p1 <- 
    x %>%
    ggplot(
      aes(x = waarde,
          y = naam,
          fill = label,
          tooltip = glue::glue("{label}<br>share:  {scales::percent(waarde)}"))
    ) +
    geom_col_interactive() +
    scale_x_continuous(labels = scales::percent) +
    guides(fill = guide_legend(nrow = 3, reverse = TRUE, byrow = TRUE)) +
    ggois::theme_ois() +
    scale_fill_manual(values = unlist(unname(ggois::os_colours))) +
    labs(x = NULL, y = NULL, fill = NULL, title = "Households with a disposable income per quintile") +
    theme(
      legend.position = "none"
    )
  
  gp1 <- 
    girafe(ggobj = p1,
         width_svg = 10, 
         height_svg = 1,
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
