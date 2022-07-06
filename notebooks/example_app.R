# libraries ----
library(shiny)
library(tidymodels)
lapply(list.files(pattern = "[.]R$", path = "src", full.names = TRUE), source)

ui <- shinyUI(fluidPage(
  
  includeCSS("assets/stylesheet_shiny.css"),
  
  sidebarLayout(
    sidebarPanel(
      h1("select a neighbourhood"),
      id = "sidebar",
      br(),
      girafeOutput("plot1"),
      br(),
      br(),
      h1(textOutput("hi")),
    ),
    mainPanel(
      br(),
      h1(textOutput("doei")),
      br(),
      br(),
      girafeOutput("hoi", height = 250),
      br(),
      girafeOutput("ha", height = 250)
      
      
    )
  )))

server <- shinyServer(function(input, output, session) {
  
  selected_area <- reactive({
    input$plot1_selected
  })
  
  output$plot1 <- renderGirafe({
    
    # see src_map_fun for code ----
    
    map_fun()
    
    
  })
  
  # prediction text ----
  
  output$hi <- renderText({
    
    if(is.null(selected_area())){
      
      print("CLICK ON A NEIGBOURHOOD TO CALCULATE PREDICTION")
      
    }else{
      
      x <- 
        open_dataset("data/bbga_geo.parquet") %>%
        filter(jaar == 2019 & naam == selected_area()) %>%
        select(naam,
               variabele,
               waarde) %>%
        collect()
      
      x <- 
        x %>%
        pivot_wider(
          values_from = waarde,
          names_from = variabele
        )
      
      pred_outcome <- 
        read_rds("data/example_model.rds") %>%
        predict(., new_data = x, type = "conf_int") 
      
      pred_lower <- 
        pred_outcome %>%
        pull(.pred_lower)
      
      pred_upper <- 
        pred_outcome %>%
        pull(.pred_upper)
      
      glue::glue("THE PREDICTED NUMBER OF YEARS FOR {selected_area()} IS BETWEEN: \n\n {round(pred_lower, 1)} AND {round(pred_upper, 1)} YEARS")
      
    }
    
  })
  
  # guiding text ----
  
  output$doei <- renderText({
    
    if(is.null(selected_area())){
      
      print("CLICK ON A NEIGBOURHOOD FOR EXTRA INFO")
      
    }else{
      
      glue::glue("NEIGBOURHOOD: {selected_area()}")
      
    }
    
  })
  
  output$hoi <- renderGirafe({
    
    likert_fun(selection = selected_area())
    
    
  })
  
  output$ha <- renderGirafe({
    
    bar_fun(selection = selected_area())
    
  })
  
  
})


shinyApp(ui, server)
