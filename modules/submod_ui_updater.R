# Dashboard SubModule: Narratives UI Updater

ui_updater_ui <- function(id){
  
  ns <- NS(id)
  
    fluidRow(width = 12,
             box(width = 3, selectInput(ns("ou_list"),        "Operating Unit",
                                        choices = "All")),
             box(width = 3, selectInput(ns("area_list"),      "Program Area",
                                        choices = "All")),
             box(width = 3, selectInput(ns("indicator_list"), "Indicator",
                                        choices = "All")),
             box(width = 3, selectInput(ns("period_list"),    "Period",
                                        choices = "All"))
    )
  
}


ui_updater_server <- function(id, n_df){
  
  moduleServer(id, function(input, output, session){
    
    # Get narratives df
    narratives <- reactive({n_df})
    
    # Get slicer inputs
    ou_label   <- reactive({input$ou_list})
    area_label <- reactive({input$area_list})
    ind_label  <- reactive({input$indicator_list})
    
    # Update slicer choices depending on other slicers
    observe({
      ou_lt <- unique(narratives()$`Operating Unit`)
      updateSelectInput(session, "ou_list",
                        label = "Operating Unit",
                        choices = append("All", ou_lt))
    })
    
    observe({
      area_lt <- unique((narratives() %>%
                           filter(if(ou_label()    != "All") (`Operating Unit` == ou_label())     else TRUE))$`Indicator Bundle`)
      updateSelectInput(session, "area_list",
                        label = "Program Area",
                        choices = append("All", area_lt))
    })
    
    observe({
      ind_lt <-  unique((narratives() %>%
                           filter(if(ou_label()   != "All") (`Operating Unit`   == ou_label())   else TRUE) %>%
                           filter(if(area_label() != "All") (`Indicator Bundle` == area_label()) else TRUE))$Indicator)
      updateSelectInput(session, "indicator_list",
                        label = "Indicator",
                        choices = append("All", ind_lt))
    })
    
    # Return filtered narratives
    narratives_sub <- reactive({narratives() %>%
        filter(if(ou_label()   != "All") (`Operating Unit`   == ou_label())   else TRUE) %>%
        filter(if(area_label() != "All") (`Indicator Bundle` == area_label()) else TRUE) %>%
        filter(if(ind_label()  != "All") (`Indicator`        == ind_label())  else TRUE)
    })
    
    ui_df <- reactive({narratives_sub()})
    
    return(ui_df)
    
  })
}