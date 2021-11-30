# Dashboard SubModule: Narratives UI Updater

ui_updater_ui <- function(id){
  
  ns <- NS(id)
  
  fluidRow(width = 12,
           box(width = 3, uiOutput(ns("ous"))),
           box(width = 3, uiOutput(ns("ars"))),
           box(width = 3, uiOutput(ns("inds"))),
           box(width = 3, uiOutput(ns("pers")))
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
    per_label  <- reactive({input$period_list})
    
    # Update slicer choices depending on other slicers
    output$ous <- renderUI({
      selectInput (session$ns("ou_list"), 
                   label   = "Operating Unit", 
                   choices = append(sort(unique(narratives()$`Operating Unit`)), "All"))
    })
    output$ars <- renderUI({
      shiny::validate(
        need(length(input$ou_list) > 0, "Calculating...")
      )
      selectInput (session$ns("area_list"), 
                   label   = "Program Area", 
                   choices = append("All", sort(unique(filter(narratives(),if(input$ou_list != "All") (`Operating Unit` == input$ou_list) else TRUE)$`Indicator Bundle`))))
    })
    output$inds <- renderUI({
      shiny::validate(
        need(length(input$ou_list) > 0, "Calculating..."),
        need(length(input$area_list) > 0, "Calculating...")
      )
      selectInput (session$ns("indicator_list"), 
                   label   = "Indicator", 
                   choices = append("All", sort(unique((narratives() %>% 
                                                         filter(if(input$ou_list   != "All") (`Operating Unit` == input$ou_list)     else TRUE) %>%
                                                         filter(if(input$area_list != "All") (`Indicator Bundle` == input$area_list) else TRUE))$`Indicator`))))
    })
    output$pers <- renderUI({
      selectInput (session$ns("period_list"), 
                   label   = "Period", 
                   choices = append(sort(unique(filter(narratives(),!str_detect(`Period`, "Target"))$`Period`), decreasing = T), "All"))
    })

    
    # Return filtered narratives
    narratives_sub <- reactive({
      narratives() %>%
        filter(if(input$ou_list        != "All") (`Operating Unit`   == input$ou_list)         else TRUE) %>%
        filter(if(input$area_list      != "All") (`Indicator Bundle` == input$area_list)       else TRUE) %>%
        filter(if(input$indicator_list != "All") (`Indicator`        == input$indicator_list)  else TRUE) %>%
        filter(if(input$period_list    != "All") (`Period`           == input$period_list)     else TRUE)
    })
    
    
    ui_df <- reactive({narratives_sub()})
    
    return(    
      list(
        ui_df,
        ou_label,
        area_label,
        ind_label,
        per_label
      ))
    
  })
  
}
