triangulation_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(width = 12,
            fluidRow(width = 12,
                     ui_updater_ui(ns("ui_a"))),
            fluidRow(width = 3,
                     box(width = 3, 
                         title = "Summary",
                         textOutput(ns("title"))),
                     box(width = 9,
                         title = "Narrative",
                         textOutput(ns("content")))
            ),
            
            tabBox(width = 12,
                   tabPanel("Narratives",
                            dataTableOutput(ns("narrativesdt"))),
                   tabPanel("Pivot Table",
                            div(style = 'overflow-y: scroll',
                                rpivotTableOutput(ns("filter_msd_df"))
                            ))
            )
  )
}


triangulation_server <- function(id, n_df, m_df){
  
  moduleServer(id, function(input, output, session){
    
    narratives <- reactive({n_df})
    msd <- reactive({m_df})
    
    # Sub-Modules
    ui_df <- ui_updater_server("ui_a", narratives())
    
    # Sub-Module Outputs
    output$narrativesdt <- DT::renderDataTable({
      DT::datatable(ui_df()[,c(1,3,5,6,7,12)], # Pass UI selector input?
                    selection = "single",
                    rownames=FALSE,
                    filter="top",
                    options = list(
                      searchHighlight = TRUE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = 700
                      
                    )
      )
    })
    
    # User selection through datatable
    row_count          <- reactive({input$narrativesdt_rows_selected}) 
    operatingunit_name <- reactive({req(row_count()) 
      narratives()[[row_count(), 1]]}) 
    indicator_name     <- reactive({req(row_count())
      narratives()[[row_count(), 5]]})
    im_name            <- reactive({req(row_count())
      narratives()[[row_count(), 8]]})
    support_name       <- reactive({req(row_count())
      narratives()[[row_count(), 6]]})
    narratives_content <- reactive({req(row_count())
      narratives()[[row_count(), 12]]})
    
    # Narratives summary after user selection
    output$title <- renderText({
      paste(row_count(),operatingunit_name(), indicator_name(), support_name(), im_name(), sep = "; ")
    })
    
    output$content <- renderText({
      req(row_count())
      narratives_content()
    })
    
    # MSD filter after selection through datatable
    output$filter_msd_df <- renderRpivotTable({
      req(row_count())
      rpivotTable(msd()%>%
                    filter(operatingunit == operatingunit_name()) %>%
                    filter(indicator     == indicator_name()) %>%
                    filter(mech_name     == im_name()) %>%
                    filter(indicatortype == support_name()) %>% 
                    select(-contains("uid")),
                  rows="period", cols=c("operatingunit", "psnu", "standardizeddisaggregate"),
                  vals = "value", aggregatorName = "Integer Sum")
      
    })
    
  })
  
}