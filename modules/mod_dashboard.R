dashboard_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ui_updater_ui(ns("ui")),
    
    fluidRow(width = 12,
             box(width = 6,
                 title = "Sentiments",
                 status = "warning",
                 solidHeader = T,
                 plotOutput(ns("sentimentplot"))
             )),
    fluidRow(width = 12,
             box(width = 12, 
                 title = "Narratives",
                 status = "primary",
                 solidHeader = T,
                 DT::dataTableOutput(ns("narrativesdt")))
    ))
  
}



dashboard_server <- function(id, n_df, sentiment, stop){
  
  moduleServer(id, function(input, output, session){
    
    # Resources
    narratives <- reactive({n_df})
    bing       <- reactive({sentiment})
    stopwords  <- reactive({stop})
    
    # Sub-Modules
    subn_df      <- ui_updater_server("ui", narratives()) # Narratives Subset
    
    # Sub-Module Outputs
    output$narrativesdt <- DT::renderDataTable({
      
      DT::datatable(subn_df()[,c(1,3,5,6,7,12)], 
                    selection = "single",
                    rownames=FALSE,
                    filter="top",
                    options = list(
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = 700))
      
    })
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Sentiments
    sentiment_df <- reactive({
      
      subn_df() %>%
        mutate(row_num = 1:n()) %>%
        group_by(`Operating Unit`) %>%
        mutate(narratives_per_ou = n()) %>%
        ungroup() %>%
        unnest_tokens(word, Narrative)%>%
        anti_join(stopwords()) %>%
        inner_join(bing()) %>%
        count(`Operating Unit`, `Indicator Bundle`, Indicator, narratives_per_ou, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
      
    })
    
    output$sentimentplot <- renderPlot({
      
      if(length(unique(sentiment_df()$`Indicator Bundle`)) == 1) {
        ggplot(sentiment_df(), aes(`Indicator`, sentiment, fill = `Indicator`)) +
          geom_bar(stat = "identity", show.legend = T) +
          facet_wrap(~`Operating Unit`, scales = "free_x") +
          theme_linedraw() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.background = element_blank(),
                plot.background = element_blank(),
                legend.background = element_blank())
      } else {
      ggplot(sentiment_df(), aes(`Indicator Bundle`, sentiment, fill = `Indicator Bundle`)) +
        geom_bar(stat = "identity", show.legend = T) +
        facet_wrap(~`Operating Unit`, scales = "free_x") +
        theme_linedraw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_blank())
      }
    }, bg = "transparent")
    
  })
  
  # /////////////////////////////////////////////////////////////////////////////////////////
  # TF_IDF
  
  
}


