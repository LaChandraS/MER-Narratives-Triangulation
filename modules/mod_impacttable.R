impact_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    ui_updater_ui(ns("ui_b")),
    fluidRow(box(width = 3, 
                 status = "warning",
                 numericInput(ns("ngram"), "Ngram", value=2, min = 1, max = NA, step = 1)),
             box(width = 3, 
                 status = "warning",
                 textInput(ns("term"), "Sentence Filter (uses regex)", "\blow|decrease|decreased"))
    ),
    fluidRow(tabBox(width = 12,
                    tabPanel("Ngrams", dataTableOutput(ns("narratives_ngrams"))),
                    tabPanel("Sentences", dataTableOutput(ns("narratives_sentences")))
    ))
  )
  
  
}


impact_server <- function(id, n_df){
  
  moduleServer(id, function(input, output, session){
    
    narratives <- reactive({n_df})
    term       <- reactive({input$term})
    ui_info    <- ui_updater_server("ui_b", narratives()) # Narratives Subset
    
    ngram_num  <- reactive({input$ngram})
    
    output$narratives_ngrams <- renderDataTable({
      
      DT::datatable(
        ui_info[[1]]() %>%
          unnest_tokens(ngram, Narrative, token = "ngrams", n = ngram_num()) %>%
          unite("Name", any_of(c("Operating Unit", "Org Level", "Organisation Site"))) %>%
          select(-any_of(c("Fiscal Year", "Fiscal Quarter", "Metrics"))),
        
        extensions = 'Buttons',
        #rownames=FALSE,
        #filter="top",
        options = list(searchHighlight = TRUE,
                       dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel')))
      
    })
    
    output$narratives_sentences <- renderDataTable({
      
      DT::datatable(
        ui_info[[1]]() %>%
          unnest_tokens(sentence, Narrative, token = "sentences") %>%
          filter(str_detect(sentence, term())) %>%
          group_by(`Operating Unit`,`Org Level`, `Organisation Site`, `Indicator Bundle`, Indicator, `Support Type`, `Funding Mechanism`, `Implementing Mechanism`) %>%
          summarise(Sentences = paste0(sentence, collapse = "[...]")) %>%
          ungroup() %>%
          unite("Name", any_of(c("Operating Unit", "Org Level", "Organisation Site"))), 
        
        extensions = 'Buttons',
        #rownames=FALSE,
        #filter="top",
        options = list(
          searchHighlight = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'))
      )
      
    })
    
  })
  
}
