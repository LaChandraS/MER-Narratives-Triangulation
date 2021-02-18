impact_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    ui_updater_ui(ns("ui_b")),
    fluidRow(box(width = 3, 
                 status = "warning",
                 textInput(ns("term"), "Term", "low|decrease|decreased"))),
    fluidRow(box(width = 12,
                 title = "Impact",
                 status = "primary",
                 solidHeader = T,
                 dataTableOutput(ns("narratives_sentences"))))
  )
  
  
}


impact_server <- function(id, n_df){
  
  moduleServer(id, function(input, output, session){
    
    narratives <- reactive({n_df})
    term       <- reactive({input$term})
    subn_df    <- ui_updater_server("ui_b", narratives()) # Narratives Subset
    
    output$narratives_sentences <- renderDataTable({
      subn_df() %>%
        unnest_tokens(sentence, Narrative, token = "sentences") %>%
        filter(str_detect(sentence, paste0("\\b",term(),"\\b"))) %>%
        group_by(`Operating Unit`,`Indicator Bundle`, Indicator) %>%
        summarise(Sentences = paste0(sentence, collapse = "| + |")) %>%
        ungroup()
    })
    
  })
  
}