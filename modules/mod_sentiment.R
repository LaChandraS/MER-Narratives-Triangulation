sentiment_ui <- function(id){
  
  ns <- NS(id)
  
  fluidRow(width = 12,
           box(width = 6,
               title = "Sentiments",
               dataTableOutput(ns("sentimenttab"))
           )
  )
  
}


sentiment_server <- function(id, subn_df, sent, stop){
  
  moduleServer(id, function(input, output, session){
    
    narratives <- reactive({subn_df})
    sentiments <- reactive({sent})
    stopwords  <- reactive({stop})
    
    sentiment_df <- reactive({
      narratives() %>%
        mutate(row_num = 1:n()) %>%
        group_by(`Operating Unit`) %>%
        mutate(narratives_per_ou = n()) %>%
        ungroup() %>%
        unnest_tokens(word, Narrative)%>%
        anti_join(stopwords()) %>%
        inner_join(sentiments()) %>%
        count(`Operating Unit`, `Indicator Bundle`, Indicator, narratives_per_ou, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
    })
    
    output$sentimenttab <- renderDataTable(sentiment_df())
    
    output$sentimentplot <- renderPlot({
      
      ggplot(sentiment_df(), aes(`Indicator Bundle`, sentiment, fill = `Indicator Bundle`)) +
        geom_bar(stat = "identity", show.legend = T) +
        facet_wrap(~`Operating Unit`, scales = "free") +
        theme_linedraw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank())
      
      # if(unique(sentiment_df()$`Operating Unit` == 1)){
      #   
      #   ggplot(sentiment_df(), aes(`Indicator Bundle`, sentiment, fill = `Indicator Bundle`)) +
      #     geom_bar(stat = "identity", show.legend = T) +
      #     facet_wrap(~`Indicator Bundle`, scales = "free") +
      #     theme_linedraw() +
      #     theme(axis.title.x = element_blank(),
      #           axis.title.y = element_blank(),
      #           axis.text.x = element_blank())
      #   
      # } else {
      #   
      # ggplot(sentiment_df(), aes(`Indicator Bundle`, sentiment, fill = `Indicator Bundle`)) +
      #   geom_bar(stat = "identity", show.legend = T) +
      #   facet_wrap(~`Operating Unit`, scales = "free") +
      #   theme_linedraw() +
      #   theme(axis.title.x = element_blank(),
      #         axis.title.y = element_blank(),
      #         axis.text.x = element_blank())
      #   
      # }
      
    })
    
  })
  
}