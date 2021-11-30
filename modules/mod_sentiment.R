sentiment_ui <- function(id){
  
  ns <- NS(id)
  
  
  tabBox(width = 6,
         tabPanel("Sentiments",
                  plotOutput(ns("sentimentplot"))
         ),
         tabPanel("Contribution",
                  plotOutput(ns("contribution"))
         )
  )

  
  
}


sentiment_server <- function(id, sub_narr, sent, stop){
  
  moduleServer(id, function(input, output, session){
    
    subn_df <- reactive({sub_narr})
    sentiments <- reactive({sent})
    stopwords  <- reactive({stop})
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Sentiments
    sentiment_df <- reactive({
      
      subn_df() %>%
        unnest_tokens(word, Narrative) %>%
        anti_join(stopwords()) %>%
        inner_join(bing()) %>%
        count(`Operating Unit`, `Indicator Bundle`, Indicator, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
      
    })
    
    contribution_df <- reactive({
      
      subn_df() %>%
        unnest_tokens(word, Narrative)%>%
        anti_join(stopwords()) %>%
        inner_join(bing()) %>%
        count(word, sentiment, sort = TRUE) %>%
        mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
        mutate(word = reorder(word, n))
      
    })
    
    
    output$contribution <- renderPlot({
      
      contribution_df() %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col() +
        coord_flip() +
        scale_x_discrete(guide=guide_axis(n.dodge=2)) + 
        theme_linedraw() +
        theme(axis.title.x       = element_blank(),
              axis.title.y       = element_blank(),
              axis.text.x        = element_blank(),
              axis.ticks.x       = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background   = element_blank(),
              plot.background    = element_blank(),
              legend.background  = element_blank(),
              legend.position    = "top")
    })
    
    output$sentimentplot <- renderPlot({

      if(length(unique(sentiment_df()$`Indicator Bundle`)) == 1) {

        ggplot(sentiment_df() %>%
                 group_by(`Operating Unit`, `Indicator Bundle`, `Indicator`) %>%
                 summarise(sentiment = sum(sentiment,na.rm = T)) %>%
                 ungroup(), aes(`Indicator`, sentiment, fill = `Indicator`)) +
          geom_col(show.legend = T, na.rm = T) +
          facet_wrap(~`Operating Unit`, scales = "free_x") +
          theme_linedraw() +
          theme(axis.title.x       = element_blank(),
                axis.title.y       = element_blank(),
                axis.text.x        = element_blank(),
                axis.ticks.x       = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.background   = element_blank(),
                plot.background    = element_blank(),
                legend.background  = element_blank())

      } else {

        ggplot(sentiment_df() %>%
                 group_by(`Operating Unit`, `Indicator Bundle`) %>%
                 summarise(sentiment = sum(sentiment,na.rm = T)) %>%
                 ungroup(), aes(`Indicator Bundle`, sentiment, fill = `Indicator Bundle`)) +
          geom_col(show.legend = T, na.rm = T) +
          facet_wrap(~`Operating Unit`, scales = "free_x") +
          theme_linedraw() +
          theme(axis.title.x       = element_blank(),
                axis.title.y       = element_blank(),
                axis.text.x        = element_blank(),
                axis.ticks.x       = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.background   = element_blank(),
                plot.background    = element_blank(),
                legend.background  = element_blank())
      }

    }, bg = "transparent")
    
    
  })
  
}