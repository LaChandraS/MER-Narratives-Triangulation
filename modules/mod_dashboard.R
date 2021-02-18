dashboard_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ui_updater_ui(ns("ui")),
    
    fluidRow(width = 12,
             infoBox(width = 3,
                     title = "Overall Sentiment",
                     textOutput(ns("sentimentscore")),
                     icon = icon("comments"),
                     fill = T
             ),
             infoBox(width = 3,
                     title = "Most Positive",
                     div(style="font-size:10px;",
                         uiOutput(ns("global_sent_max"))
                     ),
                     icon = icon("thumbs-up"),
                     color = "green",
                     fill = T
             ),
             infoBox(width = 3,
                     title = "Most Negative",
                     div(style="font-size:10px;",
                         uiOutput(ns("global_sent_min"))
                     ),
                     icon = icon("thumbs-down"),
                     color = "red",
                     fill = T
             ),
             infoBox(width = 3,
                     title = "Trending",
                     textOutput(ns("trending")),
                     icon = icon("arrow-up"),
                     color = "yellow",
                     fill = T
             )
    ),
    fluidRow(width = 12,
             box(width = 6,
                 height = "auto",
                 title = "Sentiments",
                 status = "primary",
                 solidHeader = T,
                 plotOutput(ns("sentimentplot"))
             ),
             box(width = 6,
                 #height = "600px",
                 title = "TF-IDF",
                 status = "primary",
                 solidHeader = T,
                DT::dataTableOutput(ns("tfidf_df"))
             )
    ),
    fluidRow(width = 12,
             box(width = 6,
                 title = "Bigram Network",
                 status = "primary",
                 solidHeader = T,
                 plotOutput(ns("bigramviz"))
             ),
             box(width = 6,
                 title = "Wordcloud",
                 status = "primary",
                 solidHeader = T,
                 wordcloud2Output(ns("wordcloudy"))
             )
    ),
    fluidRow(width = 12,
             box(width = 12, 
                 title = "Narratives",
                 status = "warning",
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
    subn_df    <- ui_updater_server("ui", narratives()) # Narratives Subset
    
    # Sub-Module Outputs
    output$narrativesdt <- DT::renderDataTable({
      
      DT::datatable(subn_df()[,c(1,3,5,6,7,12)], 
        rownames=FALSE,
        filter="top",
        options = list(
          searchHighlight = TRUE,
          scroller = TRUE,
          scrollX = TRUE,
          scrollY = 700))
      
    })
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Sentiments
    sentiment_df <- reactive({
      
      subn_df() %>%
        unnest_tokens(word, Narrative)%>%
        anti_join(stopwords()) %>%
        inner_join(bing()) %>%
        count(`Operating Unit`, `Indicator Bundle`, Indicator, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
      
    })
    
    output$sentimentscore <- renderText({
      
      format(as.numeric(sum(sentiment_df()$sentiment, na.rm = T)), big.mark=",")
      
    })
    
    ou_sum <- reactive({
      sentiment_df() %>%
        group_by(`Operating Unit`) %>%
        summarize(total = sum(sentiment, na.rm = T)) %>%
        ungroup
    })

    ib_sum <- reactive({
      sentiment_df() %>%
        group_by(`Operating Unit`, `Indicator Bundle`) %>%
        summarize(total = sum(sentiment, na.rm = T)) %>%
        ungroup
    })

    in_sum <- reactive({
      sentiment_df() %>%
        group_by(`Operating Unit`, `Indicator Bundle`, `Indicator`) %>%
        summarize(total = sum(sentiment, na.rm = T)) %>%
        ungroup
    })
    
    output$global_sent_min <- renderUI({
      
      HTML(
        paste(
          ou_sum()$`Operating Unit`[ou_sum()$total   == min(ou_sum()$total)],
          ib_sum()$`Indicator Bundle`[ib_sum()$total == min(ib_sum()$total)],
          in_sum()$`Indicator`[in_sum()$total        == min(in_sum()$total)], 
          sep = "<br/>"
        )
      )
      
    })
    
    output$global_sent_max <- renderUI({
      
      HTML(
        paste(
          ou_sum()$`Operating Unit`[ou_sum()$total   == max(ou_sum()$total)],
          ib_sum()$`Indicator Bundle`[ib_sum()$total == max(ib_sum()$total)],
          in_sum()$`Indicator`[in_sum()$total        == max(in_sum()$total)], 
          sep = "<br/>"
        )
      )
      
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
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Bigrams
    
    output$bigramviz <- renderPlot({
      
      count_bigrams(subn_df(), stopwords()) %>%
        top_n(50) %>%
        visualize_bigrams()
      
    }, bg = "transparent")
    
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # TF_IDF
    
    output$tfidf_df <- DT::renderDataTable({
      
      DT::datatable(
        prepare_bigrams(subn_df(), stopwords()) %>%
          bind_tf_idf(ngram, Name, n) %>%
          arrange(desc(tf_idf)) %>%
          mutate(tf     = round(tf,     2),
                 idf    = round(idf,    2),
                 tf_idf = round(tf_idf, 2)),
        rownames=FALSE,
        options = list(
          searchHighlight = TRUE, scrollY = TRUE)
      )
      
    })
    
    output$wordcloudy <- renderWordcloud2({
      
      wordcloud2(
        count_bigrams(subn_df(), stopwords()) %>%
          unite(word, word1, word2, sep = " ") %>%
          rename("freq" = "n"), 
        color = "black", 
        backgroundColor = "transparent"
      )
      
    })
    
  })
  
  
}


