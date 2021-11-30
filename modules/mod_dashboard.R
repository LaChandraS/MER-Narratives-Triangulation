dashboard_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    tags$style(type="text/css", "body { overflow-y: scroll; }"),
    
    ui_updater_ui(ns("ui")),
    
    fluidRow(width             = 12,
             infoBox(width     = 3,
                     title     = "Overall Sentiment",
                     textOutput(ns("sentimentscore")),
                     icon      = icon("comments"),
                     fill      = T
             ),
             infoBox(width     = 3,
                     title     = "Most Positive",
                     div(style ="font-size:10px;",
                         uiOutput(ns("max_overall"))
                     ),
                     icon      = icon("thumbs-up"),
                     color     = "green",
                     fill      = T
             ),
             infoBox(width     = 3,
                     title     = "Most Negative",
                     div(style = "font-size:10px;",
                         uiOutput(ns("min_overall"))
                     ),
                     icon     = icon("thumbs-down"),
                     color    = "red",
                     fill     = T
             ),
             infoBox(width    = 3,
                     title    = "Trending",
                     textOutput(ns("trending")),
                     icon     = icon("arrow-up"),
                     color    = "yellow",
                     fill     = T
             )
    ),
    fluidRow(width            = 12,
             box(width        = 6,
                 title        = "Sentiments",
                 status       = "primary",
                 solidHeader  = T,
                 plotOutput(ns("sentimentplot"))
             ),
             box(width       = 6,
                 title       = "Sentiment Contributions",
                 status      = "primary",
                 solidHeader = T,
                 plotOutput(ns("contribution"))
             )
    ),
    fluidRow(width           = 12,
             box(width       = 6,
                 title       = "Bigram Network",
                 status      = "primary",
                 solidHeader = T,
                 plotOutput(ns("bigramviz"))
             ),
             box(width       = 6,
                 title       = "Wordcloud",
                 status      = "primary",
                 solidHeader = T,
                 wordcloud2Output(ns("wordcloudy"))
             )
    ),
    fluidRow(width           = 12,
             tabBox(width    = 12, 
                    tabPanel("TF_IDF",     DT::dataTableOutput(ns("tfidf_df"))),
                    tabPanel("Narratives", DT::dataTableOutput(ns("narrativesdt")))
             )
    )
  )
  
}



dashboard_server <- function(id, 
                             n_df, 
                             nm_df,
                             bing, stopwords, negationwords
                             ){
  
  moduleServer(id, function(input, output, session){
    
    # Resources
    narratives         <- reactive({n_df})
    narratives_meta    <- reactive({nm_df})

    
    # Sub-Modules
    ui_info        <- ui_updater_server("ui", narratives_meta()) # Narratives Subset
    
    # Sub-Module Outputs
    output$narrativesdt <- DT::renderDataTable({
      
      DT::datatable(narratives() %>%
                      filter(if(ui_info[[2]]() != "All") (`Operating Unit`   == ui_info[[2]]()) else TRUE) %>%
                      filter(if(ui_info[[3]]() != "All") (`Indicator Bundle` == ui_info[[3]]()) else TRUE) %>%
                      filter(if(ui_info[[4]]() != "All") (`Indicator`        == ui_info[[4]]()) else TRUE) %>%
                      filter(if(ui_info[[5]]() != "All") (`Period`           == ui_info[[5]]()) else TRUE),
                    rownames = FALSE,
                    filter   = "top",
                    options  = list(
                      searchHighlight = TRUE,
                      scroller        = TRUE,
                      scrollX         = TRUE,
                      scrollY         = 700))
      
    })
    
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # bigram df
    
    prepared_bigram <- reactive({ 
      shiny::validate(
        need(length(ui_info[[2]]()) > 0, "Calculating..."),
        need(length(ui_info[[3]]()) > 0, "Calculating..."),
        need(length(ui_info[[4]]()) > 0, "Calculating..."),
        need(length(ui_info[[5]]()) > 0, "Calculating...")
      )
      narratives() %>%
        filter(if(ui_info[[2]]() != "All") (`Operating Unit`   == ui_info[[2]]()) else TRUE) %>%
        filter(if(ui_info[[3]]() != "All") (`Indicator Bundle` == ui_info[[3]]()) else TRUE) %>%
        filter(if(ui_info[[4]]() != "All") (`Indicator`        == ui_info[[4]]()) else TRUE) %>%
        filter(if(ui_info[[5]]() != "All") (`Period`           == ui_info[[5]]()) else TRUE) %>%
        prepare_bigrams(., bing, stopwords, negationwords)
    })
    
    prepared_sent <- reactive({
      prepared_bigram() %>%
        prepare_sentiments()
    })
    
    prepared_sent_c <- reactive({ 
      prepared_bigram() %>% 
        prepare_sent_contributes()
    })
    
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Sentiments
    
    summaries <- reactive({ sent_summary(prepared_sent()) })
    
    ## Infoboxes
    output$sentimentscore <- renderText({

      format(as.numeric(sum(prepared_sent()$sentiment, na.rm = T)), big.mark=",")
      
    })
    
    output$min_overall <- renderUI({

      summaries()[[1]]
      
    })
    
    output$max_overall <- renderUI({

      summaries()[[2]]
      
    })
    
    output$trending <- renderText({   

      str_to_title((prepared_sent() %>%
                      count(ngram, sort = T) %>%
                      top_n(1))$ngram[[1]])
    })
    
    
    ## Plots
    output$sentimentplot <- renderPlot({

      if(length(unique(prepared_sent()$`Indicator Bundle`)) == 1) {
        
        ggplot(prepared_sent() %>%
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
                legend.background  = element_blank(),
                legend.title       = element_blank())
        
      } else {
        
        ggplot(prepared_sent() %>%
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
                legend.background  = element_blank(),
                legend.title       = element_blank())
      }
      
    }, bg = "transparent")
    
    output$contribution <- renderPlot({

      ggplot(prepared_sent_c(), aes(ngram, n, fill = sentiment), environment = environment()) +
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
              legend.title       = element_blank(),
              legend.position    = "top")
      
      
    }, bg = "transparent")
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Bigrams
    
    output$bigramviz <- renderPlot({

      prepared_bigram() %>%
        count(word1, word2, sort = T) %>%
        top_n(50) %>%
        visualize_bigrams()
      
    }, bg = "transparent")
    
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # TF_IDF
    
    output$tfidf_df <- DT::renderDataTable({

      DT::datatable(
        prepared_bigram() %>%
          count(Name, ngram, sort = T) %>%
          bind_tf_idf(ngram, Name, n) %>%
          arrange(desc(tf_idf)) %>%
          mutate(tf     = round(tf,     2),
                 idf    = round(idf,    2),
                 tf_idf = round(tf_idf, 2)) %>%
          separate(Name, c("Operating Unit", "Indicator Bundle", "Indicator"), sep = "\\|"),
        rownames=FALSE,
        filter="top",
        options = list(
          searchHighlight = TRUE,
          scroller = TRUE,
          scrollX = TRUE,
          scrollY = 700)
      )
      
    })
    
    # /////////////////////////////////////////////////////////////////////////////////////////
    # Wordcloud
    
    output$wordcloudy <- renderWordcloud2({

      wordcloud2a(
        prepared_bigram() %>%
          count(ngram, sort = T) %>%
          rename("word" = "ngram") %>%
          rename("freq" = "n"),
        color = "black",
        backgroundColor = "transparent"
      )
      
    })
    
  })
  
  
}


