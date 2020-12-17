library(shiny)
library(shinythemes)
library(shiny)
library(readxl)
library(googlesheets4)
library(tidyverse)
library(rpivotTable)
library(tidytext)
library(DT)
library(reshape2)
library(wordcloud)

source("data_management.R")


options(shiny.maxRequestSize=4000*1024^2)

shinyServer(function(input, output, session) {
  
  #### UI UPDATES ####
  
  observe({
    
    ou_lt <- unique(narratives()$`Operating Unit`)
    
    # Clear choices if no narratives imported
    if (is.null(narratives()))
      ou_lt <- character(0)
    
    # Update selectors once narratives imported
    updateSelectInput(session, "ou_list",
                      label = "Operating Unit",
                      choices = append("All",ou_lt)
    )
    
  })
  
  observe({
    
    ind_lt <- unique(narratives()$Indicator)
    
    # Clear choices if no narratives imported
    if (is.null(narratives()))
      ind_lt <- character(0)
    
    # Update selectors once narratives imported
    updateSelectInput(session, "indicator_list",
                      label = "Indicator",
                      choices = append("All",ind_lt)
    )
    
  })
  
  observe({
    
    area_lt <- unique(narratives()$`Indicator Bundle`)
    
    # Clear choices if no narratives imported
    if (is.null(narratives()))
      area_lt <- character(0)
    
    # Update selectors once narratives imported
    updateSelectInput(session, "area_list",
                      label = "Program Area",
                      choices = append("All",area_lt)
    )
    
  })
  
  #### PARAMETERS ####
  
  row_count <- reactive({input$narrativesdt_rows_selected}) # User selection through datatable
  
  operatingunit_name <- reactive({narratives()[[row_count(), 1]]}) 
  indicator_name <- reactive({narratives()[[row_count(), 5]]})
  im_name <- reactive({narratives()[[row_count(), 8]]})
  support_name <- reactive({narratives()[[row_count(), 6]]})
  
  narratives_content <- reactive({narratives()[[row_count(), 12]]})
  
  #### NARRATIVE RESOURCES ####
  
  bing <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing",
                     sheet = "HIV Sentiments (based on Bing et al.)")
  
  stopwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing",
                          sheet = "Stopwords")
  
  covidwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing",
                          sheet = "Covidwords")
  
  output$resourcedt <- DT::renderDataTable({
    if(input$resource_list == "Bing") {
      DT::datatable(bing, options = list(pageLength = 25))
    } else if(input$resource_list == "Stop Words"){
      DT::datatable(stopwords, options = list(pageLength = 25))
    } else {
      DT::datatable(covidwords, options = list(pageLength = 25))
    }
  })
  
  
  #### NARRATIVES IMPORT ####
  
  narratives <- reactive({
    if(is.null(input$import))
    {
      return()
    }
    isolate({
      narrative_path <- input$import
      data <- read_excel(narrative_path$datapath,
                         col_types = "text",
                         skip = 7)
    })
  })
  
  
  output$narrativesdt <- DT::renderDataTable({
    
    if(input$ou_list == "All" & input$area_list == "All"){
      narratives_df <- narratives()
    } else if(input$ou_list == "All" & input$area_list != "All"){
      narratives_df <- filter(narratives(), `Indicator Bundle` == input$area_list)
    } else if(input$ou_list != "All" & input$area_list == "All"){
      narratives_df <- filter(narratives(), `Operating Unit` == input$ou_list)
    } else{
    narratives_df <- narratives() %>% 
      filter(`Operating Unit` == input$ou_list) %>%
      filter(`Indicator Bundle` == input$area_list)
    }
    DT::datatable(narratives_df[,c(1,3,5,6,7,12)], 
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
  
  #### MSD ####
  
  data <- reactive({
    inFile <- input$import1
    
    if (is.null(inFile))
      return(NULL)
    
    new_msd <- msd_import(inFile$datapath)
    
  })
  
  #### PIVOT TABLE & VISUALS ####
  
  output$op <- renderText({
    operatingunit_name()
  })
  
  output$ind <- renderText({
    indicator_name()
  })
  
  output$sup <- renderText({
    support_name()
  })
  
  output$im <- renderText({
    im_name()
  })
  
  output$title <- renderText({
    paste(operatingunit_name(),indicator_name(),support_name(),im_name(), sep = "; ")
  })
  
  output$content <- renderText({
    narratives_content()
  })
  
  observeEvent(input$display,{
    
    
    output$msd_df <- renderRpivotTable({
      
      rpivotTable(data()%>%
                    filter(operatingunit == operatingunit_name())%>%
                    filter(indicator == indicator_name()) %>%
                    filter(mech_name == im_name()) %>%
                    filter(indicatortype == support_name()) %>% 
                    select(-contains("uid")),
                  rows="period", cols=c("operatingunit", "psnu", "standardizeddisaggregate"),
                  vals = "value", aggregatorName = "Integer Sum")
      
    })
  })
  
  
  #### TEXT ANALYSIS ####
  
  text_prepare <- reactive({
    
    clean_textdf <- narratives() %>%
      unnest_tokens(word, Narrative)%>%
      anti_join(stop_words)
    
  })
  
  #### SENTIMENTS ####
  
  sentiment_df <- reactive({
    
    mersentiment <- text_prepare() %>%
      inner_join(bing) %>%
      count(`Operating Unit`, `Indicator Bundle`, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
  })
  
  
  observeEvent(input$display,{
    
    output$sentiment_ous <- renderPlot(
      
      ggplot(sentiment_df(), aes(`Indicator Bundle`, sentiment, fill = `Operating Unit`)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme(axis.text.x = element_text(size = rel(0.5))) +
        facet_wrap(~`Operating Unit`, scales = "free")+
        theme_linedraw() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(angle=90))
      
    )
  })
  
  observeEvent(input$display,{
    
    output$sentiment_ou <- renderPlot(
      
      if(input$ou_list != "All"){
      ggplot(sentiment_df() %>% filter(`Operating Unit`==input$ou_list), aes(`Indicator Bundle`, sentiment, fill = `Operating Unit`)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme_linedraw() +
        theme(axis.title.x = element_blank())} else {
      
      ggplot(sentiment_df(), aes(`Indicator Bundle`, sentiment, fill = `Operating Unit`)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme_linedraw()+
        theme(axis.title.x = element_blank())}
      
    )
  })
  
  
  observeEvent(input$display,{
    
    output$sentiment_ou_contribution <- renderPlot({
      
      if(input$ou_list != "All"){
      text_prepare() %>% 
        filter(`Operating Unit` == input$ou_list) %>%
        inner_join(bing) %>%
        count(`Indicator Bundle`, word, sentiment, sort = TRUE) %>%
        mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col() +
        coord_flip() +
        labs(y = "Contribution to sentiment")+
        facet_wrap(~`Indicator Bundle`, scales = "free_y") +
        scale_x_discrete(guide=guide_axis(n.dodge=2)) + 
        theme_linedraw() +
        theme(legend.position = "top", axis.title.x = element_blank())} else{
      
      text_prepare() %>% 
        inner_join(bing) %>%
        count(`Indicator Bundle`, word, sentiment, sort = TRUE) %>%
        mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col() +
        coord_flip() +
        labs(y = "Contribution to sentiment")+
        facet_wrap(~`Indicator Bundle`, scales = "free_y") +
        scale_x_discrete(guide=guide_axis(n.dodge=2)) + 
        theme_linedraw() +
        theme(legend.position = "top", axis.title.x = element_blank())}
    })
  })
  
  
  # observeEvent(input$display,{
  #   
  #   output$sentiment_ou_contribution_ind <- renderPlot({
  #     
  #     text_prepare() %>% 
  #       filter(`Operating Unit`== input$ou_list) %>%
  #       filter(Indicator==input$indicator_list) %>%
  #       inner_join(bing) %>%
  #       count(`Indicator Bundle`, Indicator, word, sentiment, sort = TRUE) %>%
  #       mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  #       mutate(word = reorder(word, n)) %>%
  #       ggplot(aes(word, n, fill = sentiment)) +
  #       geom_col() +
  #       coord_flip() +
  #       labs(y = "Contribution to sentiment")+
  #       facet_wrap(~Indicator, scales = "free_y") +
  #       scale_x_discrete(guide=guide_axis(n.dodge=2)) + 
  #       theme_linedraw() +
  #       theme(legend.position = "none", axis.title.x = element_blank())
  #   })
  # })
  
  
  #### WORD CLOUD ####
  observeEvent(input$display,{
    
    output$compare_cloud_ou <- renderPlot({
      if(input$ou_list != "All"){
      text_prepare() %>% 
        filter(`Operating Unit`==input$ou_list) %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)}
      text_prepare() %>% 
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
    })
  })
  
  observeEvent(input$display,{
    
    output$compare_cloud_ou_area <- renderPlot({
      if(input$ou_list != "All" & input$area_list != "All"){
      text_prepare() %>% 
        filter(`Operating Unit`==input$ou_list) %>%
        filter(`Indicator Bundle`==input$area_list) %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
    } else if(input$ou_list == "All" & input$area_list != "All"){
      text_prepare() %>% 
        filter(`Indicator Bundle`==input$area_list) %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
      } else if(input$ou_list != "All" & input$area_list == "All"){
        text_prepare() %>% 
        filter(`Operating Unit`==input$ou_list) %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
      } else {
        text_prepare() %>% 
          inner_join(bing) %>%
          count(word, sentiment, sort = TRUE) %>%
          acast(word ~ sentiment, value.var = "n", fill = 0) %>%
          comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                           max.words = 100)
        }
    })
  })
  
  observeEvent(input$display,{
    output$compare_cloud_ou_test <- renderPlot({
      text_prepare() %>% 
        filter(`Operating Unit`==input$ou_list) %>%
        filter(`Indicator Bundle`=="Testing") %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
        
    })
  })
  
  observeEvent(input$display,{
    output$compare_cloud_ou_treatment <- renderPlot({
      text_prepare() %>% 
        filter(`Operating Unit`==input$ou_list) %>%
        filter(`Indicator Bundle`=="Treatment") %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
      
    })
  })
  
  observeEvent(input$display,{
    output$compare_cloud_ou_vl <- renderPlot({
      text_prepare() %>% 
        filter(`Operating Unit`==input$ou_list) %>%
        filter(`Indicator Bundle`=="Viral Suppression") %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
      
    })
  })
  
  observeEvent(input$display,{
    output$compare_cloud_ou_prevent <- renderPlot({
      text_prepare() %>% 
        filter(`Operating Unit`==input$ou_list) %>%
        filter(`Indicator Bundle`=="Prevention") %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
      
    })
  })
  
  observeEvent(input$display,{
    output$compare_cloud_ou_hs <- renderPlot({
      text_prepare() %>% 
        filter(`Operating Unit`==input$ou_list) %>%
        filter(`Indicator Bundle`=="Health Systems") %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                         max.words = 100)
      
    })
  })
  
  #### TF-IDF ####
  
  output$tfidfdt <- DT::renderDataTable({
    
    narratives_df <- narratives()
    
    num_narratives = nrow(narratives_df)
    
    tf_idf_calc <- narratives_df %>%
      mutate(row_num = 1:n()) %>%
      unnest_tokens(word, Narrative) %>%
      anti_join(stop_words) %>%
      mutate(covid = if_else(word %in% covidwords$word, 1, 0)) %>%
      group_by_at(vars(-word, -covid)) %>%
      summarise(total = n(),
                covid_n = sum(covid)) %>%
      ungroup() %>%
      mutate(covid_tf = covid_n/ total) %>%
      mutate(covid_idf = ifelse(covid_n == 0, 0, log(num_narratives/covid_n))) %>%
      mutate(covid_tfidf = covid_tf * covid_idf) %>%
      group_by(`Operating Unit`) %>%
      summarise('Total Words' = sum(total), 'Covid Words' = sum(covid_n), 'Covid TF' = sum(covid_tf), 'COVID IDF' = sum(covid_idf), 'TFIDF' = mean(covid_tfidf))
    
    tf_idf_calc <- tf_idf_calc[order(-tf_idf_calc$TFIDF),]
    
    DT::datatable(tf_idf_calc, 
                  rownames=FALSE,
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = 700,
                    pageLength = 50
                  )
    )
  })
  
})
