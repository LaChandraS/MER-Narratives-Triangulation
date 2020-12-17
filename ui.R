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

shinyUI(navbarPage(theme = shinytheme("simplex"),
                   
                   "Narratives & Visualizations",
                   
                   tabPanel(
                     "Home",
                     sidebarLayout(
                       sidebarPanel(fileInput("import", "Choose Narratives File (xlsx)",
                                              multiple = FALSE),
                                    fileInput("import1", "Choose MSD File (txt)",
                                              multiple = FALSE),
                                    selectInput("ou_list", "Operating Unit",
                                                choices = NULL),
                                    selectInput("area_list", "Program Area",
                                                choices = NULL),
                                    # selectInput("indicator_list", "Indicator",
                                    #             choices = NULL),
                                    actionButton("display","Analyze!")),
                       mainPanel(
                         tags$h1("Narratives Triangulation Tool"),
                         tags$h3("Introduction"),
                         p("This tool triangulates Monitoring, Evaluation, and Reporting (MER) narratives with indicator results.
                                  The Narratives Traingulation Tool takes in MER Narratives and MER Structured Datasets (MSD) providing cross-filtering between the two data sources.
                                  Once the required data sources have been imported, the pivot table visualizer can be used to investigate the content of the selected narrative using the MER indicators.
                                  This tool also has advanced features for textual analysis of the narratives including sentiment analysis, tf-idf, n-grams and correlations."),
                         hr(),
                         tags$h3("Instructions"),
                         p("1. Upload your narratives file (.xlsx)"),
                         p("2. Upload your MSD file (.txt)"),
                         p(tags$i("Note: Any type of MSD can be used, but results will be limited to the level of the MSD")),
                         p("3. Select an Operating Unit and/or Program Area"),
                         p("4. Click Analyze!"),
                         p("5. Explore different text mining analyses under Narrative Analysis"),
                         p("6. Choose a narrative in Narratives Explorer"),
                         p("7. Use Narratives Trends to see any reported narratives from previous quarters"),
                         p("8. Triangulate the narrative with MER indicators using under Result"),
                         hr(),
                         tags$h3("Sections Under Construction"),
                         p("N-Grams and Correlations, Latent Dirichlet Allocation, Resources, Narrative Trends"),
                         hr(),
                         tags$h3("Questions?"),
                         tags$strong("Randy Yee (pcx5@cdc.gov)"),
                         p("CDC/GDIT | CGH | DGHT | HIDMSB | DUAT-ICPI"),
                         tags$strong("Stephanie Fowler (sfowler@baosystems.com)"),
                         p("PEPFAR PRIME Systems | PEPFAR Data Analytics Platform"),
                         tags$strong("Siddharth Dixit (sdixit@baosystems.com)"),
                         p("PEPFAR PRIME Systems | PEPFAR Data Analytics Platform")
                       )
                     )
                     
                   ),
                   navbarMenu("Narrative Analyses",
                              tabPanel("Sentiment Analysis",
                                       tags$h2("Introduction"),
                                       p("Sentiment analysis is performed using a modified lexicon of Bing Liu and collaborators. 
                                         Unigrams with sentiment designations are joined to the narrative datasets and scored according to pre-defined sentiments of positive or negative.
                                         Modifications have been made to the Bing lexicon so that the sentiments are in agreement with the language of PEPFAR (i.e. suppression which is commonly understood to be positive, as in viral suppression)."),
                                       tags$h2("Global Sentiments"),
                                       p("This section shows the general sentiment of the PEPFAR program across all operating units by indicator bundles."),
                                       plotOutput("sentiment_ous", height = "800px"),
                                       tags$h2("Sentiments of the Operating Unit"),
                                       p("If the following sections are empty, please go to the Home page and select an operating unit."),
                                       plotOutput("sentiment_ou", height = "400px"),
                                       tags$h2("Contribution of Each Word to Sentiments"),
                                       p("The following plots show the relative contribution of each word to the overall indicator bundle."),
                                       plotOutput("sentiment_ou_contribution", height = "800px")#,
                                       #plotOutput("sentiment_ou_contribution_ind")
                              ),
                              tabPanel("Wordclouds",
                                       tags$h2("Introduction"),
                                       p("Word clouds showing top positive and negative words. 
                                         Size of word indicates frequency.
                                         If the following sections are empty, please go to the Home page and select an operating unit."),
                                       tags$h2("Operating Unit Comparison Cloud: Positive vs. Negative Sentiment"),
                                       p("Comparison word cloud showing the top positive and negative sentiments of the selected operating unit."),
                                       plotOutput("compare_cloud_ou"),
                                       tags$h2("Program Area Comparison Cloud: Positive vs. Negative Sentiment"),
                                       p("Comparison word cloud showing the top positive and negative sentiments of the selected operating unit by program area."),
                                       plotOutput("compare_cloud_ou_area")
                                       # column(4,
                                       #        tags$h3("Testing"),
                                       #        plotOutput("compare_cloud_ou_test")),
                                       # 
                                       # column(4,
                                       #        tags$h3("Treatment"),
                                       #        plotOutput("compare_cloud_ou_treatment")),
                                       # 
                                       # column(4,
                                       #        tags$h3("Viral Suppression"),
                                       #        plotOutput("compare_cloud_ou_vl")),
                                       # 
                                       # column(6,
                                       #        tags$h3("Prevention"),
                                       #        plotOutput("compare_cloud_ou_prevent")),
                                       # 
                                       # column(6,
                                       #        tags$h3("Health Systems"),
                                       #        plotOutput("compare_cloud_ou_hs")
                                       ),
                              tabPanel("tf-idf",
                                       tags$h2("Introduction"),
                                       p("TF-IDF is a statistical measure that evaluates how relevant a word is to a Narrative in a collection of Narratives. 
                                         TF stands for Term Frequency, which means how many times a word appears in a Narrative.
                                         IDF is Inverse Document Frequency of a word across the set of MER Narratives.
                                         By multiplying these values, we get the TF-IDF measure. The higher this measure, the more relevant a term is in those documents"),
                                       tags$h2("TF-IDF Comparison by OUs"),
                                       p("Presenting the OUs with the highest occurences of COVID related words.", 
                                         DT::dataTableOutput('tfidfdt')),
                                       ),
                              tabPanel("N-grams and Correlations"),
                              tabPanel("Latent Dirichlet Allocation"),
                              tabPanel("Resources",
                                       sidebarLayout(
                                         
                                         sidebarPanel(tags$h2("Lexicons"),
                                                      p("These are the lexicons used in the narratives analyses. They are standard lexicons in NLP literature. 
                                                        Obviously, the language used in the narratives is particular to HIV/AIDS, so standard lexicons may not always agree with usage (i.e. \"suppression\" being a positive term in PEPFAR)."),
                                                      tags$h2("Instructions"),
                                                      p("Here you can edit entries to improve the results of the analyses.
                                                        If you disagree with an entry, feel free to delete it from the lexicon. 
                                                        If you find an entry incorrectly valued, feel free to revalue it.
                                                        Any changes made will be saved to a Google Sheet and be made persistently available to the application."),
                                                      p("If you would like to make edits to the Google Sheet directly:"),
                                                      tags$a(href="https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit#gid=1057635978", "Lexicons"),
                                                      hr(),
                                                      selectInput("resource_list", "Choose Resource",
                                                                  choices = c("Bing", "Stop Words", "Covid Words")),
                                                      actionButton("save", "Save")),
                                         mainPanel(
                                           DT::dataTableOutput("resourcedt"))
                                                      ))
                   ),
                   navbarMenu("Narratives",
                              tabPanel("Narratives Explorer",
                                       DT::dataTableOutput('narrativesdt')),
                              tabPanel("Narrative Trends")),
                   tabPanel("Triangulation",
                            tags$h2("Summary"),
                            textOutput("title"),
                            p(),
                            textOutput("content"),
                            hr(),
                            tags$h2("Pivot Table"),
                            rpivotTableOutput("msd_df"))
)
)
