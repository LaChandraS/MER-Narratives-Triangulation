library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(readxl)
library(googlesheets4)
library(tidyverse)
library(rpivotTable)
library(tidytext)
library(DT)
library(reshape2)
library(wordcloud2)
library(ggplot2)
library(igraph)
library(ggraph)
library(leaflet)
library(crayon)
#if (!require("shinysky")) devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
library(aws.s3)
library(readxl)
library(paws)

# Diagnostics library(profvis) library(reactlog)

# options(shiny.reactlog = TRUE)

##### Functions ####
source("./functions/data_management.R")
source("./functions/sentiments_management.R")
source("./functions/bigrams_management.R")

##### Modules ####
source("./modules/mod_home.R")
source("./modules/mod_dashboard.R")
source("./modules/submod_ui_updater.R")
source("./modules/mod_impacttable.R")
source("./modules/mod_resources.R")
source("./modules/mod_triangulation.R")

#### Text Resources ####
gs4_deauth()

bing <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing", 
    sheet = "HIV Sentiments (based on Bing et al.)")

stopwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing", 
    sheet = "Stopwords")

negationwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing", 
    sheet = "Negation")

covidwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing", 
    sheet = "Covidwords")

#### Narratives Resources ####
# Import Narratives from AWS
#
# narratives_df <- aws.s3::s3read_using(FUN       = readxl::read_excel, 
#                                       col_types = "text", 
#                                       bucket    = "sandbox.pepfar.data.data-extracts", 
#                                       object    = "Narratives/MER Narratives.xlsx") %>%
#     filter(`Operating Unit` != "\r\nOffice of U.S. Foreign Assistance Resources")

# Prepare Narratives
# bigrams_df <- narratives_df %>%
#     unnest_tokens(ngram, Narrative, token = "ngrams", n = 2) %>%
#     mutate(ngram = str_replace(ngram, "\\s", "|")) %>%
#     separate(ngram, into = c("word1", "word2"), sep = "\\|")
# saveRDS(bigrams_df, "MER-Narratives-Triangulation/data/bigrams_df")
#
# sent_df <- narratives_df %>%
#     unnest_tokens(word, Narrative)
# 
# saveRDS(sent_df, "MER-Narratives-Triangulation/data/sent_df")

# Read Narratives
narratives_df   <- readRDS("./data/narratives_df")
bigrams_df      <- readRDS("./data/bigrams_df")
# sent_df         <- readRDS("data/sent_df")

narratives_meta <- narratives_df %>%
    select(`Operating Unit`, `Indicator Bundle`, `Indicator`, `Period`) %>%
    unique()

#### MSD Resources ####
# Import MSD from AWS
# msd_df <- aws.s3::s3read_using(FUN           = readr::read_delim, "\t", 
#                                escape_double = FALSE, 
#                                trim_ws       = TRUE, 
#                                col_types     = readr::cols(.default   = readr::col_character(), 
#                                                            targets    = readr::col_double(), 
#                                                            qtr1       = readr::col_double(), 
#                                                            qtr2       = readr::col_double(), 
#                                                            qtr3       = readr::col_double(), 
#                                                            qtr4       = readr::col_double(), 
#                                                            cumulative = readr::col_double()), 
#                                bucket        = "sandbox.pepfar.data.data-extracts", 
#                                object        = "MER Structured Dataset (MSD)/MER FY2021 Q1 Pre-Cleaning/MER_Structured_Datasets_OU_IM_FY19-21_20210212_v1_1.txt")
msd_df <- readRDS("./data/msd_df")


#--------------------------------------------------------------------------------------------#

server <- function(input, output, session) {
    
    
    # Server Modules
    dashboard_server(    "d", narratives_meta, narratives_df, bigrams_df, bing, stopwords, negationwords)
    
    impact_server(       "i", narratives_df)
    
    resources_server(    "r", bing, stopwords, covidwords)
    
    triangulation_server("t", narratives_df, msd_df)
    
    
    # Login
    user_input <- reactiveValues(authenticated = FALSE, status = "", d2_session = NULL)
    
    observeEvent(input$login_button, {
        
        tryCatch({
            datimutils::loginToDATIM(base_url         = "https://www.datim.org/", 
                                     username         = input$user_name, 
                                     password         = input$password, 
                                     d2_session_envir = parent.env(environment()))
        }, error = function(e) {
            shinyWidgets::sendSweetAlert(session, 
                                         title = "Login failed", 
                                         text  = "Please check your username/password!", 
                                         type  = "error")
        })
        
        if (exists("d2_default_session")) {
            
            if (user_input$authenticated == FALSE) {print("here")}
            
            user_input$authenticated <- T
        }
        
        return(user_input)
    })
    
    output$ui <- renderUI({
        if (user_input$authenticated == FALSE) {
            ##### UI code for login page
            fluidPage(fluidRow(column(width = 2, offset = 5, 
                                      br(), br(), br(), br(), uiOutput("uiLogin"))))
        } else {
            
            #### UI: Sidebar ####
            sidebar <- dashboardSidebar(sidebarMenu(
                menuItem("Home",              tabName = "home",        icon = icon("home")), 
                menuItem("Dashboard",         tabName = "analyses",    icon = icon("line-chart")),
                menuItem("Ngrams Explorer",   tabName = "impacttable", icon = icon("compass")), 
                menuItem("MER Triangulation", tabName = "triangulate", icon = icon("puzzle-piece")), 
                menuItem("Resources",         tabName = "resources",   icon = icon("book"))
            ), 
            busyIndicator()
            )


            #### UI: Body ####
            body <- dashboardBody(shinyDashboardThemes(theme = "poor_mans_flatly"), 
                tabItems(tabItem(tabName = "home",        home_ui()), 
                         tabItem(tabName = "analyses",    dashboard_ui("d")), 
                         tabItem(tabName = "impacttable", impact_ui("i")), 
                         tabItem(tabName = "triangulate", triangulation_ui("t")),
                         tabItem(tabName = "resources",   resources_ui("r"))
                  )
                )


            dashboardPage(title = "Narrator", dashboardHeader(title = "Narratives Application"), 
                sidebar, body)
        }
    })
    
    output$uiLogin <- renderUI({
        wellPanel(fluidRow(img(src = "pepfar.png", align = "center"), h4("Welcome to the MER-Narratives App. Please login with your DATIM credentials:")), 
            fluidRow(textInput("user_name",  "Username: ", width = "600px"), 
                passwordInput("password",    "Password:",  width = "600px"), 
                actionButton("login_button", "Log in!")))
    })


}
#--------------------------------------------------------------------------------------------#

