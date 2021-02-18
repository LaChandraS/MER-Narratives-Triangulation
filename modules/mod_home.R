home_ui <- function(){
  
  #ns <- NS(id)
  
  fluidRow(
    box(width = 12,
        tags$h1("Narratives Triangulation Tool"),
        tags$h3("Introduction"),
        p("This tool triangulates Monitoring, Evaluation, and Reporting (MER) narratives with indicator results.
          The Narratives Traingulation Tool takes in MER Narratives and MER Structured Datasets (MSD) providing cross-filtering between the two data sources.
          Once the required data sources have been imported, the pivot table visualizer can be used to investigate the content of the selected narrative using the MER indicators.
          This tool also has advanced features for textual analysis of the narratives including sentiment analysis, tf-idf, n-grams and correlations."),
        hr(),
        tags$h3("Instructions"),
        tags$a(href="https://pepfar-panorama.org/pepfarlanding/#login", "Download Narratives and MSDs here!"),
        tags$ol(
          tags$li("Upload your narratives file (.xlsx)"),
          tags$li("Upload your MSD file (.txt)"),
          tags$li("Explore the narratives using the dashboard"),
          tags$li("Summarize program changes using the impact table"),
          tags$li("Evaluate lexicon resources"),
          tags$li("Triangulate the narrative with MER indicators")
        ),
        hr(),
        tags$i("Note: Any type of MSD can be used, but results will be limited to the level of the MSD"),
        hr(),
        tags$h3("Questions?"),
        tags$strong("Randy Yee (pcx5@cdc.gov)"),
        p("CDC/GDIT | CGH | DGHT | HIDMSB | DUAT-ICPI"),
        tags$strong("Stephanie Fowler (sfowler@baosystems.com)"),
        p("PEPFAR PRIME Systems | PEPFAR Data Analytics Platform"),
        tags$strong("Siddharth Dixit (sdixit@baosystems.com)"),
        p("PEPFAR PRIME Systems | PEPFAR Data Analytics Platform")
    ) # End home/fluidrow/box
  ) # End home/fluidrow
}