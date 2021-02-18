# Data Import FXNs

msd_import <- function(msd_txt){
  
  new_msd <- read_delim(msd_txt, 
                        "\t", 
                        escape_double = FALSE,
                        trim_ws = TRUE,
                        col_types = cols(.default = col_character(), 
                                         targets = col_double(),
                                         qtr1 = col_double(),
                                         qtr2 = col_double(),
                                         qtr3 = col_double(),
                                         qtr4 = col_double(),
                                         cumulative = col_double()
                        ) 
  )
  
  new_msd <- pivot_longer(new_msd,
                          targets:cumulative,
                          names_to = "period",
                          values_to = "value")
  
  new_msd <- unite(new_msd, 
                   "period", 
                   c("fiscal_year", "period"),
                   sep = "_", 
                   remove = T)
}