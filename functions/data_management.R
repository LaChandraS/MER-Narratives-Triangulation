# Data Import FXNs

msd_import <- function(msd_txt){
  
  new_msd <- aws.s3::s3read_using(FUN = readr::read_delim, "\t",
                                            escape_double = FALSE,
                                            trim_ws = TRUE,
                                            col_types = readr::cols(.default = readr::col_character(),
                                                             targets = readr::col_double(),
                                                             qtr1 = readr::col_double(),
                                                             qtr2 = readr::col_double(),
                                                             qtr3 = readr::col_double(),
                                                             qtr4 = readr::col_double(),
                                                             cumulative = readr::col_double()), bucket = "sandbox.pepfar.data.data-extracts", object = msd_txt)

  new_msd <- pivot_longer(new_msd,
                          targets:cumulative,
                          names_to = "period",
                          values_to = "value")

  new_msd <- unite(new_msd,
                   "period",
                   c("fiscal_year", "period"),
                   sep = "_",
                   remove = T)

  return(new_msd)
}

nar_import <- function(nar_txt){

  new_nar <- aws.s3::s3read_using(FUN = readxl::read_excel, col_types = "text",
                                  skip = 7, bucket = "sandbox.pepfar.data.data-extracts", object = nar_txt)
  
  #remove office locations
  new_nar <- new_nar[!(grepl("Office", new_nar$`Operating Unit`)),]
  
  #remove first row (blank)
  new_nar <- new_nar[-1,]
  
  #remove all NA columns
  new_nar <- new_nar[,colSums(is.na(new_nar))<nrow(new_nar)]
  
  #change last column name to "Narrative"
  colnames(new_nar)[length(colnames(new_nar))] <- "Narrative"
  
  return(new_nar)
}

