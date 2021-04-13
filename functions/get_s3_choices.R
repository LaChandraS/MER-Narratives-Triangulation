library(aws.s3)
library(readxl)

get_s3_choices <- function(type) {

Sys.setenv(
    AWS_PROFILE = "AWS-SANDBOX-SYSTEM_NARRATIVES"
  )

#Lists all of bucket contents
choices <- aws.s3::get_bucket(bucket = "sandbox.pepfar.data.data-extracts")

#get just path names
choices <- lapply(choices, '[[', 1)

#decide if narratives or mer
if(type == "narratives"){
choices <- choices[grepl("^Narratives", choices)]
} else { choices <- choices[grepl("^MER", choices)]}

#get just file names
cleaned_choices <- lapply(choices, function(x) gsub(".*\\/", "", x))

#make dataframe of file names and path names
choices <- do.call(rbind, Map(data.frame, "file_names" = cleaned_choices, "path_names" = choices, stringsAsFactors = FALSE))

#filter just files that end in txt or xlsx
choices <- choices[grepl("txt$|xlsx$",choices$file_names),]

#reset row names
rownames(choices) <- NULL

return(choices)

}


