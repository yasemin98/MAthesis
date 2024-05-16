# DATA COLLECTION with Flempar ####

## set working directory ####

getwd()
setwd("C:/Users/yasem/Desktop/THESIS/data/")


## packages ####

#install.packages("devtools")
library("devtools")
require(devtools)
#install.packages("remotes")
library(remotes)
#install.packages("vctrs")
#install_github("PolscienceAntwerp/flempar")
#remotes::install_github("PolscienceAntwerp/flempar")
library(flempar)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(data.table)
library(lubridate)
library(foreach)
library(purrr)
#install.packages("reticulate")
library(reticulate)
library(scales)
library(ggplot2)
#install.packages("RcppRoll")
library(RcppRoll)
library(doconv)
#remove.packages("cli")
#install.packages("cli")
library(tidyverse)
## set paths ####

# I set a path for the written questions (path_doc) and the details (path_details)
path_doc <- "C:/Users/yasem/Desktop/THESIS/data/written questions/"
path_details <-
  "C:/Users/yasem/Desktop/THESIS/data/written questions details v2/"

## set time period ####

# I create a df with start- and end-dates 
# I collect data from 1995-2024
data.frame(datum_start= seq(ymd("1995-03-01"),
                            ymd("2024-05-01"),
                            by = 182)) %>% # you can edit the number of days in interval, here it's half a year
  mutate(datum_end = lead(datum_start)-1) %>%
  mutate(datum_end = if_else(is.na(datum_end),ymd("2024-05-01"),datum_end))-> date

## DATA RETRIEVAL ####

### details ####

# I collect the details of the written questions (MP information, title, term year ...)
# a for loop is created to extract the data in intervals
# the interval time for each batch of data is 182 days (half a year)
# the get_work function from the Flempar packages is used to retrieve the data

# there are a few dates that give an error that the file is not in JSON format
# and thus stops retrieving data, these dates are disregarded and the date df was
# adjusted to retrieve all data except for those days

# here, I give an example of the error
#Error:
#Searching for written questions.
#Getting the details on the written questions.
#Making 1416 calls.
#Error in { : task 945 failed - "File returned is not in JSON format."
#Timing stopped at: 0.52 0.17 100.6

#the debugging can be found in the DEBUGGING section


for(i in 1:nrow(date)){
  
  output <- try({
    
    get_work(date_range_from=date$datum_start[[i]]
             ,date_range_to=date$datum_end[[i]]
             ,type="details" 
             ,fact="written_questions"
             ,plen_comm="plen")
  })
  
  if(any(stringr::str_detect(tolower(output[1]),"no sessions found"))){
    
    next()
    
  }  
  
  if(any(class(output) == "try-error")){
    
    message("error, will sleep for 1 minute")
    Sys.sleep(60)
    message("slept for 1 minute, let's retry")
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="details"
               ,fact="written_questions"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    message("another error, will sleep for 2 minutes")
    Sys.sleep(2*60)
    message("slept for 2 minutes, let's retry")
    
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="details"
               ,fact="written_questions"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    message("another error, powernap of 15 minutes")
    Sys.sleep(15*60)
    message("slept for 15 minutes, let's try one more time")
    
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="details"
               ,fact="written_questions"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    stop()
    message("function stopped")
    
  }
  
  saveRDS(output, paste0(path_details,date$datum_start[[i]],".rds")) 
  
  message(i,"/",nrow(date))
  
}

### documents ####

# I collect the written questions (documents - the actual questions in text)
# I use the same for-loop with the get_work function from Flempar
# where type=document instead of type=details 
data.frame(datum_start= seq(ymd("1995-03-01"),
                            ymd("2024-05-01"),
                            by = 182)) %>% # you can edit the number of days in interval, here it's half a year
  mutate(datum_end = lead(datum_start)-1) %>%
  mutate(datum_end = if_else(is.na(datum_end),ymd("2024-05-01"),datum_end))-> date

for(i in 1:nrow(date)){
  
  output <- try({
    
    get_work(date_range_from=date$datum_start[[i]]
             ,date_range_to=date$datum_end[[i]]
             ,type="document"
             ,fact="written_questions"
             ,plen_comm="plen"
             ,use_parallel = TRUE,
             two_columns_pdf = FALSE)
  })
  
  if(any(stringr::str_detect(tolower(output[1]),"no sessions found"))){
    
    next()
    
  }  
  
  if(any(class(output) == "try-error")){
    
    message("error, will sleep for 1 minute")
    Sys.sleep(60)
    message("slept for 1 minute, let's retry")
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="document"
               ,fact="written_questions"
               ,plen_comm="plen"
               ,use_parallel = TRUE,
               two_columns_pdf = FALSE)
    })}
  if(any(class(output) == "try-error")){
    
    message("another error, will sleep for 2 minutes")
    Sys.sleep(2*60)
    message("slept for 2 minutes, let's retry")
    
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="document"
               ,fact="written_questions"
               ,plen_comm="plen"
               ,use_parallel = TRUE,
               two_columns_pdf = FALSE)
    })}
  if(any(class(output) == "try-error")){
    
    message("another error, powernap of 15 minutes")
    Sys.sleep(15*60)
    message("slept for 15 minutes, let's try one more time")
    
    try({
      get_work(date_range_from=date$datum_start[[i]]
               ,date_range_to=date$datum_end[[i]]
               ,type="document"
               ,fact="written_questions"
               ,plen_comm="plen")
    })}
  if(any(class(output) == "try-error")){
    
    stop()
    message("function stopped")
    
  }
  
  saveRDS(output, paste0(path_doc,date$datum_start[[i]],".rds")) 
  
  message(i,"/",nrow(date))
  
}


### MP information ####

# I collect data on the current MPs 
# fact="bio" indicates that 
current <- get_mp(selection="current"
                  , fact="raw")
saveRDS(current, file = "currentMP.RData")

currentbio <- get_mp(selection="current"
                  , fact="bio")
saveRDS(currentbio, file = "currentMP_bio.RData")

# I collect data on the former MPs 
former <- get_mp(selection="former"
                 ,fact="raw")
saveRDS(former, file = "formerMP.RData")

formerbio <- get_mp(selection="former"
                 ,fact="bio")
saveRDS(formerbio, file = "formerMP_bio.RData")

## CREATE DATASET ####

### combine all data ####

# written question documents
written_questions <- list.files(path = path_doc,pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)

# written question details
path <- fs::dir_ls(path_details)  	
list <- vector(mode="list",length= length(path))
for(i in seq_along(path)){
  readRDS(path[[i]]) %>% 
    select(id_fact, title, onderwerp, result_thema_1,
           zittingsjaar, naam_vragensteller, voornaam_vragensteller,
           id_vragensteller, bevraagde_minister_naam, bevraagde_minister_voornaam,
           bevraagde_minister_id, result_procedureverloop, soort_antwoord, tijdig) -> list[[i]]
}

details <- rbindlist(list)
details <- as.data.frame(details)

# combine documents and details
details %>%
  left_join(written_questions, by = "id_fact", relationship="many-to-many") -> full_dataset
#Row 145 of `x` matches multiple rows in `y`.
#Row 8434 of `y` matches multiple rows in `x`.

# exclude list-type column (result_procedureverloop.x)
full_dataset_subset <- full_dataset[, !sapply(full_dataset, is.list)]

# save as CSV
write.csv(full_dataset_subset, file = "dataset_unfiltered.csv", row.names = FALSE)

### search term filtering ####

# load the full dataset (unfiltered)
dataset_unfiltered <- read.csv("dataset_unfiltered.csv")
head(dataset_unfiltered)


# I create 3 possible lists of LGBT-related search terms to filter the data
# these lists range from more broad to more specific terms 
# I check whether the filtered subsets of data are very different
# in terms of size and LGBT-related content 

list_broad = c("homo's",
               "homoseksueel",
               "homosexual",
               "homoseksuele",
               "homoseksualiteit",
               "antihomoseksueel",
               "homokoppel",
               "homohuwelijk",
               "homofobie",
               "homofoob",
               "homofobe",
               "homohaat",
               "seksuele geaardheid",
               "seksuele identiteit",
               "seksuele minderheid",
               "seksuele minderheden",
               "seksuele en genderminderheid",
               "homonegativiteit",
               "holebi",
               "holebiseksueel",
               "holebiseksuele",
               "holebigemeenschap",
               "heteroseksualiteit",
               "heteroseksisme",
               "heterokoppel",
               "lesbisch",
               "lesbian",
               "lesbienne",
               "biseksueel",
               "bisexual",
               "biseksualiteit",
               "gay",
               "queer",
               "LGBT")
               
list_intermediate = c(list_broad, 
                      "transgender",
                      "transgenderisme",
                      "trans personen",
                      "transgenders",
                      "transseksualiteit",
                      "transfobie",
                      "transfoob",
                      "transfobe",
                      "holebitrans",
                      "intersekse", 
                      "interseksualiteit",
                      "geslachtsverandering",
                      "geslachtstransformatie",
                      "gendertransitie",
                      "gendertransformatie",
                      "LHBT",
                      "GLBT",
                      "GLBQ",
                      "GLBTQ",
                      "LGB",
                      "LGBTQ",
                      "LGBTI",
                      "LGBTIQ",
                      "LGBTQI",
                      "LGBTQIA",
                      "heteroflexibel",
                      "transvrouw",
                      "transman",
                      "trans man",
                      "trans vrouw",
                      "trans mannen",
                      "trans vrouwen",
                      "niet-binaire persoon",
                      "niet-binaire personen",
                      "niet-binaire mensen")

  
list_specific = c(list_intermediate, 
                  "cisgender", 
                  "holebifederatie", 
                  "çavaria", 
                  "Roze Zaterdag",
                  "heteronormativiteit",
                  "gendernonconformiteit",
                  "genderconformiteit", 
                  "genderdysforie",
                  "homosueel", 
                  "regenboog vlag",
                  "Antwerp Pride",
                  "Brussels Pride",
                  "Ghent Pride"
)


# function with tryCatch for filtering
filter_dataset <- function(dataset_unfiltered, search_terms) {
  filtered_dataset <- dataset_unfiltered %>%
    filter(str_detect(text, paste(search_terms, collapse = "|")))
  
  return(filtered_dataset)
}

# filter your dataset with different lists of search terms
data_broad <- filter_dataset(dataset_unfiltered, list_broad)
data_intermediate <- filter_dataset(dataset_unfiltered, list_intermediate)
data_specific <- filter_dataset(dataset_unfiltered, list_specific)

write.csv(data_specific, file = "dataset specific.csv", row.names = FALSE)
write.csv(data_intermediate, file = "dataset intermediate.csv", row.names = FALSE)
write.csv(data_broad, file = "dataset broad.csv", row.names = FALSE)

### final dataset ####

# inspect duplicates based on text variable
duplicates_broad <- data_broad[duplicated(data_broad$text) | duplicated(data_broad$text, fromLast = TRUE), ]
duplicates_broad <- unique(duplicates_broad) #keeping only 1 instance of the duplicates

duplicates_inter <- data_intermediate[duplicated(data_intermediate$text) | duplicated(data_intermediate$text, fromLast = TRUE), ]
duplicates_inter <- unique(duplicates_inter) #keeping only 1 instance of the duplicates

duplicates_spec <- data_specific[duplicated(data_specific$text) | duplicated(data_specific$text, fromLast = TRUE), ]
duplicates_spec <- unique(duplicates_spec) #keeping only 1 instance of the duplicates

cat("There are", nrow(data_broad), "written questions in the broad dataset, of which", nrow(duplicates_broad), "are unique duplicates.\n")
cat("There are", nrow(data_intermediate), "written questions in the intermediate dataset, of which", nrow(duplicates_inter), "are unique duplicates.\n")
cat("There are", nrow(data_specific), "written questions in the specific dataset, of which", nrow(duplicates_spec), "are unique duplicates.\n")

# inspect different instances in the dataframes
instances_in_intermediate_not_in_broad <- setdiff(data_intermediate$text, data_broad$text)
instances_in_specific_not_in_intermediate <- setdiff(data_specific$text, data_intermediate$text)

instances_unique_to_intermediate <- data_intermediate[data_intermediate$text %in% instances_in_intermediate_not_in_broad, ]
instances_unique_to_specific <- data_specific[data_specific$text %in% instances_in_specific_not_in_intermediate, ]


print(instances_unique_to_intermediate[instances_unique_to_intermediate$id_fact == "884602", ])


# filter out instances containing the phrase "antwoord op vraag"
filtered_df <- data_specific %>% 
  filter(!grepl("antwoord op vraag", text) 
         & !grepl("antwoord  op vraag", text)
         & !grepl("antwoord ,  op vraag", text)
         & !grepl("antwoord , op vraag", text)
         & !grepl("antwoord  ,  op vraag", text))

nrow(filtered_df)

# filtering duplicates based on exact same "text" value
duplicates <- filtered_df[duplicated(filtered_df$text) | duplicated(filtered_df$text, fromLast = TRUE), ]
duplicates <- unique(duplicates) #keeping only 1 instance of the duplicates

# removing one instance of the duplicates 
final_df <- distinct(filtered_df, text)
nrow(final_df)
# save as CSV
write.csv(final_df, file = "final dataset.csv", row.names = FALSE)


## DEBUGGING ####

# this is one of the days where a file is giving a JSON error
date_range_from <- "2009-02-19" 
date_range_to <- "2009-02-19"

list <- flempar:::use_generalized_query(date_range_from = date_range_from, date_range_to = date_range_to)

list %>%
  dplyr::mutate(id_fact = stringr::str_extract(id_fact, "[0-9]+")) %>%
  dplyr::select(-document) %>%
  dplyr::distinct() -> list

# call_api_multiple_times disregards the entire day (13 interrogations)
result <- flempar:::call_api_multiple_times(
  iterator = list$id_fact,
  URL = list$url,
  path = NULL,
  query = list(),
  resultVector = NULL,
  use_parallel = TRUE
)

# however the only interrogation with a missing json is the last one (line 13)
# see this: 
# interrogation 12 is correctly retrieved 

flempar:::call_api_once(
  URL =  list$url[12],
  path = NULL,
  query =  list(),
  list$id_fact[[12]]
)

# interrogation 13 is not retrieved due to the missing JSON 
flempar:::call_api_once(
  URL =  list$url[13],
  path = NULL,
  query =  list(),
  list$id_fact[[13]]
)

# all the other info contained in list for interrogation 13 are also lost
list[13,]

# the issue appears to be the lack of any data from the open data link from the flemish parliament. You can copy and past the link in the browser for these two different interrogations in the browser to see where the issue lies. 

list[12,"opendata"]
list[13,"opendata"]

# trying to record the files that give the error 
# empty lists to store the results
combined_list <- list()
current_list <- list()

# loop through each year from 2000 to 2024
for (year in 2000:2023) {
  for (trimester_start_month in c(1, 4, 7, 10)) {
    # Define the start and end dates for each trimester
    date_range_from <- sprintf("%04d-%02d-01", year, trimester_start_month)
    date_range_to <- sprintf("%04d-%02d-01", year + ifelse(trimester_start_month == 10, 1, 0), 
                             ifelse(trimester_start_month == 10, 1, trimester_start_month + 2))
    
    # Create a list for the current trimester and append its elements to the combined list
    current_list <- flempar:::use_generalized_query(date_range_from = date_range_from, date_range_to = date_range_to)
    combined_list <- c(combined_list, list(current_list))
  }
}


combined_df <- rbindlist(combined_list)

combined_df %>%
  dplyr::mutate(id_fact = stringr::str_extract(id_fact, "[0-9]+")) %>%
  dplyr::select(-document) %>%
  dplyr::distinct() -> combined_df

# record the files with errors

# initialize empty vector to store the id_fact values with errors
error_id_facts <- vector("list", nrow(combined_df))

# loop through each row in the dataframe
for (i in seq_len(nrow(combined_df))) {
  # extract URL and id_fact for the current row
  current_url <- combined_df$url[i]
  current_id_fact <- combined_df$id_fact[i]
  
  tryCatch({
    # call the flempar:::call_api_once function for the current instance
    flempar:::call_api_once(
      URL = current_url,
      path = NULL,
      query = list(),
      id_fact = current_id_fact
    )
  }, error = function(e) {
    # if error occurs, record id_fact in the error_id_facts list
    error_id_facts[i] <- current_id_fact
    # print the error message
    cat("Error occurred for id_fact:", current_id_fact, "\n")
  })
}

# something went wrong with recording the id_facts,
# but these should be the files with errors according to the console:

# No encoding supplied: defaulting to UTF-8.
# Error occurred for id_fact: 548065 
# No encoding supplied: defaulting to UTF-8.
# Error occurred for id_fact: 548064 
# Error occurred for id_fact: 610435 
# Error occurred for id_fact: 680202 
# No encoding supplied: defaulting to UTF-8.
# Error occurred for id_fact: 964043 
# Error occurred for id_fact: 1273388 
# Error occurred for id_fact: 1548677 

# id_facts of the files that gave an error
id_facts_errors <- c(548065, 548064, 610435, 680202, 964043, 1273388, 1548677)

# filter the combined_df dataframe for rows where id_fact is in the id_facts_errors vector
error_files_df <- combined_df[combined_df$id_fact %in% id_facts_errors, ]

# extract the URLs & dates for the files that gave error
error_url <- error_files_df$opendata
error_dates <- error_files_df$publicatiedatum

print(error_url)
print(error_dates)

error_files_df

# checking if details can be retrieved for time periods inbetween the error files
# these should all work!!
wqd2<- get_work(date_range_from="2009-01-01",
                date_range_to="2009-02-18",
                type="details",
                fact="written_questions",
                plen_comm="plen")

wqd2<- get_work(date_range_from="2009-02-20",
                date_range_to="2009-02-22",
                type="details",
                fact="written_questions",
                plen_comm="plen")

wqd2<- get_work(date_range_from="2009-02-24",
                date_range_to="2009-12-08",
                type="details",
                fact="written_questions",
                plen_comm="plen")

wqd3<- get_work(date_range_from="2010-06-01",
                date_range_to="2010-12-08",
                type="details",
                fact="written_questions",
                plen_comm="plen")

wqd4 <- get_work(date_range_from="2012-01-01",
                 date_range_to="2012-07-12",
                 type="details",
                 fact="written_questions",
                 plen_comm="plen")

wqd5 <- get_work(date_range_from="2012-07-14",
                 date_range_to="2012-12-31",
                 type="details",
                 fact="written_questions",
                 plen_comm="plen")

wqd6 <- get_work(date_range_from="2015-01-01",
                 date_range_to="2015-04-27",
                 type="details",
                 fact="written_questions",
                 plen_comm="plen")

wqd7 <- get_work(date_range_from="2015-04-29",
                 date_range_to="2015-09-01",
                 type="details",
                 fact="written_questions",
                 plen_comm="plen")

wqd8 <- get_work(date_range_from="2018-11-01",
                 date_range_to="2018-11-13",
                 type="details",
                 fact="written_questions",
                 plen_comm="plen")

wqd9 <- get_work(date_range_from="2018-11-15",
                 date_range_to="2018-11-30",
                 type="details",
                 fact="written_questions",
                 plen_comm="plen")

wqd10 <- get_work(date_range_from="2021-10-01",
                  date_range_to="2021-10-24",
                  type="details",
                  fact="written_questions",
                  plen_comm="plen")

wqd11 <- get_work(date_range_from="2021-10-26",
                  date_range_to="2021-11-26",
                  type="details",
                  fact="written_questions",
                  plen_comm="plen")

# testing the error file dates -> these should all give an error
# update: only 3 of them give the error, 
# so only 3 dates that give an error when trying the get_work function 
wqd_error <- get_work(date_range_from="2012-07-13",
                      date_range_to="2012-07-13",
                      type="details",
                      fact="written_questions",
                      plen_comm="plen")

wqd_error <- get_work(date_range_from="2009-02-23",
                      date_range_to="2009-02-23",
                      type="details",
                      fact="written_questions",
                      plen_comm="plen")

wqd_error <- get_work(date_range_from="2009-02-19",
                      date_range_to="2009-02-19",
                      type="details",
                      fact="written_questions",
                      plen_comm="plen")

wqd_error <- get_work(date_range_from="2010-12-09",
                      date_range_to="2010-12-09",
                      type="details",
                      fact="written_questions",
                      plen_comm="plen")

wqd_error <- get_work(date_range_from="2015-04-28",
                      date_range_to="2015-04-28",
                      type="details",
                      fact="written_questions",
                      plen_comm="plen")

wqd_error <- get_work(date_range_from="2018-11-14",
                      date_range_to="2018-11-14",
                      type="details",
                      fact="written_questions",
                      plen_comm="plen")

wqd_error <- get_work(date_range_from="2021-10-25",
                      date_range_to="2021-10-25",
                      type="details",
                      fact="written_questions",
                      plen_comm="plen")

#only 3 dates contain a file with an error, so those 3 days are disregarded


