# DATA COLLECTION with Flempar ####

## references ####

# 1. Willems, E., & Heylen, F. (2023). flempar: An R-package for analyzing data
# from the Flemish Parliament. https://doi.org/10.31235/osf.io/7qwvt

# 2. OpenAI. (2023). GPT-4 Language Model. https://openai.com/research/gpt-4


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
library(readr)
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
# I collect parliamentary data from 1995-2024
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
current <- get_mp(selection="current"
                  , fact="raw", use_parallel = TRUE)
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
mp_bio <- get_mp(selection="current",fact="bio",use_parallel=TRUE)


## CREATE DATASET ####

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

## CLEAN COLUMNS ####
colnames(full_dataset_subset)
str(full_dataset_subset)
summary(full_dataset_subset)

#identify the empty columns -> none
all_na_columns <- apply(full_dataset_subset, 2, function(x) all(is.na(x)))
all_empty_columns <- apply(full_dataset_subset, 2, function(x) all(x == ""))
all_na_or_empty_columns <- all_na_columns | all_empty_columns
na_or_empty_colnames <- names(full_dataset_subset)[all_na_or_empty_columns]
print(na_or_empty_colnames)

#title column

na_count_title_x <- sum(is.na(full_dataset_subset$title.x))
na_count_title_y <- sum(is.na(full_dataset_subset$title.y))

fulldataset <- full_dataset_subset %>%
  mutate(title = case_when(
    !is.na(title.x) & !is.na(title.y) & title.x == title.y ~ title.x,   # Both titles are equal
    !is.na(title.x) & is.na(title.y) ~ title.x,                       # title.x has value and title.y is NA
    is.na(title.x) & !is.na(title.y) ~ title.y,                       # title.y has value and title.x is NA
    !is.na(title.x) & !is.na(title.y) & nchar(title.x) >= nchar(title.y) ~ title.x,  # Both have values, choose the longest
    !is.na(title.x) & !is.na(title.y) & nchar(title.x) < nchar(title.y) ~ title.y,   # Both have values, choose the longest
    TRUE ~ title.x    # Default case (shouldn't occur based on logic, but as a safeguard)
  ))

na_count_title <- sum(is.na(fulldataset$title))
na_count_title

fulldataset <- fulldataset %>%
  select(-title.x, -title.y)

# onderwerp column
na_count_onderwerp_x <- sum(is.na(fulldataset$onderwerp.x))
na_count_onderwerp_y <- sum(is.na(fulldataset$onderwerp.y))

fulldataset <- fulldataset %>%
  mutate(onderwerp = case_when(
    !is.na(onderwerp.x) & !is.na(onderwerp.y) & onderwerp.x == onderwerp.y ~ onderwerp.x,       # Both values are equal
    !is.na(onderwerp.x) & is.na(onderwerp.y) ~ onderwerp.x,                                      # onderwerp.x has value and onderwerp.y is NA
    is.na(onderwerp.x) & !is.na(onderwerp.y) ~ onderwerp.y,                                      # onderwerp.y has value and onderwerp.x is NA
    !is.na(onderwerp.x) & !is.na(onderwerp.y) & nchar(onderwerp.x) >= nchar(onderwerp.y) ~ onderwerp.x,  # Both have values, choose the longest
    !is.na(onderwerp.x) & !is.na(onderwerp.y) & nchar(onderwerp.x) < nchar(onderwerp.y) ~ onderwerp.y,   # Both have values, choose the longest
    TRUE ~ onderwerp.x    # Default case (shouldn't occur based on logic, but as a safeguard)
  ))

fulldataset <- fulldataset %>%
  select(-onderwerp.x, -onderwerp.y)

na_count_onderwerp <- sum(is.na(fulldataset$onderwerp))

same_value_count <- fulldataset %>% #basically, onderwerp has the same value as title
  filter(title == onderwerp) %>%
  nrow()

na_onderwerp_rows <- fulldataset %>%
  filter(is.na(onderwerp))

fulldataset <- fulldataset %>% #delete 'onderwerp' column since it's the same as title
  select(-onderwerp)

# zittingsjaar column
na_count_zittingsjaar_x <- sum(is.na(fulldataset$zittingsjaar.x))
na_count_zittingsjaar_y <- sum(is.na(fulldataset$zittingsjaar.y))

same_value_count <- fulldataset %>% 
  filter(zittingsjaar.x == zittingsjaar.y) %>%
  nrow() #the rest is NA for zittingsjaar.y, so they have the same values, let's keep zittingsjaar.x

fulldataset <- fulldataset %>%
  select(-zittingsjaar.y)

fulldataset <- fulldataset %>% #rename
  rename(zittingsjaar = zittingsjaar.x)

# id_vragensteller column
na_count_id_vragensteller_x <- sum(is.na(fulldataset$id_vragensteller.x))
na_count_id_vragensteller_y <- sum(is.na(fulldataset$id_vragensteller.y))

fulldataset <- fulldataset %>%
  select(-id_vragensteller.y)

fulldataset <- fulldataset %>% #rename
  rename(id_vragensteller = id_vragensteller.x)

# naam_vragensteller column
na_count_naam_vragensteller_x <- sum(is.na(fulldataset$naam_vragensteller.x))
na_count_naam_vragensteller_y <- sum(is.na(fulldataset$naam_vragensteller.y))

fulldataset <- fulldataset %>%
  select(-naam_vragensteller.y)

fulldataset <- fulldataset %>% #rename
  rename(naam_vragensteller = naam_vragensteller.x)

# voornaam_vragensteller column
na_count_voornaam_vragensteller_x <- sum(is.na(fulldataset$voornaam_vragensteller.x))
na_count_voornaam_vragensteller_y <- sum(is.na(fulldataset$voornaam_vragensteller.y))

fulldataset <- fulldataset %>%
  select(-voornaam_vragensteller.y)

fulldataset <- fulldataset %>% #rename
  rename(voornaam_vragensteller = voornaam_vragensteller.x)

# result_thema_1 column
na_count_result_thema_1_x <- sum(is.na(fulldataset$result_thema_1.x))
na_count_result_thema_1_y <- sum(is.na(fulldataset$result_thema_1.y))

fulldataset <- fulldataset %>%
  mutate(thema_1 = case_when(
    !is.na(result_thema_1.x) & !is.na(result_thema_1.y) & result_thema_1.x == result_thema_1.y ~ result_thema_1.x,       # Both values are equal
    !is.na(result_thema_1.x) & is.na(result_thema_1.y) ~ result_thema_1.x,                                      # onderwerp.x has value and onderwerp.y is NA
    is.na(result_thema_1.x) & !is.na(result_thema_1.y) ~ result_thema_1.y,                                      # onderwerp.y has value and onderwerp.x is NA
    !is.na(result_thema_1.x) & !is.na(result_thema_1.y) & nchar(result_thema_1.x) >= nchar(result_thema_1.y) ~ result_thema_1.x,  # Both have values, choose the longest
    !is.na(result_thema_1.x) & !is.na(result_thema_1.y) & nchar(result_thema_1.x) < nchar(result_thema_1.y) ~ result_thema_1.y,   # Both have values, choose the longest
    TRUE ~ result_thema_1.x    # Default case (shouldn't occur based on logic, but as a safeguard)
  ))

na_count_thema_1 <- sum(is.na(fulldataset$thema_1))
na_count_thema_1

fulldataset <- fulldataset %>%
  select(-result_thema_1.x, -result_thema_1.y)

na_count_result_thema_2 <- sum(is.na(fulldataset$result_thema_2))
na_count_result_thema_3 <- sum(is.na(fulldataset$result_thema_3))
na_count_result_thema_4 <- sum(is.na(fulldataset$result_thema_4))
na_count_result_thema_5 <- sum(is.na(fulldataset$result_thema_5))
na_count_result_thema_6 <- sum(is.na(fulldataset$result_thema_6))

fulldataset <- fulldataset %>% #check for a non-missing value in other thema columns until replaced
  mutate(thema_1 = coalesce(thema_1, result_thema_2, result_thema_3, result_thema_4, result_thema_5, result_thema_6))

na_count_thema_1 <- sum(is.na(fulldataset$thema_1))
na_count_thema_1 #same amount of missing values...

fulldataset <- fulldataset %>%
  rename(
    thema_2 = result_thema_2,
    thema_3 = result_thema_3,
    thema_4 = result_thema_4,
    thema_5 = result_thema_5,
    thema_6 = result_thema_6
  )

# bevraagde_minister_naam column
na_count_bevraagde_minister_naam_x <- sum(is.na(fulldataset$bevraagde_minister_naam.x))
na_count_bevraagde_minister_naam_y <- sum(is.na(fulldataset$bevraagde_minister_naam.y))

fulldataset <- fulldataset %>%
  select(-bevraagde_minister_naam.y)

fulldataset <- fulldataset %>% #rename
  rename(bevraagde_minister_naam = bevraagde_minister_naam.x)

# bevraagde_minister_voornaam column
na_count_bevraagde_minister_voornaam_x <- sum(is.na(fulldataset$bevraagde_minister_voornaam.x))
na_count_bevraagde_minister_voornaam_y <- sum(is.na(fulldataset$bevraagde_minister_voornaam.y))

fulldataset <- fulldataset %>%
  select(-bevraagde_minister_voornaam.y)

fulldataset <- fulldataset %>% #rename
  rename(bevraagde_minister_voornaam = bevraagde_minister_voornaam.x)

# bevraagde_minister_id column
na_count_bevraagde_minister_id_x <- sum(is.na(fulldataset$bevraagde_minister_id.x))
na_count_bevraagde_minister_id_y <- sum(is.na(fulldataset$bevraagde_minister_id.y))

fulldataset <- fulldataset %>%
  select(-bevraagde_minister_id.y)

fulldataset <- fulldataset %>% #rename
  rename(bevraagde_minister_id = bevraagde_minister_id.x)

# delete soort_antwoord and tijdig columns -> not necessary for this study
fulldataset <- fulldataset %>%
select(-soort_antwoord.y, -soort_antwoord.x, -tijdig.x, -tijdig.y)

# text column
na_count_text <- sum(is.na(fulldataset$text)) #delete these rows because useless

fulldataset <- fulldataset %>%
  filter(!is.na(text))

#save as csv
write.csv(fulldataset, file = "fulldataset_cleanedcolumns.csv", row.names = FALSE)

## MP DATA ####

former <- readr::read_rds("formerMP.RData")
current <- readr::read_rds("currentMP.RData")
former_bio <- readr::read_rds("formerMP_bio.RData")
current_bio <- readr::read_rds("currentMP_bio.RData")

#I will only use the former_bio and current_bio since they contain the relevant information
colnames(former_bio)

former_bio <- former_bio %>%
  select(-domicillieadres_deelgemeente, -domicillieadres_postcode,
         -domicillieadres_nr, -domicillieadres_straat, -domicillieadres_telnr,
         -gsmnr, -email_1, -website, -geboorteplaats)

colnames(current_bio)

current_bio <- current_bio %>%
  select(-domicillieadres_deelgemeente, -domicillieadres_postcode,
         -domicillieadres_nr, -domicillieadres_straat, -domicillieadres_telnr,
         -gsmnr, -email_1, -Website, -Instagram, -Facebook, -Twitter,
         -email_2, -email_3, -geboorteplaats)

unique(current_bio$party_naam)

write.csv(current_bio, file = "currentMP.csv", row.names = FALSE)
write.csv(former_bio, file = "formerMP.csv", row.names = FALSE)


## DATA FILTERING ####

### search term filtering ####

# load the full dataset (unfiltered)
fulldataset <- read.csv("fulldataset_cleanedcolumns.csv")
head(fulldataset)


# I create 3 possible lists of LGBT-related search terms to filter the data
# these lists range from more broad to more specific terms 
# I check whether the filtered subsets of data are very different
# in terms of size and LGBT-related content 

list_broad = c("\\bhomo\\b",
               "\\bhomo's\\b",
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
               "homonegativiteit",
               "holebi.*",
               "heteroseksualiteit",
               "heteroseksisme",
               "heterokoppel",
               "lesbisch",
               "lesbian",
               "lesbienne",
               "biseksueel",
               "bisexual",
               "biseksualiteit",
               "LGBT",
               "\\bqueer\\b",
               "panseksueel",
               "aseksueel")
               
list_intermediate = c(list_broad, 
                      "transgender.*",
                      "trans personen",
                      "transseksualiteit",
                      "transfobie",
                      "transfoob",
                      "transfobe",
                      "holebitrans",
                      "interseks.*", 
                      "geslachtsverandering",
                      "geslachtstransformatie",
                      "gendertransitie",
                      "gendertransformatie",
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
                      "niet-binaire mensen",
                      "genderfluïd.*")

  
list_specific = c(list_intermediate, 
                  "çavaria", 
                  "Roze Zaterdag",
                  "regenboogvlag",
                  "regenbooggezin.*",
                  "Antwerp Pride",
                  "Brussels Pride",
                  "Ghent Pride"
)


# function with tryCatch for filtering
filter_dataset <- function(fulldataset, search_terms) {
  filtered_dataset <- fulldataset %>%
    filter(str_detect(text, paste(search_terms, collapse = "|")))
  
  return(filtered_dataset)
}

# filter your dataset with different lists of search terms
data_broad <- filter_dataset(fulldataset, list_broad)
data_intermediate <- filter_dataset(fulldataset, list_intermediate)
data_specific <- filter_dataset(fulldataset, list_specific)

write.csv(data_specific, file = "data_specific.csv", row.names = FALSE)
write.csv(data_intermediate, file = "data_intermediate.csv", row.names = FALSE)
write.csv(data_broad, file = "data_broad.csv", row.names = FALSE)

### final dataset ####
data_broad[2, ]
instances_unique_to_specific[8, ] #Antwerp Pride but unrelated to LGBTQI+
instances_unique_to_specific[5, ] #regenboogvlag on Eurovision but unrelated
instances_unique_to_specific[16, ] #çavaria but unrelated
instances_unique_to_specific[17, ] #çavaria but unrelated
instances_unique_to_specific[1, ] #çavaria but unrelated

# I decided to use the data based on the intermediate search term list
# since it contains the most relevant cases 
# more specific terms such as "çavaria' give results that are unrelated to LGBTQI+
# because the question then only contains çavaria but no other terms related to groups in the LGBTQI+ community

# inspect duplicates based on text variable
#duplicates_broad <- data_broad[duplicated(data_broad$text) | duplicated(data_broad$text, fromLast = TRUE), ]
#duplicates_broad <- unique(duplicates_broad) #keeping only 1 instance of the duplicates

duplicates_inter <- data_intermediate[duplicated(data_intermediate$text) | duplicated(data_intermediate$text, fromLast = TRUE), ]
duplicates_inter <- unique(duplicates_inter) #keeping only 1 instance of the duplicates

#duplicates_spec <- data_specific[duplicated(data_specific$text) | duplicated(data_specific$text, fromLast = TRUE), ]
#duplicates_spec <- unique(duplicates_spec) #keeping only 1 instance of the duplicates

#cat("There are", nrow(data_broad), "written questions in the broad dataset, of which", nrow(duplicates_broad), "are unique duplicates.\n")
cat("There are", nrow(data_intermediate), "written questions in the intermediate dataset, of which", nrow(duplicates_inter), "are unique duplicates.\n")
#cat("There are", nrow(data_specific), "written questions in the specific dataset, of which", nrow(duplicates_spec), "are unique duplicates.\n")

# inspect different instances in the dataframes
instances_in_intermediate_not_in_broad <- setdiff(data_intermediate$text, data_broad$text)
instances_in_specific_not_in_intermediate <- setdiff(data_specific$text, data_intermediate$text)

instances_unique_to_intermediate <- data_intermediate[data_intermediate$text %in% instances_in_intermediate_not_in_broad, ]
instances_unique_to_specific <- data_specific[data_specific$text %in% instances_in_specific_not_in_intermediate, ]

# filter out instances which are only replies to a written question
filtered_df <- data_intermediate %>% 
  filter(!grepl("antwoord op vraag", text) 
         & !grepl("antwoord  op vraag", text)
         & !grepl("antwoord ,  op vraag", text)
         & !grepl("antwoord , op vraag", text)
         & !grepl("antwoord  ,  op vraag", text))

nrow(filtered_df)

# identifying duplicates based on exact same "text" value
duplicates <- filtered_df[duplicated(filtered_df$text) | duplicated(filtered_df$text, fromLast = TRUE), ]
duplicates <- unique(duplicates) #keeping only 1 instance of the duplicates

# removing one instance of the duplicates 
final_df <- filtered_df %>%
  distinct(text, .keep_all = TRUE)

# save as CSV
write.csv(final_df, file = "final_dataset.csv", row.names = FALSE)

### combine final dataset with MP data ####

# use datafiles: currentMP.csv, formerMP.csv, final_dataset.csv
final_dataset <- read.csv("final_dataset.csv")
currentMP <- read.csv("currentMP.csv")
formerMP <- read.csv("formerMP.csv")

# create one df for former and current MPs
combined_df <- bind_rows(formerMP, currentMP)

# remove duplicates based on 'id_mp' variable
df_MP <- combined_df %>%
  distinct(id_mp, .keep_all = TRUE)

# left-join to combine final dataset with MP information 
# based on id_mp and id_vragensteller
questions_MP <- final_dataset %>%
  left_join(df_MP, by = c("id_vragensteller" = "id_mp"))

# check for missing values in party_naam = 0 :) amazing
na_count_party_naam <- sum(is.na(questions_MP$party_naam))

# save complete dataset
write.csv(questions_MP, file = "questions_MP.csv", row.names = FALSE)

# now, I have a complete dataset that contains both the questions and MP data
# for further cleaning and preprocessing, I will use Python

# This is the end of my R script that I used for my thesis


## DEBUGGING I####
#debugging of the collection of written parliamentary questions

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


## DEBUGGING II####

#debugging of the collection of MP information -> this did not work anymore in May-June
try_this <- function (selection = "current", fact = "bio", date_at = NULL, 
                      use_parallel = TRUE) 
{
  facts <- c("raw", "bio", "education", "career", "political_info", 
             "presences_commissions", "presences_plenary")
  if (length(selection) != 1) {
    stop("You have selected multiple selection options. Please set selection to either 'current', 'all' or 'date'.")
  }
  if (length(fact) != 1) {
    stop("You have selected multiple type options. Please set type to ", 
         toString(tolower(facts)), ".")
  }
  if (any(!fact %in% facts)) {
    stop("Not a valid type. Valid options are ", toString(tolower(facts)), 
         ". Select one type.")
  }
  if (selection == "current") {
    if (!is.null(date_at)) {
      message("Selection is set to current, date_at will be ignored.")
      date_at <- NULL
    }
    date_at_conv <- Sys.Date() %>% format("%d%m%Y")
    robj <- call_api_once(URL = "http://ws.vlpar.be:443/e/opendata/", 
                          path = "/vv/op-datum", query = list(datum = date_at_conv))
    mainlist <- call_api_multiple_times(iterator = robj$items$volksvertegenwoordiger$id, 
                                        URL = "http://ws.vlpar.be:443/e/opendata/", path = "/vv", 
                                        query = list(lang = "nl"), resultVector = NULL, 
                                        use_parallel = use_parallel)
  }
  if (selection == "date") {
    if (is.null(date_at)) {
      stop("You have selected date as selection criteria, but failed to provide a date. Please set date_at (yyyy-mm-dd).")
    }
    if (is.na(lubridate::ymd(date_at))) {
      stop("Wrong Date Format, please use yyyy-mm-dd.")
    }
    date_at_conv <- lubridate::ymd(date_at) %>% format("%d%m%Y")
    robj <- call_api_once(URL = "http://ws.vlpar.be:443/e/opendata/", 
                          path = "/vv/op-datum", query = list(datum = date_at_conv))
    mainlist <- call_api_multiple_times(iterator = robj$items$volksvertegenwoordiger$id, 
                                        URL = "http://ws.vlpar.be:443/e/opendata/", path = "/vv", 
                                        query = list(lang = "nl"), resultVector = NULL, 
                                        use_parallel = use_parallel)
  }
  if (selection == "former") {
    if (!is.null(date_at)) {
      message("Selection is set to former, date_at will be ignored")
      date_at <- NULL
    }
    robj <- call_api_once(URL = "http://ws.vlpar.be:443/e/opendata/", 
                          path = "/vv/gewezen", query = list())
    mainlist <- call_api_multiple_times(iterator = robj$items$volksvertegenwoordiger$id, 
                                        URL = "http://ws.vlpar.be:443/e/opendata/", path = "/vv", 
                                        query = list(lang = "nl"), resultVector = NULL, 
                                        use_parallel = use_parallel)
  }
  fact <- tolower(fact)
  if (fact == "raw") {
    result <- mainlist %>% tibble::tibble(vv = .) %>% tidyr::unnest_wider(vv)
    return(result)
  }
  if (fact == "bio") {
    result <- mainlist %>% tibble::tibble(vv = .) %>% tidyr::unnest_wider(vv) %>% 
      guarantee_field(c("domicillieadres")) %>% dplyr::select(id_mp = id, 
                                                              voornaam, achternaam = naam, geslacht, geboortedatum, 
                                                              geboorteplaats, domicillieadres, gsmnr, email, website, 
                                                              huidigefractie) %>% tidyr::unnest_wider(huidigefractie) %>% 
      dplyr::select(id_mp, voornaam, achternaam, geslacht, 
                    geboortedatum, geboorteplaats, domicillieadres, 
                    gsmnr, email, website, party_id = id, party_naam = naam) %>% 
      dplyr::mutate(geboortedatum = lubridate::date(lubridate::ymd_hms(geboortedatum))) %>% 
      guarantee_field(c("domicillieadres")) %>% tidyr::unnest_wider(domicillieadres, 
                                                                    names_sep = "_") %>% guarantee_field(c("domicillieadres_deelgemeente", 
                                                                                                           "domicillieadres_nr", "domicillieadres_straat", 
                                                                                                           "domicillieadres_telnr")) %>% tidyr::unnest_wider(domicillieadres_deelgemeente, 
                                                                                                                                                             names_sep = "_") %>% guarantee_field(c("domicillieadres_deelgemeente_naam", 
                                                                                                                                                                                                    "domicillieadres_deelgemeente_postnr")) %>% dplyr::select(id_mp, 
                                                                                                                                                                                                                                                              voornaam, achternaam, geslacht, geboortedatum, geboorteplaats, 
                                                                                                                                                                                                                                                              domicillieadres_deelgemeente = domicillieadres_deelgemeente_naam, 
                                                                                                                                                                                                                                                              domicillieadres_postcode = domicillieadres_deelgemeente_postnr, 
                                                                                                                                                                                                                                                              domicillieadres_nr, domicillieadres_straat, domicillieadres_telnr, 
                                                                                                                                                                                                                                                              gsmnr, email, website, party_id, party_naam) %>% 
      tidyr::unnest_wider(email, names_sep = "_") %>% 
      tidyr::unnest(website, names_sep = "_", keep_empty = TRUE) %>% 
      guarantee_field(c("website_soort", "website_value")) %>% 
      tidyr::pivot_wider(names_from = website_soort, values_from = website_value) %>% 
      dplyr::select(-`NA`)
    return(result)
  }
  if (fact == "education") {
    result <- mainlist %>% tibble::tibble(vv = .) %>% tidyr::unnest_wider(vv) %>% 
      guarantee_field(c("opleiding")) %>% dplyr::select(id_mp = id, 
                                                        voornaam, naam, opleiding) %>% tidyr::unnest(opleiding, 
                                                                                                     keep_empty = TRUE)
    return(result)
  }
  if (fact == "career") {
    result <- mainlist %>% tibble::tibble(vv = .) %>% tidyr::unnest_wider(vv) %>% 
      guarantee_field(c("beroep")) %>% dplyr::select(id_mp = id, 
                                                     voornaam, naam, beroep) %>% tidyr::unnest(beroep, 
                                                                                               keep_empty = TRUE) %>% guarantee_field(c("datumvanformaat", 
                                                                                                                                        "datumtotformaat")) %>% dplyr::select(-datumtotformaat, 
                                                                                                                                                                              -datumvanformaat) %>% dplyr::select(id_mp, voornaam, 
                                                                                                                                                                                                                  naam, datumvan, datumtot, titel, werkgever)
    return(result)
  }
  if (fact == "presences_commissions") {
    result <- mainlist %>% tibble::tibble(vv = .) %>% tidyr::unnest_wider(vv) %>% 
      dplyr::select(id_mp = id, voornaam, naam, aanwezigheden = `aanwezigheden-huidige-legislatuur`) %>% 
      tidyr::unnest_longer(aanwezigheden) %>% dplyr::filter(aanwezigheden_id == 
                                                              "commissie-aanw") %>% tidyr::unnest_wider(aanwezigheden) %>% 
      tidyr::unnest(c(commissie, `vast-lid-aanwezigheid`, 
                      `plaatsvervangend-lid-aanwezigheid`), names_sep = "_", 
                    keep_empty = TRUE)
    return(result)
  }
  if (fact == "presences_plenary") {
    result <- mainlist %>% tibble::tibble(vv = .) %>% tidyr::unnest_wider(vv) %>% 
      dplyr::select(id_mp = id, voornaam, naam, aanwezigheden = `aanwezigheden-huidige-legislatuur`) %>% 
      tidyr::unnest_longer(aanwezigheden) %>% dplyr::filter(aanwezigheden_id == 
                                                              "plenaire-aanw") %>% tidyr::unnest_wider(aanwezigheden)
    return(result)
  }
  if (fact == "political_info") {
    result <- mainlist %>% tibble::tibble(vv = .) %>% tidyr::unnest_wider(vv) %>% 
      guarantee_field(c("interesse")) %>% dplyr::select(id_mp = id, 
                                                        voornaam, naam, huidigefractie, mandaat_vl = `mandaat-vlaams-parlement`, 
                                                        mandaat_andere = `mandaat-andere`, kieskring, deelstaatsenator, 
                                                        lidmaatschap, interesse) %>% tidyr::unnest_wider(lidmaatschap, 
                                                                                                         names_sep = "_") %>% tidyr::unnest(c(lidmaatschap_datumVan, 
                                                                                                                                              lidmaatschap_fractie, lidmaatschap_datumTot), names_sep = "_", 
                                                                                                                                            keep_empty = TRUE) %>% dplyr::mutate(lidmaatschap_datumVan = lubridate::date(lidmaatschap_datumVan)) %>% 
      tidyr::unnest_wider(huidigefractie, names_sep = "_") %>% 
      tidyr::unnest_wider(interesse, names_sep = "_") %>% 
      guarantee_field(c("interesse_volgorde", "interesse_interesse")) %>% 
      dplyr::select(-interesse_volgorde, -lidmaatschap_datumTot) %>% 
      tidyr::unnest_wider(interesse_interesse, names_sep = "_") %>% 
      dplyr::select(-huidigefractie_kleur, -huidigefractie_logo, 
                    -lidmaatschap_fractie_id, -lidmaatschap_fractie_kleur, 
                    -lidmaatschap_fractie_logo, -lidmaatschap_fractie_naam, 
                    -`lidmaatschap_fractie_zetel-aantal`) %>% dplyr::rename(party_id = huidigefractie_id, 
                                                                            party_name = huidigefractie_naam, party_seats = `huidigefractie_zetel-aantal`)
    return(result)
  }
}

call_api_once <- function(URL, path = NULL, query, ...) {
  if (is.null(path)) {
    response <- httr::GET(file.path(URL, ...), query = query, httr::accept_json())
  } else {
    response <- httr::GET(file.path(URL, path, ...), query = query, httr::accept_json())
  }
  if (is.null(path)) {
    response <-
      httr::GET(file.path(URL, ...), query = query, httr::accept_json())
  } else {
    response <-
      httr::GET(file.path(URL, path, ...), query = query, httr::accept_json())
  }
  if (any(as.numeric(httr::status_code(response)) %in% 200:204)) {
  } else {
    stop("Error: ", httr::http_status(response)[[1]])
  }
  
  content <- httr::content(response, "text")
  
  if (!identical(httr::headers(response)$`content-type`, "application/json;charset=UTF-8")) {
    stop("File returned is not in JSON format.")
  }
  
  robj <- jsonlite::fromJSON(content)
  return(robj)
}


call_api_multiple_times <- function(iterator, URL, path, query, resultVector, use_parallel = TRUE) {
  message("Making ", length(iterator), " calls.")
  
  if (use_parallel == TRUE) {
    if (parallel::detectCores() == 1) {
      stop("You only have one core, dividing the work over cores is not possible. Please set 'use_parallel=FALSE'. ")
    }
    
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    
    time_used <- system.time({
      list <- foreach::foreach(
        i = seq_along(iterator),
        .packages = c("dplyr", "purrr", "httr", "jsonlite"),
        .export = c("call_api_once"),
        .errorhandling = "stop"
      ) %dopar% {
        if (length(URL) == 1) {
          call_api_once(
            URL[[1]],
            path,
            query,
            iterator[[i]]
          ) -> df
        } else {
          call_api_once(
            URL[[i]],
            path,
            query,
            iterator[[i]]
          ) -> df
        }
        
        if (!is.null(resultVector)) {
          purrr::pluck(df, !!!resultVector) %>% return()
        } else {
          df %>% return()
        }
      } # endparallel
    }) # endtiming
    
    
    message("Made ", length(iterator), " calls in ", round(time_used[[3]], 1), " seconds.")
    
    names(list) <- iterator
  } # end if
  
  if (use_parallel == FALSE) {
    list <- vector(mode = "list", length = length(iterator))
    
    message("Getting the data. Take into account that dividing the tasks between workers by setting 'use_parallel=TRUE' may be much faster.")
    
    time_used <- system.time({
      for (i in seq_along(iterator)) {
        tryCatch(
          {
            if (length(URL) == 1) {
              call_api_once(
                URL,
                path,
                query,
                iterator[[i]]
              ) -> df
            } else {
              call_api_once(
                URL[[i]],
                path,
                query,
                iterator[[i]]
              ) -> df
            }
            
            if (!is.null(resultVector)) {
              purrr::pluck(df, !!!resultVector) %>% return()
            } else {
              df %>% return()
            }
          },
          error = function(e) {
            return(toString(e))
            stop()
          }
        ) -> list[[i]]
      }
    }) # endtiming
    
    message("Made ", length(iterator), " calls in ", round(time_used[[3]], 1), " seconds.")
    
    names(list) <- iterator
  } # end if
  
  return(list)
}

try1 <- try_this(selection="current"
                 , fact="raw", use_parallel = TRUE)

getwd()
source("currentMP.RData")

readRDS("currentMP.RData")

# although R indicated that my files were corrupted
# it seems like the files can be perfectly loaded like this
install.packages("readr")
library(readr)
df_former <- readr::read_rds("formerMP.RData")
df_current <- readr::read_rds("currentMP.RData")
df_former_bio <- readr::read_rds("formerMP_bio.RData")
df_current_bio <- readr::read_rds("currentMP_bio.RData") 

#saving them as CSV files to be sure
write.csv(df_former_bio, file = "formerMP_bio.csv", row.names = FALSE)
write.csv(df_current_bio, file = "currentMP_bio.csv", row.names = FALSE)
