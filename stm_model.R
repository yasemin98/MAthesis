# Structural Topic Model with stm ####

## set working directory ####
getwd()
setwd("C:/Users/yasem/MAthesis/")

## packages ####
#install.packages(c("ggplot2", "Rcpp", "RcppArmadillo", "geometry", "gsl"))
#install.packages("stm")
library(stm)
library(readr)
library(dplyr)

## read the data ####
data <- read_csv("df_final2.csv")

## split the data into text and metadata ####
documents <- data %>% select(id_fact, 
                             text)

metadata <- data %>% select(id_fact, 
                            zittingsperiode, 
                            geslacht,
                            age_category,
                            galtan_equal_width_bins, 
                            gov_opp)


## preprocessing the data for STM ####
custom_stopwords <- c("schriftelijke", "vraag", 
                      "minister", "vlaams", "parlement",
                      "volksvertegenwoordiger", 'Vlaams',
                      "regering", "Vlaanderen", "vlaanderen")

custom_punctuation <- c("ΓÇô", "├⌐├⌐", "├⌐") # symbols found in text that need to be removed

processed_documents <- textProcessor(documents = documents$text, 
                                     metadata = metadata,
                                     lowercase = TRUE,
                                     removestopwords = TRUE,
                                     removenumbers = TRUE,
                                     removepunctuation = TRUE,
                                     ucp = FALSE,
                                     stem = TRUE,
                                     wordLengths = c(3, Inf),# remove words with 2 or less characters
                                     sparselevel = 1, # not removing sparse words as these can be meaningful
                                     language = "dutch",
                                     verbose = TRUE, # prints information as it processes
                                     onlycharacter = TRUE, 
                                     striphtml = TRUE, # remove HTML tags
                                     customstopwords = custom_stopwords,
                                     custompunctuation = custom_punctuation, 
                                     v1 = FALSE)

## prepare documents for STM ####
out <- prepDocuments(processed_documents$documents, processed_documents$vocab, processed_documents$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# STM model including metadata 
model1 <- stm(documents = docs, vocab = vocab, K = 12, 
             prevalence =~ zittingsperiode + galtan_equal_width_bins, 
             data = meta, max.em.its = 75)

summary(model1)
plot(model1)

# effect of zittingsperiode on topic prevalence
prep_zittingsperiode <- estimateEffect(1:12 ~ zittingsperiode, model1, meta = meta, uncertainty = "Global")
summary(prep_zittingsperiode)

plot(prep_zittingsperiode, "zittingsperiode", method = "difference", 
     xlab = "Difference in Topic Proportions", 
     main = "Effect of Parliament Term on Topics")

# effect of GALTAN on topic prevalence
prep_galtan <- estimateEffect(1:12 ~ galtan_equal_width_bins, model1, meta = meta, uncertainty = "Global")
summary(prep_galtan)

plot(prep_galtan, "galtan_equal_width_bins", method = "difference", 
     cov.value1 = "GAL", cov.value2 = "TAN", 
     xlab = "Difference in Topic Proportions", 
     main = "Effect of Galtan on Topics")



