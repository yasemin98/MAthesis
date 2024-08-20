# Structural Topic Modeling ####

## references ####
# 1. https://scholar.harvard.edu/files/dtingley/files/jss-stm.pdf 
# 2. https://github.com/bstewart/stm/issues/250
# 3. https://cran.r-project.org/web/packages/stm/stm.pdf 
# 4. OpenAI. (2023). GPT-4 Language Model. https://openai.com/research/gpt-4 


## set working directory ####
getwd()
setwd("C:/Users/yasem/MAthesis/")

## packages ####
#install.packages(c("ggplot2", "Rcpp", "RcppArmadillo", "geometry", "gsl"))
#install.packages("stm")
library(stm)
library(readr)
library(dplyr)
#install.packages("spacyr")
library(spacyr)
#install.packages("xtable")
library(xtable)
#install.packages("quanteda")
library(quanteda)
library(ggplot2)
library(tidyr)
library(tm)
library(reshape2)
library(gridExtra)
#install.packages("textTinyR")
library(textTinyR)
#install.packages("Rtsne")
#install.packages("rsvd")
#install.packages("geometry")
library(Rtsne)
library(rsvd)
library(geometry)
#install.packages("stmCorrViz")
library(stmCorrViz)
library(devtools)
#devtools::install_github("mroberts/stmBrowser",dependencies=TRUE)
library(stmCorrViz)
# Update htmltools package
#install.packages("htmltools")
library(stmBrowser)

## read the data ####
data <- read_csv("df_final2.csv")

# removed because no text in question column
data <- data %>% 
  filter(!id_fact %in% c(452884, 465067, 462979))

## split the data into text and metadata ####
documents <- data %>% select(id_fact, 
                             question)

metadata <- data %>% select(id_fact, 
                            zittingsperiode,
                            zittingsperiode_numeric,
                            geslacht,
                            age_category,
                            galtan_equal_width_bins, 
                            gov_opp)


## preprocessing the data for STM ####
head(data)

# I eventually didn't use this curated list of stopwords since literature
# suggests that stopword list curation can be harmful to the model
# instead, the top 0.8% most frequent words across documents were removed
# custom_stopwords <- c("schriftelijke", "vraag", "viceminist","viceministerpresid",
#                       "Viceminist", "viceminister", "Viceminister", "president",
#                       "viceministers","vice-minister",
#                       "antwoord","Antwoord", "Vraag",
#                       "Minister", "Schriftelijk", "Schriftelijke",
#                       "schriftelijk", "binnen", "Binnen", "Nieuw", "Nieuwe",
#                       "welk", "Welk", "nieuw", "Rond", "rond",
#                       "minister", "vlaams", "Vlaams", "parlement",
#                       "volksvertegenwoordiger", 'Vlaams',
#                       "regering", "Vlaanderen", "vlaanderen", "vlaamse",
#                       "Vlaamse", "welke", "Welke", "ronde", "Ronde",
#                       "gelijke", "gelijk", "Gelijke", "Kansen","kansen",
#                       "President", "holebi", "datum", "werden", "hoeveel",
#                       "vragen", "graag", "Graag", "Vragen", "Hoeveel", "Werden",
#                       "LGBTQI", "lgbtqi", "bestuur", "onderzoek", "per", "nieuw",
#                       "nieuwe", "beleid", "overzicht", "Overzicht", "Beleid",
#                       "alle", "Alle", "Bart", "Liesbeth", "Homans", "homans", "homan",
#                       "Somer", "Homan", "somer", "somers",
#                       "Somers", "minister", "Minister", "geven", "zaken", "Zaken",
#                       "inzake", "cijfer", "cijfers", "vanuit", "via"
#                       )

# symbols found in text that need to be removed
custom_punctuation <- c("ΓÇô", "├⌐├⌐", "├⌐", "γçô") 

processed_documents <- textProcessor(documents = data$question, 
                                     metadata =  data[, c('id_fact', 'zittingsperiode', 'zittingsperiode_numeric', 'geslacht', 'age_category', 'gov_opp', 'galtan_equal_width_bins')],
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
                                     custompunctuation = custom_punctuation, 
                                     v1 = FALSE)

#prepare documents for STM
out <- prepDocuments(processed_documents$documents, processed_documents$vocab, processed_documents$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

## counts for each category ####

gender_counts <- table(meta$geslacht)
age_counts <- table(meta$age_category)
gov_opp_counts <- table(meta$gov_opp)
galtan_counts <- table(meta$galtan_equal_width_bins)
term_counts <- table(meta$zittingsperiode)

print(gender_counts)
print(age_counts)
print(gov_opp_counts)
print(galtan_counts)

#df with the counts
counts_df <- data.frame(
  Group = c("MP-level covariates", rep("", length(gender_counts) - 1), rep("", length(age_counts) - 1),
            "Party-level covariates", rep("", length(gov_opp_counts) - 1), rep("", length(galtan_counts) - 1)),
  Variable = c("Gender", rep("", length(gender_counts) - 1), 
               "Age", rep("", length(age_counts) - 1), 
               "Gov_Opp", rep("", length(gov_opp_counts) - 1), 
               "GalTan", rep("", length(galtan_counts) - 1)),
  Category = c(names(gender_counts), names(age_counts), names(gov_opp_counts), names(galtan_counts)),
  Count = c(as.vector(gender_counts), as.vector(age_counts), as.vector(gov_opp_counts), as.vector(galtan_counts))
)

print(counts_df)

#LaTeX table
latex_table <- xtable(counts_df)
print(latex_table)

## STM model (top 0.8% words not removed) ####
#(K = number of topics), models include metadata 

### search K ####
storage <- searchK(out$documents, out$vocab, 
                   K = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30), 
                   prevalence =~ zittingsperiode +
                   galtan_equal_width_bins + gov_opp +
                   age_category + geslacht, 
                   data = meta,
                   init.type="Spectral")
plot(storage)
plot.searchK(storage) #only shows certain diagnostics
?searchK

#new function so that we also get the exclusivity plot
plt_full_res_searchK<-function(x, ...){
  oldpar <- par(no.readonly=TRUE)
  g <- x$results
  par(mfrow=c(3,2),mar=c(4,4,2,2),oma=c(1,1,1,1))
  
  plot(g$K,g$heldout,type="p", main="Held-Out Likelihood", xlab="", ylab="")
  lines(g$K,g$heldout,lty=1,col=1)
  
  plot(g$K,g$residual,type="p", main="Residuals", xlab="", ylab="")
  lines(g$K,g$residual,lty=1,col=1 )
  
  if(!is.null(g$semcoh)){
    plot(g$K,g$semcoh,type="p", main="Semantic Coherence", xlab="", ylab="")
    lines(g$K,g$semcoh,lty=1,col=1 ) 
  }
  
  plot(g$K,g$exclus,type="p", main="Exclusivity", xlab="", ylab="")
  lines(g$K,g$exclus,lty=1,col=1 )  
  
  plot(g$K,g$bound,type="p", main="Bound", xlab="Number of Topics (K)", ylab="")
  lines(g$K,g$bound,lty=1,col=1 ) 
  
  plot(g$K,g$lbound,type="p", main="Lower Bound", xlab="Number of Topics (K)", ylab="")
  lines(g$K,g$lbound,lty=1,col=1 ) 
  
  title("Diagnostic Values by Number of Topics", outer=TRUE)  
  par(oldpar)
}

plt_full_res_searchK(storage) #shows all plots

?exclusivity

### fit models ####
stm_10 <- stm(documents = docs, vocab = vocab, K = 10, 
              prevalence =~ zittingsperiode +
                galtan_equal_width_bins +
                gov_opp +
                geslacht +
                age_category,  
              data = meta, 
              max.em.its = 500, 
              init.type="Spectral"
)

stm_11 <- stm(documents = docs, vocab = vocab, K = 11, 
              prevalence =~ zittingsperiode +
                galtan_equal_width_bins +
                gov_opp +
                geslacht +
                age_category,  
             data = meta, 
             max.em.its = 500,
             init.type="Spectral"
             )


summary(stm_10)
summary(stm_11)

plot(stm_11)
plot(stm_10, type="summary")
plot(stm_11, type="summary")

#two plots together
par(mfrow = c(1, 2))
par(mfrow = c(1, 1))


sageLabels(stm_10, n=15) #collection of words associated with each topic
labelTopics(stm_10)
sageLabels(stm_11, n=15) #collection of words associated with each topic
labelTopics(stm_11)
?sageLabels

#inspecting most frequent words in the vocabulary
word_freq <- numeric(length(out$vocab))
for (doc in out$documents) {
  word_freq[doc[1,]] <- word_freq[doc[1,]] + doc[2,]
}
word_freq_df <- data.frame(word = out$vocab, freq = word_freq)
word_freq_df <- word_freq_df[order(-word_freq_df$freq), ]
head(word_freq_df, 100) #100 most frequent words

#inspecting most frequent words based on document frequency for each word
num_words <- length(out$vocab)
doc_freq <- integer(num_words)
for (doc in out$documents) {
  word_indices <- doc[1, ]
  if (is.numeric(word_indices) && all(word_indices >= 1 & word_indices <= num_words)) {
    unique_indices <- unique(word_indices) #count each word ONCE per document
    doc_freq[unique_indices] <- doc_freq[unique_indices] + 1
  } else {
    warning("Unexpected word indices found in document")
  }
}
doc_freq_df <- data.frame(word = out$vocab, doc_freq = doc_freq, stringsAsFactors = FALSE)
print(head(doc_freq_df, 10))
doc_freq_df <- doc_freq_df[order(-doc_freq_df$doc_freq), ] #sort descending
print(head(doc_freq_df, 40))

freq_threshold <- quantile(doc_freq_df$doc_freq, 0.992, na.rm = TRUE)  # 99.2th percentile threshold
top_1_percent_words <- doc_freq_df$word[doc_freq_df$doc_freq > freq_threshold] #top 1% words
top_1_percent_words

## re-train model (filtered with 0.8% top words removed) ####
vocab_filtered <- vocab[!(vocab %in% top_1_percent_words)]
vocab_map <- setNames(seq_along(vocab_filtered), vocab_filtered)
docs_filtered <- lapply(docs, function(doc) {
  indices <- doc[1, ]
  counts <- doc[2, ]
  
  keep <- !(vocab[indices] %in% top_1_percent_words)
  new_indices <- sapply(indices[keep], function(idx) vocab_map[vocab[idx]])
  new_indices <- new_indices[!is.na(new_indices)]
  new_counts <- counts[keep]
  
  aggregated_counts <- tapply(new_counts, new_indices, sum)
  
  if (length(aggregated_counts) > 0) {
    return(matrix(c(as.integer(names(aggregated_counts)), aggregated_counts), nrow = 2, byrow = TRUE))
  } else {
    return(matrix(numeric(0), nrow = 2))  
  }
})

docs_filtered <- docs_filtered[sapply(docs_filtered, function(doc) ncol(doc) > 0)]

#correct input for stm
# docs_filtered <- lapply(docs_filtered, function(doc) {
#   matrix(c(unlist(doc)), nrow = 2, byrow = TRUE)
# })
# 
# str(docs_filtered)

out_filtered <- prepDocuments(docs_filtered, vocab_filtered, meta)
docs_filtered <- out_filtered$documents
vocab_filtered <- out_filtered$vocab
meta <- out_filtered$meta

### search K (filtered) ####
storage_filtered <- searchK(out_filtered$documents, out_filtered$vocab, 
                            K = c(3:40), 
                   prevalence =~ zittingsperiode +
                     galtan_equal_width_bins + gov_opp +
                     age_category + geslacht, 
                   data = meta,
                   init.type="Spectral")

plot(storage_filtered)
plot.searchK(storage_filtered) 
plt_full_res_searchK(storage_filtered) #shows all plots

storage_filtered$results

### STM model (filtered) ####
stm_13_filtered <- stm(documents = docs_filtered, vocab = vocab_filtered, K = 13, 
                    prevalence =~ zittingsperiode +
                      galtan_equal_width_bins +
                      gov_opp +
                      geslacht +
                      age_category,  
                    data = meta, 
                    max.em.its = 500,
                    init.type="Spectral",
                    #gamma.prior="L1"
)

stm_13_filtered_gamma <- stm(documents = docs_filtered, vocab = vocab_filtered, K = 13, 
                       prevalence =~ zittingsperiode +
                         galtan_equal_width_bins +
                         gov_opp +
                         geslacht +
                         age_category,  
                       data = meta, 
                       max.em.its = 500,
                       init.type="Spectral",
                       gamma.prior="L1"
)


summary(stm_13_filtered_gamma)
?summary.STM

stm_16_filtered <- stm(documents = docs_filtered, vocab = vocab_filtered, K = 16, 
                      prevalence =~ zittingsperiode +
                        galtan_equal_width_bins +
                        gov_opp +
                        geslacht +
                        age_category,  
                      data = meta, 
                      max.em.its = 500,
                      init.type="Spectral"
)

summary(stm_13_filtered)
summary(stm_16_filtered)

plot(stm_13_filtered, type="summary")
plot(stm_16_filtered, type="summary")

#save models and results
save(stm_13_filtered, file = "stm_13_filtered.RData")
save(stm_16_filtered, file = "stm_16_filtered.RData")
save(storage_filtered, file="storage_filtered.RData")

## comparison of two best models ####

#comparing model diagnostics of models with K=13 and K=16
semcoh_13 <- semanticCoherence(model=stm_13_filtered, documents=docs_filtered)
semcoh_16 <- semanticCoherence(model=stm_16_filtered, documents=docs_filtered)
exclusivity_13 <- exclusivity(stm_13_filtered)
exclusivity_16 <- exclusivity(stm_16_filtered)

mean(semcoh_13)
mean(semcoh_16)
mean(exclusivity_13)
mean(exclusivity_16)

#topic correlations for K=13 model
topic_corr_13 <- topicCorr(stm_13_filtered)
cor_matrix_13 <- topic_corr_13$cor

#topic correlations for K=16 model
topic_corr_16 <- topicCorr(stm_16_filtered)
cor_matrix_16 <- topic_corr_16$cor

#visualize the correlation matrices with using heatmap

#K=13 model
melted_cor_13 <- melt(cor_matrix_13)
heatmap_13 <- ggplot(data = melted_cor_13, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  labs(title="Topic Corr. Matrix for STM with K=13") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#K=16 model
melted_cor_16 <- melt(cor_matrix_16)
heatmap_16 <- ggplot(data = melted_cor_16, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  labs(title="Topic Corr. Matrix for STM with K=16") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot the heatmaps 
grid.arrange(heatmap_13, heatmap_16, ncol = 2)

## FINAL MODEL ####
stm_13_filtered
labelTopics(stm_13_filtered, n=20)
?summary.STM

load("stm_13_filtered.RData")

## results ####

#making parliamentary term numeric so it can be plotted as continuous variable
data$zittingsperiode_numeric <- as.numeric(as.character(factor(data$zittingsperiode, 
                                                               levels = c("1995-1999", "1999-2004", "2004-2009", 
                                                                          "2009-2014", "2014-2019", "2019-2024"), 
                                                               labels = c(1995, 1999, 2004, 2009, 2014, 2019))))

meta$geslacht <- as.factor(meta$geslacht)
meta$age_category <- as.factor(meta$age_category)
meta$galtan_equal_width_bins <- as.factor(meta$galtan_equal_width_bins)

# convergence plot
plot(stm_13_filtered$convergence$bound, type = "l", 
     ylab = "Approximate Objective", main = "Convergence")

### associated words ####

#13 topics with labels and topic proportions
plot(stm_13_filtered, n=5, topic.names=c("EDUCATION",
                                          "INCIDENTS & TARGET GROUPS",
                                          "HIV & PUBLIC HEALTH",
                                          "EQUALITY & ACCESSIBILITY",
                                          "MENTAL HEALTH",
                                          "ACTION PLANS",
                                          "ADOPTION",
                                          "YOUTH,MEDIA & DIVERSITY",
                                          "TRANSGENDER & SPORTS",
                                          "VIOLENCE & HATE CRIMES",
                                          "EMPLOYMENT",
                                          "INTERNATIONAL AFFAIRS",
                                          "FINANCIAL SUPPORT"
), 
text.cex=0.8,
main="13 Topics with Highest Probability Words")

?plot.STM


#plot two topics with their top words
plot(stm_13_filtered, 
     topics=c(2,4, 6),
     type = "labels",
     n = 40)
?plot.STM

#plot two or more topics with their document-topic proportions in histogram
plot(stm_13_filtered, topics=c(1,2,3),type = "hist")

#plot two or more topics
plot(stm_13_filtered, topics=c(1,2,3,4,5),type = "labels")
plot(stm_13_filtered, topics=c(6,7,8,9),type = "labels")
plot(stm_13_filtered, topics=c(1,2,3,4,5),
     type = "labels",
     labeltype = "frex")

plot(stm_13_filtered, topics=c(7, 8,9),
     type = "labels",
     labeltype = "prob")

### associated documents ####

?findThoughts

thoughts_topic2 <- findThoughts(stm_13_filtered, 
                         texts = data$question, 
                         n = 15, 
                         topics = 2)


thoughts_topic6 <- findThoughts(stm_13_filtered, 
                                texts = data$question, 
                                n = 15, 
                                topics = 6)


thoughts_topic6 <- findThoughts(stm_13_filtered, 
                                texts = data$question, 
                                n = 15, 
                                topics = 9)

thoughts_all <- findThoughts(stm_13_filtered, 
                             texts = data$question, 
                             n = 15, 
                             #topics=
                             )

sink("topics_docs.txt")
print(thoughts_all)
sink()


### topic correlations ####

mod.out.corr <- topicCorr(stm_13_filtered_continuous)
plot(mod.out.corr)

stmCorrViz(mod = stm_13_filtered, 
           file_out = "topic_corr.html",
           documents_matrix = docs_filtered,
           title="Topic Hierarchies with Topic Correlations")
?stmCorrViz

### refit model with continuous parliamentary term ####
meta$zittingsperiode_numeric <- as.numeric(as.character(factor(meta$zittingsperiode, 
                                                               levels = c("1995-1999", "1999-2004", "2004-2009", 
                                                                          "2009-2014", "2014-2019", "2019-2024"), 
                                                               labels = c(1995, 1999, 2004, 2009, 2014, 2019))))

stm_13_filtered_continuous <- stm(documents = docs_filtered, 
                       vocab = vocab_filtered, 
                       K = 13, 
                       prevalence =~ zittingsperiode_numeric +
                         galtan_equal_width_bins +
                         gov_opp +
                         geslacht +
                         age_category,  
                       data = meta, 
                       max.em.its = 500,
                       init.type = "Spectral")

summary(stm_13_filtered_continuous)

save(stm_13_filtered_continuous, file="stm_13_filtered_continuous.RData")

prep_continuous <- estimateEffect(1:13 ~ zittingsperiode_numeric +
                         galtan_equal_width_bins +
                         gov_opp +
                         geslacht +
                         age_category, 
                       stmobj = stm_13_filtered_continuous, 
                       meta = meta, 
                       uncertainty = "Global")

summary(prep_continuous)
prep$terms
str(meta)
summary(prep)
plot(prep_continuous, 
     covariate = "zittingsperiode_numeric", 
     method = "continuous", 
     xlab = "Difference in Topic Proportions", 
     main = "Effect of Parliamentary Term on Topics",
     topics=10
)
?plot.estimateEffect

### covariate effects ####
?estimateEffect

meta$zittingsperiode <- as.numeric(meta$zittingperiode)

prep <- estimateEffect(1:13 ~ zittingsperiode_numeric +
                         galtan_equal_width_bins +
                         gov_opp +
                         geslacht +
                         age_category, 
                       stmobj = stm_13_filtered_continuous,
                       meta = meta, 
                       uncertainty = "Global")

summary(prep)

# effect of parliamentary term on topic prevalence
par(lwd = 1.5)  # Change the thickness as needed

plot(prep_continuous, "zittingsperiode_numeric", method = "continuous", 
     main = "Effect of Parliamentary Term on Topic Prevalence",
     xlab = "Parliamentary Term (1995-2024)",
     linecol = c("darkblue", "darkgreen","darkviolet","darkred"),
     width=10,
     printlegend=FALSE,
     topics= c(3,5,10,11),
     labeltype = "custom",
     custom.labels = c("HIV & PUBLIC HEALTH",
                       "MENTAL HEALTH",
                       "VIOLENCE & HATE CRIMES",
                       "EMPLOYMENT")
     )

legend(2009, -0.2, c("HIV & PUBLIC HEALTH",
                     "MENTAL HEALTH",
                     "VIOLENCE & HATE CRIMES",
                     "EMPLOYMENT"),
       lwd = 2,
       col =c("darkblue", "darkgreen","darkviolet","darkred"))

?plot.estimateEffect

# interaction effects

prep_interaction <- estimateEffect(c(1:13) ~ zittingsperiode_numeric +
                                       galtan_equal_width_bins +
                                       gov_opp +
                                       geslacht +
                                       age_category +
                                       galtan_equal_width_bins * zittingsperiode_numeric, 
                                     stm_13_filtered_continuous, 
                                     metadata = meta, 
                                     uncertainty = "Global")


summary(prep_interaction)

# effect of GALTAN on topic prevalence

plot(prep_continuous, "galtan_equal_width_bins", method = "difference", 
     cov.value1 = "TAN", cov.value2 = "GAL",
     main = "Effect of GALTAN on Topic Prevalence",
     xlab = "(GAL) ................... Difference in Topic Prevalence ................ (TAN)",
     #linecol = c("darkblue", "darkgreen","darkviolet","darkred"),
     width=8,
     #printlegend=TRUE,
     topics= c(1:13),
     labeltype = "custom",
     custom.labels=c("EDUCATION",
                     "INCIDENTS & TARGET GROUPS",
                     "HIV & PUBLIC HEALTH",
                     "EQUALITY & ACCESSIBILITY",
                     "MENTAL HEALTH",
                     "ACTION PLANS",
                     "ADOPTION",
                     "YOUTH, MEDIA & DIVERSITY",
                     "TRANSGENDER & SPORTS",
                     "VIOLENCE & HATE CRIMES",
                     "EMPLOYMENT",
                     "INTERNATIONAL AFFAIRS",
                     "FINANCIAL SUPPORT")
)


?plot.estimateEffect
summary(prep_continuous)
# effect of government-opposition on topic prevalence

plot(prep_continuous, "gov_opp", method = "difference", 
     cov.value1 = "0", cov.value2 = "1", 
     xlab = "(Government) .....................Difference in Topic Prevalence.......................... (Opposition)",
     main = "Effect of Government-Opposition on Topic Prevalence",
     ci.level = 0.95,
     labeltype="custom",
     custom.labels=c("EDUCATION",
                     "INCIDENTS & TARGET GROUPS",
                     "HIV & PUBLIC HEALTH",
                     "EQUALITY & ACCESSIBILITY",
                     "MENTAL HEALTH",
                     "ACTION PLANS",
                     "ADOPTION",
                     "YOUTH,MEDIA & DIVERSITY",
                     "TRANSGENDER & SPORTS",
                     "VIOLENCE & HATE CRIMES",
                     "EMPLOYMENT",
                     "INTERNATIONAL AFFAIRS",
                     "FINANCIAL SUPPORT"
     ))


# effect of MP age on topic prevalence

plot(prep, "age_category", method = "difference", 
     xlab = "(Up to 35) ..............................Difference in Topic Prevalence.................................... (Above 50)", 
     cov.value1 = "Above 50", cov.value2 = "Up to 35", 
     main = "Effect of Age on Topic Prevalence",
     #ci.level=0.1,
     topics=c(1:13),
     labeltype="custom",
     custom.labels=c("EDUCATION",
                     "INCIDENTS & TARGET GROUPS",
                     "HIV & PUBLIC HEALTH",
                     "EQUALITY & ACCESSIBILITY",
                     "MENTAL HEALTH & POVERTY",
                     "ACTION PLANS",
                     "ADOPTION",
                     "YOUTH,MEDIA & DIVERSITY",
                     "TRANSGENDER & SPORTS",
                     "VIOLENCE & HATE CRIMES",
                     "EMPLOYMENT",
                     "INTERNATIONAL AFFAIRS",
                     "FINANCIAL SUPPORT"
     ))

summary(stm_13_filtered)

plot(prep, "age_category", method = "difference", 
     xlab = "36-50 ......................... Above 50", 
     cov.value1 = "Above 50", cov.value2 = "36-50", 
     main = "Effect of MP Age on Topic Prevalence",
     topics=c(1:13),
     labeltype="custom",
     custom.labels=c("EDUCATION",
                     "INCIDENTS & TARGET GROUPS",
                     "HIV & PUBLIC HEALTH",
                     "EQUALITY & ACCESSIBILITY",
                     "MENTAL HEALTH & POVERTY",
                     "ACTION PLANS",
                     "ADOPTION",
                     "YOUTH,MEDIA & DIVERSITY",
                     "TRANSGENDER & SPORTS",
                     "VIOLENCE & HATE CRIMES",
                     "EMPLOYMENT",
                     "INTERNATIONAL AFFAIRS",
                     "FINANCIAL SUPPORT"
     ))

# effect of MP sex on topic prevalence
plot(prep_continuous, "geslacht", method = "difference", 
     cov.value1 = "M", cov.value2 = "V", 
     xlab = "Difference in Topic Proportions", 
     main = "Effect of MP Sex on Topic Prevalence (M compared to V)",
     labeltype="custom",
     custom.labels=c("EDUCATION",
                     "INCIDENTS & TARGET GROUPS",
                     "HIV & PUBLIC HEALTH",
                     "EQUALITY & ACCESSIBILITY",
                     "MENTAL HEALTH",
                     "ACTION PLANS",
                     "ADOPTION",
                     "YOUTH,MEDIA & DIVERSITY",
                     "TRANSGENDER & SPORTS",
                     "VIOLENCE & HATE CRIMES",
                     "EMPLOYMENT",
                     "INTERNATIONAL AFFAIRS",
                     "FINANCIAL SUPPORT"
     ))
