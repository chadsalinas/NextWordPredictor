# -----------------------------------
# WordPrediction
# CreateNgrams.R
# 
# author: Chad Salinas
# date:
# revision: 2.3
# -----------------------------------

setwd("~/Documents/JohnsHopkins/Capstone")
library(tidyr)
library(tm)
library(RWeka)
library(slam)
library(dplyr)
library(stringr)

load("combCorpus.Rda")  # Train Corpus 

buildTestGrams = 1

####  BUILD UNIGRAMS  ####
print("Building Unigrams")
unigrams <- TermDocumentMatrix(combCorpus, control = list(wordLengths = c(1, Inf)))
unigrams <- row_sums(unigrams)
unigrams <- sort(unigrams, decreasing = TRUE)
dfUnigrams <- data.frame(term = names(unigrams), count = unigrams, stringsAsFactors = FALSE)
rm(unigrams)
row.names(dfUnigrams) <- NULL
  
# First time dealing with tokens instead of strings so last
# chance to tighten up unigrams to get coverage level up

# Get rid of long words
bigWords <- dfUnigrams %>%
  select(everything()) %>%
  filter(nchar(dfUnigrams$term) > 16)

smallWords <- dfUnigrams %>%
  select(everything()) %>%
  filter(nchar(dfUnigrams$term) < 3)

dfUnigrams <- dfUnigrams %>%
  select(everything()) %>%
  filter(nchar(dfUnigrams$term) < 17)

# Get rid of non-contraction apostrophes
dfUnigrams <- dfUnigrams %>%
  select(term, count) %>%
  filter(!(str_detect(term, "^'") | str_detect(term, "'$")))

# Get rid of low frequency words 
dfUnigrams <- dfUnigrams %>%
  select(term, count) %>%
  filter(count > 25)


####  BUILD BIGRAMS  ####
print("Building Bigrams")
BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = ' \r\n\t.,;:"()?!'))}
bigrams <- TermDocumentMatrix(combCorpus, control = list(tokenize = BigramTokenizer, bounds = list(global = c(1,Inf))))
bigrams <- row_sums(bigrams)
bigrams <- sort(bigrams, decreasing = TRUE)
dfBigrams <- data.frame(term = names(bigrams), count = bigrams, stringsAsFactors = FALSE)
rm(bigrams)
dfBigrams <- separate(data = dfBigrams, col = term, into = c("term1", "term2"), sep = " ")
row.names(dfBigrams) <- NULL

# Get rid of non-contraction apostrophes only out of the new term we are bringing in
dfBigrams <- dfBigrams %>%
  select(term1, term2, count) %>%
  filter(!(str_detect(term2, "^'") | str_detect(term2, "'$")))

dfBigrams <- merge(dfBigrams, dfUnigrams, by.x = "term1", by.y = "term")
dfBigrams <- cbind(dfBigrams, probability = (dfBigrams[,3]/dfBigrams[,4]))
names(dfBigrams)[3] <- "count"
dfBigrams[4] <- NULL
dfBigrams <- dfBigrams[order(dfBigrams$probability, decreasing = TRUE), ]

# Get rid of low frequency bigrams 
dfBigrams <- dfBigrams %>%
  select(everything()) %>%
  filter(count > 10)

save(dfUnigrams, file = "unigrams.Rda")
#rm(dfUnigrams)  # Don't need unigrams after the merge


####  BUILD TRIGRAMS  ####
print("Building Trigrams")
TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = ' \r\n\t.,;:"()?!'))}
trigrams <- TermDocumentMatrix(combCorpus, control = list(tokenize = TrigramTokenizer, bounds = list(global = c(1,Inf))))
trigrams <- row_sums(trigrams)
trigrams <- sort(trigrams, decreasing = TRUE)
dfTrigrams <- data.frame(term = names(trigrams), count = trigrams, stringsAsFactors = FALSE)
rm(trigrams)
dfTrigrams <- separate(data = dfTrigrams, col = term, into = c("term1", "term2", "term3"), sep = " ")
row.names(dfTrigrams) <- NULL

# Get rid of non-contraction apostrophes only out of the new term we are bringing in
dfTrigrams <- dfTrigrams %>%
  select(term1, term2, term3, count) %>%
  filter(!(str_detect(term3, "^'") | str_detect(term3, "'$")))

dfTrigrams <- merge(dfTrigrams, dfBigrams[,c("term1","term2","count")], by = c("term1", "term2"))
dfTrigrams <- cbind(dfTrigrams, probability = (dfTrigrams[,4]/dfTrigrams[,5]))
names(dfTrigrams)[4] <- "count"
dfTrigrams[5] <- NULL
dfTrigrams <- dfTrigrams[order(dfTrigrams$probability, decreasing = TRUE), ]

# Get rid of low frequency trigrams 
dfTrigrams <- dfTrigrams %>%
  select(everything()) %>%
  filter(count > 2)

names(dfBigrams)[2] <- "prediction"

####  BUILD QUADGRAMS  ####
print("Building Quadgrams")
QuadgramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = ' \r\n\t.,;:"()?!'))}
quadgrams <- TermDocumentMatrix(combCorpus, control = list(tokenize = QuadgramTokenizer, bounds = list(global = c(1,Inf))))
quadgrams <- row_sums(quadgrams)
quadgrams <- sort(quadgrams, decreasing = TRUE)
dfQuadgrams <- data.frame(term = names(quadgrams), count = quadgrams, stringsAsFactors = FALSE)
rm(quadgrams)
dfQuadgrams <- separate(data = dfQuadgrams, col = term, into = c("term1", "term2", "term3", "term4"), sep = " ")
row.names(dfQuadgrams) <- NULL

# Get rid of non-contraction apostrophes only out of the new term we are bringing in
dfQuadgrams <- dfQuadgrams %>%
  select(term1, term2, term3, term4, count) %>%
  filter(!(str_detect(term4, "^'") | str_detect(term4, "'$")))

dfQuadgrams <- merge(dfQuadgrams, dfTrigrams[,c("term1","term2","term3", "count")], by = c("term1", "term2", "term3"))
dfQuadgrams <- cbind(dfQuadgrams, probability = (dfQuadgrams[,5]/dfQuadgrams[,6]))
names(dfQuadgrams)[5] <- "count"
dfQuadgrams[6] <- NULL
dfQuadgrams <- dfQuadgrams[order(dfQuadgrams$probability, decreasing = TRUE), ]

# Get rid of low frequency Quadgrams
dfQuadgrams <- dfQuadgrams %>%
  group_by(count) %>%
  select(everything()) %>%
  filter(count > 1)

####  Done BUILDING ALL THE GRAMS  ####

# Prune back bigrams, trigrams, and quadgrams to top 5 predictions

# prune to top 5 Bigrams
dfBigrams <- dfBigrams[order(dfBigrams$probability, decreasing = TRUE), ]
dfBigrams <- unite_(dfBigrams, "justTerms", c("term1"), sep = " ", remove = FALSE)
dfBigrams <- dfBigrams[order(dfBigrams$justTerms, decreasing = TRUE), ]
dfBigrams <- dfBigrams %>%
  group_by(justTerms) %>%
  top_n(n = 5, wt = probability) %>% 
  arrange(justTerms, -probability) %>% slice(1:5)
dfBigrams <- dfBigrams[order(dfBigrams$probability, decreasing = TRUE), ]
dfBigrams$justTerms <- NULL

save(dfBigrams, file = "bigrams.Rda")
#rm(dfBigrams)

# prune to top 5 Trigrams
dfTrigrams <- dfTrigrams[order(dfTrigrams$probability, decreasing = TRUE), ]
dfTrigrams <- unite_(dfTrigrams, "justTerms", c("term1","term2"), sep = " ", remove = FALSE)
dfTrigrams <- dfTrigrams[order(dfTrigrams$justTerms, decreasing = TRUE), ]
dfTrigrams <- dfTrigrams %>%
  group_by(justTerms) %>%
  top_n(n = 5, wt = probability) %>% 
  arrange(justTerms, -probability) %>% slice(1:5)
dfTrigrams <- dfTrigrams[order(dfTrigrams$probability, decreasing = TRUE), ]
dfTrigrams$justTerms <- NULL
names(dfTrigrams)[3] <- "prediction"

save(dfTrigrams, file = "trigrams.Rda")
#rm(dfTrigrams)

# prune to top 5 Quadgrams
dfQuadgrams <- dfQuadgrams[order(dfQuadgrams$probability, decreasing = TRUE), ]
dfQuadgrams <- unite_(dfQuadgrams, "justTerms", c("term1","term2", "term3"), sep = " ", remove = FALSE)
dfQuadgrams <- dfQuadgrams[order(dfQuadgrams$justTerms, decreasing = TRUE), ]
dfQuadgrams <- dfQuadgrams %>%
  group_by(justTerms) %>%
  top_n(n = 5, wt = probability) %>% 
  arrange(justTerms, -probability) %>% slice(1:5)
dfQuadgrams <- dfQuadgrams[order(dfQuadgrams$probability, decreasing = TRUE), ]
dfQuadgrams$justTerms <- NULL
names(dfQuadgrams)[4] <- "prediction"

save(dfQuadgrams, file = "quadgrams.Rda")
#rm(dfQuadgrams)
rm(combCorpus)
  
if(buildTestGrams) {
  
  ####  BUILD TEST NGRAMS  #####
  print("Building Test Grams")
  load("combTestCorpus.Rda")  # Test Corpus
  
  print("Building Test Unigrams")
  unigrams <- TermDocumentMatrix(combTestCorpus, control = list(wordLengths = c(1, Inf)))
  unigrams <- row_sums(unigrams)
  unigrams <- sort(unigrams, decreasing = TRUE)
  dfTestUnigrams <- data.frame(term = names(unigrams), count = unigrams, stringsAsFactors = FALSE)
  rm(unigrams)
  row.names(dfTestUnigrams) <- NULL
  
  # Get rid of long words
  dfTestUnigrams <- dfTestUnigrams %>%
    select(everything()) %>%
    filter(nchar(dfTestUnigrams$term) < 2)  #17 for .64 sample
  
  # Get rid of non-contraction apostrophes
  dfTestUnigrams <- dfTestUnigrams %>%
    select(term, count) %>%
    filter(!(str_detect(term, "^'") | str_detect(term, "'$")))
  
  # Get rid of low frequency words 
  dfTestUnigrams <- dfTestUnigrams %>%
    select(term, count) %>%
    filter(count > 0)
  
  
  print("Building Test Bigrams")
  BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = ' \r\n\t.,;:"()?!'))}
  bigrams <- TermDocumentMatrix(combTestCorpus, control = list(tokenize = BigramTokenizer, bounds = list(global = c(1,Inf))))
  bigrams <- row_sums(bigrams)
  bigrams <- sort(bigrams, decreasing = TRUE)
  dfTestBigrams <- data.frame(term = names(bigrams), count = bigrams, stringsAsFactors = FALSE)
  rm(bigrams)
  dfTestBigrams <- separate(data = dfTestBigrams, col = term, into = c("term1", "term2"), sep = " ")
  row.names(dfTestBigrams) <- NULL
  
  # Get rid of non-contraction apostrophes only out of the new term we are bringing in
  dfTestBigrams <- dfTestBigrams %>%
    select(term1, term2, count) %>%
    filter(!(str_detect(term2, "^'") | str_detect(term2, "'$")))
  
  dfTestBigrams <- merge(dfTestBigrams, dfTestUnigrams, by.x = "term1", by.y = "term")
  dfTestBigrams <- cbind(dfTestBigrams, probability = (dfTestBigrams[,3]/dfTestBigrams[,4]))
  names(dfTestBigrams)[3] <- "count"
  dfTestBigrams[4] <- NULL
  dfTestBigrams <- dfTestBigrams[order(dfTestBigrams$probability, decreasing = TRUE), ]
  dfTestBigrams <- dfTestBigrams[dfTestBigrams[ ,4] <= 1,] # probability < 1
  
  # Get rid of low frequency bigrams 
  dfTestBigrams <- dfTestBigrams %>%
    select(everything()) %>%
    filter(count > 0)
  
  
  print("Building Test Trigrams")
  TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = ' \r\n\t.,;:"()?!'))}
  trigrams <- TermDocumentMatrix(combTestCorpus, control = list(tokenize = TrigramTokenizer, bounds = list(global = c(1,Inf))))
  trigrams <- row_sums(trigrams)
  trigrams <- sort(trigrams, decreasing = TRUE)
  dfTestTrigrams <- data.frame(term = names(trigrams), count = trigrams, stringsAsFactors = FALSE)
  rm(trigrams)
  dfTestTrigrams <- separate(data = dfTestTrigrams, col = term, into = c("term1", "term2", "term3"), sep = " ")
  row.names(dfTestTrigrams) <- NULL
  
  # Get rid of non-contraction apostrophes only out of the new term we are bringing in
  dfTestTrigrams <- dfTestTrigrams %>%
    select(term1, term2, term3, count) %>%
    filter(!(str_detect(term3, "^'") | str_detect(term3, "'$")))
  
  dfTestTrigrams <- merge(dfTestTrigrams, dfTestBigrams[,c("term1","term2","count")], by = c("term1", "term2"))
  dfTestTrigrams <- cbind(dfTestTrigrams, probability = (dfTestTrigrams[,4]/dfTestTrigrams[,5]))
  names(dfTestTrigrams)[4] <- "count"
  dfTestTrigrams[5] <- NULL
  dfTestTrigrams <- dfTestTrigrams[order(dfTestTrigrams$probability, decreasing = TRUE), ]
  
  # Get rid of low frequency trigrams 
  dfTestTrigrams <- dfTestTrigrams %>%
    select(everything()) %>%
    filter(count > 0)
  
  
  print("Building Test Quadgrams")
  QuadgramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = ' \r\n\t.,;:"()?!'))}
  quadgrams <- TermDocumentMatrix(combTestCorpus, control = list(tokenize = QuadgramTokenizer, bounds = list(global = c(1,Inf))))
  quadgrams <- row_sums(quadgrams)
  quadgrams <- sort(quadgrams, decreasing = TRUE)
  dfTestQuadgrams <- data.frame(term = names(quadgrams), count = quadgrams, stringsAsFactors = FALSE)
  rm(quadgrams)
  dfTestQuadgrams <- separate(data = dfTestQuadgrams, col = term, into = c("term1", "term2", "term3", "term4"), sep = " ")
  row.names(dfTestQuadgrams) <- NULL
  
  # Get rid of non-contraction apostrophes only out of the new term we are bringing in
  dfTestQuadgrams <- dfTestQuadgrams %>%
    select(term1, term2, term3, term4, count) %>%
    filter(!(str_detect(term4, "^'") | str_detect(term4, "'$")))
  
  dfTestQuadgrams <- merge(dfTestQuadgrams, dfTestTrigrams[,c("term1","term2","term3", "count")], by = c("term1", "term2", "term3"))
  dfTestQuadgrams <- cbind(dfTestQuadgrams, probability = (dfTestQuadgrams[,5]/dfTestQuadgrams[,6]))
  names(dfTestQuadgrams)[5] <- "count"
  dfTestQuadgrams[6] <- NULL
  dfTestQuadgrams <- dfTestQuadgrams[order(dfTestQuadgrams$probability, decreasing = TRUE), ]
  
  # Get rid of low frequency Quadgrams
  dfTestQuadgrams <- dfTestQuadgrams %>%
    group_by(count) %>%
    select(everything()) %>%
    filter(count > 0)
  
  names(dfTestQuadgrams)[4] <- "prediction"
  names(dfTestBigrams)[2] <- "prediction"
  names(dfTestTrigrams)[3] <- "prediction"
  
  save(dfTestUnigrams, dfTestBigrams, dfTestTrigrams, dfTestQuadgrams, file = "testNgrams.Rda")
  rm(combTestCorpus)
  
  ####  END OF BUILD TEST NGRAMS  ####
}

#rm(list = ls())
#gc()
