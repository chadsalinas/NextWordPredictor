# -----------------------------------
# WordPrediction
# GetTidyCorpus.R
# 
# author: Chad Salinas
# date:
# revision: 2.3
# -----------------------------------

setwd("~/Documents/JohnsHopkins/Capstone/final/en_US")
library(RCurl)  
library(tm)  

# Get Data
url  <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
directory <- "~/Documents/JohnsHopkins/Capstone/"
filename <- "Coursera-SwiftKey.zip"
path <- paste(directory, filename, sep = "/")
if (!file.exists(path)) {
  download.file(url, destfile = path, method = "curl")
}
unzip(zipfile = path, exdir = directory)
list.files()

blogData <- readLines(con <- file("en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(con)

newsData <- readLines(con <- file("en_US.news.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(con)

twitData <- readLines(con <- file("en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(con)

# Set & Get Train & Test Samples
blog_train_sample_size = length(blogData) * 0.64
news_train_sample_size = length(newsData) * 0.64
twit_train_sample_size = length(twitData) * 0.64

blog_test_sample_size = blog_train_sample_size * 0.01     
news_test_sample_size = news_train_sample_size * 0.01 
twit_test_sample_size = twit_train_sample_size * 0.01

set.seed(1964) 
inTrain <- sample(1:length(blogData), blog_train_sample_size, replace = FALSE)
blogSample <- blogData[inTrain]
blogTest <- blogData[-inTrain]         
blog_test_sample <- sample(1:length(blogTest), blog_test_sample_size, replace = FALSE)
blogTest <- blogTest[blog_test_sample] 

inTrain <- sample(1:length(newsData), news_train_sample_size, replace = FALSE)
newsSample <- newsData[inTrain]
newsTest <- newsData[-inTrain]        
news_test_sample <- sample(1:length(newsTest), news_test_sample_size, replace = FALSE)
newsTest <- newsTest[news_test_sample]

inTrain <- sample(1:length(twitData), twit_train_sample_size, replace = FALSE)
twitSample <- twitData[inTrain]
twitTest <- twitData[-inTrain]        
twit_test_sample <- sample(1:length(twitTest), twit_test_sample_size, replace = FALSE)
twitTest <- twitTest[twit_test_sample]

rm(blogData)
rm(newsData)
rm(twitData)

# Combine samples before tidying up and creating VCorpus
combSample <- c(blogSample, newsSample, twitSample)

# Exclude poor predicting lines e.g. shortlines
shorts <- quantile(nchar(combSample), .25)
combSample <- combSample[nchar(combSample) > shorts]

combTest <- c(blogTest, newsTest, twitTest)
rm(blogSample)
rm(newsSample)
rm(twitSample)
rm(blogTest)
rm(newsTest)
rm(twitTest)

# Tidy up the data
combSample <- iconv(combSample, 'UTF-8', 'ASCII', "byte")
combTest <- iconv(combTest, 'UTF-8', 'ASCII', "byte")

# Setup to get rid of profanity
profanityURL <- getURL('https://www.cs.cmu.edu/~biglou/resources/bad-words.txt', ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
profanity <- read.csv(textConnection(profanityURL), header = FALSE)

#' tidy_corpus()
#'
#' Cleans up the corpus, so we can just extract the root words.
#'
#' @param corpus a volatile text corpus
#' @return clean corpus after transformations
#' @details
#' We remove punctuation except for ', numbers, profanity, URLs, and whitespace. 
#' We convert to lower case
#' We do not stem because stemming creates "togeth" from "together", "broth" from "brother"
#' We do not remove English stopwords because sentence semantics goes down
tidy_corpus <- function(corpus) {
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  corpus <- tm_map(corpus, content_transformer(removeURL))
  removeOrdinals <-function(x) gsub("[0-9](?:st|nd|rd|th)", "", x, ignore.case = FALSE, perl = TRUE) 
  corpus <- tm_map(corpus, content_transformer(removeOrdinals))
  removePunctuationExceptApos <- function(x) gsub("[^[:^punct:]']", "", x, perl = TRUE)
  corpus <- tm_map(corpus, content_transformer(removePunctuationExceptApos))
  removeNumbers <- function(x) gsub("[0-9]*", "", x, ignore.case = FALSE, perl = TRUE)
  corpus <- tm_map(corpus, content_transformer(removeNumbers))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, profanity[ ,1])  
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create the Sample(Train) Corpus
combCorpus <- VCorpus(VectorSource(combSample))
rm(combSample)

combCorpus <- tidy_corpus(combCorpus)
save(combCorpus, file="../../combCorpus.Rda")  
rm(combCorpus)

# Create the Test(Hold out set) Corpus
combTestCorpus <- VCorpus(VectorSource(combTest))
rm(combTest)
combTestCorpus <- tidy_corpus(combTestCorpus)
save(combTestCorpus, file="../../combTestCorpus.Rda") 

rm(list = ls())
gc()
