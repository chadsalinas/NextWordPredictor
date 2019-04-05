# -----------------------------------
# WordPrediction
# Server.R
# 
# author: Chad Salinas
# date:
# revision: 2.3
# -----------------------------------
library(tm)
library(scales)
load("unigrams.Rda")
load("bigrams.Rda")
load("trigrams.Rda")
load("quadgrams.Rda")

predictNextWords <- function(var) {
  
  inputs <- var
  
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
    removeOrdinals <-function(x) gsub("[0-9](?:st|nd|rd|th)", "", x, ignore.case=F, perl=T) 
    corpus <- tm_map(corpus, content_transformer(removeOrdinals))
    removePunctuationExceptApos <- function(x) gsub("[^[:^punct:]']", "", x, perl = TRUE)
    corpus <- tm_map(corpus, content_transformer(removePunctuationExceptApos))
    removeNumbers <- function(x) gsub("[0-9]*", "", x, ignore.case=F, perl=T)
    corpus <- tm_map(corpus, content_transformer(removeNumbers))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
  }
  
  checkQuadgrams <- function(terms) {
    dfPrediction <- dfQuadgrams[(dfQuadgrams$term1 == terms[1] & 
                                   dfQuadgrams$term2 == terms[2] & 
                                   dfQuadgrams$term3 == terms[3]), ]
    row.names(dfPrediction) <- NULL
    output <- getPredictions(dfPrediction)
    return(output)
  }
  
  checkTrigrams <- function(terms) {
    dfPrediction <- dfTrigrams[(dfTrigrams$term1 == terms[1] & 
                                  dfTrigrams$term2 == terms[2]), ]
    row.names(dfPrediction) <- NULL
    output <- getPredictions(dfPrediction)
    return(output)
  }
  
  checkBigrams <- function(terms) {
    dfPrediction <- dfBigrams[(dfBigrams$term1 == terms[1]), ]
    row.names(dfPrediction) <- NULL
    output <- getPredictions(dfPrediction)
    return(output)
  }
  
  getPredictions <- function(dfPrediction) {
    row.names(dfPrediction) <- NULL
    term1 <- dfPrediction$prediction[1]
    term2 <- dfPrediction$prediction[2]
    term3 <- dfPrediction$prediction[3]
    term4 <- dfPrediction$prediction[4]
    term5 <- dfPrediction$prediction[5]
    probability1 <- dfPrediction$probability[1]
    if(!is.na(probability1)) {probability1 <- percent(probability1)}
    probability2 <- dfPrediction$probability[2]
    if(!is.na(probability2)) {probability2 <- percent(probability2)}
    probability3 <- dfPrediction$probability[3]
    if(!is.na(probability3)) {probability3 <- percent(probability3)}
    probability4 <- dfPrediction$probability[4]
    if(!is.na(probability4)) {probability4 <- percent(probability4)}
    probability5 <- dfPrediction$probability[5]
    if(!is.na(probability5)) {probability5 <- percent(probability5)}
    output <- (c(term1, term2, term3, term4, term5, probability1, probability2, probability3, probability4, probability5))
    return(output)
  }
  
  # Tidy User Input
  inputCorpus <- VCorpus(VectorSource(inputs))
  inputCorpus <- tidy_corpus(inputCorpus)
  tidy_user_input <- inputCorpus[[1]]$content
  
  inputTokens <- unlist(strsplit(tidy_user_input, " "))
  inputTokens <- iconv(inputTokens, 'UTF-8', 'ASCII', "byte")
  numTerms <- length(inputTokens)
  
  if(numTerms < 1) { 
    output <- ""
  }
  
  if(numTerms == 1) {
    term1 <- inputTokens[numTerms]
    output <- checkBigrams(inputTokens[numTerms])
  }
  
  if(numTerms == 2) {
    output <- checkTrigrams(inputTokens[1:numTerms])
    if (is.na(output[1])) {
      output <- checkBigrams(inputTokens[numTerms])
    }
  }
  
  if(numTerms >= 3) {
    last3Tokens <- c(inputTokens[numTerms-2], inputTokens[numTerms-1], inputTokens[numTerms])
    output <- checkQuadgrams(last3Tokens)
    if (is.na(output[1])) {
      output <- checkTrigrams(last3Tokens[2:3])
    }
    if (is.na(output[1])) {
      output <- checkBigrams(inputTokens[numTerms])
    }
  }
  
  output <- matrix(output[1:10], nrow = 5, ncol = 2)
  
  # Handle default values case 
  if(is.na(output[1, 1])) {
    denom <- sum(dfUnigrams$count)
    output <- head(dfUnigrams[1:5, ])
    output$count <- percent(output$count/denom)
  }
  colnames(output) <- c("Next Term","Probability")
  output <-  na.omit(output)
  return(output)
}

library(shiny)
shinyServer(function(input, output, session) {
  
  prediction <- reactive({
    predictNextWords(input$var)
  })
  
  output$predictionTable <- renderTable({
    preds <- prediction()
    preds
  })    
})