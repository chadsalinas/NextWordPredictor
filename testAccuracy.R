# -----------------------------------
# WordPrediction
# TestAccuracy.R
# 
# author: Chad Salinas
# date:
# revision: 2.3
# -----------------------------------

setwd("~/Documents/JohnsHopkins/Capstone")
load("bigrams.Rda")
load("trigrams.Rda")
load("quadgrams.Rda")
load("testNgrams.Rda")

numTop1Success = 0
numTop5Success = 0
numTrials = nrow(dfTestQuadgrams)

#########################################################################################
predict <- function(inputs, prediction) {

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
    probability2 <- dfPrediction$probability[2]
    probability3 <- dfPrediction$probability[3]
    probability4 <- dfPrediction$probability[4]
    probability5 <- dfPrediction$probability[5]
    output <- (c(term1, term2, term3, term4, term5, probability1, probability2, probability3, probability4, probability5))
    return(output)
  }
  
  inputTokens <- inputs
  numTerms <- length(inputTokens)
      
  if(numTerms == 3) {
    output <- checkQuadgrams(inputTokens[1:numTerms])
    if (is.na(output[1])) {
      output <- checkTrigrams(inputTokens[2:numTerms])
    }
    if (is.na(output[1])) {
      output <- checkBigrams(inputTokens[numTerms])
    }
  }
    
  if(numTerms == 2) {
    output <- checkTrigrams(inputTokens[1:numTerms])
    if (is.na(output[1])) {
      output <- checkBigrams(inputTokens[numTerms])
    }
  }
    
  if(numTerms == 1) {
    output <- checkBigrams(inputTokens[numTerms])
  }

  # Assert: Only ever going to have 1 exact match, just need to know where
  if ((!is.na(output[1])) && (prediction == output[1])) {
    numTop1Success <<- numTop1Success + 1
    numTop5Success <<- numTop5Success + 1
  }
  else if ((!is.na(output[2])) && (prediction == output[2])) {
    numTop5Success <<- numTop5Success + 1
  }
  else if ((!is.na(output[3])) && (prediction == output[3])) {
    numTop5Success <<- numTop5Success + 1
  }
  else if ((!is.na(output[4])) && (prediction == output[4])) {
    numTop5Success <<- numTop5Success + 1
  }
  else if ((!is.na(output[5])) && (prediction == output[5])) {
    numTop5Success <<- numTop5Success + 1
  }
}

# Get accuracy by running sample of hold-out set against training sample
getAccuracy = function(x, output) {
  term1 = x[1]
  term2 = x[2]
  term3 = x[3]
  inputs <- c(term1, term2, term3)
  prediction = x[4]
  predict(inputs, prediction)
}

apply(dfTestQuadgrams, 1, getAccuracy)

print("Top 1 Success: ")
print(numTop1Success)
print(numTrials)
print(paste(c("Accuracy: ", (numTop1Success/numTrials))))

print("")
print("Top 5 Success: ")
print(numTop5Success)
print(numTrials)
print(paste(c("Accuracy: ", (numTop5Success/numTrials))))

