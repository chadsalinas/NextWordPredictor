# -----------------------------------
# WordPrediction
# ui.R
# 
# author: Chad Salinas
# date:
# revision: 2.3
# -----------------------------------

library(shiny)

shinyUI(fluidPage(
  titlePanel("Next Word Predictor App"),
  headerPanel("Overview"),
  sidebarPanel(
    p("Next Word Predictor predicts up to 5 candidate next words based on the last 3 words of your partial phrase. "),
    p("As you type, our algorithm successively culls through prepared quadgrams, trigrams, and bigrams to proffer
      the 5 top choices for the next word." ),
    p("Those grams emanated from a carefully curated corpus of blog, twitter, and news data. ")
    ),
  mainPanel(
    h3("Enter a partial phrase below"),
    textInput("var", "           ... and let Next Word Predictor recommend up to 5 next words by probability: ", 
              value = "The answer is trivial and is left as an exercise for the...", width = "90%"),
    uiOutput("predictionTable"),
    br(),
    wellPanel(
      h4("Links"),
      HTML("<p> Github Repo <a href='https://github.com/' target='_blank'>https://github.com/</a></p>"),
      HTML("<p> Slide Deck Preso <a href='http://rpubs.com/' target='_blank'>http://rpubs.com/</a></p>"),
      h4("Author:"),
      p("Chad Salinas")
    )
  )
))