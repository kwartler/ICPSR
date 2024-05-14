#' Title: Intro: TidyText Sentiment
#' Purpose: Sentiment nonsense
#' Author: Ted Kwartler
#' Date: May 12, 2024
#'


# Libs
library(tidytext)
library(dplyr)
library(tm)
library(radarchart)
library(textdata)
library(ggplot2)
library(tidyr)

# Data
filePathA <- '~/Desktop/ICPSR/ICPSR_Day3/data/goldmanSachs_2023_3k.csv'
filePathB <- '~/Desktop/ICPSR/ICPSR_Day3/data/BARCLAYS_BANK_DELAWARE_2023_3k.csv'

# Custom Functions
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Sub Function
complaintSubstitutions <- function(narrativeVector){
  x <- gsub('(X{2}\\/X{2}\\/X{4})|(X{2}\\/X{2}\\/[0-9]{2,4})|([0-9]{2}\\/[0-9]{2}\\/[0-9]{2,4})', 'REDACTED_DATE', narrativeVector, perl = T)
  x <- gsub('(X{2}\\/X{2}\\/X{4})|(X{2}\\/X{2}\\/[0-9]{2,4})|([0-9]{2}\\/[0-9]{2}\\/[0-9]{2,4})', 'REDACTED_DATE', x, perl = T)
  x <- gsub('X+', 'REDACTED', x)
  return(x)
}

# Create custom stop words
stops <- c(stopwords('english'), 'redacteddate', 'redacted')

# Read in files


# End