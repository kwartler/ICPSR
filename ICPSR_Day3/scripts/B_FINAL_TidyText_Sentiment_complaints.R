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
library(lubridate)

# Data list.files()
filePathA <- 'https://raw.githubusercontent.com/kwartler/ICPSR/main/ICPSR_Day3/data/goldmanSachs_2023_3k.csv'
filePathB <- 'https://raw.githubusercontent.com/kwartler/ICPSR/main/ICPSR_Day3/data/BARCLAYS_BANK_DELAWARE_2023_3k.csv'
txtFiles  <- c(filePathA, filePathB)

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

# Read in as a list
all <- lapply(txtFiles,read.csv)

# This could be made more concise but we're going to do it within a loop
cleanTibbles <- list()
for(i in 1:length(all)){
  x <- VCorpus(VectorSource(all[[i]])) #declare as a corpus
  x <- cleanCorpus(x, stops) #clean each corpus
  x <- DocumentTermMatrix(x) #make a DTM
  x <- tidy(x) #change orientation
  x$document <- txtFiles[i]
  cleanTibbles[[txtFiles[i]]] <- x #put it into the list
  print(paste('complete with',i))
}

# Organize into a single tibble
allText <- do.call(rbind, cleanTibbles)

# Let's use BING
bing <- get_sentiments(lexicon = c("bing"))

# Perform the join
bingSent <- inner_join(allText,
                       bing, 
                       by=c('term'='word'))

# Now examine overall in the year  - Raw and proportion by doc and sentiment
bingSent %>%
  group_by(document, sentiment) %>%
  summarise(count = sum(count)) %>%
  mutate(proportion = count / sum(count))


# Let's try the AFINN
afinn <- get_sentiments(lexicon = c("afinn")) 

# Perform the join
afinnSent <- inner_join(allText,
                        afinn, 
                        by=c('term'='word'))

# Examine overall in the year  - sum values
afinnSent %>%
  group_by(document) %>%
  mutate(sentTotal = count * value) %>%
  summarise(sentTotal = sum(sentTotal)) 

# Examine overall in the year  - keeping pos/neg separate
afinnPosNeg <- afinnSent %>%
  group_by(document, sentiment = ifelse(value < 0, "negative", "positive")) %>%
  mutate(sentTotal = count * value)%>%
  summarise(sentTotal = sum(sentTotal)) %>%
  mutate(proportion = sentTotal / sum(abs(sentTotal)))
afinnPosNeg

## Now let's examine it temporally
# This could be made more concise but we're going to do it within a loop
cleanMonthlyTibbles <- list()
for(i in 1:length(all)){
  print(paste('starting', txtFiles[i]))
  # Separate 
  oneSetComplaints <- all[[i]]
  
  # Calculate the month
  oneSetComplaints$date  <- ymd(oneSetComplaints$Date.received)
  oneSetComplaints$month <- month.abb[month(oneSetComplaints$date)]
  
  # Retain the columns of interest
  monthlyList <- list()
  for(j in 1:length(unique(oneSetComplaints$month))){
    print(paste('working on',unique(oneSetComplaints$month)[j]))
    x <- subset(oneSetComplaints,month == unique(oneSetComplaints$month)[j])
    x <- VCorpus(VectorSource(x$Consumer.complaint.narrative)) 
    x <- cleanCorpus(x, stops) 
    x <- DocumentTermMatrix(x) 
    x <- tidy(x) 
    x$month <- unique(oneSetComplaints$month)[j]
    x$topic <- txtFiles[i]
    monthlyList[[j]] <- x
  }
  
  # Flatten
  monthlyList <- do.call(rbind, monthlyList)
  
  # Append to the top level list
  cleanMonthlyTibbles[[i]] <- monthlyList 
}

# Now we can flatten and explore sentiment by month
cleanMonthlySent<- do.call(rbind, cleanMonthlyTibbles)
cleanMonthlySent

# Group by topic, month and sentiment
posNegMonth <- cleanMonthlySent %>% 
  inner_join(bing, by=c('term'='word')) %>%
  group_by(topic, month, sentiment)%>%
  summarise(count = sum(count)) %>%
  mutate(proportion = count / sum(count))
posNegMonth

# Change the factor order instead of alpha
posNegMonth$month <- factor(posNegMonth$month, levels = month.abb)

ggplot(posNegMonth, aes(x = month, y = count, fill = topic)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count by Month", x = "Month", y = "Count")

# Finally let's explore as a radar chart using NRC
nrc <- lexicon_nrc()

# 
x <- lapply(cleanMonthlyTibbles, inner_join, nrc, by = c('term' = 'word'))


# End