#' Demonstration of Linguistic KPI
#' TK
#' May 23, 2024
#' 
# Flesch Reading Ease: sentence length to syllable count; 0-100, avg=60-70 (higher is easier)
# Flesch-Kincaid Grade Level: average number of words per sentence and syllables per word; 0-12 grade level
# Automated Readability Index: average number of words per sentence and characters per word; 0-20 (lower is easier); 8-10 may be 13-15yrs old
# Simple Measure of Gobbledygook: sentence length and vocabulary difficulty: 0-100, avg=60-70 (higher is easier)
# Coleman-Liau: average sentence length and letter frequency 0-20, lower is easier

# Libs
#library(readability)
#devtools::install_git('https://github.com/trinker/readability')
library(sylcount)

# Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/ICPSR/main/ICPSR_Day3/data/BARCLAYS_BANK_DELAWARE_2023_3k.csv')

# Document info stats for the reading scores
tmp <- doc_counts(text$Consumer.complaint.narrative)
head(tmp)

# From the package:
#The function will have some difficulty on poorly processed and cleaned data. For example, if all punctuation is stripped out, then the number of sentences detected will always be zero. However, we do recommend removing quotes (single and double), as contractions can confuse the parser.

# chars the total numberof characters
# wordchars the number of alphanumeric characters
# words text tokens that are probably English language words
# nonwords text tokens that are probably not English language words
# sents the number of sentences recognized in the text
# sylls the total number of syllables (ignores all non-words)
# polys the total number of "polysyllables", or words with 3+ syllables
# re Flesch reading ease score
# gl Flesch-Kincaid grade level score
# ari Automatic Readability Index score
# smog Simple Measure of Gobbledygook (SMOG) score
# cl the Coleman-Liau Index score

linguisticKPI <- readability(text$Consumer.complaint.narrative)
head(linguisticKPI)
text$Consumer.complaint.narrative[3]

# End