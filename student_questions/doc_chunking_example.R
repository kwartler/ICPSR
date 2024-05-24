

# Define the get_chunks function
get_chunks <- function(filePath, wordChunkSize = 100, chunkOverlap = 10) {
  
  library(tm)
  library(stringr)
  
  # Load the text data
  text_data <- readLines(filePath)
  
  # Convert to Corpus
  text_corpus <- Corpus(VectorSource(text_data))
  
  # Preprocess the text (lowercase, stopwords, punctuation, numbers, whitespace)
  text_corpus_clean <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus_clean <- tm_map(text_corpus_clean, removeWords, stopwords("english"))
  text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
  text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
  text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)
  
  # Convert back to text
  text_clean <- sapply(text_corpus_clean, as.character)
  text_clean <- paste(text_clean, collapse = ' ')
  
  # Split into words
  words <- unlist(str_split(text_clean, "\\s+"))
  
  # Define size and overlap
  chunk_size <- wordChunkSize
  overlap <- chunkOverlap  # 10% of 100 = 10 words overlap
  
  # Initialize chunks list
  chunks <- list()
  
  # Loop over words in steps of (chunk_size - overlap)
  for (i in seq(1, length(words), by = chunk_size - overlap)) {
    # Make sure not to exceed word vector length
    if((i + chunk_size - 1) > length(words)) {
      break;
    }
    chunk <- words[i:(i + chunk_size - 1)]
    chunk <- paste(chunk, collapse = " ")
    chunk <- data.frame(fileName = unlist(lapply(strsplit(filePath, '/'),tail, 1)),
                        chunk  = chunk,
                        row.names = NULL)
    
    chunks[[length(chunks) + 1]] <- chunk
  }
  
  chunks <- do.call(rbind, chunks)
  # Return chunks
  return(chunks)
}



# Usage:
 chunks <- get_chunks("~/Desktop/ICPSR/ICPSR_Day3/data/AutoAndElectronics/rec.autos/101551.txt")
 
tmp <-  list.files(path = '~/Desktop/ICPSR/ICPSR_Day3/data/AutoAndElectronics/rec.autos',
                   
            pattern = '.txt', 
            full.names = T)



tmpList <- list()
for(i in 10:20){
  print(i)
  tmpList[[i]]<-  get_chunks(tmp[i])
}
tmpDF <- do.call(rbind, tmpList)
