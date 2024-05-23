
# libs
library(dplyr)
library(httr)
library(jsonlite)

txtFiles <- list.files(path = '~/Desktop/ICPSR/student_questions/Skip',
                       pattern = '*.txt',
                       full.names = T)

allTxt <- lapply(txtFiles, readLines)

# Extract relevant section from each doc
documentLocations <- list()
for(i in 1:length(allTxt)){
  
  # Get one doc
  x <- allTxt[[i]]
  
  # Find the section
  st <- grep('c. Torture and Cruel', x)
  en <- grep('d. Arbitrary Arrest, Detention, or Exile', x)
  
  # ID torture lines
  tortureID <- grep('torture', x)
  
  tortureDescription <- grep('The types of torture include|The types of torture alleged include:', x)
  
  # Keep track of errors
  if(length(st)==0){st <- 'no st section found'}
  if(length(en)==0){en <- 'no en section found'}
  if(length(tortureID)==0){tortureID <- 'no tortureID found'}
  if(length(tortureDescription)==0){tortureDescription <- 'no tortureDescription found'}
  
  response <- data.frame(
    filePath = txtFiles[i],
    startSectionLine = st,
    endSectionLine = en,
    tortureDescription = tortureDescription,
    tortureMentions = paste(tortureID, collapse = ','))
  
  documentLocations[[i]] <- response
  print(i)
}

# Organize the information and save it
documentLocations <- do.call(rbind, documentLocations)
write.csv(documentLocations, 
          '~/Desktop/ICPSR/student_questions/Skip/documentLocations.csv', 
          row.names = F)

# Using the saved info let's extract the information of interest
goodDocs <- subset(documentLocations, documentLocations$startSectionLine != 'no st section found')

# Extract each chunk
allChunks <- list()
for(i in 1:length(allTxt)){
  print(paste('working on doc',txtFiles[i]))
  st <- documentLocations$startSectionLine[i]
  en <- documentLocations$endSectionLine[i]
  x  <- allTxt[[i]]
  docChunk <- x[st:en]
  docChunk <- paste(docChunk, collapse = '\n')
  
  allChunks[[i]] <- data.frame(filePath = txtFiles[i],
                               docChunk, 
                               row.names = NULL)
}

# Save it along the way with the chunk as a column
allChunks <- do.call(rbind, allChunks)
consolidatedInfo <- left_join(documentLocations, allChunks, by = 'filePath')
write.csv(consolidatedInfo, 
          '~/Desktop/ICPSR/student_questions/Skip/consolidatedInfo.csv', 
          row.names = F)

# Now let's see if we can use the logic of a LLM to understand who, verb, who relationship
# llmModel <- 'lmstudio-ai/gemma-2b-it-GGUF' # too dumb to figure out the task
llmModel <- 'lmstudio-community/Meta-Llama-3-8B-Instruct-GGUF'
headers <- c(`Content-Type` = "application/json")

# Apply to each doc
llmExtraction <- list()
for(i in 1:nrow(consolidatedInfo)){
  print(paste('working on',i, 'out of',nrow(consolidatedInfo)))
  # Organize Request
  dataLLM <- list(model = llmModel,
                  messages = list(
                    list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant helping a professor to extract information from State Department data. You always fulfill the user's requests to the best of your ability.  As a professor doing research I need have document chunks regarding alleged torture.  I would like to extract who the perpetrator of the torture is, a description of the torture, and who it was perpetrated upon.  For example after reviewing a document, you would respond in markdown:\n
                       \n```\n
                       # Perp: Police\n
                       # Type: Excessive Force\n
                       # Victim: Female.\n\n
                       \n```\n
                       Here is the new document chunk, please extract the offending party, actions taken and describe the victim:\n"),
                    list(role = "user", content = consolidatedInfo$docChunk[i])),
                  temperature = 0.7,
                  max_tokens = 512,
                  stream = FALSE)
  # Request header
  res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                    httr::add_headers(.headers = headers),
                    body = toJSON(dataLLM, auto_unbox = TRUE))
  
  # Extract the response
  llmResponse <- httr::content(res)$choices[[1]]$message$content
  cat(llmResponse)
  
  rawLLM <- capture.output(cat(llmResponse))
  rawLLM <- rawLLM[nchar(rawLLM)>0]
  llmExtraction[[i]]<-rawLLM
  
  idx <- grep('[#]',rawLLM)
  #if()
  response <- data.frame(perp = rawLLM,
                         type = ,
                         victim = ,
                         row.names = NULL)
}

# Save the raw outputs in case more data munging is needed
saveRDS(llmExtraction,'~/Desktop/ICPSR/personalFiles/llmExtraction.rds')

# final organization
for(i in 1:length())

# End
