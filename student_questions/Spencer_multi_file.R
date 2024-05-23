tmpFiles <- list.files(path = '~/Desktop/ICPSR/ICPSR_Day3/data/AutoAndElectronics/rec.autos',
                       pattern = '*.txt',
                       full.names = T)

allPosts <- lapply(tmpFiles, readLines)

findKeywords <- list()
for(i in 1:length(allPosts)){
  x <- allPosts[[i]]
  x <- paste(x, collapse = ' ')
  x <- grepl('car', x, ignore.case = T)
  findKeywords[[i]] <- x
}
unlist(findKeywords)
