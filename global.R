library(tm)
library(wordcloud)
library(memoise)

# The list of valid books
candidates <<- list("Donald Trump" = "Donald_Trump_Acceptance_Speech",
               "Bernie Sanders" = "Bernie_Sanders_Acceptance_Speech")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(candidate) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(candidate %in% candidates))
    stop("Unknown Candidate")
  
  text <- readLines(sprintf("./%s.txt", candidate),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"),  "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

