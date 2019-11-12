source("https://raw.githubusercontent.com/UKGANG/IST-687/master/report/Project_Code.R", echo=FALSE)

# Prepare for sentenment dictionaries
pos <- "~/Desktop/classes/IST/687/assessment/week11/opinion-lexicon-English/positive-words.txt"
neg <- "~/Desktop/classes/IST/687/assessment/week11/opinion-lexicon-English/negative-words.txt"
p <- scan(pos, character(0), sep = "\n")
n <- scan(neg, character(0), sep = "\n")
p <- p[-1:-34]
n <- n[-1:-34]

countSentmentWords <- function(dataset, corpusDataset) {
  matched <- match(dataset, corpusDataset, nomatch = 0)
  mCounts <- wordCounts[which(matched != 0)]
  length(mCounts)
  ## for log purpuse
  mWords <- names(mCounts)
  cnt <- sum(mCounts)
  return (cnt)
}

installLibrary("tm")
installLibrary("tidyverse")

wordTable <- rawData$Flight.freeText %>% 
  na.omit() %>% 
  VectorSource() %>% 
  Corpus() %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  TermDocumentMatrix()

posCnt <- countSentmentWords(wordTable, p)
negCnt <- countSentmentWords(wordTable, n)
sentmentAnalyticResult <- c(posCnt, negCnt)
sentmentAnalyticResult <- data.frame(count=sentmentAnalyticResult)
rownames(sentmentAnalyticResult) <- c("Positive Word", "Negative Word")

head(sentmentAnalyticResult)