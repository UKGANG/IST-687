source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/Data_Cleaner.R")

View(rawData)
rawData$commented <- as.numeric(!is.na(rawData$Flight.freeText))
comments <- na.omit(rawData$Flight.freeText)

View(comments)
installLibrary("tm")
installLibrary("tidyverse")
tdMatrix <- comments %>% 
  VectorSource() %>% 
  Corpus() %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  TermDocumentMatrix()

inspect(tdMatrix)

# Find the most frequent(80 percentile) word among documents, sort in descending order
wordCounts <- tdMatrix %>% 
  as.matrix() %>% 
  rowSums() %>% 
  sort(decreasing = T)
criticalWordCounts <- wordCounts[which(wordCounts > quantile(wordCounts, c(0.99))[[1]])]
head(criticalWordCounts)

# Get the actual words
criticalWords <- names(criticalWordCounts)
head(criticalWords)

# Visualize the word cloud
installLibrary("ggplot")
installLibrary("wordcloud")
criticalCloudFrame <- data.frame(word = criticalWords, freq = criticalWordCounts)
View(criticalCloudFrame)
ggplot(data = criticalCloudFrame) + 
  aes(x=reorder(word, freq), y=freq) + 
  geom_point() + 
  theme(axis.text.x=element_text(angle=90, hjust=1))

set.seed(777)
str(wordcloud(criticalCloudFrame$word, criticalCloudFrame$freq, colors = brewer.pal(8, "Dark2")))
View(criticalCloudFrame)