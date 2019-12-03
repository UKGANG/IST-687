echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/Data_Cleaner.R")

installLibrary("caret")
View(rawData)
treeModelDataset <- select(rawData, c(
  "Recommend.Likelihood"
  , "Flight.Cancelled"
  , "Flight.Distance.Cnt"
  # , "Flight.Time.Minutes"
  , "Flight.Travel.Type"
  ));
scoreTree <- train(Recommend.Likelihood ~ ., data=treeModelDataset, method="rpart") 
prp(scoreTree$finalModel, faclen = 0, cex = 0.8, extra = 1)
