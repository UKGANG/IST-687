echo = F
source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/Data_Cleaner.R")
installLibrary("rattle")
installLibrary("rpart.plot")
installLibrary("RColorBrewer")
installLibrary("lubridate")
rawData$weekDay <- wday(strptime(rawData$Flight.Date, "%m/%d/%y"));
partnerFilter <- function(data, type) {
  return (filter(data, Flight.Ticket.Partner.Code %in% type));
}

type <- c(""
          , "WN"
          , "DL"
          # , "OO"
          , "EV"
          , "OU"
          , "US"
          , "AA"
          , "MQ"
          , "B6"
          , "AS"
          , "FL"
          , "F9"
          , "VX"
          , "HA"
);

rawData <- partnerFilter(rawData, type)

# Create age bins
# set up cut-off values 
breaks <- c(0, 8, 10)
# specify interval/bin labels
tags <- c("Other","Promoter")
# bucketing values into bins
rawData$nps <- cut(rawData$Recommend.Likelihood, 
                   breaks=breaks, 
                   include.lowest=TRUE, 
                   right=FALSE, 
                   labels=tags)

trainList <- createDataPartition(y=rawData$Recommend.Likelihood, p=.67, list=F)

trainSet <- rawData[trainList,]
testSet <- rawData[-trainList,]
categorizedTable <- rawData %>% 
  group_by(Flight.Travel.Type, Flight.Cabin.Class, Flight.Airline.Membership.Class) %>% 
  summarize(
    median = median(Recommend.Likelihood)
    , stdev = sd(Recommend.Likelihood)
    , n = n()) %>% 
  mutate(freq = n/dim(rawData)[1]) %>% 
  arrange(desc(n))

categorizedTable

calculateAccuracy <- function(model) {
  predictedNps <- as.factor(predict(scoreTree, newdata=testSet, type = "prob")[,1] > 0.5)
  
  actualNps <- as.factor(testSet$nps == "Promoter") 
  confMatrix <- table(predictedNps,actualNps) 
  confMatrix
  
  accuracy <- sum(diag(confMatrix)) / sum(confMatrix)
  return (accuracy);
}

treeData <- select(rawData, c("Flight.Travel.Type", "Flight.Cabin.Class", "Flight.Airline.Membership.Class", "Recommend.Likelihood"))

rpartExp <- nps ~ 
  # Geom.Arrival.Airport.Longitude +
  # Geom.Arrival.Airport.Latitude +
  # Geom.Departure.Airport.Longitude +
  # Geom.Departure.Airport.Latitude +
  
  # Personal information
  Person.Age + 
  Person.Gender + 
  Person.Loyalty + 
  Person.Price.Sensitivity + 
  Person.First.Flight.Year + 
  Person.Flights.Per.Year + 
  Person.Total.Freq.Flyer.Accts + 
  
  # Current Flight timing information
  Flight.Departure.Delay.Minute +
  Flight.Departure.Scheduled.Hour +
  Flight.Arrival.Delay.Minute +
  Flight.Time.Minutes +
  Flight.Distance.Cnt +
  Flight.Cancelled +
  # 
  # Cost Related Information
  Flight.Airport.Shopping.Amount +
  Flight.Airport.Food.Amount +
  Flight.Airline.Membership.Class +
  Flight.Travel.Type +
  Flight.Cabin.Class

scoreTree <- rpart(rpartExp, data=trainSet, method="class") 

# View(trainSet)

fancyRpartPlot(scoreTree)

accuracy <- calculateAccuracy(scoreTree);

accuracy

## Personal traveller
trainSet <- filter(trainSet, Flight.Travel.Type == 'Personal Travel')

scoreTree <- rpart(rpartExp, data=trainSet, method="class") 

fancyRpartPlot(scoreTree)

accuracy <- calculateAccuracy(scoreTree);

accuracy
