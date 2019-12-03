source("https://raw.githubusercontent.com/UKGANG/IST-687/master/mungling/Data_Cleaner.R")
installLibrary("fastDummies")
installLibrary("lubridate")
rawData$year <- year(strptime(rawData$Flight.Date, "%m/%d/%y"));
rawData$month <- month(strptime(rawData$Flight.Date, "%m/%d/%y"));
rawData$day <- day(strptime(rawData$Flight.Date, "%m/%d/%y"));
rawData$weekDay <- wday(strptime(rawData$Flight.Date, "%m/%d/%y"));
rawData <- arrange(rawData, desc(rawData$month))
rawData <- dummy_columns(rawData, "month", remove_first_dummy = T)
rawData <- arrange(rawData, desc(rawData$weekDay))
rawData <- dummy_columns(rawData, "weekDay")

rawData$January <- rawData[,"month_1"]
rawData$February <- rawData[,"month_2"]
rawData$Monday <- rawData[,"weekDay_1"]
rawData$Tuesday <- rawData[,"weekDay_2"]
rawData$Wednesday <- rawData[,"weekDay_3"]
rawData$Thursday <- rawData[,"weekDay_4"]
rawData$Friday <- rawData[,"weekDay_5"]
rawData$Saturday <- rawData[,"weekDay_6"]
rawData$Sunday <- rawData[,"weekDay_7"]

rawData <- select(rawData, -c("month_1", "month_2"
                              , "weekDay_1", "weekDay_2"
                              , "weekDay_3", "weekDay_4"
                              , "weekDay_5", "weekDay_6"
                              , "weekDay_7"))

