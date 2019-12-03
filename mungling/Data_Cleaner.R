# Utilities
'%!in%' <- function(x,y)!('%in%'(x,y))
installLibrary <- function(libraryName) {
  # Install package if not exists
  if (!require(libraryName, character.only = TRUE)) {
    install.packages(pkgs = libraryName);
    require(libraryName, character.only = TRUE);
  }
}
installLibrary("logging");
installLibrary("RCurl");
installLibrary("tidyverse");
installLibrary("jsonlite");
installLibrary("caret")

basicConfig();
logReset()
addHandler(writeToConsole)
loginfo("initializing...");
if (exists("echo") && !echo) {
  removeHandler("writeToConsole")
}

loginfo("Environment setup done. ");
# Data mungling
# Data type conversion index
doubleColumn <- c("Loyalty"
                  , "Shopping.Amount.at.Airport"
                  , "Eating.and.Drinking.at.Airport"
                  , "olong"
                  , "olat"
                  , "dlong"
                  , "dlat");
integerColumn <- c("Age"
                   , "Year.of.First.Flight"
                   , "Flights.Per.Year"
                   , "Total.Freq.Flyer.Accts"
                   , "Departure.Delay.in.Minutes"
                   , "Arrival.Delay.in.Minutes"
                   , "Flight.time.in.minutes"
                   , "Flight.Distance"
                   , "Likelihood.to.recommend");
dateColumn <- c("Flight.date");
stringColumn <- c("freeText"
                  , "Destination.City"
                  , "Origin.City"
                  , "Origin.State"
                  , "Destination.State"
                  , "Partner.Code"
                  , "Partner.Name"
);
factorColumn <- c("Gender"
                  , "Price.Sensitivity"
                  , "Airline.Status"
                  , "Type.of.Travel"
                  , "Class"
                  , "Scheduled.Departure.Hour"
                  , "Flight.cancelled"
                  , "Day.of.Month"
);

convert <- function(dataset, columnNames, func) {
  classType <- class(select(dataset, columnNames[1]) %>% pull() %>% func());
  map(columnNames, function(columnName) {
    dataset[columnName] <- select(dataset, columnName) %>% pull() %>% func();
  })
  loginfo(sprintf("Conversion complete on data type %s.", classType));
}

# Filling NA values
naFilter <- function(ds) {
  columnNames <- colnames(ds);
  selectedNAColumns <- c();
  for (columnName in columnNames) {
    if (length(ds[columnName][is.na(rawData[columnName])]) > 0) {
      selectedNAColumns <- c(selectedNAColumns, columnName);
    }
  }
  
  return (select(ds, selectedNAColumns));
}

strategy.meanStrategy <- function(ds) {
  loginfo("Displace all NAs with mean. ");
  tmpDs <- naFilter(ds);
  for (columnName in colnames(tmpDs)) {
    if ("freeText" == columnName) {
      next;
    }
    ds[is.na(ds[,columnName]),columnName] <- select(ds, columnName) %>% 
      pull() %>% 
      mean(na.rm = T);
  }
  
  return (ds);
}

strategy.modeStrategy <- function(ds) {
  loginfo("Displace all NAs with mode. ");
  tmpDs <- naFilter(ds);
  for (columnName in colnames(tmpDs)) {
    if ("freeText" == columnName) {
      next;
    }
    ds[is.na(ds[,columnName]),columnName] <- 
      select(ds, columnName) %>% 
      na.omit() %>% 
      table() %>% 
      as.data.frame() %>% 
      arrange(desc(Freq)) %>% 
      slice(1) %>% 
      pull(); 
  }
  
  return (ds);
}

strategy.medianStrategy <- function(ds) {
  loginfo("Displace all NAs with median");
  tmpDs <- naFilter(ds);
  for (columnName in colnames(tmpDs)) {
    if ("freeText" == columnName) {
      next;
    }
    
    ds[is.na(ds[,columnName]),columnName] <- select(ds, columnName) %>% 
      pull() %>% 
      median(na.rm = T);
  }
  
  return (ds);
}

strategy.omitStrategy <- function(ds) {
  loginfo("Omitting all NAs. ");
  return (na.omit(ds));
}

strategy.winsorizeStrategy <- function(ds, vectors=colnames(ds)) {
  #ectors <- nameVectors
  lowerFencePercentile <- ifelse(exists("strategy.winsorizeStrategy.lowerFencePercentile")
                                 , strategy.winsorizeStrategy.lowerFencePercentile, .02);
  upperFencePercentile <- ifelse(exists("strategy.winsorizeStrategy.upperFencePercentile")
                                 , strategy.winsorizeStrategy.upperFencePercentile, .98);
  loginfo("Winsorize outliers. ");
  installLibrary("DescTools");
  for (i in vectors) {
    dataType <- class(ds[, i]);
    if (!dataType %in% c("integer", "numeric")) {
      next;
    }
    lowerFence <- quantile(ds[, i], lowerFencePercentile);
    upperFence <-quantile(ds[, i], upperFencePercentile);
    if ("integer" == dataType) {
      lowerFence <- round(lowerFence);
      upperFence <- round(upperFence);
    }
    loginfo(sprintf("Winsorizing for column: %s", i));
    loginfo(sprintf("Lower fence: %.3f, percentile: %.3f", lowerFence, lowerFencePercentile));
    loginfo(sprintf("Upper fence: %.3f, percentile: %.3f", upperFence, upperFencePercentile));
    ds[i] <- Winsorize(ds[, i]
                       , minval = lowerFence
                       , maxval = upperFence
                       , probs = c(lowerFencePercentile, upperFencePercentile)
                       , na.rm = FALSE, type = 7);
  }
  
  return (ds);
}

na.handler <- function(ds, strategy) {
  tryCatch({
    loginfo("Start manipulating values. ");
    return (strategy(ds));
  }, finally = {
    loginfo("End manipulating values. ");
  })
}

loginfo("Function initialization complete! ", logger="")

# 1. Read data from dataset
dsUrl = "https://raw.githubusercontent.com/UKGANG/IST-687/master/dataset/fall2019-survey-M04.json";
rawData <- jsonlite::fromJSON(getURL(dsUrl));
loginfo(sprintf("JSON data loaded, row=%d, column=%d. "
                , dim(rawData)[1], dim(rawData)[2]));

convert(rawData, doubleColumn, as.numeric);
convert(rawData, integerColumn, as.integer);
convert(rawData, dateColumn, function (vec) {
  as.Date(vec, format="%m/%d/%y");
});
convert(rawData, stringColumn, as.character);
convert(rawData, factorColumn, as.factor);
loginfo("Data type conversion complete. ");

strategy.winsorizeStrategy.lowerFencePercentile <- 0.01
strategy.winsorizeStrategy.upperFencePercentile <- 0.98
# 2. NA values handling
# rawData <- na.handler(rawData, strategy = strategy.medianStrategy); 
# 3. Outliers handling
# rawData <- na.handler(rawData, strategy = strategy.winsorizeStrategy);

# 4. Renaming
originColNames <- colnames(rawData);
rawData <- mutate(rawData, rownum = row_number())
rawData <- rawData %>% mutate(
  #rownum
  rownum=rownum,
  
  # Geographic information
  Geom.Arrival.Airport.Longitude=dlong,
  Geom.Arrival.Airport.Latitude=dlat,
  Geom.Arrival.State=Destination.State,
  Geom.Arrival.State.Abbr=trimws(str_split(Destination.City, ",", simplify = T)[,2], which= "both"),
  Geom.Arrival.City.Label=trimws(str_split(Destination.City, ",", simplify = T)[,1], which= "both"),
  Geom.Departure.Airport.Longitude=olong, 
  Geom.Departure.Airport.Latitude=olat,
  Geom.Departure.State=Origin.State,
  Geom.Departure.State.Abbr=trimws(str_split(Origin.City, ",", simplify = T)[,2], which= "both"),
  Geom.Departure.City.Label=trimws(str_split(Origin.City, ",", simplify = T)[,1], which= "both"),
  
  # Personal information
  Person.Age=Age,
  Person.Gender=Gender,
  Person.Loyalty=Loyalty,
  Person.Price.Sensitivity=Price.Sensitivity,
  Person.First.Flight.Year=Year.of.First.Flight,
  Person.Flights.Per.Year=Flights.Per.Year,
  Person.Total.Freq.Flyer.Accts=Total.Freq.Flyer.Accts,
  
  # Current Flight timing information
  Flight.Departure.Delay.Minute=Departure.Delay.in.Minutes,
  Flight.Departure.Scheduled.Hour=Scheduled.Departure.Hour,
  Flight.Arrival.Delay.Minute=Arrival.Delay.in.Minutes,
  Flight.Time.Minutes=Flight.time.in.minutes,
  Flight.Distance.Cnt=Flight.Distance,
  Flight.Date=Flight.date,
  Flight.Cancelled=Flight.cancelled,
  
  # Cost Related Information
  Flight.Airport.Shopping.Amount=Shopping.Amount.at.Airport,
  Flight.Airport.Food.Amount=Eating.and.Drinking.at.Airport,
  Flight.Airline.Membership.Class=Airline.Status,
  Flight.Travel.Type=Type.of.Travel,
  Flight.Cabin.Class=Class,
  
  # Miscellaneous
  Flight.Ticket.Partner.Code=Partner.Code,
  Flight.Ticket.Partner.Name=Partner.Name,
  Flight.freeText=freeText,

  # Dependent variable
  Recommend.Likelihood=Likelihood.to.recommend
) %>% select(-originColNames);

convert(rawData, c("Geom.Arrival.State.Abbr", "Geom.Departure.State.Abbr"), as.factor);
loginfo("Factor column converted for [Geom.Arrival.State.Abbr] and [Geom.Departure.State.Abbr]. ");

loginfo("Data renaming complete. ");

# View(rawData);
loginfo("Data wrangling finished! ")