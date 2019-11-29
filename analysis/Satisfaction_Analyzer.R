distRateMod <- lm(formula = Recommend.Likelihood~Flight.Time.Minutes, data = rawData)

plot(formula = Recommend.Likelihood~Flight.Time.Minutes, data = rawData)
abline(distRateMod)

plot(rawData$Flight.Time.Minutes, rawData$Recommend.Likelihood) 
abline(distRateMod)