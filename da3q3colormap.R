library(rworldmap)
data <- read.csv(file="F:/DA/business_rankings.csv", header=TRUE, sep=",")
data <- subset(data,data[2]<=20)


theCountries <- c()
i <- 1
while(i<=20)
{
theCountries[i] <- toString(data[i,1])
i <- i + 1
}

DF <- data.frame(country = theCountries,
  colour = as.numeric(unlist(data[2])))

Map <- joinCountryData2Map(DF, joinCode = "NAME",
  nameJoinColumn = "country")


mapCountryData(Map, nameColumnToPlot="colour", catMethod = "categorical",
  missingCountryCol = gray(.8), mapTitle ="Ease of Doing Business Rank")
