# Libraries
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)
# Loading European map:     
map <- get_map(location = 'Europe', zoom = 4)

# Madrid coordinates
df <- data.frame(lon=c(-3.757324), lat=c(40.441721))

ggmap(map) + geom_point(data = df, aes(x = lon, y = lat))

#library(rworldmap)
#airports <- read.csv("http://openflights.svn.sourceforge.net/viewvc/openflights/openflights/data/airports.dat", header = FALSE)
#colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
#newmap <- getMap(resolution = "low")
#points(airports$lon, airports$lat, col = "red", cex = .6)
#plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)

data <- read.csv(file="F:DA/flights.csv", header=TRUE, sep=",")
data <- subset(data,data[2]<=20)
qmplot(lon, lat, data = data, colour = I('red'), size = I(3), darken = .3)
