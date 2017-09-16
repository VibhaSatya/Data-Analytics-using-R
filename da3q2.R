data <- read.csv(file="F:DA/flights.csv", header=TRUE, sep=",")
planes <- read.csv(file="F:DA/airlines.csv", header=TRUE, sep=",")

i <- 1
j <- 2
max <- 1
x <- planes[1,1]
y <- planes[2,1]
while(i<=nrow(planes))
{
name <- planes[i,1]
print(name)
ret1 <- subset(data,AIRLINE==name)
ret1 <- unique(ret1["DESTINATION_AIRPORT"])
j <- i + 1
while(j<=nrow(planes))
{
name <- planes[j,1]
print(name)
ret2 <- subset(data,AIRLINE==name)
ret2 <- unique(ret2["DESTINATION_AIRPORT"])
num <-  nrow(unique(cbind(c(ret1),c(ret2)uni))

if(num >= max)
{
x <- planes[i,1] 
y <- planes[j,1]
}
j <- j+1

}

i <- i + 1
}

