data <- read.csv(file="F:/deliveries.csv", header=TRUE, sep=",")
matches <- read.csv(file="F:/matches.csv", header=TRUE, sep=",")
lions <- subset(matches,winner == "Gujarat Lions")
winID <- cbind(as.numeric(unlist(lions["id"])))


count <- 1
rtot = c()
ttot = c()
per = c()
id <- 1
while(id <= max(lions["id"]))
{
if(id == winID[count])
{
raina <- subset(data,match_id == id & batting_team=="Gujarat Lions" & batsman == "SK Raina" )
rtot[count] <- colSums(raina["total_runs"])

 
team <- subset(data,match_id == id & batting_team=="Gujarat Lions")
ttot[count] <- colSums(team["total_runs"])
 per[count] <- (rtot[count] / ttot[count])*100
count <- count + 1
}
id <- id + 1

}