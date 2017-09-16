data <- read.csv(file="F:/deliveries.csv", header=TRUE, sep=",")
matches <- read.csv(file="F:/matches.csv", header=TRUE, sep=",")
#1
id <- 1
inning = c()
batting_team =c()
total_runs =c()
count <- 1
while(id<=max(data["match_id"]))
{
match_id[count] <- count
ret <- subset(data, match_id == id & inning == 1)
total_runs[count] <- colSums(ret["total_runs"])
inning[count] <- 1
batting_team[count] <-toString(ret[1,3])

count <- count + 1
match_id[count] <-count
ret <- subset(data, match_id == id & inning == 2)
total_runs[count] <- colSums(ret["total_runs"])
inning[count] <- 2
batting_team[count] <-toString(ret[1,3])

count <- count + 1
id <- id + 1
}
output <- cbind(match_id,inning,batting_team,total_runs)
output

#2
ret <- subset(matches,dl_applied == 0 & result != "no result")
id <- cbind(as.numeric(unlist(ret["id"])))
total_runs = c()
batting_team =c()
i <- 1
count <- 1
while(i<=max(data["match_id"]))
{
if(i==id[count])
{

ret <- subset(data, match_id == i & inning == 1)
total_runs[count] <- colSums(ret["total_runs"])
batting_team[count] <-toString(ret[1,3])

count <- count + 1
ret <- subset(data, match_id == i & inning == 2)
total_runs[count] <- colSums(ret["total_runs"])
batting_team[count] <-toString(ret[1,3])
count <- count + 1
}
i <- i + 1
}
output <- cbind(batting_team,total_runs)
max <- subset(output, total_runs == max(as.numeric(unlist(total_runs))))
min <- subset(output, total_runs == min(as.numeric(unlist(total_runs))))
max
min

