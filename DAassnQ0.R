data <- read.csv(file="F:/deliveries.csv", header=TRUE, sep=",")
ret <- subset(data, match_id == "577" & batting_team == "Royal Challengers Bangalore")
l <- c()
 i <- 1
 while(i<=20)
 {
 sub <- subset(ret, over==i)
 l[i] <- colSums(sub["total_runs"])
 i <- i + 1
 }
 barplot(l, xlab ="Overs",ylab ="Number of runs",names.arg=c(1:20),col=c(1:20),main="RCB: no of runs per over")
colSums(ret["total_runs"])

colSums(ret["total_runs"])/20

