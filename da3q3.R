data <- read.csv(file="F:/DA/business_rankings.csv", header=TRUE, sep=",")
data <- subset(data,data[2]<=20)


# library
library(ggplot2)
 
# create a dataset
specie=c()
condition=rep(c("Starting a Business" , "Dealing with Construction Permits" , "Protecting Minority Investors") , 20)
value = c()
i <- 1
count <- 1
while(i<=20)
{
value[count] <- data[i,3]
specie[count] <- toString(data[i,1])
count <- count+1
value[count] <- data[i,4]
specie[count] <- toString(data[i,1])
count <- count+1
value[count] <- data[i,5]
specie[count] <- toString(data[i,1])
count <- count+ 1
i <- i+1
}
value <- as.numeric(unlist(value))
data=data.frame(specie,condition,value)
 
# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
    geom_bar(position="dodge", stat="identity")  