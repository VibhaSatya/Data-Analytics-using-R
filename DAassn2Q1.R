data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")
#population
d <- density(data$Total)
plot(d,xlab="Total",main="Population")
 
#simple random sampling
tot <- 0.7 * max(data["Number"])
simple <- subset(data, Number<=tot)
d <- density(simple$Total)
plot(d,xlab="Total",main="Simple Random Sample")
 

#systematic sampling
sys <- subset(data, (Number%%3)==1)
d <- density(sys$Total)
plot(d,xlab="Total",main="Systematic Sample")
 

#stratified
strata1 <- subset(data, Pr_Male>=0.5)
strata2 <- subset(data, Pr_Male<0.5)
d <- density(strata1$Total)
plot(d,xlab="Total",main="Stratified (Male)")
d <- density(strata2$Total)
plot(d,xlab="Total",main="Stratified (Female)")

#clustering
types <- unique(data["Type_1"])
num <- 0.7* nrow(types)
count <- 1
clust = c()
while(count<=num)
{
d <- subset(data, Type_1==types[count,1])
i<-1
while(i<=nrow(d["Total"]))
{
clust[i] <- d[i,"Total"]
i <- i+1
}
count<-count+1
}
d <- density(clust)
plot(d,xlab="Total",main="Clustering (70% of types)")