data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")

count <- 1
hp <- data["HP"]
attack <- data["Attack"]
defense <- data["Defense"]
spAtk <- data["Sp_Atk"]
spDef <- data["Sp_Def"]
speed <- data["Speed"]
tot <- data["Total"]
ht <- data["Height_m"]


#means 
hp1 <- mean(as.numeric(unlist(hp)))
attack1 <- mean(as.numeric(unlist(attack)))
defense1 <- mean(as.numeric(unlist(defense)))
spAtk1 <- mean(as.numeric(unlist(spAtk)))
spDef1 <- mean(as.numeric(unlist(spDef)))
speed1 <- mean(as.numeric(unlist(speed)))
tot1 <- mean(as.numeric(unlist(tot)))
ht1 <- mean(as.numeric(unlist(ht)))

#sd 
hp2 <- sd(as.numeric(unlist(hp)))
attack2 <- sd(as.numeric(unlist(attack)))
defense2 <- sd(as.numeric(unlist(defense)))
spAtk2 <- sd(as.numeric(unlist(spAtk)))
spDef2 <- sd(as.numeric(unlist(spDef)))
speed2 <- sd(as.numeric(unlist(speed)))
tot2 <- sd(as.numeric(unlist(tot)))
ht2 <- sd(as.numeric(unlist(ht)))

while(count<=max(data["Number"]))
{
#new range
hp[count] <- (hp[count] - hp1)/(hp2)
attack[count] <- (attack[count] - attack1)/(attack2)
defense[count] <- (defense[count] - defense1)/(defense2)
spAtk[count] <- (spAtk[count] - spAtk1)/(spAtk2)
spDef[count] <- (spDef[count] - spDef1)/(spDef2)
speed[count] <- (speed[count] - speed1)/(speed2)
tot[count] <- (tot[count] - tot1)/(tot2)
ht[count] <- (ht[count] - ht1)/(ht2)
count <- count+1
}
data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")

count <- 1
hp <- data["HP"]
attack <- data["Attack"]
defense <- data["Defense"]
spAtk <- data["Sp_Atk"]
spDef <- data["Sp_Def"]
speed <- data["Speed"]
tot <- data["Total"]
ht <- data["Height_m"]


#means 
hp1 <- mean(as.numeric(unlist(hp)))
attack1 <- mean(as.numeric(unlist(attack)))
defense1 <- mean(as.numeric(unlist(defense)))
spAtk1 <- mean(as.numeric(unlist(spAtk)))
spDef1 <- mean(as.numeric(unlist(spDef)))
speed1 <- mean(as.numeric(unlist(speed)))
tot1 <- mean(as.numeric(unlist(tot)))
ht1 <- mean(as.numeric(unlist(ht)))

#sd 
hp2 <- sd(as.numeric(unlist(hp)))
attack2 <- sd(as.numeric(unlist(attack)))
defense2 <- sd(as.numeric(unlist(defense)))
spAtk2 <- sd(as.numeric(unlist(spAtk)))
spDef2 <- sd(as.numeric(unlist(spDef)))
speed2 <- sd(as.numeric(unlist(speed)))
tot2 <- sd(as.numeric(unlist(tot)))
ht2 <- sd(as.numeric(unlist(ht)))

while(count<=max(data["Number"]))
{
#new range
hp[count] <- (hp[count] - hp1)/(hp2)
attack[count] <- (attack[count] - attack1)/(attack2)
defense[count] <- (defense[count] - defense1)/(defense2)
spAtk[count] <- (spAtk[count] - spAtk1)/(spAtk2)
spDef[count] <- (spDef[count] - spDef1)/(spDef2)
speed[count] <- (speed[count] - speed1)/(speed2)
tot[count] <- (tot[count] - tot1)/(tot2)
ht[count] <- (ht[count] - ht1)/(ht2)
count <- count+1
}

d1 <- density(as.numeric(unlist(hp)))
d2 <- density(as.numeric(unlist(attack)))
d3 <- density(as.numeric(unlist(defense)))
d4 <- density(as.numeric(unlist(spAtk)))
d5 <- density(as.numeric(unlist(spDef)))
d6 <- density(as.numeric(unlist(speed)))
d7 <- density(as.numeric(unlist(tot)))
d8 <- density(as.numeric(unlist(ht)))

plot(d1,xlab="Attributes",main="Transformed values after range conversion",col="blue")
data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")

count <- 1
hp <- data["HP"]
attack <- data["Attack"]
defense <- data["Defense"]
spAtk <- data["Sp_Atk"]
spDef <- data["Sp_Def"]
speed <- data["Speed"]
tot <- data["Total"]
ht <- data["Height_m"]


#means 
hp1 <- mean(as.numeric(unlist(hp)))
attack1 <- mean(as.numeric(unlist(attack)))
defense1 <- mean(as.numeric(unlist(defense)))
spAtk1 <- mean(as.numeric(unlist(spAtk)))
spDef1 <- mean(as.numeric(unlist(spDef)))
speed1 <- mean(as.numeric(unlist(speed)))
tot1 <- mean(as.numeric(unlist(tot)))
ht1 <- mean(as.numeric(unlist(ht)))

#sd 
hp2 <- sd(as.numeric(unlist(hp)))
attack2 <- sd(as.numeric(unlist(attack)))
defense2 <- sd(as.numeric(unlist(defense)))
spAtk2 <- sd(as.numeric(unlist(spAtk)))
spDef2 <- sd(as.numeric(unlist(spDef)))
speed2 <- sd(as.numeric(unlist(speed)))
tot2 <- sd(as.numeric(unlist(tot)))
ht2 <- sd(as.numeric(unlist(ht)))

while(count<=max(data["Number"]))
{
#new range
hp[count] <- (hp[count] - hp1)/(hp2)
attack[count] <- (attack[count] - attack1)/(attack2)
defense[count] <- (defense[count] - defense1)/(defense2)
spAtk[count] <- (spAtk[count] - spAtk1)/(spAtk2)
spDef[count] <- (spDef[count] - spDef1)/(spDef2)
speed[count] <- (speed[count] - speed1)/(speed2)
tot[count] <- (tot[count] - tot1)/(tot2)
ht[count] <- (ht[count] - ht1)/(ht2)
count <- count+1
}
data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")

count <- 1
hp <- data["HP"]
attack <- data["Attack"]
defense <- data["Defense"]
spAtk <- data["Sp_Atk"]
spDef <- data["Sp_Def"]
speed <- data["Speed"]
tot <- data["Total"]
ht <- data["Height_m"]


#means 
hp1 <- mean(as.numeric(unlist(hp)))
attack1 <- mean(as.numeric(unlist(attack)))
defense1 <- mean(as.numeric(unlist(defense)))
spAtk1 <- mean(as.numeric(unlist(spAtk)))
spDef1 <- mean(as.numeric(unlist(spDef)))
speed1 <- mean(as.numeric(unlist(speed)))
tot1 <- mean(as.numeric(unlist(tot)))
ht1 <- mean(as.numeric(unlist(ht)))

#sd 
hp2 <- sd(as.numeric(unlist(hp)))
attack2 <- sd(as.numeric(unlist(attack)))
defense2 <- sd(as.numeric(unlist(defense)))
spAtk2 <- sd(as.numeric(unlist(spAtk)))
spDef2 <- sd(as.numeric(unlist(spDef)))
speed2 <- sd(as.numeric(unlist(speed)))
tot2 <- sd(as.numeric(unlist(tot)))
ht2 <- sd(as.numeric(unlist(ht)))

while(count<=max(data["Number"]))
{
#new range
hp[count] <- (hp[count] - hp1)/(hp2)
attack[count] <- (attack[count] - attack1)/(attack2)
defense[count] <- (defense[count] - defense1)/(defense2)
spAtk[count] <- (spAtk[count] - spAtk1)/(spAtk2)
spDef[count] <- (spDef[count] - spDef1)/(spDef2)
speed[count] <- (speed[count] - speed1)/(speed2)
tot[count] <- (tot[count] - tot1)/(tot2)
ht[count] <- (ht[count] - ht1)/(ht2)
count <- count+1
}

d1 <- density(as.numeric(unlist(hp)))
d2 <- density(as.numeric(unlist(attack)))
d3 <- density(as.numeric(unlist(defense)))
d4 <- density(as.numeric(unlist(spAtk)))
d5 <- density(as.numeric(unlist(spDef)))
d6 <- density(as.numeric(unlist(speed)))
d7 <- density(as.numeric(unlist(tot)))
d8 <- density(as.numeric(unlist(ht)))

plot(d1,xlab="Attributes",main="Transformed values after range conversion",col="blue")
lines(d2,col="green")
data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")

count <- 1
hp <- data["HP"]
attack <- data["Attack"]
defense <- data["Defense"]
spAtk <- data["Sp_Atk"]
spDef <- data["Sp_Def"]
speed <- data["Speed"]
tot <- data["Total"]
ht <- data["Height_m"]


#means 
hp1 <- mean(as.numeric(unlist(hp)))
attack1 <- mean(as.numeric(unlist(attack)))
defense1 <- mean(as.numeric(unlist(defense)))
spAtk1 <- mean(as.numeric(unlist(spAtk)))
spDef1 <- mean(as.numeric(unlist(spDef)))
speed1 <- mean(as.numeric(unlist(speed)))
tot1 <- mean(as.numeric(unlist(tot)))
ht1 <- mean(as.numeric(unlist(ht)))

#sd 
hp2 <- sd(as.numeric(unlist(hp)))
attack2 <- sd(as.numeric(unlist(attack)))
defense2 <- sd(as.numeric(unlist(defense)))
spAtk2 <- sd(as.numeric(unlist(spAtk)))
spDef2 <- sd(as.numeric(unlist(spDef)))
speed2 <- sd(as.numeric(unlist(speed)))
tot2 <- sd(as.numeric(unlist(tot)))
ht2 <- sd(as.numeric(unlist(ht)))

while(count<=max(data["Number"]))
{
#new range
hp[count] <- (hp[count] - hp1)/(hp2)
attack[count] <- (attack[count] - attack1)/(attack2)
defense[count] <- (defense[count] - defense1)/(defense2)
spAtk[count] <- (spAtk[count] - spAtk1)/(spAtk2)
spDef[count] <- (spDef[count] - spDef1)/(spDef2)
speed[count] <- (speed[count] - speed1)/(speed2)
tot[count] <- (tot[count] - tot1)/(tot2)
ht[count] <- (ht[count] - ht1)/(ht2)
count <- count+1
}
data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")

count <- 1
hp <- data["HP"]
attack <- data["Attack"]
defense <- data["Defense"]
spAtk <- data["Sp_Atk"]
spDef <- data["Sp_Def"]
speed <- data["Speed"]
tot <- data["Total"]
ht <- data["Height_m"]


#means 
hp1 <- mean(as.numeric(unlist(hp)))
attack1 <- mean(as.numeric(unlist(attack)))
defense1 <- mean(as.numeric(unlist(defense)))
spAtk1 <- mean(as.numeric(unlist(spAtk)))
spDef1 <- mean(as.numeric(unlist(spDef)))
speed1 <- mean(as.numeric(unlist(speed)))
tot1 <- mean(as.numeric(unlist(tot)))
ht1 <- mean(as.numeric(unlist(ht)))

#sd 
hp2 <- sd(as.numeric(unlist(hp)))
attack2 <- sd(as.numeric(unlist(attack)))
defense2 <- sd(as.numeric(unlist(defense)))
spAtk2 <- sd(as.numeric(unlist(spAtk)))
spDef2 <- sd(as.numeric(unlist(spDef)))
speed2 <- sd(as.numeric(unlist(speed)))
tot2 <- sd(as.numeric(unlist(tot)))
ht2 <- sd(as.numeric(unlist(ht)))

while(count<=max(data["Number"]))
{
#new range
hp[count] <- (hp[count] - hp1)/(hp2)
attack[count] <- (attack[count] - attack1)/(attack2)
defense[count] <- (defense[count] - defense1)/(defense2)
spAtk[count] <- (spAtk[count] - spAtk1)/(spAtk2)
spDef[count] <- (spDef[count] - spDef1)/(spDef2)
speed[count] <- (speed[count] - speed1)/(speed2)
tot[count] <- (tot[count] - tot1)/(tot2)
ht[count] <- (ht[count] - ht1)/(ht2)
count <- count+1
}

d1 <- density(as.numeric(unlist(hp)))
d2 <- density(as.numeric(unlist(attack)))
d3 <- density(as.numeric(unlist(defense)))
d4 <- density(as.numeric(unlist(spAtk)))
d5 <- density(as.numeric(unlist(spDef)))
d6 <- density(as.numeric(unlist(speed)))
d7 <- density(as.numeric(unlist(tot)))
d8 <- density(as.numeric(unlist(ht)))

plot(d1,xlab="Attributes",main="Transformed values after range conversion",col="blue")
lines(d2,col="green")
lines(d3,col="red")
lines(d4,col="yellow")
lines(d5,col="black")
lines(d6,col="brown")
lines(d7,col="pink")
lines(d8,col="purple")



