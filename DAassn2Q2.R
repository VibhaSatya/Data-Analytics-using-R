data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")
###########grass##########################
grass <- subset(data, Type_1=="Grass")

maxHP <- max(grass["HP"])
minHP <- min(grass["HP"])

maxAttack <- max(grass["Attack"])
minAttack <- min(grass["Attack"])

maxDefense <- max(grass["Defense"])
minDefense <- min(grass["Defense"])

maxSpAtk <- max(grass["Sp_Atk"])
minSpAtk <- min(grass["Sp_Atk"])

maxSpDef <- max(grass["Sp_Def"])
minSpDef <- min(grass["Sp_Def"])

maxSpeed <- max(grass["Speed"])
minSpeed <- min(grass["Speed"])

maxTot <- max(grass["Total"])
minTot <- min(grass["Total"])

maxHt <- max(grass["Height_m"])
minHt <- min(grass["Height_m"])

count <- 1
hp <- grass["HP"]
attack <- grass["Attack"]
defense <- grass["Defense"]
spAtk <- grass["Sp_Atk"]
spDef <- grass["Sp_Def"]
speed <- grass["Speed"]
tot <- grass["Total"]
ht <- grass["Height_m"]

while(count<=max(grass["Number"]))
{
#normalizing
hp[count] <- (hp[count] - minHP)/(maxHP-minHP)
attack[count] <- (attack[count] - minAttack)/(maxAttack-minAttack)
defense[count] <- (defense[count] - minDefense)/(maxDefense-minDefense)
spAtk[count] <- (spAtk[count] - minSpAtk)/(maxSpAtk-minSpAtk)
spDef[count] <- (spDef[count] - minSpDef)/(maxSpDef-minSpDef)
speed[count] <- (speed[count] - minSpeed)/(maxSpeed-minSpeed)
tot[count] <- (tot[count] - minTot)/(maxTot-minTot)
ht[count] <- (ht[count] - minHt)/(maxHt-minHt)
count <- count+1
}

#means (grass)
hp1 <- mean(as.numeric(unlist(hp)))
attack1 <- mean(as.numeric(unlist(attack)))
defense1 <- mean(as.numeric(unlist(defense)))
spAtk1 <- mean(as.numeric(unlist(spAtk)))
spDef1 <- mean(as.numeric(unlist(spDef)))
speed1 <- mean(as.numeric(unlist(speed)))
tot1 <- mean(as.numeric(unlist(tot)))
ht1 <- mean(as.numeric(unlist(ht)))

###########fire##########################
fire <- subset(data, Type_1=="Fire")

maxHP <- max(fire["HP"])
minHP <- min(fire["HP"])

maxAttack <- max(fire["Attack"])
minAttack <- min(fire["Attack"])

maxDefense <- max(fire["Defense"])
minDefense <- min(fire["Defense"])

maxSpAtk <- max(fire["Sp_Atk"])
minSpAtk <- min(fire["Sp_Atk"])

maxSpDef <- max(fire["Sp_Def"])
minSpDef <- min(fire["Sp_Def"])

maxSpeed <- max(fire["Speed"])
minSpeed <- min(fire["Speed"])

maxTot <- max(fire["Total"])
minTot <- min(fire["Total"])

maxHt <- max(fire["Height_m"])
minHt <- min(fire["Height_m"])

count <- 1
hp <- fire["HP"]
attack <- fire["Attack"]
defense <- fire["Defense"]
spAtk <- fire["Sp_Atk"]
spDef <- fire["Sp_Def"]
speed <- fire["Speed"]
tot <- fire["Total"]
ht <- fire["Height_m"]

while(count<=max(grass["Number"]))
{
#normalizing
hp[count] <- (hp[count] - minHP)/(maxHP-minHP)
attack[count] <- (attack[count] - minAttack)/(maxAttack-minAttack)
defense[count] <- (defense[count] - minDefense)/(maxDefense-minDefense)
spAtk[count] <- (spAtk[count] - minSpAtk)/(maxSpAtk-minSpAtk)
spDef[count] <- (spDef[count] - minSpDef)/(maxSpDef-minSpDef)
speed[count] <- (speed[count] - minSpeed)/(maxSpeed-minSpeed)
tot[count] <- (tot[count] - minTot)/(maxTot-minTot)
ht[count] <- (ht[count] - minHt)/(maxHt-minHt)
count <- count+1
}

#means (fire)
hp2 <- mean(as.numeric(unlist(hp)))
attack2 <- mean(as.numeric(unlist(attack)))
defense2 <- mean(as.numeric(unlist(defense)))
spAtk2 <- mean(as.numeric(unlist(spAtk)))
spDef2 <- mean(as.numeric(unlist(spDef)))
speed2 <- mean(as.numeric(unlist(speed)))
tot2 <- mean(as.numeric(unlist(tot)))
ht2 <- mean(as.numeric(unlist(ht)))

hp1-hp2
attack1-attack2
defense1-defense2
spAtk1-spAtk2
spDef1-spDef2
speed1-speed2
tot1-tot2
ht1-ht2

grass=c(hp1,attack1,defense1,spAtk1,spDef1,speed1,tot1,ht1)
fire=c(hp2,attack2,defense1,spAtk2,spDef2,speed2,tot2,ht2)

plot(grass,type = "o",ylab="mean", col = "green",main = "Grass")
plot(fire,type = "o", ylab="mean",col = "red",main = "Fire")


