data <- read.csv(file="F:/pokemon.csv", header=TRUE, sep=",")
sub <- cbind(data["Total"],data["HP"],data["Attack"],data["Defense"],data["Sp_Atk"],data["Sp_Def"],data["Speed"],data["Generation"],data["Pr_Male"],data["Height_m"],data["Weight_kg"],data["Catch_Rate"])
#finding correlation
cov(data)

#dropping 2 columns
sub <- cbind(data["Total"],data["HP"],data["Attack"],data["Defense"],data["Sp_Atk"],data["Sp_Def"],data["Speed"],data["Generation"],data["Height_m"],data["Catch_Rate"])

normalize_data <- function(df, colname){
    df[ ,colname] <- (df[ ,colname]- min(df[ ,colname]))/ (max(df[, colname]) - min(df[ ,colname]) )
    return(df[ ,colname])
  }
  for(i in names(sub)){
    sub[ , i] <- normalize_data(sub, i)
  }

datapca <- PCA(sub, scale.unit = TRUE, graph = FALSE)
datapca$eig



 l <- datapca$eig
 l <- as.numeric(unlist(l[2]))
plot(l,type = "o",ylab="Percentage of Varience",col = "green",main = "Varience")

#new data
new <- cbind(data[0],data[1],data[2],data[3],data[7],data[8],data[11],data[12],data[13],data[14],data[16],data[17],data[18])
#scatterplot
pc1 <- datapca$ind$coord[ ,4]
pc2 <- datapca$ind$coord[ ,5]
l<- cbind(pc1,pc2)
plot(l,xlab="Defense",ylab="Special Attack",col = "green",main = "Defense vs Special Attack")


