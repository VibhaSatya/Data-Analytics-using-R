data <- read.csv(file="D:DA/cancer_tailored.csv", header=TRUE, sep=",")
mydata<- as.data.frame(data)
library(corrplot)

#removing rows with NA
na <- which(is.na(mydata),arr.ind=TRUE)
i <- 1
while(nrow(na))
{
mydata <- mydata[-c(na[1,1]),]
na <- which(is.na(mydata),arr.ind=TRUE)
i <- i+1
}
#to find the correlation between attributes
cormat <- cor(data.matrix(mydata))
corrplot(cormat,method="ellipse",type="lower")

#removing columns with correlation above 0.9
tmp <- cormat
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0

newdata <- mydata[,!apply(tmp,2,function(x) any(x > 0.9))]
rmCol <- setdiff(colnames(mydata),colnames(newdata))
dim(new)



#heatmap of correlations

#hist(cormat)
#library(reshape2)
#melted_cormat <- melt(cormat)
#library(ggplot2)
#ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
#  geom_tile()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#    size = 12, hjust = 1))

# Scatterplot Matrices from the glus Package 
#library(gclus)

#dta <- mydata
#dta.r <- abs(cormat) # get correlations
#dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
#dta.o <- order.single(dta.r) 
#cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
#main="Variables Ordered and Colored by Correlation" )

#---------------------------------------------------------------

#install.packages("caret")
#install.packages("e1071")

library(caret)
M<-confusionMatrix(mydata$predicted,mydata$diagnosis,positive = "M")

table<-M$table

TN<-c(table[1,1],"True Negative","Predicted Negative(Benign) and actual also Negative")
FN<-c(table[2,1],"False Negative","Predicted Negative but actually Positive(Malignant)")
TP<-c(table[2,2],"True Positive","Predicted Positive(Malignant)and actual also Positive")
FP<-c(table[1,2],"False Positive","Predicted Positive but actually Negative(Benign)")

TN
FN
TP
FP

#(i)Accuracy
print(M$overall[1])

#(ii)Misclassification Rate (1-accuracy)
print(1-M$overall[1])

#(iii)Recall
print(M$byClass[6])

#(iv)Precision
print(M$byClass[5])

#(v)Specificity
print(M$byClass[2])

#(vi)F Score with β = 2 and 0.5(also state what the corresponding values of β indicate)

#the F1 score (also F-score or F-measure) is a measure of a test's accuracy. 
# F= ( (1+beta^2) * precision * recall )/( (beta^2 * precision) + recall)

precision<-M$byClass[5]
names(precision)<-NULL
recall<-M$byClass[6]
names(recall)<-NULL

F_2<-( (1+4) * precision * recall )/( (4 * precision) + recall)
F_2

F_0.5<-( (1+0.25) * precision * recall )/( (0.25 * precision) + recall)
F_0.5


