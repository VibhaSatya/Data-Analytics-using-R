#LOGISTIC REGRESSION AND SVM
data <- read.csv(file="D:/DA/Indian Liver Patient Dataset (ILPD).csv", header=TRUE, sep=",")
data<- as.data.frame(data)
#names(mydata)

#to check if data has na values
sapply(data, function(x) sum(is.na(x)))

#filling missing values with mean
i<- 1
alkMean <- mean(as.numeric(unlist(data["alkphos"])),na.rm=TRUE)
alkMean

while(i<=nrow(data))
{
if(is.na(data[i,"alkphos"]))
data[i,"alkphos"] <- alkMean

i<- i+1
}
sapply(data, function(x) sum(is.na(x)))

#shuffling rows of the data frame for random sampling
set.seed(20)
data <- data[sample(nrow(data)),]

#TRAINING
#splitting data into 70/30 test and train
train<- round(0.7*nrow(data))
traindata <- data[1:train,]

#Sample class imbalance
#number of 1s and 2s
table(traindata$is_patient)

class1<-sum(traindata$is_patient==1)
class2<-sum(traindata$is_patient==2)

x<-data.frame("Class1",class1)
names(x)<-c("Class","val")
y<-data.frame("Class2",class2)
names(y)<-c("Class","val")
x<-rbind(x,y)
x #x has number of class1 and class2


library(plotly)

#pie chart of x
p1<- plot_ly(x, labels = ~Class, values = ~val, type = 'pie') %>%
  layout(title = 'Imbalance in class1 and class2 liver patients',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p1

fd<-factor(traindata$is_patient)
library(caret)
#upsampling for is_patient column
new_data<-upSample(traindata,fd)
nrow(new_data)
table(new_data$is_patient)


class1<-sum(new_data$is_patient==1)
class2<-sum(new_data$is_patient==2)

x2<-data.frame("Class1",class1)
names(x2)<-c("Class","val")
y2<-data.frame("Class2",class2)
names(y2)<-c("Class","val")
x2<-rbind(x2,y2)
x2 #x2 has number of class1 and class2

p2 <- plot_ly(x2, labels = ~Class, values = ~val, type = 'pie') %>%
  layout(title = 'After upsampling :Imbalance in is_patient attribute',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p2

traindata <- new_data

###########################################################################################
#LOGISTIC REGRESSION
#to make y have 0 and 1 values instead of 1 and 2


l<-as.numeric(unlist(traindata[,"is_patient"]-1))

#Finding the best model based on AIC value

#AIC values will differ every time because of shuffling
#All attributes
#AIC 661.75
model<- glm(l~age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+ag_ratio+sgpt+sgot+alkphos,family=binomial(link='logit'),data=traindata)
summary(model)

#taking significant components 
#AIC 685.32
model1<- glm(l~age+albumin+ag_ratio+sgpt+sgot+alkphos,family=binomial(link='logit'),data=traindata)
summary(model1)

#dropping highly correlated attributes
melted_cormat<- cor(data.matrix(data))
library(reshape2)
melted_cormat <- melt(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))

#Correalted attributes: 
#1.direct_bilirubin, tot_bilirubin
#2.ag_ratio, albumin
#3.sgpt,sgot
#4.sgot,alkphos

#removing direct_bilirubin
#661.96
model<- glm(l~age+gender+tot_bilirubin+tot_proteins+albumin+ag_ratio+sgpt+sgot+alkphos,family=binomial(link='logit'),data=traindata)
summary(model)
#removing tot_bilirubin
#659.9 
model<- glm(l~age+gender+direct_bilirubin+tot_proteins+albumin+ag_ratio+sgpt+sgot+alkphos,family=binomial(link='logit'),data=traindata)
summary(model)

#removing ag_ratio 
#659.85
model<- glm(l~age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+sgpt+sgot+alkphos,family=binomial(link='logit'),data=traindata)
summary(model)
#removing albumin 
#671.25
model<- glm(l~age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+ag_ratio+sgpt+sgot+alkphos,family=binomial(link='logit'),data=traindata)
summary(model)

#removing sgot  
#694.81
model<- glm(l~age+albumin+ag_ratio+sgpt+alkphos,family=binomial(link='logit'),data=traindata)
summary(model)

#removing sgpt
#664.15
model<- glm(l~age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+ag_ratio+sgot+alkphos,family=binomial(link='logit'),data=traindata)
summary(model)

#removing alkphos
#660.17
model<- glm(l~age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+ag_ratio+sgpt+sgot,family=binomial(link='logit'),data=traindata)
summary(model) 

#removing tot_bilirubin and ag_ratio
#658.01 best model
model2<- glm(l~age+gender+direct_bilirubin+tot_proteins+albumin+sgpt+sgot+alkphos,family=binomial(link='logit'),data=traindata)
summary(model2)

#removing tot_bilirubin and ag_ratio and alkphos
#656.48 best model
model2<- glm(l~age+gender+direct_bilirubin+tot_proteins+albumin+sgpt+sgot,family=binomial(link='logit'),data=traindata)
summary(model2)

#TESTING
#model2 is better (lower AIC and no 0 1 warning)
testdata <- data[(train+1):nrow(data),]

#install.packages("InformationValue")
library(InformationValue)
l<-as.numeric(unlist( data[(train+1):nrow(data) ,"is_patient"]-1))

#MODEL2
p2<-predict(model2,testdata,type='response')
optCutOff2 <- optimalCutoff(l, p2)[1] 
#optCutOff2
M <- confusionMatrix(as.numeric(unlist(l)), p2, threshold = optCutOff2)
accuracy <- (M[1,1]+M[2,2])/sum(M)*100
accuracy
logreg <- accuracy

###########################################################################################
#DECISION TREE
train <- traindata
test_data <- testdata
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

#target: using all the columns of the dataset to predict is_alive
target<-is_patient ~ age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+
                      ag_ratio+sgpt+sgot+alkphos

tree<- rpart(target, data = train,method="class")
rpart.plot(tree,main="Tree 1")
predictions1 = predict(tree, newdata = test_data,type="class")
confMat1<-table( test_data$is_patient,predictions1)
accuracy1 <- sum(diag(confMat1))/sum(confMat1) *100
accuracy1
#pruning the tree
printcp(tree)
#Variables actually used in tree construction:
#[1] age              albumin          direct_bilirubin sgot             tot_bilirubin    tot_proteins 
pfit <- prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
rpart.plot(pfit,main="Pruned tree")
predictions2 = predict(pfit, newdata = test_data,type="class")
confMat2<-table( test_data$is_patient,predictions2)
accuracy2 <- sum(diag(confMat2))/sum(confMat2) *100
accuracy2

dtree <- accuracy2

###########################################################################################
#SVM
#install.packages("e1071")
library(e1071)

train<- round(0.7*nrow(data))
traindata <- data[1:train,]
#to make y have 0 and 1 values instead of 1 and 2
y<-as.numeric(unlist(traindata[,"is_patient"]-1))

model_svm <- svm(y ~ age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+ag_ratio+sgpt+sgot+alkphos,family=binomial(link='logit'),kernal='radial',data=traindata)



#TESTING
#install.packages("InformationValue")
library(InformationValue)
testdata <- data[(train+1):nrow(data),]
pred <- predict(model_svm, testdata)
l<-as.numeric(unlist( data[(train+1):nrow(data) ,"is_patient"]-1))
optCutOff <- optimalCutoff(l, pred)[1] 
M <- confusionMatrix(l, pred, threshold = optCutOff)
accuracy1 <- (M[1,1]+M[2,2])/sum(M)*100
accuracy1

#to improve accuracy
# perform a grid search

#to make y have 0 and 1 values instead of 1 and 2
y<-as.numeric(unlist(traindata[,"is_patient"]-1))

# perform a grid search
#We can specify the values for the cost parameter and epsilon which is 0.1 by default. 
#A simple way is to try for each value of epsilon between 0 and 1 (We can take steps of 0.01) 
#and similarly try for cost function from 4 to 2^9 (We can take exponential steps of 2 here). 
#We are taking 101 values of epsilon and 8 values of cost function, 
#thus testing 808 models to find out which ones perform the best. 



#svm_tune <- tune(svm, y ~ as.numeric(unlist(traindata["age"]))+as.numeric(unlist(traindata["gender"]))+as.numeric(unlist(traindata["tot_bilirubin"]))+as.numeric(unlist(traindata["direct_bilirubin"]))+as.numeric(unlist(traindata["tot_proteins"]))+as.numeric(unlist(traindata["albumin"]))+as.numeric(unlist(traindata["ag_ratio"]))+as.numeric(unlist(traindata["sgpt"]))+as.numeric(unlist(traindata["sgot"]))+as.numeric(unlist(traindata["alkphos"])), data = traindata,
# ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
#)
#print(svm_tune)
#Parameter tuning of ‘svm’:
  
# - sampling method: 10-fold cross validation 
 
#- best parameters:
# epsilon cost
#0.27     512
 
#- best performance: 0.1151504  
 
#The best model
best_mod <-svm(y ~ age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+ag_ratio+sgpt+sgot+alkphos,kernal='radial', traindata,epsilon=0.27,cost=512)
best_mod_pred <- predict(best_mod, testdata) 
l<-as.numeric(unlist( data[(train+1):nrow(data) ,"is_patient"]-1))
optCutOff <- optimalCutoff(l, best_mod_pred)[1] 
M <- confusionMatrix(l, best_mod_pred, threshold = optCutOff)
accuracy2 <- (M[1,1]+M[2,2])/sum(M)*100
accuracy2
svm <- accuracy2


###########################################################################################
#NAIVE BAYE'S

train<- round(0.7*nrow(data))
train <- data[1:train,]
target<-as.factor(is_patient) ~ age+gender+tot_bilirubin+direct_bilirubin+tot_proteins+albumin+
  ag_ratio+sgpt+sgot+alkphos
library(e1071)
model <- naiveBayes(target, data = train)
preds <- predict(model, newdata = test_data,type = "class")
confMat<-table( test_data$is_patient,preds)
accuracy <- sum(diag(confMat))/sum(confMat) * 100
accuracy

attr_data<-subset(mydata,select=-c(is_patient,gender))
cormat<-cor(attr_data)
library(plotly)
plot_ly(z=cormat,x=colnames(cormat),y=rownames(cormat),type="heatmap")

new_target<-as.factor(is_patient) ~ age+gender+tot_bilirubin+tot_proteins+albumin+
  ag_ratio+sgpt+alkphos
model2 <- naiveBayes(new_target, data = train)
preds2 <- predict(model2, newdata = test_data,type = "class")
confMat2<-table( test_data$is_patient,preds2)
accuracy2 <- sum(diag(confMat2))/sum(confMat2) * 100
accuracy2
baye <- accuracy

print("Accuracy")
print("Logistic Regression ")
logreg
print("Decision trees ")
dtree
print("SVM ")
svm
print("Naive Baye's ")
baye











