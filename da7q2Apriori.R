####################################################################################################################################
#The given  data, handwriting_recognition.csvspecifieshow  the  gender  and  profession  of  a person  canpossibly
#affect  the  recognition  of  his/her handwriting. What,  if  any,  are  the association rules with the default setting?

#install.packages("arulesViz")
#install.packages("arules")
# Load the libraries
library(arules)
library(arulesViz)

data<- read.csv("handwriting_recognition.csv",header=TRUE,sep=",")
data<-as.data.frame(data)

newdata<- data[rep(row.names(data), data$Freq), 2:4]
newdata<-as(newdata,"transactions")
rules <- apriori(newdata, parameter = list(supp = 0.001, conf = 0.8))
inspect(rules)

#Find the support, confidence and lift of the following association rules:
#(i){Artist, Female} => Recognized
rules<-apriori(data=newdata, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(lhs=c("Profession=Artist","Gender=Female"),rhs="Recognition=Recognized"),
               control = list(verbose=F))
inspect(rules[3])

#(ii){Engineer} => Male
rules<-apriori(data=newdata, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(lhs=c("Profession=Engineer"),rhs="Gender=Male"),
               control = list(verbose=F))
inspect(rules)

#(iii){Actor, Recognized} => Female
rules<-apriori(data=newdata, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(lhs=c("Profession=Actor","Recognition=Recognized"),rhs="Gender=Female"),
               control = list(verbose=F))
inspect(rules[3])

#(iv){Doctor, Male} => Unrecognized
rules<-apriori(data=newdata, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(lhs=c("Profession=Doctor","Gender=Male"),rhs="Recognition=Unrecognized"),
               control = list(verbose=F))
inspect(rules[3])