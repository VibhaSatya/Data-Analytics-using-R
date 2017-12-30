###########################################################################################################
# a)Use k-means clustering to cluster the dataset into multiple clusters. Use 200 iterations. 
# How  many  instances  of  each  digit  do  each  of  the clusters contain  (give  complete details)? 
# Also, label each cluster based on the maximum number of instances of a digit
# that is present in it.

data <- read.csv(file="D:/DA/optdigits.csv", header=TRUE, sep=",")
mydata<- as.data.frame(data)

set.seed(10)
ncol(mydata)
fit<-kmeans(mydata[,ncol(mydata)-1:ncol(mydata)],9,iter.max=200)
#fit

#Instances of each digit. Column: each cluster Row: instances of each digit
t<-table(fit$cluster,mydata$digit)
t
#Label for each cluster
for(i in 1:nrow(t)){
  r<-t[i,]
  index<-as.integer(which(r==max(r)))
  print(c("Cluster",i,"Label",index-1))
}

###########################################################################################################
# b)One of the clusters seem to have an almost equal distribution of two digits in it. 
# Perform hierarchical clustering only on the instances present in this cluster.
# Print the dendrogram displaying  branches  only  above  a  height  of  50. 
# Bring  down  the  number  of  clusters formed via hierarchical clustering to 2 
# and print the number of instances of each digit contained in each cluster.

data <- read.csv(file="D:/DA/optdigits.csv", header=TRUE, sep=",")
mydata<- as.data.frame(data)

set.seed(10)
ncol(mydata)
fit<-kmeans(mydata[,ncol(mydata)-1:ncol(mydata)],10,iter.max=200)
#fit
#Instances of each digit. Column: each cluster Row: instances of each digit
t<-table(fit$cluster,mydata$digit)
#t
#Cluster 1 has almost equal distribution of digit 1 (136 instances) and digit 9 (98 instances)
a<-as.data.frame(fit$cluster)
names(a)<-c("fit_cluster")
#extracting row numbers that fall under cluster 1
row<-which(a$fit_cluster==1)

cluster1<-data.frame()
for(i in row){
  r<-mydata[i,]
  cluster1<-rbind(cluster1,r)
}
#cluster1 is a dataframe with rows from mydata that fell under cluster 1

#Now to perform hierarchical clustering on this
clusters <- hclust(dist(cluster1[,ncol(cluster1)-1:ncol(cluster1)]))
hcd<-as.dendrogram(clusters)
#plot(hcd,hang=-1,main = "Original dendogram")

#dendrogram displaying  branches  only  above  a  height  of  50
plot(cut(hcd,h=50)$upper,main = "Upper tree of cut at h=50")

#forming 2 clusters
clusterCut<-cutree(clusters,2) 

#Instances of each digit. Column: each cluster Row: instances of each digit
table(clusterCut, cluster1$digit)
#It can be seen that cluster 1 : max is digit 9 (98 instances) and cluster 2 : max is digit 1 (134 instances)
###########################################################################################################
# c)Load the test dataand calculate the distance between each instance in the test data with 
# the centres of the clusters formed in question (a).
# Based on the distances, identify the number written in eachimage.
# Mention the image number along with its classification (cluster number and digit in the image)

#Loading test data
test_data <- read.csv(file="D:/DA/optdigits_test.csv", header=TRUE, sep=",")
testdata<- as.data.frame(test_data)

#Centers has the centers generated from k means clustering(question a)
centers<-fit$centers

#Computes eucleadian distance between two vectors
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#Compute distance between one row from test data to all rows from centers data
DistanceFromCenters<-function(v){
  distance_vector<-c()
  for(i in 1:nrow(centers)){
    #for every row in centers 
    c<-centers[i,]
    #compute eucledian distance between centers cluster and v
    euc<-euc.dist(v,c)
    #append to distance vector
    distance_vector<-c(distance_vector,euc)
  }
  #Distance vector contains distances of v from each cluster
  return(distance_vector)
}

#Label test data
for(i in 1:nrow(testdata)){
  
  row<-testdata[i,]
  img<-as.integer(row[1])
  row<-row[,2:ncol(row)]
  #comute distance vector
  distance_vector<-DistanceFromCenters(row)
  #Minimum distance
  min_index<-which(distance_vector==min(distance_vector))
  cluster_number<-min_index
  label<-min_index-1
  print(c("Image",img,"Cluster Number",cluster_number,"Digit",label))
}

