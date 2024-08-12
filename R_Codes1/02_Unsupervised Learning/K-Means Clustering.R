setwd("E:/R  Imp/Chapter 7 Machine Learning/DatasetsMLDS")

####################################################################
#Basics
#We would need the following Packages and Libraries
#Package installation
#install.packages("psych")
# install.packages("klaR")
# install.packages("fpc")
# install.packages("dbscan")


library(dbscan)
library(klaR)
library(psych) # dummy coding
library(fpc)

####################################################################

#Stage I : Data Preparation
#Step IA : Convert Factor into Dummy Codes
#First run str command to identify factor variables
telecomclus<-read.csv("Lesson_02_Unsupervised_Learning/ClusterDataset.csv")
str(telecomclus)
View(telecomclus)


#Factor Variable : age, gender, devicetype, locality, region

#Dummycoding Factor Variables
dummyage        <- as.data.frame(dummy.code(telecomclus$age))
dummygender     <- as.data.frame(dummy.code(telecomclus$gender))
dummydevicetype <- as.data.frame(dummy.code(telecomclus$devicetype))
dummylocality   <- as.data.frame(dummy.code(telecomclus$locality))
dummyregion     <- as.data.frame(dummy.code(telecomclus$region))
names(dummyage)         <- c('age_a','age_b','age_c','age_d')
names(dummygender)      <- c('male','female')
names(dummydevicetype)  <- c('smart','nonsmart')
names(dummylocality)    <- c('urban','rural')
names(dummyregion)      <- c('north','south','east','west')
dummy <- data.frame(dummyage,dummygender,dummydevicetype,dummylocality,dummyregion)



#Merge telecomclus and dummy into new dataframe telecomclusdata
telecomclusdata <- data.frame(telecomclus,dummy)
View(telecomclusdata)


#Selecting the variables for clusteranalysis
#Identify numeric or integer variables from telecomclusdata
vars.to.use <- NULL
for (Var in names(telecomclusdata)) {
  if(class(telecomclusdata[,Var]) == 'integer' | class(telecomclusdata[,Var]) == 'numeric') {
    vars.to.use <- c(vars.to.use,Var)
  }
}

#Remove "id" field
vars.to.use <- vars.to.use[-1]
vars.to.use


####################################################################


#Step IB : Data Scaling and Centraling


# We are not showing Scaling now!!!!!!!!!!!!!!!!

clusdata<-telecomclusdata


####################################################################


#Stage II : Execute the Clustering Algorithm

#Cluster Analysis using k-means#

#In k-means, you have to indicate the number of clusters
#Explore solutions containing different number of clusters
#Identify the best solution

kmeans.c0 = 2 #set kmeans.c0 = 2 to get a solution with 2 clusters
kmeans.c1 = 3 #set kmeans.c1 = 3 to get a solution with 3 clusters
kmeans.c2 = 4 #set kmeans.c2 = 4 to get a solution with 4 clusters
kmeans.c3 = 5 #set kmeans.c3 = 5 to get a solution with 5 clusters

#Execute and store cluster solution
kmeans_aa <- kmeans(clusdata,kmeans.c0,nstart=100,iter.max=100)
kmeans_a  <- kmeans(clusdata,kmeans.c1,nstart=100,iter.max=100)
kmeans_b  <- kmeans(clusdata,kmeans.c2,nstart=100,iter.max=100)
kmeans_c  <- kmeans(clusdata,kmeans.c3,nstart=100,iter.max=100)

#Extract the cluster label from each cluster solution
groups_aa = kmeans_aa$cluster
groups_a  = kmeans_a$cluster
groups_b  = kmeans_b$cluster
groups_c  = kmeans_c$cluster

#Attach the labels solution-wise to the dataset for analysis
#Attach to the unscaled data ... it will be easy for interpretation
rm(telecomclusters)
telecomclusters <- data.frame(telecomclusdata,groups_aa,groups_a,groups_b)
View(telecomclusters)

#Visually Explore the Characteristics of cluster in the three solutions
telecomclusters$groups_aa <- as.factor(telecomclusters$groups_aa)
telecomclusters$groups_a <- as.factor(telecomclusters$groups_a)
telecomclusters$groups_b <- as.factor(telecomclusters$groups_b)

#Understanding Cluster Characteristics
#Viewing Cluster Centers

#Extract Cluster Centers

rm(clustercenters_abc)

clustercenters_aa <- t(aggregate(telecomclusters[,7:18],by=list(telecomclusters$groups_aa),mean,na.rm=TRUE))
clustercenters_aa <- as.data.frame(clustercenters_aa[-1,]) #Removing unnecessary row
names(clustercenters_aa) <- c("groupaa_c1","groupaa_c2")

clustercenters_a <- t(aggregate(telecomclusters[,7:18],by=list(telecomclusters$groups_a),mean,na.rm=TRUE))
clustercenters_a <- as.data.frame(clustercenters_a[-1,]) #Removing unnecessary row
names(clustercenters_a) <- c("groupa_c1","groupa_c2","groupa_c3")

clustercenters_b <- t(aggregate(telecomclusters[,7:18],by=list(telecomclusters$groups_b),mean,na.rm=TRUE))
clustercenters_b <- as.data.frame(clustercenters_b[-1,]) #Removing unnecessary row
names(clustercenters_b) <- c("groupb_c1","groupb_c2","groupb_c3","groupb_c4")

clustercenters_kmeans <- data.frame(clustercenters_aa,clustercenters_a,clustercenters_b)
View(clustercenters_kmeans)



#Use table to understand categories
attach(telecomclusdata)

#groups_aa
table(age,groups_aa)
table(gender,groups_aa)
table(locality,groups_aa)
table(region,groups_aa)
table(devicetype,groups_aa)

#groups_a
table(age,groups_a)
table(gender,groups_a)
table(locality,groups_a)
table(region,groups_a)
table(devicetype,groups_a)

########################################################################


#Stage III : K-means Evaluation#

#While visual aids provide a basic understanding, its difficult
#to arrive at a conclusion. There are some statistical tests that will
#help us in the evaluation of cluster solutions


#Method III: Screeplot
wss <- (nrow(clusdata)-1)*sum(apply(clusdata,2,var));
for (i in 2:20) wss[i] <- sum(kmeans(clusdata,centers=i)$withinss);
options(jupyter.plot_mimetypes = "image/svg+xml");
plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")





#########################################################################
# Run K-Means using Scaling
clusdata <-scale(telecomclusdata[,vars.to.use])
View(clusdata)
View(describe(clusdata))