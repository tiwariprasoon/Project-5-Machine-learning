# *****************Step-1: Load the data********************

setwd("# Set working directory #")
data<-read.csv("LogisticRegressionCaseStudy_Advanced.csv")
data1<-data[0:3001,] ## Rows from 3002 onwards contains zero values
data1<-data1[1:8]
head(data1)
attach(data1)


#install.packages("psych")   # install psych package to use "describe" function
library("psych")
describe(data1)
str(data1)

# Data preprocessing
data1$Response <- as.factor(data1$Response)
data1$Followup <- as.factor(data1$Followup)
data1$Channeltype <- as.factor(data1$Channeltype)
data1$mail  <- as.factor(data1$mail)
data1$email <- as.factor(data1$email)
data1$phone <- as.factor(data1$phone)
str(data1)


# *****************Step-2: Split the data in training & testing dataset********************
training<- (Customerid<2600) # Choose customerid carefully
testing<-!training
training_data<-data1[training,]
testing_data<-data1[testing,]
actual<-Response[testing]

# *****************Step-3: Fit a logistic regression model using Training data********************
log_model<-glm(Response~Channeltype,data=training_data,family="binomial")
log_model
summary(log_model)
coefficients(log_model)

# Including only one dependent variable: Channeltype
log_model<-glm(training_data$Response~training_data$Channeltype+training_data$mail+training_data$email+training_data$phone,family="binomial")
summary(log_model)

# Including all the dependent variable: Multicolinearity may exist
log_model<-glm(training_data$Response~training_data$Channeltype+training_data$Followup+training_data$email+training_data$phone,data=training_data,family="binomial")
summary(log_model)

# Drop ChannelType and see how it performs
log_model<-glm(Response~mail+email+phone,data=training_data,family="binomial")
summary(log_model)

# Only with two dependent variable: mail and phone
log_model<-glm(Response~mail+phone,data=training_data,family="binomial")
summary(log_model)


# *****************Step-4: Use the fitted model to do predictions for the test data********************
model_predict<-predict(log_model,testing_data,type="response")  ## logistic regression predict the probability; not the class
predicted<-rep("0",nrow(testing_data))
predicted[model_predict>0.5]=1


# *****************Step-5: Create a confusion matrix & compute the misclassification rate********************
table(actual,as.factor(predicted))
mean(predicted!=actual)


#################################### Another Option to Train and Test a classification Model #################################################
# This option is used more often

#install.packages("caTools")
require(caTools)
set.seed(101) 
sample <- sample.split(data1$Customerid, SplitRatio = .75)
train <- subset(data1, sample == TRUE)
test  <- subset(data1, sample == FALSE)


# *****************Step-3: Fit a logistic regression model using Training data********************
log_model<-glm(train$Response~train$mail+train$phone,data=train,family="binomial") # binomial("logit")
log_model
summary(log_model)
coefficients(log_model)

# *****************Step-4: Use the fitted model to do predictions for the test data********************
model_predict<-predict(log_model,test,type="response")  ## logistic regression predict the probability; not the class
predicted<-rep("0",nrow(test))
predicted[model_predict>0.5]=1
predicted<-predicted[0:nrow(test)]

table(test$Response,as.factor(predicted))
mean(predicted!=actual)
