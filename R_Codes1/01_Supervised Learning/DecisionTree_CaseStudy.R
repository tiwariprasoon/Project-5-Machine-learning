#Classfication#

#It is a supervised machine-learning approach. classification is the problem of 
#identifying to which of a set of categories (sub-populations) a new observation belongs,
#on the basis of a training set of data containing observations (or instances) 
#whose category membership is known

#Classification Techniques#
#2. Decision Trees#


#Data#
#For the purpose of demonstration We shall use the real-world public data set
#https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
#The data set provides demographic and socio-economic indicators and
#Indicates if a person has subscribed to term-deposit or not

#Citation#
#[Moro et al., 2014] S. Moro, P. Cortez and P. Rita. 
#A Data-Driven Approach to Predict the Success of Bank Telemarketing. 
#Decision Support Systems, Elsevier, 62:22-31, June 2014

#Variables in data and description#
#Input variables:
#bank client data:
#age (numeric)
#job : type of job (categorical)
#marital : marital status (categorical)
#education (categorical)
#default: has credit in default? 
#housing: has housing loan?
#loan: has personal loan? 
#contact: contact communication type 
#month: last contact month of year 
#day_of_week: last contact day of the week 
#duration: last contact duration, in seconds 
#campaign: number of contacts performed during this campaign and for this client 
#pdays: number of days that passed by after the client was last contacted 
#previous: number of contacts performed before this campaign and for this client (numeric)
#poutcome: outcome of the previous marketing campaign (categorical)

# social and economic context attributes
#emp.var.rate: employment variation rate - quarterly indicator (numeric)
#cons.price.idx: consumer price index - monthly indicator (numeric) 
#cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
#euribor3m: euribor 3 month rate - daily indicator (numeric)
#nr.employed: number of employees - quarterly indicator (numeric)

#Output variable (desired target):
#Has the client subscribed a term deposit? (binary: 'yes','no')

#######################################################################################

#Classification Steps
# Step 1 : Prepare data {factor, missing values}
# Step 2 : Select validation strategy and divide into test and train data
# Step 3 : Execute classification
# Step 3 : Model Evaluation

#Basics
#install.packages("party")
library(party)  #Decision Trees


#######################################################################################
# Step 1 : Prepare data

#Imported Data : bank_additional
#Save it another object 
setwd("Set Working directory")
bank_additional<-read.csv("BankData.csv")
bank <- bank_additional

#Explore the data
str(bank)
#------------------------------------------------------------------------------------------
#Interpretation:
  # 1. Outputs the internal structure of the dataset such as no. of rows and columns; type of the variables etc. 
  # 2. As an example, the variable "age" is an integer variable; "contact" is a categotical variable with two factors namely "cellular" & "telephone"
#------------------------------------------------------------------------------------------

#We find that following needs to be converted into factors
#job : Convert to factor 
#marital : convert to factor
#education : convert to factor

bank$job        <- as.factor(bank$job)
bank$marital    <- as.factor(bank$marital)
bank$education  <- as.factor(bank$education)
bank$default    <- as.factor(bank$default)
bank$housing    <- as.factor(bank$housing)
bank$loan       <- as.factor(bank$loan)
bank$contact    <- as.factor(bank$contact)
bank$month      <- as.factor(bank$month)
bank$day_of_week<- as.factor(bank$day_of_week)
bank$poutcome   <- as.factor(bank$poutcome)
bank$y          <- as.factor(bank$y)
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. A categorical variable only belongs to a limited number of categoties/ can take limited number of different values. 
# 2. As R is a statistical programming language, there is a specific data structure for categorical variables named as "factor".
# 3. If you store categorical data as factor, all statistical modelling techniques will handle such data correctly.
# 4. For R, it is not clear whether the entered values are simple charecters or factors. If you printout factor variables, there
#    will not be any double quotes anymore i.e. the variables are not charecters anymore; but converted to categories.
# 5. R basically does two things when a factor function is called:
#                 (i) It scans through all the vectors to see through the different caegories in there such as "cellular" & "telephone" in contact variable
#                 (ii) Next, R converts the charecter vector to a vector of integer values. If you use "str" function you can see some integers in the output.
#                     These integers correspond to each of the charecter values to use when the factor is displayed. 
# 6. You can use "levels" function on the factored column to see the factor(category) names.
#------------------------------------------------------------------------------------------


#Identify factor levels with only few counts and convert to unknown
bank$default[bank$default == "yes"] <- "unknown"
bank$education[bank$education == "illeterate"] <- "unknown"

#Check if any of the attributes has NULL Values
#If yes replace by mean or median
table(is.na(bank))
#Luckily we don't have any


str(bank)
#######################################################################################

# Step 2 : Select validation strategy and divide into test and train data
# One of the critical tasks in machine learning is the ability to validate results
# We use k-fold validation. In this example We shall use 4-fold validation

# In a k-fold validation method. The dataset is divided into k-folds of equal size
# Data is trained on k-1 folds and tested on the kth fold
# Let's split the data into four-folds

# step 2a : attach a random number column

set.seed(10)
bank$random <- runif(nrow(bank),min=0,max=nrow(bank))
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. set.seed(n) is used to control the random number generation. 
# 2. In the other word, if you want to reproduce the same set of random numbers again and again, you have
#    to set the seed value "n" to one particular number and keep generating the random number using distributions like runif() etc.
# 3. If you do not set the seed, each time different set of random number will be generated. As an example, run the following commannds
#   in R console to check the difference:
#            (i). runif(5) ;    runif(5) ;
#            (ii) set.seed(5);  runif(5); set.seed(5); runif(5);
# 5. If you run (i) & (ii), you can see that, in the first case, two different set of numbers have been generated 
#    whereas, in the second case, two exact same set of numbers have been generated.
#------------------------------------------------------------------------------------------


#Folds are to be generated randomly
bank$fold   <- ifelse(bank$random <= quantile(bank$random,0.25),1,
                      ifelse((bank$random > quantile(bank$random,0.25) & bank$random < quantile(bank$random,0.5)),2,
                             ifelse((bank$random > quantile(bank$random,0.5) & bank$random < quantile(bank$random,0.75)),3,4)))
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. One new column called "fold" is created.  
# 2. The value of the columns are as following:
#       (i) If the random number value in "random" column lies in the 1st Quartile, corresponding "fold" value will be 1.
#       (ii) If the random number value in "random" column lies in the 2nd Quartile, corresponding "fold" value will be 2. 
#       (iii) If the random number value in "random" column lies in the 3rd Quartile, corresponding "fold" value will be 3. 
#       (iv) If the random number value in "random" column lies in the 4th Quartile, corresponding "fold" value will be 4.
#------------------------------------------------------------------------------------------


# step 2b : create testing folds

testfold1  <- bank[bank$fold==1, ]                  
testfold2  <- bank[bank$fold==2, ] 
testfold3  <- bank[bank$fold==3, ]                  
testfold4  <- bank[bank$fold==4, ]
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Four different Testing dataset are created (testfold1,testfold2,testfold3,testfold4).  
# 2. Our model will be tested on four different datasets to check the efficiency of the model.
#------------------------------------------------------------------------------------------


# step 2c : Create training folds

trainfold1  <- bank[bank$fold !=1, ]                  
trainfold2  <- bank[bank$fold !=2, ] 
trainfold3  <- bank[bank$fold !=3, ]                  
trainfold4  <- bank[bank$fold !=4, ]
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Four different Training dataset are created (trainfold1,trainfold2,trainfold3,trainfold4).  
# 2. Our model will be developed on four different datasets and be tested on the corresponding test datasets.
#------------------------------------------------------------------------------------------

#  *** Please note that we are using 25% of the dataset for testing prupose and 75% of the dataset in training purpose.




#Measures across four folds should be similar
#You can control the threshold to impact the accuracy, recall, precision and sensitivity measures
#The ideal measure depends on the business requirement

#######################################################################################
#Decision Trees#

#Step 3: Evaluate Model#

treemodela <- ctree(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data=trainfold1)
treemodelb <- ctree(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data=trainfold2)
treemodelc <- ctree(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data=trainfold3)
treemodeld <- ctree(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data=trainfold4)
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Y is considered as Dependent variable and rest are independnet/predictor variable in all the models.  
# 2. "treemodela" is built on trainfold1,"treemodelb" is built on trainfold2,"treemodelc" is built on trainfold3,"treemodeld" is built on trainfold4.
# 3. You can type "treemodela" in the R console to check the statistics/expanded version of the tree.
# 4. From the output, we can conclude that there are total 14 classes (* on the nodes are terminal nodes). 
# 5. You can type plot(treemodela) to draw the tree.
# 6. If we plot the tree, we can see that there are 14 classes (# of terminal nodes).
#------------------------------------------------------------------------------------------


testfold1$treepred <- predict(treemodela,newdata=testfold1,type="response")
testfold2$treepred <- predict(treemodelb,newdata=testfold2,type="response")
testfold3$treepred <- predict(treemodelc,newdata=testfold3,type="response")
testfold4$treepred <- predict(treemodeld,newdata=testfold4,type="response")
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Model "testmodela" is applied on "testfold1" data to predict "y".    
# 2. The parameter "response" is responsible for predicting the class of "y". 
#------------------------------------------------------------------------------------------



testfold        <- rbind(testfold1,testfold2,testfold3,testfold4)
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. "rbind" is responsible for combining rows of multiple dataframes.Hence, it combines all the rows of 
#    the four dataframes. Type "class(testfold1)" to check the class of testfold1.
#------------------------------------------------------------------------------------------


#generate confusion matrix
tmeasuresfold1 <- table(pred=testfold1$treepred, actual=testfold1$y)
tmeasuresfold2 <- table(pred=testfold2$treepred, actual=testfold2$y)
tmeasuresfold3 <- table(pred=testfold3$treepred, actual=testfold3$y)
tmeasuresfold4 <- table(pred=testfold4$treepred, actual=testfold4$y)
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Confusion matrix is used to compare Predicted vs. Actual values in table format.
# 2. If we run "tmeasuresfold1", we will get the following output:
#                    actual
#           pred     no yes
#               no  884  69
#               yes  33  44
#    It means that total [884 (no,no) + 44 (yes,yes)] times output is correctly classified by the model.
#                  total [69 (no,yes) + 33 (yes,no)] times output is wrongly classified by the model
# 3. We can infer the same from tmeasuresfold2,tmeasuresfold3,tmeasuresfold4.
#------------------------------------------------------------------------------------------


#Decision Tree - Confusion matrix measures
#Test Fold 1
taccuracyfold1    <- (tmeasuresfold1[1,1] + tmeasuresfold1[2,2])/sum(tmeasuresfold1) # TP+FP / TP+TN+FP+FN 
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Accuracy means the percentage of times the model has correctly predicted output.
# 2. Accuracy of this model is 90.09% 
#------------------------------------------------------------------------------------------

trecallfold1      <-  tmeasuresfold1[2,2]/(tmeasuresfold1[1,2]+tmeasuresfold1[2,2])  # TP/TP+FN also known as sensitivity 
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Sensitivity (or Recall) refers to the True Positive rate.
# 2. Recall= (Total Number of True Positive)/(Total Number of Actual Positives)
# 3. Recall of this model is 38% i.e. the proportion of customers that actually subscribed to the term deposit were 
#    predicted by the model as subscribed customer.
# 4. Intuition:
#         (i) Prob(Retrived/Relevant)
#         (ii) How much of the good/correct stuff did the model miss? i.e. this probability should be as low as possible
#         (iii) In medical test, recall refers to the ability of a test to to correctly identify those with the disease
#------------------------------------------------------------------------------------------

tprecisionfold1   <-  tmeasuresfold1[2,2]/(tmeasuresfold1[2,1]+tmeasuresfold1[2,2])  # TP/TP+FP
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Precision refers positive predictive value.
# 2. Precision = (Total Number of True Positive)/(Total Number of Predicted Positives)
# 3. Percentage of selected items those are correct. i.e. if you select 10 items, what is the probability that you have collected correct items.
# 4. In the context of our problem, the precison of the model is 57%. i.e. the proportion of the customer the model predicted to subscribe to 
#    the term deposit had actually subscribed to the term deposit.
# 5. Intuitions:
#         (i) Prob(Relevant/Retrived)   [reverse of Recall]
#         (ii) Probability of selected items that are actually relevant/correct to the user
#         (iii) How much junk the model is giving to the user?
#------------------------------------------------------------------------------------------

tspecificityfold1 <-  tmeasuresfold1[1,1]/(tmeasuresfold1[1,1]+tmeasuresfold1[2,1])  # TN/TN+FP
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Intuition: In medical test, Specificity refers to the the ability of the test to correctly identify those without the disease
# 2. In the context of ou problem, the specificity of the model 96.4% which is very good. i.e. the model 96.4% cases correctly predicts the 
#   customers who have not subscribed to the term deposit   
#------------------------------------------------------------------------------------------


#Test Fold 2
taccuracyfold2    <- (tmeasuresfold2[1,1] + tmeasuresfold2[2,2])/sum(tmeasuresfold2) # TP+FP / TP+TN+FP+FN 
trecallfold2      <-  tmeasuresfold2[2,2]/(tmeasuresfold2[1,2]+tmeasuresfold2[2,2])  # TP/TP+FN also known as sensitivity 
tprecisionfold2   <-  tmeasuresfold2[2,2]/(tmeasuresfold2[2,1]+tmeasuresfold2[2,2])  # TP/TP+FP
tspecificityfold2 <-  tmeasuresfold2[1,1]/(tmeasuresfold2[1,1]+tmeasuresfold2[2,1])  # TN/TN+FP

#Test Fold 3
taccuracyfold3    <- (tmeasuresfold3[1,1] + tmeasuresfold3[2,2])/sum(tmeasuresfold3) # TP+FP / TP+TN+FP+FN 
trecallfold3      <-  tmeasuresfold3[2,2]/(tmeasuresfold3[1,2]+tmeasuresfold3[2,2])  # TP/TP+FN also known as sensitivity 
tprecisionfold3   <-  tmeasuresfold3[2,2]/(tmeasuresfold3[2,1]+tmeasuresfold3[2,2])  # TP/TP+FP
tspecificityfold3 <-  tmeasuresfold3[1,1]/(tmeasuresfold3[1,1]+tmeasuresfold3[2,1])  # TN/TN+FP

#Test Fold 4
taccuracyfold4    <- (tmeasuresfold4[1,1] + tmeasuresfold4[2,2])/sum(tmeasuresfold4) # TP+FP / TP+TN+FP+FN 
trecallfold4      <-  tmeasuresfold4[2,2]/(tmeasuresfold4[1,2]+tmeasuresfold4[2,2])  # TP/TP+FN also known as sensitivity 
tprecisionfold4   <-  tmeasuresfold4[2,2]/(tmeasuresfold4[2,1]+tmeasuresfold4[2,2])  # TP/TP+FP
tspecificityfold4 <-  tmeasuresfold4[1,1]/(tmeasuresfold4[1,1]+tmeasuresfold4[2,1])  # TN/TN+FP
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Same interpretations are applicable to the above models as well.
#------------------------------------------------------------------------------------------




accuracy        <- c(taccuracyfold1,taccuracyfold2,taccuracyfold3,taccuracyfold4)
recall          <- c(trecallfold1, trecallfold2, trecallfold3, trecallfold4)
precision       <- c(tprecisionfold1,tprecisionfold2,tprecisionfold3,tprecisionfold4)
specificity     <- c(tspecificityfold1,tspecificityfold2,tspecificityfold3,tspecificityfold4)
technique       <- c("decisiontree","decisiontree","decisiontree","decisiontree") 
fold_index      <- c("fold1","fold2","fold3","fold4")

treetable <- data.frame(technique,fold_index,accuracy,recall,precision,specificity)
#------------------------------------------------------------------------------------------
#Interpretation:
# 1. Overall comparison of all the models in table format.
#------------------------------------------------------------------------------------------



