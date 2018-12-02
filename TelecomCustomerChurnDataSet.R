###################################################################
##### Group A Assigment ML II - Telecom Customer Churn Data Set 
#####
##### daniel.lopez@student.ie.edu
##### heba.abdellatif@student.ie.edu
##### fjguerra@student.ie.edu
##### mvarga@student.ie.edu
##### riyad.kutabish@student.ie.edu
#####
##### Created: 28/10/2018
###################################################################


##### clean, install and load area ####
rm(list = ls())
dev.off()
cat("\014")



##### installing packages ####
#install.packages("PRROC")
library(PRROC)
library(caTools)
library(class)
library(plyr)
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("ggthemes")
library(ggthemes)
library(caret)
library(MASS)
#install.packages("randomForest")
library(randomForest)
#install.packages("party")
library(party)
library(e1071)
#install.packages("prediction")
library(prediction)
library(dplyr)
library(rpart)
#install.packages("gbm")
library(gbm)
#install.packages("polycor")
library(polycor)
#install.packages("DiscriMiner")
library(DiscriMiner)
#install.packages("hmeasure")
library(hmeasure)
#install.packages("pROC")
library(pROC)
#install.packages("ROCR")
library(ROCR)
#install.packages("DT")
library(DT)
library(rpart)





##### Exploratory Data Analysis #####

d_churn <- read.csv2("C:/Users/Daniel/Desktop/Machine L II/Group Assignment/WA_Fn-UseC_-Telco-Customer-Churn.csv",header = TRUE,sep = ",",na.strings=c("", "NA"))

# Let's analyse how many empty values there are in the dataset:
sapply(d_churn, function(x) sum(is.na(x)))

# As we see there are 11 rows which do not have a value for total charges, since it is a small sample
# of the population, let's delete it.
# Also we make a backup of the original dataset
d_original_churn  <- d_churn
d_churn  <- d_churn[complete.cases(d_churn),]

# Let's see whether it worked:
sapply(d_churn, function(x) sum(is.na(x)))
# FIXED!!

# Before starting with the modiling we need to transform the str variables into dichotomous
# variables in order to work with them:
# We will change variables "no internet service" to "NO" for all colums
cols_rename <- c(10:15)
for (i in 1:ncol(d_churn[,cols_rename])) {
          d_churn[,cols_rename][,i] <- as.factor(mapvalues(d_churn[,cols_rename]
[,i],from = c("No internet service"),to=c("No")))}
          
# Once we have made this modification we will change "No phone service" to "No"
d_churn$MultipleLines <- as.factor(mapvalues(d_churn$MultipleLines,from=c("No phone service"),
                                           to=c("No")))

# Let's categorise the tenure of the contracts by analising min and max values.
min(d_churn$tenure);max(d_churn$tenure)

# min 1 and max 72 so let's categorise in 12 month groups
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
d_churn$tenure_group <- sapply(d_churn$tenure,group_tenure)
d_churn$tenure_group <- as.factor(d_churn$tenure_group)

# Now that we have grouped the variables let's homogenise the data set by changing 
# Senior Citizen from 0 1 to yes/no format.
d_churn$SeniorCitizen <- as.factor(mapvalues(d_churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

# Clean those columns which are no longer needed.
d_churn$customerID <- NULL
d_churn$tenure <- NULL

## Now that we have finished with the data cleansing let's analyse which variables to include
# Let's analyse the correlation between the numeric variables of the set
d_churn$TotalCharges<-as.numeric(d_churn$TotalCharges)
d_churn$MonthlyCharges<-as.numeric(d_churn$MonthlyCharges)

numeric.var <- sapply(d_churn, is.numeric)
corr.matrix <- cor(d_churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables",method="number")
# The model seems to show no correlation between the variables so let's keep them






##### Baseline the model ####
intrain<- createDataPartition(d_churn$Churn,p=0.7,list=FALSE)
set.seed(2017)

training<- d_churn[intrain,]
testing<- d_churn[-intrain,]

# Let's confirm the slitt is correct:
dim(training); dim(testing)
# QC check OK!

# Fitting the model
baseline_model <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(baseline_model))

# Analysis of the efficiency of the model:
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- 0
testing$Churn[testing$Churn=="Yes"] <- 1
fitted.results <- predict(baseline_model,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# plot confusion matrix
testing$Churn <- as.factor(testing$Churn)
fitted.results <- as.factor(fitted.results)

print("Confusion Matrix for Logistic Regression")
table(testing$Churn, fitted.results > 0.5)


confusionMatrix(testing$Churn,fitted.results)

accuracy_results <- data.frame ("Method"= "Baseline" , "Accuracy"= 0.79)




##### Variables analysis ####

# Let's check for the distribution of the variables and see if they are evenly distributed.

p1 <- ggplot(d_churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(d_churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(d_churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(d_churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

# Good distribution

p5 <- ggplot(d_churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(d_churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(d_churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(d_churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)

# Could check phone service as it may be a bit biased towards Yes

p9 <- ggplot(d_churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(d_churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(d_churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(d_churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)

# Good distribution

p13 <- ggplot(d_churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(d_churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(d_churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(d_churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(d_churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)

# Good distribution

## All categorical variables show good distributions so let's keep all the variables.



##### Feature analysis ####

anova(baseline_model,test="Chisq")

# here we can check the variables that have the highest significance in the model so far



##### Feature engineering #### 

# Let's create some variables to enhance the model

# First variable -> debtors, if the monthly charges are bigger than the total charges

d_churn$debtors <- 0

d_churn$debtors[d_churn$MonthlyCharges>d_churn$TotalCharges]<- 1

d_churn$debtors[d_churn$Churn == "No"] <- 0

d_churn$debtors<- as.factor(d_churn$debtors)

table(d_churn$debtors)

# They do not seem to show a clear correlation

cor(training_1$TotalCharges,training_1$MonthlyCharges)

# Let's analyse correlation between vars

polycor::polychor(training_1$TotalCharges,training_1$debtors)

polycor::polychor(training_1$MonthlyCharges,training_1$debtors)

# Delete Monthly and Total charges

d_churn$TotalCharges <- NULL
d_churn$MonthlyCharges <- NULL
d_churn$MonthlyCharges <- NULL
d_churn$TotalCharges <- NULL

# Let's create more variables, this one is to analyse the dinosaurs

protect <- function(protect){
  if (d_churn$OnlineBackup == 'Yes' & d_churn$OnlineSecurity == 'No' & d_churn$DeviceProtection == 'No'){
    return('High Risk')
  }else if(d_churn$OnlineBackup == 'Yes' & d_churn$OnlineSecurity == 'Yes' & d_churn$DeviceProtection == 'Yes'){
    return('No risk')
  }else if (d_churn$OnlineBackup == 'Yes' & d_churn$OnlineSecurity == 'No' & d_churn$DeviceProtection == 'Yes'){
    return('Online risk')
  }else if (d_churn$OnlineBackup == 'Yes' & d_churn$OnlineSecurity == 'Yes' & d_churn$DeviceProtection == 'No'){
    return('Device Risk')
  }else if (d_churn$OnlineBackup == 'No'){
    return('Not client')
  }
}

d_churn$protect_level[(d_churn$OnlineBackup == 'Yes' & d_churn$OnlineSecurity == 'No' & 
                         d_churn$DeviceProtection == 'No')]<- 'High Risk'

d_churn$protect_level[(d_churn$OnlineBackup == 'Yes' & d_churn$OnlineSecurity == 'Yes' & 
                         d_churn$DeviceProtection == 'Yes')]<- 'Low Risk'

d_churn$protect_level[(d_churn$OnlineBackup == 'Yes' & d_churn$OnlineSecurity == 'No' & 
                         d_churn$DeviceProtection == 'Yes')]<- 'Online Risk'

d_churn$protect_level[(d_churn$OnlineBackup == 'Yes' & d_churn$OnlineSecurity == 'Yes' & 
                         d_churn$DeviceProtection == 'No')]<- 'Device Risk'

d_churn$protect_level[(d_churn$OnlineBackup == 'No')]<- 'Not client'

d_churn$protect_level<-as.factor(d_churn$protect_level)

table(d_churn$protect_level)

# H0: There is no statistically relationship between the variables.
# H1: THere is a significant relationship between variables.

chisq.test(d_churn$protect_level,d_churn$OnlineBackup)

# p-value < 0.05, reject null hypothesis.

d_churn$OnlineBackup <- NULL


# H0: There is no statistically relationship between the variables.
# H1: THere is a significant relationship between variables.

chisq.test(d_churn$protect_level,d_churn$OnlineSecurity)

# p-value < 0.05, reject null hypothesis.

d_churn$OnlineSecurity <- NULL


# H0: There is no statistically relationship between the variables.
# H1: THere is a significant relationship between variables.

chisq.test(d_churn$protect_level,d_churn$DeviceProtection)

# p-value < 0.05, reject null hypothesis.

d_churn$DeviceProtection <- NULL

## Other variable

d_churn$client_category <- 0


d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'Fiber optic' 
                        
                        & d_churn$StreamingTV == 'Yes'& d_churn$StreamingMovies == 'Yes'] <- 'Gold'

d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'DSL' 
                        
                        &d_churn$StreamingTV == 'Yes'& d_churn$StreamingMovies == 'Yes'] <- 'Gold'


d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'Fiber optic' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'Yes'] <- 'Silver'


d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'DSL' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'Yes'] <- 'Silver'


d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'Fiber optic' &
                          d_churn$StreamingTV == 'Yes'& d_churn$StreamingMovies == 'No'] <- 'Silver'

d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'DSL' &
                          d_churn$StreamingTV == 'Yes'& d_churn$StreamingMovies == 'No'] <- 'Silver'


d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'Fiber optic' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'No'] <- 'Bronze'


d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'DSL' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'No'] <- 'Bronze'

d_churn$client_category[d_churn$PhoneService == 'No' & d_churn$InternetService == 'Fiber optic' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'No'] <- 'Geek'


d_churn$client_category[d_churn$PhoneService == 'No' & d_churn$InternetService == 'DSL' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'No'] <- 'Geek'


d_churn$client_category[d_churn$PhoneService == 'Yes' & d_churn$InternetService == 'No' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'No'] <- 'Talker'

d_churn$client_category[d_churn$PhoneService == 'No' & d_churn$InternetService == 'DSL' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'Yes'] <- 'Watcher'


d_churn$client_category[d_churn$PhoneService == 'No' & d_churn$InternetService == 'Fiber optic' &
                          d_churn$StreamingTV == 'No'& d_churn$StreamingMovies == 'Yes'] <- 'Watcher'


d_churn$client_category[d_churn$PhoneService == 'No' & d_churn$InternetService == 'DSL' &
                          d_churn$StreamingTV == 'Yes'& d_churn$StreamingMovies == 'No'] <- 'Watcher'


d_churn$client_category[d_churn$PhoneService == 'No' & d_churn$InternetService == 'Fiber optic' &
                          d_churn$StreamingTV == 'Yes'& d_churn$StreamingMovies == 'No'] <- 'Watcher'


d_churn$client_category[d_churn$PhoneService == 'No' & d_churn$InternetService == 'Fiber optic' &
                          d_churn$StreamingTV == 'Yes'& d_churn$StreamingMovies == 'Yes'] <- 'Watcher'

d_churn$client_category[d_churn$PhoneService == 'No' & d_churn$InternetService == 'DSL' &
                          d_churn$StreamingTV == 'Yes'& d_churn$StreamingMovies == 'Yes'] <- 'Watcher'

d_churn$client_category <- as.factor(d_churn$client_category) 

table(d_churn$client_category)

# Let's delete the transformed variables:

d_churn$PhoneService <- NULL
d_churn$InternetService <- NULL
d_churn$StreamingMovies <- NULL
d_churn$StreamingTV <- NULL

# Let's analyse the model to see the performance improvement if any.



##### Featured Variable analysis ####

p18 <- ggplot(d_churn, aes(x=debtors)) + ggtitle("Debtors") + xlab("Debtors") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p19 <- ggplot(d_churn, aes(x=protect_level)) + ggtitle("protect level") + xlab("protect level") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p20 <- ggplot(d_churn, aes(x=client_category)) + ggtitle("Client Category") + xlab("Client Category") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(p18, p19, p20, ncol=2)




##### Model 1 Tree/Random Forest ####

intrain_1<- createDataPartition(d_churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training_1<- d_churn[intrain_1,]
testing_1<- d_churn[-intrain_1,]

# Let's confirm the slitt is correct:

dim(training_1); dim(testing_1)

# QC check OK!

# Fitting the model

model_1 <- glm(Churn ~ .,family=binomial(link="logit"),data=training_1)
print(summary(model_1))

# Analysis of the efficiency of the model:

testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(baseline_model,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# plot confusion matrix

print("Confusion Matrix for Logistic Regression")
table(testing$Churn, fitted.results > 0.5)


## Model 1 Feature analysis.

anova(model_1,test = "Chisq")

# Most important variables :
# - Senior Citizen
# - Partner
# - Tech Support
# - Contract
# - Payment Method
# - tenure_group
# - debtors
# - client category


# Let's create a decision tree 

tree_1 <- ctree(Churn ~ .,training_1)
plot(tree_1)

# Decision tree confusion matrix

pred_tree_1 <- predict(tree_1,testing_1)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree_1, Actual= testing_1$Churn)

pred_tree_1 <- as.factor(pred_tree_1)

confusionMatrix(testing_1$Churn,pred_tree_1)

accuracy_results <- rbind("Method"= "Tree" , "Accuracy"= 0.78)


## Random Forest

rfmodel1 <- randomForest(Churn ~ .,data=training_1)
print(rfmodel1)

# Confusion matrix

pred_rf_1 <- predict(rfmodel1,testing_1)
caret::confusionMatrix(pred_rf_1,testing_1$Churn)

accuracy_results <- data.frame ("Method"= "Random Forest" , "Accuracy"= 0.8)

# Random Forest Error Rate 

plot(rfmodel1)

# We can see how at the level of 100 to 200 trees we cannot get the error rate down.

# Random forest tunning

t_1 <- tuneRF(training_1[,-16],training_1[,16],stepFactor = 0.5,plot=TRUE, ntreeTry = 200, trace= TRUE, improve= 0.05)

# Here we get an idea of the number of trees to select, we see a clear elbow effect so we will choose to use 6 trees.

### Random forest post tune

rfModel_2 <- randomForest(Churn ~ debtors
                          + tenure_group
                          + Contract
                          + client_category
                          + TechSupport
                          + PaymentMethod
                          + PaperlessBilling
                          + SeniorCitizen
                          + protect_level
                          + Partner 
                          , data=training_1, ntree=200, mtry=3,importance =TRUE, proximity=TRUE)
print(rfModel_2)

# We managed to reduce the OOB error from 18.16% to 18.4%

# Random forest prediction and CF

pred_rf_2 <- predict(rfModel_2,testing_1)
caret::confusionMatrix(pred_rf_2,testing_1$Churn)


names(accuracy_results) <- cbind ("Method"= "Random Forest Post Tune" , "Accuracy"= 0.8)


## Random forest feature importance

varImpPlot(rfModel_2,sort = T,n.var = 10,main = "Top 10 Feature Importance")

# We will introduce the vars once we stablish the best performing algorythm



##### Model 2 Gradient boosting ####

model_1_gmb <- gbm(Churn ~ ., data=training_1,distribution="gaussian", n.trees= 150,
                   interaction.depth=2, shrinkage=0.01)

summary(model_1_gmb)

# Plot of response variable with lstat variable

plot(model_1_gmb,i="Contract")
plot(model_1_gmb,i="client_category")

# Generating a prediction matrix for each tree

n.trees <- seq(from=0, to=1000, by=20)

predmatrix <- predict(model_1_gmb,training_1,n.trees = n.trees)

dim(predmatrix)

# Calculating the Mean Squared Error test

test.error <- with(training_1,apply((predmatrix)^2,2,mean))
head(test.error)

# Plot test error vs nº of trees

plot(n.trees,test.error,pch=19,col="blue",xlab="Number of Trees", ylab="Test Error",main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("right",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)

# As we see Performance Boosting does not outperform random forests.




##### Model 3 LDA ####


training_num = training_1
testing_num = testing_1

training_num <- data.frame(lapply(training_num, function(x) as.numeric(as.factor(x))))
testing_num <- data.frame(lapply(testing_num, function(x) as.numeric(as.factor(x))))

training_num$Churn <- as.integer(c(0,1)[as.factor(training_num$Churn)])
testing_num$Churn <- as.integer(c(0,1)[as.factor(testing_num$Churn)])

prop.table(table(training_num$Churn))

x1 <- cbind(as.matrix(training_num[,1:9]))
x1.1 <- as.matrix(training_num[,11:14])

x1 <- cbind(x1,x1.1)
x1 <- as.matrix(x1)
y1 <- as.vector(training_num[,10])

# Establishing the hypothesis

# H0: All the means of IV are equal 
# H1: All the means of IV are NOT equal 

manova_1 <- manova(x1 ~ y1)

summary(manova_1,test="Wilks")

# Means of the IV are NOT equal p-value < 0.05

# Now we look for the combination of features that separates customers from churning

discPower(x1,y1)

# Fischer discriminat functions

desDA(x1,y1)

# Sort the variables by descending order of the coeff, to understand each indiv influence

# debtors            3.071819
# PaperlessBilling   0.437109
# SeniorCitizen      0.377901
# MultipleLines      0.222390
# protect_level      0.083011
# PaymentMethod      0.053759
# gender             0.006219
# client_category   -0.064409
# Partner           -0.122461
# tenure_group      -0.136499
# Dependents        -0.158753
# TechSupport       -0.305241
# Contract          -0.790330
# constant          -2.390027

# So we could say that as debtors increase the prob of churning increases
# and the higher the contract the lower the prob of churn.

# Differentiation if IV and Wilks' Lambda

# The best variable is gender

# Clasify records and predict

sublda <- lda(Churn ~ debtors          
              + PaperlessBilling   
              + SeniorCitizen      
              + MultipleLines      
              + protect_level      
              + PaymentMethod     
              + gender             
              + client_category   
              + Partner           
              + tenure_group  
              + Dependents        
              + TechSupport       
              + Contract , training_num)
sublda

means <- sublda$means

colnames(means)

means <- rbind(means,abs(means[1,]-means[2,]))

barchart(means[3,],means,main="Difference in means",ylab="I. Variable",xlab="Difference in means") 

# The highest mean dif is for contract and the lowest for gender. 

# Let's see which variables contribute more to group separation

discriminant_analisis <- sublda$scaling

barchart(discriminant_analisis)

# Let's analyse what our churn (1) clientes look compared who those who do not churn (2)

plot(sublda,dimen=1,type="b")

# It clearly shows that the two groups ae not completly separeted

# Let's predict churn based on the x variables of the test set.

lda.pred <- predict(sublda,testing_num)

class.lda <- lda.pred$class

true.class <- testing_num[,10]

lda.counts <- misclassCounts(class.lda,true.class)

lda.counts$conf.matrix


# Let's check the metrixcs

print(lda.counts$metrics,digits = 3)

# Performance of the model is 77.6%

accuracy_results <- data.frame ("Method"= "LDA" , "Accuracy"= 0.78)

##### Model 4 PCA ####

pr_train <- prcomp(training_num,center=T,scale. = T)

summary(pr_train)

# Evaluation of variances

plot(pr_train, type="l")

# Relationship between vars

biplot(pr_train, scale = 0)

# Gender and tenure_group seem to be cetroid

pca_var <- pr_train$sdev ^ 2

pca_var_prop <- pca_var / sum(pca_var)

plot(cumsum(pca_var_prop), type = 'b', ylab = "Cummulative Variance", 
     xlab = "Principal Components",main = "Principal Components VS Variance Explained")

# Let's test since no variable need to be removed

rpart_pca <- rpart(Churn ~ ., data=training_num)

predict_rpart.pca <- predict(rpart_pca,newdata = testing_num)

pred_rpart_pca <- prediction(predict_rpart.pca,testing_num$Churn==0)

perf_rpart_pca <- performance(pred_rpart_pca,"tpr","fpr")

plot(perf_rpart_pca, colorize= TRUE)

myroc <- roc(testing_num$Churn == 0, predict_rpart.pca)

c_pca <- coords(myroc,"best",ret = "threshold")

churned <- ifelse(predict_rpart.pca > c_pca[[1]],1,0)

churned <- as.factor(churned)

testing_num$Churn <- as.factor(testing_num$Churn)

cm_pca <- confusionMatrix(churned,testing_num$Churn)

cm_pca


accuracy_results <- data.frame ("Method"= "PCA" , "Accuracy"= 0.64)

# Model precision 0.64



##### Model 5 KNN ####

k <- sqrt(nrow(d_churn))
k <- round(k,0)
k

# Setting up train controls

repeats= 10
numbers = 30
tunnel = 3

x=trainControl(method="repeatedcv",number=numbers,repeats = repeats,classProbs = TRUE,summaryFunction = twoClassSummary)

knn1 <- train(Churn ~ .,data=training_1,method="knn",preProcess = c("center","scale"),
              trControl= x, metric= "ROC",
              tuneLength = tunnel)

# Summary of model

knn1
plot(knn1,print.thres=0.5,type="s")

# model based on K=9

# Validation

knnpred <- predict(knn1,newdata=testing_1,type="prob")

knnroc <- roc(testing_1$Churn,knnpred[,"No"],levels = c("No","Yes"))

knnroc

# Model Scores

pred_val <- prediction(valid_pred[,2],testing_1$Churn)

# Calculation AUC

perf_val <- performance(pred_val,"auc")
perf_val

# Plot AUC

perf_val <- performance(pred_val,"tpr","fpr")

plot(perf_val,col="green",lwd=1.5)

# Calculating KS

ks <- max(attr(perf_val,'y.values')[[1]]-(attr(perf_val,'x.values')[[1]]))
ks

# The area under the curve for test set is 0.5


# Confusion matrix

knnpredict <- predict(knn1,newdata=testing_1)

confusionMatrix(knnpredict,testing_1$Churn)

# 78.9 % accuracy 



##### Final Model Random Forest #####

rfmodelfinal <- randomForest(Churn ~ debtors + tenure_group
                              + Contract
                              + client_category
                              + TechSupport
                              + PaymentMethod
                              + PaperlessBilling
                              + SeniorCitizen
                              + protect_level
                              + Partner ,data=training_1)
print(rfmodelfinal)



# Matrix to penalise false negatives

penaltymatrix <- matrix(c(0,2,5,0), byrow= TRUE,nrow=2)

rfmodelfinpen <- randomForest(Churn ~ debtors 
                              + tenure_group
                              + Contract
                              + client_category
                              + TechSupport
                              + PaymentMethod
                              + PaperlessBilling
                              + SeniorCitizen
                              + protect_level
                              + Partner ,data=training_1,
                              ntree= 50,
                              nodesize=20,
                              cp=0.005,
                              parms= list(loss=penaltymatrix))


predrf_final <- predict(rfmodelfinpen, testing_1)

# Confusion matrix

caret::confusionMatrix(predrf_final,testing_1$Churn)

# We managed to increase neg pred val to 65 %

# ROC curve and AUC

predrf_pred <- predict(rfmodelfinpen, testing_1,type = "prob")

pred <- prediction(predrf_pred[,2],testing_1$Churn)

ROCRperf <- performance(pred,"tpr","fpr")

auc <- as.numeric(performance(pred,"auc")@y.values)

auc # AUC is high

# calculating AUPRC

prc_dataFrame <- data.frame(predrf_final,testing_num$Churn)

prc <- pr.curve(prc_dataFrame[prc_dataFrame$testing_num.Churn == 1,]$predrf_final,prc_dataFrame[prc_dataFrame$testing_num.Churn==0,]$predrf_final,curve = T)

prc

plot(prc)