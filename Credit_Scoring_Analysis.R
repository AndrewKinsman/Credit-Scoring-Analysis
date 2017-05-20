###### CREDIT SCORING ######

# Packages required: nortest, mice, ggplot2, caTools, rpart, rpart.plot, rattle, MASS, ROCR, caret,
# class, e1071, pROC, gbm.

# First download data from https://www.kaggle.com/c/GiveMeSomeCredit/data and set the 
# working directory to the folder where those data are stored. In my case this is
setwd("C:/Program Files/R/R_Working_Directory/Kaggle_Credit") 
# but that directory will of course be different for others.

# Now read in both the training and test sets supplied by Kaggle:
credit <- read.csv("cs-training.csv")
cs_test <- read.csv("cs-test.csv")

### DATA IMPORT ###

# Initial look at training set:
str(credit) 

# Here we see that there are 150000 observations (rows) with 12 variables (columns). The first 
# column, "X", is just a row number, and we then have the dependent (or "target") variable 
# "SeriousDlqin2yrs" followed by the ten dependent (or # "explanatory") variables.

### EXPLORATORY DATA ANALYSIS (Summaries and Visualisations)
summary(credit)

# Analyse target variable (Serious Delinquency within two years)
mean(credit$SeriousDlqin2yrs) # mean is only 0.06684, so only 6.7% of loans resulted in 
# delinquency.
table(credit$SeriousDlqin2yrs) # 139974 non-delinquents vs. 10026 delinquents so this is an 
# "unbalanced" dataset.

# Bar chart of target variable (FIG. 1)
ylim <- c(0, 1.1*max(table(credit$SeriousDlqin2yrs))) # create space for text
bp <- barplot(table(credit$SeriousDlqin2yrs), main="Bar Chart of Seriously Delinquent Loans", 
              xlab="Loan Outcome", ylab="Number of Loans", ylim=ylim, 
              names.arg=c("Non-Delinquent", "Delinquent"), col=c("blue", "red"))
text(x=bp, y=table(credit$SeriousDlqin2yrs), label=table(credit$SeriousDlqin2yrs), cex=1, pos=3) 

# Age
table(credit$age) 

# Side-by-side plots for age variable (FIG. 2)
par(mfrow=c(1,2)) # set up side-by-side plot display
hist(credit$age, main="Histogram of \nBorrower's Age", 
     xlab="Age", col="brown") 
qqnorm(credit$age, main="Normal Q-Q Plot \nof Borrower's Age", col="brown") 
par(mfrow=c(1,1)) # return to single plot display

# Test age for normality
library(nortest)
lillie.test(credit$age)

# Monthly income
table(credit$MonthlyIncome > 1000000/12) 

# Side-by-side plot of monthly income (FIG. 3)
par(mfrow=c(1,2)) # set up side-by-side plot display
hist(credit$MonthlyIncome, main="Histogram of \nMonthly Income", 
     xlab="Income", col="gold3") # FIG. 3
qqnorm(credit$MonthlyIncome, main="Normal Q-Q Plot \nof Monthly Income", col="gold3")

# Boxplot of monthly income (FIG .4)
boxplot(credit$MonthlyIncome, main="Standard R Boxplot \nof Monthly Income \n(with outliers)", 
        ylab="Monthly Income", col="purple") 
boxplot(credit$MonthlyIncome, main="Boxplot of Monthly \nIncome (outliers removed)", 
        ylab="Monthly Income", outline=F, col="green") 
par(mfrow=c(1,1)) # return to single plot display

# Number of open credit loans or lines
hist(credit$NumberOfOpenCreditLinesAndLoans)  # heavily right-skewed
table(credit$NumberOfOpenCreditLinesAndLoans)

# Number of dependents
hist(credit$NumberOfDependents) 
table(credit$NumberOfDependents) 

# Number of real estate loans or lines
hist(credit$NumberRealEstateLoansOrLines)
table(credit$NumberRealEstateLoansOrLines) 

# Revolving utilization of unsecured lines
hist(credit$RevolvingUtilizationOfUnsecuredLines) # heavily right-skewed
table(credit$RevolvingUtilizationOfUnsecuredLines > 10)

# Debt ratio    
hist(credit$DebtRatio) # heavily right-skewed
table(credit$DebtRatio > 10000)

# 30-59, 60-89 and 90+ days late
table(credit$NumberOfTime30.59DaysPastDueNotWorse) 
table(credit$NumberOfTime60.89DaysPastDueNotWorse) 
table(credit$NumberOfTimes90DaysLate) # All three tables show the same outliers: exactly 5 values 
# of 96 and 264 values of 98. 
hist(credit$NumberOfTime30.59DaysPastDueNotWorse) # Right-skewed

### DATA PREPARATION ###

# Merge training and testing datasets for combined preparation
credit_merged <- rbind(credit, cs_test)

# Review combined dataset
str(credit_merged)
summary(credit_merged)

# Replace 96 and 98 values in 30-59, 60-89 and 90+ days late columns by replacing them with 
# median
credit_merged$NumberOfTime30.59DaysPastDueNotWorse[credit_merged$
                                                      NumberOfTime30.59DaysPastDueNotWorse == 96 |
                                                      credit_merged$NumberOfTime30.59DaysPastDueNotWorse == 98] <- 
                                                      median(credit_merged$NumberOfTime30.59DaysPastDueNotWorse)
credit_merged$NumberOfTime60.89DaysPastDueNotWorse[credit_merged$
                                                      NumberOfTime60.89DaysPastDueNotWorse == 96 |
                                                      credit_merged$NumberOfTime60.89DaysPastDueNotWorse == 98] <-
                                                      median(credit_merged$NumberOfTime60.89DaysPastDueNotWorse)
credit_merged$NumberOfTimes90DaysLate[credit_merged$NumberOfTimes90DaysLate == 96 |
                                                      credit_merged$NumberOfTimes90DaysLate == 98] <-  
                                                      median(credit_merged$NumberOfTimes90DaysLate)

# Deal with erroneous "zero" age values (replace with median age value)
median(credit_merged$age)
credit_merged$age[credit_merged$age==0] <- median(credit_merged$age)
min(credit_merged$age) # here we see that the minimum age is now 21.

# Deal with many thousand missing values for monthly income and number of dependents using 
# multiple imputation
library(mice)
md.pattern(credit_merged) # missing 6550 no of dependebts and 49834 monthly income values 
# across merged dataset.
vars_for_imputation <- credit_merged[c("MonthlyIncome", "NumberOfDependents")] # The two 
# variables that require imputation.
set.seed(180) # This sets the seed for the random number generator so that results will be 
# reproducible.
# ***WARNING - The next step takes a very long time to process; once it has been completed it is 
# easiest just to load the new values via a csv file.***
imputed <- complete(mice(vars_for_imputation)) 
write.csv(imputed, "imputed_export.csv") # saves imputed values as csv file for later use. 
# imputed <- read.csv("imputed_export.csv", header=T) # run this if/when importing the new 
# csv file (which can be supplied if required).
credit_merged$MonthlyIncome <- imputed$MonthlyIncome # incorporates imputed income values 
# to dataframe.
credit_merged$NumberOfDependents <- imputed$NumberOfDependents # incorporates imputed 
# no of dependents values to dataframe.

# Review dataset
summary(credit_merged) # Here we see that some of the columns have been tidied up, with all 
# missing values removed except for the 101,289 target variable values that we are trying 
# to predict. 

# split dataset back into original two components, training set and test set.
credit <- credit_merged[1:150000,]
cs_test <- credit_merged[150001:251503,]

# Construct correlation matrix.
ctab <- cor(credit)
ctab <- round(ctab, 2) # Round to two decimal places for easier viewing.
ctab 

# Look at sd of each variable
apply(credit, 2, sd)

# For use with plot below
median(credit$NumberOfTime30.59DaysPastDueNotWorse[credit$SeriousDlqin2yrs==1])
median(credit$NumberOfTime30.59DaysPastDueNotWorse[credit$SeriousDlqin2yrs==0])
mean(credit$NumberOfTime30.59DaysPastDueNotWorse[credit$SeriousDlqin2yrs==1])
mean(credit$NumberOfTime30.59DaysPastDueNotWorse[credit$SeriousDlqin2yrs==0])

# Turn dependent variable into factor for modelling. 
credit$SeriousDlqin2yrs <- factor(credit$SeriousDlqin2yrs, labels=c("Not Seriously Delinquent", 
                                                                    "Seriously Delinquent")) 

# More advanced boxplot (FIG. 5) 
library(ggplot2)
ggplot(credit, aes(x = SeriousDlqin2yrs, y = NumberOfTime30.59DaysPastDueNotWorse, 
                  fill = SeriousDlqin2yrs)) + geom_boxplot() + 
                  labs(title="Number of Times \n30-59 Days Past Due \nvs. Serious Delinquency", 
                  x="Delinquency within Two Years", 
                  y="Number of Times 30-59 Days past Due", face="bold") + 
theme(plot.title = element_text(size=16, face="bold", vjust=1)) + 
# modify title header
  theme(axis.title = element_text(size=13, face="bold")) + 
  # modify x-axis header
  guides(fill=FALSE) # remove legend

# Split data into training set and validation set for modelling using caTools package
library(caTools)
set.seed(180)
split <- sample.split(credit$SeriousDlqin2yrs, SplitRatio = 0.7) # split using target variable as base
train <- subset(credit, split == TRUE)
val <- subset(credit, split == FALSE)

### DECISION TREE MODEL ###

# Create  model using rpart package
library(rpart)
trainTree <- rpart(SeriousDlqin2yrs ~ .-X, data=train)
summary(trainTree)

# Plot decision tree (FIG.6)
library(rpart.plot)
library(rattle)
fancyRpartPlot(trainTree, main="Decision Tree") 

# Establish baseline accuracy rate 
table(val$SeriousDlqin2yrs) 
41992/nrow(val) # 93.32% of these loans were non-delinquent (baseline).

# Make predictions on (unseen) validation data
val$predicted_risk_Tree <- predict(trainTree, newdata=val, type="class")
summary(val$predicted_risk_Tree)
table(val$predicted_risk_Tree)
867/nrow(val) # only 2% delinquency rate so model is underrepresenting delinquency rate   
treeTable <- table(val$SeriousDlqin2yrs, val$predicted_risk_Tree, dnn=c("Actual", "Predicted"))
treeTable 

# Assess performance using function from R in Action (Kabacoff, p.406)
model_performance <- function(table, n=5){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}

model_performance(treeTable)

### LOGISTIC REGRESSION MODEL ###

# First attempt - complete separation issue
trainLog <- glm(SeriousDlqin2yrs ~ . -X, data=train, family=binomial)
# "Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred"

# Identify and resolve issue
table(train$SeriousDlqin2yrs, train$MonthlyIncome > 250000)
train$MonthlyIncome_cat <- cut(train$MonthlyIncome, breaks = c(0, 1000, 5000, 10000, 
                                                               50000, 10000000), include.lowest=TRUE)
val$MonthlyIncome_cat <- cut(val$MonthlyIncome, breaks =  c(0, 1000, 5000, 10000, 
                                                            50000, 10000000), include.lowest=TRUE)
table(train$MonthlyIncome_cat)

# New logistic regression model with income in bins
trainLog1 <- glm(SeriousDlqin2yrs ~ .-X -MonthlyIncome, 
                 data=train, family=binomial)
summary(trainLog1)

# Stepwise regression
library(MASS)
trainLog2 <- stepAIC(trainLog1, direction="backward")
trainLog2

# View coefficients
coef(trainLog2)

# Create example prediction
prediction <- -1.808513+(40*-0.02768499)+(1*0.04225236)+(0.03647074)
prediction # -2.83719

# Convert to probability
odds <- exp(prediction)
odds # 0.0585901
probability <- odds/(odds + 1)
probability # 0.0553473

# Odds ratio
exp(coef(trainLog2))
exp(confint(trainLog2)) # 95% confidence intervals for the coefficients (on an odds scale).

# compare to trainLog1 using ANOVA
anova(trainLog2, trainLog1, test="Chisq") # 0.4658 so statistically significant difference in fit.

# Make predictions on (unseen) validation data
val$predicted_risk_Log2 <- predict(trainLog2, type="response", newdata=val)
summary(val$predicted_risk_Log2)
table(val$predicted_risk_Log2 > 0.5)
777/nrow(val) # again less than 2% of loans are being predicted as serious delinquency
log2Table_0.5 <- table(val$SeriousDlqin2yrs, val$predicted_risk_Log2 > 0.5, dnn=c("Actual", 
                                                                                  "Predicted"))
log2Table_0.5
model_performance(log2Table_0.5)

# Change threshold to 0.2
log2Table_0.2 <- table(val$SeriousDlqin2yrs, val$predicted_risk_Log2 > 0.2, dnn=c("Actual", 
                                                                                  "Predicted"))
log2Table_0.2
model_performance(log2Table_0.2)

# Percentage of predictions that were predicted serious delinquency
(958+1211)/nrow(val) # 0.0482

# AUC
library(ROCR)
ROCRLog2 <- prediction(val$predicted_risk_Log2, val$SeriousDlqin2yrs)
aucLog2 <- as.numeric(performance(ROCRLog2, "auc")@y.values)
aucLog2 # 0.8096439


# Plot accuracy vs. threshold (# FIG.8)
plot(performance(ROCRLog2, measure = "acc"), col="purple", xlab="Threshold", main=
       "Logit Model Accuracy \nfor Different Thresholds") 
axis(1, at=c(0.1, 0.3, 0.5, 0.7, 0.9))
axis(2, at=c(0.1, 0.3, 0.5, 0.7, 0.9)) 

# Prepare ROC performance for subsequent gbm plot
perf_Log2 <- performance(ROCRLog2, "tpr", "tnr")

### GBM MODEL ### *** WARNING - THIS MAY TAKE SEVERAL HOURS TO PROCESS

# load caret and other required packages
library(caret)
library(class)
library(e1071)
library(pROC)
library(gbm)

train$MonthlyIncome_cat <- NULL
val$MonthlyIncome_cat <- NULL
levels(train$SeriousDlqin2yrs) <- c("No", "Yes")
levels(val$SeriousDlqin2yrs) <- c("No", "Yes")
gbmControl <- trainControl(method='cv', number=3, returnResamp='none', 
                           summaryFunction = twoClassSummary, classProbs = TRUE)
set.seed(180)
gbm <- train(SeriousDlqin2yrs ~ .-X, data=train,
             method="gbm", 
             trControl=gbmControl,           
             metric = "ROC",
             preProc = c("center", "scale"), verbose = FALSE)
summary(gbm) # which variables were most important to the model?
plot(varImp(gbm,scale=F)) # plot variable importance
ggplot(gbm) # plots ROC vs. boosting iterations of the three training models
print(gbm) # find out Which tuning parameters were most important to the model

# Evaluate
val$gbm_predictions <- predict(gbm, val, type="raw") 
postResample(val$gbm_predictions, val$SeriousDlqin2yrs) # 0.9365111 accuracy 
gbmTable <- table(val$gbm_predictions, val$SeriousDlqin2yrs, dnn=c("Actual", "Predicted")) 
gbmTable 
model_performance(gbmTable) # poor precision (positive predictive value) but model is making 
# many more predictions of serious deliquency occurring than tree and logit, as shown here:

# Percentage of predictions that were predicted serious delinquency
(600+2408)/nrow(val) # 0.06684444

# Now look at probabilities:
gbm_predictions <- predict(object=gbm, val, type="prob")
val$gbm_predictions_prob <- gbm_predictions[,2]
gbm_ROC <- roc(predictor=val$gbm_predictions_prob,
               response=val$SeriousDlqin2yrs,
               levels=c("No", "Yes"))
gbm_ROC #  AUC is 0.863

### RANDOM FOREST ### *** WARNING - THIS MAY TAKE SEVERAL HOURS TO PROCESS
rfControl <- trainControl(method = "cv", number = 3, returnResamp="none", 
                          summaryFunction = twoClassSummary, classProbs = TRUE)
set.seed(180)
rf <- train(SeriousDlqin2yrs ~ .-X, data=train,
            method="rf", 
            trControl=rfControl,           
            metric = "ROC",
            preProc = c("center", "scale"))
plot(varImp(rf,scale=F)) # plot variable importance
print(rf) # find out Which tuning parameters were most important to the model (tuning may 
# improve performance)

# Evaluate
val$rf_predictions <- predict(rf, val, type="raw")
postResample(val$rf_predictions, val$SeriousDlqin2yrs) # accuracy of 0.9355778
rfTable <- table(val$rf_predictions, val$SeriousDlqin2yrs, dnn=c("Actual", "Predicted")) 
rfTable
model_performance(rfTable)

# Percentage of predictions that were predicted serious delinquency
(460+2548)/nrow(val) # 0.06684444

# Now look at probabilities:
rf_predictions <- predict(rf, val, type="prob")
val$rf_predictions_prob <- rf_predictions[,2]
rf_ROC <- roc(predictor=val$rf_predictions_prob,
              response=val$SeriousDlqin2yrs,
              levels=c("No", "Yes"))
rf_ROC #  AUC is 0.8462

# Plot ROC curves
plot(gbm_ROC, col="red", main="ROC Curves" )
plot(perf_Log2, add=TRUE, col = 'blue')
plot(rf_ROC, add=TRUE, col = 'green')
legend("bottomright", legend=c("Logit", "GBM", "Random Forest"),
       col=c("blue", "red", "green"), lwd=2) # FIG.9

### GBM with additional monthly income/family size ### *** WARNING - THIS MAY TAKE AGES
family_size_train <- train$NumberOfDependents + 1 # create family size variable
family_size_val <- val$NumberOfDependents + 1
train$income_family_size <- train$MonthlyIncome/family_size_train
val$income_family_size <- val$MonthlyIncome/family_size_val
set.seed(180)
gbm2 <- train(SeriousDlqin2yrs ~ .-X, data=train,
              method="gbm", 
              trControl=gbmControl,           
              metric = "ROC",
              preProc = c("center", "scale"), verbose = FALSE)
summary(gbm2) # which variables were most important to the model?
ggplot(gbm2) # plots ROC vs. boosting iterations of the three training models
plot(varImp(gbm2,scale=F)) # plot variable importance
print(gbm2) # find out Which tuning parameters were most important to the model

# Evaluate
val$gbm2_predictions <- predict(gbm2, val, type="raw") 
postResample(val$gbm2_predictions, val$SeriousDlqin2yrs) # 0.9365111 accuracy 
gbm2Table <- table(val$gbm2_predictions, val$SeriousDlqin2yrs, dnn=c("Actual", "Predicted")) 
gbm2Table 
model_performance(gbm2Table) # poor precision (positive predictive value) but model is making 
# many more predictions of serious deliquency occurring than tree and logit, as shown here:

# Percentage of predictions that were predicted serious delinquency
(607+2401)/nrow(val) # 0.06684444

# Now look at probabilities:
gbm2_predictions <- predict(object=gbm2, val, type="prob")
val$gbm2_predictions_prob <- gbm2_predictions[,2]
gbm2_ROC <- roc(predictor=val$gbm2_predictions_prob,
                response=val$SeriousDlqin2yrs,
                levels=c("No", "Yes"))
gbm2_ROC #  AUC is 0.863


### SUBMISSIONS TO KAGGLE ###
# Edit test dataframe for consistency with training data
cs_test$SeriousDlqin2yrs <- factor(cs_test$SeriousDlqin2yrs, levels = c(0,1))

# create decision tree entry
cs_test$predicted_risk_Tree <- predict(trainTree, type ="prob", cs_test)
submissionTree <- data.frame(Id = seq(1:101503), Probability = cs_test$predicted_risk_Tree) 
names(submissionTree) <- c("Id", "Unused", "Probability")
write.csv(submissionTree[c(1,3)], "submissionTree.csv", row.names=FALSE) # 0.648290 (868th)

# create stepwise logistic regression entry
cs_test$MonthlyIncome_cat <- cut(cs_test$MonthlyIncome, breaks =  c(0, 1000, 5000, 10000, 
                                                                    50000, 10000000), include.lowest=TRUE)
cs_test$predicted_risk_Log2 <- predict(trainLog2, type ="response", cs_test)
submissionLog2 <- data.frame(Id = seq(1:101503), Probability = cs_test$predicted_risk_Log2) 
write.csv(submissionLog2, "submissionLog.csv", row.names=FALSE) # 0.812894 (756th)
cs_test$MonthlyIncome_cat <- NULL

# create GBM entry
cs_test$predicted_risk_GBM <- predict(gbm, type ="prob", cs_test)
submissionGBM <- data.frame(Id = seq(1:101503), Probability = cs_test$predicted_risk_GBM) 
names(submissionGBM) <- c("Id", "Unused", "Probability")
write.csv(submissionGBM[c(1,3)], "submissionGBM.csv", row.names=FALSE) # 0.86413 (417th place)

# create RF entry
cs_test$predicted_risk_RF <- predict(rf, type ="prob", cs_test)
submissionRF <- data.frame(Id = seq(1:101503), Probability = cs_test$predicted_risk_RF) 
names(submissionRF) <- c("Id", "Unused", "Probability")
write.csv(submissionRF[c(1,3)], "submissionRF.csv", row.names=FALSE) # 0.853354 (635th place)

# create GBM entry with additional derived variable
family_size_cs_test <- cs_test$NumberOfDependents + 1 # create family size variable
cs_test$income_family_size <- cs_test$MonthlyIncome/family_size_cs_test
cs_test$predicted_risk_GBM2 <- predict(gbm2, type ="prob", cs_test)
submissionGBM2 <- data.frame(Id = seq(1:101503), Probability = cs_test$predicted_risk_GBM2) 
names(submissionGBM2) <- c("Id", "Unused", "Probability")
write.csv(submissionGBM2[c(1,3)], "submissionGBM2.csv", row.names=FALSE) # 0.864452 
# (321st place)

# create ensemble entry
submission_Ens <- cbind(submissionGBM[c(1,3)], submissionRF[3])
submission_Ens$merge <- (submission_Ens[,2] + submission_Ens[,3])/2
names(submission_Ens) <- c("Id", "Unused1", "Unused2", "Probability")
write.csv(submission_Ens[c(1,4)], "submissionEns.csv", row.names=FALSE) # 0.865339 (221st place)