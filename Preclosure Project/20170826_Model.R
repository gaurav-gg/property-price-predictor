library(ggplot2)
library(lattice)
library(caret)
library(randomForest)
library(ROCR)
library(pROC)
library(gmodels)
library(rpart)
library(rpart.plot)
library(doSNOW)
library(DMwR)
library(xgboost)
library(dplyr)
library(broom)

################# Load and split data ###########################

# Load raw data
CompleteData <- read.csv("R_Input.csv", header = TRUE)

# Convert Foreclosed as a factor
CompleteData$Foreclosed <- as.factor(CompleteData$Foreclosed)

CompleteData$Foreclosed <- relevel(CompleteData$Foreclosed, "TRUE", "FALSE")

table(CompleteData$Foreclosed)

# Pick out the output column
CompleteData_Foreclosed <- as.factor(CompleteData$Foreclosed)

# Check and clean the data types
str(CompleteData)
CompleteData$PaymentDelay <- as.integer(CompleteData$PaymentDelay)

# 70% of the data is split, maintaining the same proportion
intrain <- createDataPartition(CompleteData_Foreclosed,p=0.6,list=FALSE)

# For the testing and training data sets using the intrain row indexes
train<-CompleteData[intrain,]
testwithfinal<-CompleteData[-intrain,]

# Split the test data in half again. One of them is to be kept for final validation
finaltestindex <- createDataPartition(testwithfinal$Foreclosed,p=0.5,list=FALSE)

test <- testwithfinal[finaltestindex,]
finaltestdata <- testwithfinal[-finaltestindex,]

# Check proportion of pre-closures in the testing and training sets
table(train$Foreclosed)
table(test$Foreclosed)

# In the next few sections, we'll look at visualizing the data for each variable and clean the data wherever necessary

####### SqftRate #################

ggplot(CompleteData, aes(x = SqftRate)) +
  geom_dotplot()

# Let's replot using a box plot to identify outliers further

ggplot(CompleteData, aes(x = 1, y = SqftRate)) +
  geom_boxplot()

# We notice there are a couple of outliers in the data. This is clearly a mistake as square feet rates are unlikely to be upwards of 5,00,000rs per square feet. (It could be, in bombay) Let's identify, exclude this point and replot the data

CompleteData <- CompleteData %>% filter(SqftRate<20000)

# Let's replot

ggplot(CompleteData, aes(x = SqftRate)) +
  geom_dotplot()

ggplot(CompleteData, aes(x = 1, y = SqftRate)) +
  geom_boxplot()

# Are the cases with >5000 rs per square feet incorrect? Perhaps they come from urban properties. Let's explore.

CompleteData %>%
    filter(SqftRate > 5000) %>%
    ggplot(aes(x = CityType)) +
    geom_histogram(stat = "count")

# Most of the high square feet rate cases seem to be from metro properties. Hence, it is reasonable to assume that these values are legitimate and may be retained for modeling. (Don't worry, these customer rows have also been verified manually)

CompleteData %>%
  ggplot(aes(x = SqftRate, fill = Foreclosed)) +
  geom_histogram(stat = "bin")

####### Loan Amount #############

CompleteData %>%
  ggplot(aes(x = 1, y = LoanAmount)) +
  geom_boxplot()

# There seem to be some loans greater than 50 lakh. This is unlikely, since HFFC restricts loan amounts to less than or equal to 50 lakh.

# Let's look at the data differently

CompleteData %>%
  filter(LoanAmount < 3000000) %>%
  ggplot(aes(x = LoanAmount, fill = Foreclosed)) +
  geom_histogram(stat = "bin")


CompleteData %>%
  filter(LoanAmount < 3000000) %>%
  ggplot(aes(x = LoanAmount, fill = Foreclosed)) +
  geom_histogram(stat = "bin", position = "fill")

# Let's remove the row where the loan amount is greater than 50 lakh
CompleteData <- CompleteData %>% filter(LoanAmount < 5000000)

####### GPTPZP #################

CompleteData %>%
  ggplot(aes(x = GPTPZP, fill = Foreclosed)) +
  geom_histogram(stat = "count")

# We notice that TP, zP properties seem to have a higher pre-closure rate. Let's look at proportions to validate this.

CompleteData %>%
  ggplot(aes(x = GPTPZP, fill = Foreclosed)) +
  geom_histogram(stat = "count", position = "fill")

####### CIBIL ############

CompleteData %>%
  filter (CIBIL <=5) %>%
  ggplot(aes(x = CIBIL, fill = Foreclosed)) +
  geom_histogram()

CompleteData %>%
  filter (CIBIL <=5) %>%
  ggplot(aes(x = CIBIL, fill = Foreclosed)) +
  geom_histogram(position = "fill")


CompleteData %>%
  filter (CIBIL >5) %>%
  ggplot(aes(x = CIBIL, fill = Foreclosed)) +
  geom_histogram()

CompleteData %>%
  filter (CIBIL >5) %>%
  ggplot(aes(x = CIBIL, fill = Foreclosed)) +
  geom_histogram(position = "fill")


CompleteData <- CompleteData %>% filter (CIBIL <=5 | CIBIL > 300)


####### vintage Default ##########

CompleteData %>%
  ggplot(aes(x = VintageDefault, fill = Foreclosed))+
  geom_histogram()

CompleteData %>%
  ggplot(aes(x = VintageDefault, fill = Foreclosed))+
  geom_histogram(position = "fill")

####### Payment Delay ##########


CompleteData %>%
  filter(Vintage >20) %>%
  ggplot(aes(x = PaymentDelay, fill = Foreclosed))+
  geom_histogram(breaks = c(0,30,60,90,120))


####### RoI ##########

CompleteData %>%
  ggplot(aes(x = 1, y = RoI))+
  geom_boxplot()

CompleteData %>%
  filter(RoI > 10, RoI < 15, Vintage > 20) %>%
  ggplot(aes(x = RoI, fill = Foreclosed))+
  geom_histogram(binwidth = 0.5)

CompleteData %>%
  filter(RoI > 10, RoI < 15) %>%
  ggplot(aes(x = RoI, fill = Foreclosed))+
  geom_histogram(position = "fill", binwidth = 0.5)

CompleteData %>%
  filter(RoI > 10, RoI < 15) %>%
  ggplot(aes(x = RoI, fill = Foreclosed))+
  geom_histogram(position = "fill", binwidth = 0.5)



####### CityType ##########

CompleteData %>%
  ggplot(aes(x = CityType, fill = Foreclosed)) +
  geom_histogram(stat = "count", position = "fill") 
  
######## Vintage ############

CompleteData %>%
  ggplot(aes(x = Vintage, fill = Foreclosed)) +
  geom_histogram(stat = "bin", position = "fill") 

CompleteData %>%
  ggplot(aes(x = Vintage, fill = Foreclosed)) +
  geom_histogram(stat = "bin") 


######## Total Units ############

CompleteData %>%
  filter(TotalUnits <= 3500) %>%
  ggplot(aes(x = TotalUnits, fill = Foreclosed)) +
  geom_histogram(stat = "bin") 


CompleteData %>%
  filter(TotalUnits <= 3000) %>%
  ggplot(aes(x = TotalUnits, fill = Foreclosed)) +
  geom_histogram(stat = "bin", position = "fill", breaks = c(100,200,300,400,500,600,700,800,900,1000)) 

CompleteData %>%
  filter(TotalUnits <= 3000) %>%
  ggplot(aes(x = TotalUnits, fill = Foreclosed)) +
  geom_histogram(stat = "bin", breaks = c(100,200,300,400,500,600,700,800,900,1000)) 


CompleteData %>%
  filter(TotalUnits <= 1000) %>%
  ggplot(aes(x = TotalUnits, fill = Foreclosed)) +
  geom_density(alpha = 0.6)

########## EMI ###########

CompleteData %>%
  filter(EMI < 45000) %>%
  ggplot(aes(x = EMI, fill = Foreclosed)) +
  geom_histogram(binwidth = 2500)


CompleteData %>%
  filter(EMI < 45000) %>%
  ggplot(aes(x = EMI, fill = Foreclosed)) +
  geom_histogram(position = "fill", binwidth = 5000)


########## LTV ###########

CompleteData %>%
  ggplot(aes(x = 1, y = LTV)) +
  geom_boxplot()


CompleteData %>%
  ggplot(aes(x = LTV, fill = Foreclosed)) +
  geom_histogram()

########## C_Income ###########

CompleteData %>%
  filter(C_Income < 300000) %>%
  ggplot(aes(x = 1, y = C_Income)) +
  geom_boxplot()

CompleteData <- CompleteData %>% filter(C_Income < 500000)

CompleteData %>%
  filter(C_Income < 200000) %>%
  ggplot(aes(x = C_Income, fill = Foreclosed)) +
  geom_histogram(breaks = c(10000,20000, 30000, 40000), position = "fill")

CompleteData %>%
  filter(C_Income < 200000) %>%
  ggplot(aes(x = C_Income, fill = Foreclosed)) +
  geom_histogram()




################# Functions to create random forests and make predictions ###########################

RunRandomForest <- function(seed, features, training, cutoffvector){
  set.seed(seed) 
  
  # Save the training data 
  rf.train <- training[,features]
  
  # Save the training set headers in a label for the random forest 
  rf.label <- as.factor(training$Foreclosed)
  
  #Train the model
  rf <- randomForest(x = rf.train, y = rf.label, importance = TRUE, ntree = 1000, cutoff = cutoffvector)
  
  #View the importance of different variables in the random forest model
  varImpPlot(rf)
  
  return(rf)
}

MakeConfusionMatrix <- function(rf, testing, features, cutoff){
  
  # Extract the relevant features from the test set
  rf.test <- testing[,features]
  
  # Predict a response for each row  
  rf.preds.prob <- predict(rf, rf.test, type = "prob")
  
  rf.preds.resp <- ifelse(rf.preds.prob[,2] > cutoff, 1, 0)
  
  #Save the true values of the test set in a separate vector
  true_val <- as.factor(testing$Foreclosed)
  
  # Store the confusion matrix
  rf.preds.confmatrix <- table(true_val,rf.preds.resp)
  
  return(rf.preds.confmatrix)
  
}

MakeProbPredictions <- function(rf, testing, features){
  
  # Extract the relevant features from the test set
  rf.test <- testing[,features]
  
  # Predict a response for each row  
  rf.preds <- predict(rf, rf.test, type = "prob")
  
  return(rf.preds)
  
}

ComputeModelParameters <- function(confmatrix, testing){
  model_accuracy <- sum(diag(confmatrix)) / sum(confmatrix)
  #Overall accuracy of the model
  
  model_sensitivity <- confmatrix[2,2]/sum(confmatrix[2,])
  #Sensitivity of the model
  
  model_specificity <- confmatrix[1,1]/sum(confmatrix[1,])
  #Specificity of the model
  
  output <- data.frame(model_accuracy,model_sensitivity,model_specificity)
  
}


########## Make matrix for xgboost ##############

# one-hot-encoding categorical features
ohe_feats = c('B_LoanAmount', 'GPTPZP', 'B_CIBIL', 'B_ProductType', 'C_Gender', 'CityType', 'PrimaryEmpType',	'B_C_MasterEmpCat', 'B_C_Education', 'CoApplicant', 'B_FOIR', 'CLSS')

# Make a new data frame with all OHE-d columns for the training and test sets


# Creating a new data frame with dummy columns for the training set

dummies <- dummyVars(~ B_LoanAmount + GPTPZP + B_CIBIL + B_ProductType + C_Gender + CityType + PrimaryEmpType + B_C_MasterEmpCat + B_C_Education + CoApplicant + B_FOIR + CLSS, data = train)

ohe_columns <- as.data.frame(predict(dummies, newdata = train))

for (i in  1:ncol(ohe_columns))
{
  ohe_columns[,i] <- as.factor(ohe_columns[,i])
}

train_ohed <- cbind(train[,-c(which(colnames(train) %in% ohe_feats))],ohe_columns)

# Repeating the procedure for the test set

dummies_test <- dummyVars(~ B_LoanAmount + GPTPZP + B_CIBIL + B_ProductType + C_Gender + CityType + PrimaryEmpType + B_C_MasterEmpCat + B_C_Education + CoApplicant + B_FOIR + CLSS, data = test)

ohe_columns_test <- as.data.frame(predict(dummies_test, newdata = test))

test_ohed <- cbind(test[,-c(which(colnames(test) %in% ohe_feats))],ohe_columns_test)

#Store true value of the test set in a variable
true_val <- as.factor(test_ohed$Foreclosed)

table(train_ohed$Foreclosed)

#Use SMOTE to decrease class imbalance
train_smote_xgb <- SMOTE(Foreclosed ~ .,train_ohed,k = 5 ,perc.over = 100, perc.under = 200)

#Bypass smote-ing
#train_smote_xgb <- train_ohed

table(train_smote_xgb$Foreclosed)

# Save the training set actuals in a variable
rf.label <- as.factor(train_smote_xgb$Foreclosed)

table(train_smote_xgb$Foreclosed)

####### Train the xgboost model #######

#Select features to be included in xgb
xgbfeatures <- c(
                  #Geography
                  "CityType",
                  #Property
                  "TotalUnits", #"SqftRate", "GPTPZP",
                  #LoanDetails
                  "RoI","LTV","EMI", "Vintage", #"LoanAmount",
                  #PaymentBehaviour
                  "PaymentDelay","TimesBounced","VintageDefault",
                  #ApplicantCharacteristics
                  "PrimaryEmpType", "B_C_Education", "B_C_MasterEmpCat", "C_Income", "B_FOIR", #CIBIL
                  #Others
                  "TopUp", "CLSS"
                  )

matches <- c(unique(grep(paste(xgbfeatures,collapse="|"), colnames(train_smote_xgb), value=TRUE)), "CIBIL", "LoanAmount")

# Excluded parameters: Insurance, CoApplicant

y <- ifelse(train_smote_xgb$Foreclosed == TRUE, 1, 0)

#y <- train_smote_xgb$Foreclosed
y <- as.numeric(as.character(y))

#Select the relevant features and create a trimmed data.frame to be used for training
train_xgb_trim <- train_smote_xgb[,matches]
test_xgb_trim <- test_ohed[,matches]

for (i in  matches)
{
  train_xgb_trim[,i] <- as.numeric(as.character(train_xgb_trim[,i]))
  test_xgb_trim[,i] <- as.numeric(as.character(test_xgb_trim[,i]))
}

weights <- ifelse(y==0, 0.1, ifelse(y == 1, 5, 10))

xgb <- xgboost(data = data.matrix(train_xgb_trim), 
               label = y, 
               eta = 0.1,
               gamma = 0.1,
               max_depth = 5, 
               nround=2000, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1234,
               eval_metric = "error",
               objective = "binary:logistic",
#               num_class = 2,
               nthread = 4,
               verbose = 0,
               colsample_bylevel = 0.5,
               weight = weights
               )

xgbpred <- predict(xgb, data.matrix(test_xgb_trim))

predict_xgb <- ifelse(xgbpred > 0.5, 1, 0)

# Construct a confusion matrix
table(true_val, predict_xgb)

xgbparam <- ComputeModelParameters(table(true_val,predict_xgb), test_xgb_trim)

xgbparam

xgbmodel <- xgb.dump(xgb, with_stats = T)

# Compute feature importance matrix

importance_matrix <- xgb.importance(matches, model = xgb)

# Nice simple graph

xgb.plot.importance(importance_matrix[1:10,])

importance_matrix

################# Make and run Random Forests ###########################
  
  
#Use SMOTE to decrease class imbalance
train_smote <- SMOTE(Foreclosed ~ .,train,k = 5 ,perc.over = 100, perc.under = 200)


#Bypass smote-ing
#train_smote <- train

# Save the predictor column headers in a feature set vector
feature_set1 <- c(
  #Geography
  "CityType",
  #Property
  "TotalUnits", "SqftRate", "GPTPZP",
  #LoanDetails
  "RoI","LTV","EMI", "LoanAmount", "CIBIL", "Vintage", 
  #PaymentBehaviour
  "PaymentDelay","TimesBounced","VintageDefault",
  #ApplicantCharacteristics
  "PrimaryEmpType", "B_C_Education", "B_C_MasterEmpCat", 
  "C_Income", "B_FOIR", # "CoApplicant",
  #Others
  "CLSS", "TopUp" #"Insurance"
)


# Set the seed
seed <- 1234

# Define the cutoff
cutoff <- 0.5

?randomForest

summary(train_smote)

# Define the corresponding cutoff vector
cutoffvector1 <- c(1 - cutoff, cutoff)

rf.1 <- RunRandomForest(seed, feature_set1, train_smote,cutoffvector1)
rf.1

rf.1.preds <- MakeProbPredictions(rf.1, test,feature_set1)


rf.1.confmatrix <- MakeConfusionMatrix(rf.1, test,feature_set1, 0.5)
rf.1.confmatrix

rf.1.modelparam <- ComputeModelParameters(rf.1.confmatrix,test)
rf.1.modelparam
  
########## Make an average of both models ###########

averageprob <- (rf.1.preds[,2] + xgbpred)/2

predict_av <- ifelse(averageprob > 0.50, 1, 0)

# Construct a confusion matrix
table(true_val, predict_av)

avmodelparam <- ComputeModelParameters(table(true_val,predict_av),test)

avmodelparam

########### Plot ROC Curves and Calculate AUCs #################

#Plot the ROC curves
ROC_rf.1 <- roc(true_val, rf.1.preds[,2])
ROC_xgb <- roc(true_val, xgbpred)
ROC_av <- roc(true_val, averageprob)
#ROC_logit <- roc(true_val,pred_log_1)
#ROC_probit <- roc(true_val, pred_probit_1)
#ROC_cloglog <- roc(true_val, pred_cloglog_1)

plot(ROC_rf.1)
lines(ROC_xgb, col = "blue")
lines(ROC_av, col = "red")
#lines(ROC_logit, col="green")
#lines(ROC_probit, col = "red")
#lines(ROC_cloglog, col = "yellow")


plot(ROC_av, col = "blue")

# Compute the AUCs
auc(ROC_rf.1)
auc(ROC_xgb)
auc(ROC_av)
#auc(ROC_cloglog)
#auc(ROC_probit)
#auc(ROC_logit)

############## Loading and testing the data #############

# Load new data
#finaltestdata <- read.csv("Final_Input_v2.csv", header = TRUE) 

#finaltestdata <- read.csv("20170808_R_Input_v1.csv", header = TRUE)

str(finaltestdata)

ohe_columns_finaltest <- as.data.frame(predict(dummies, newdata = finaltestdata))

finaltest_ohed <- cbind(finaltestdata[,-c(which(colnames(finaltestdata) %in% ohe_feats))],ohe_columns_finaltest)

finaltest_xgb_trim <- finaltest_ohed[,matches]

for (i in  matches)
{
  finaltest_xgb_trim[,i] <- as.numeric(as.character(finaltest_xgb_trim[,i]))
}

xgbpredfinal <- predict(xgb, data.matrix(finaltest_xgb_trim))

rf.final.preds <- MakeProbPredictions(rf.1,finaltestdata, feature_set1)

averagefinalpred <- (rf.final.preds[,2]+xgbpredfinal)/2

finalpredictions <- cbind(finaltestdata, xgbpredfinal, rf.final.preds[,2], averagefinalpred)

finalpredbin <- ifelse(averagefinalpred >= 0.5, TRUE, FALSE)
xgbpredbin <- ifelse(xgbpredfinal >= 0.5, TRUE, FALSE)
rfpredbin <- ifelse(rf.final.preds[,2]>=0.5, TRUE,FALSE)
table(finaltestdata$Foreclosed, finalpredbin)
table(finaltestdata$Foreclosed, xgbpredbin)
table(finaltestdata$Foreclosed, rfpredbin)

finalparam <- ComputeModelParameters(table(finaltestdata$Foreclosed, finalpredbin),finaltestdata)
finalparam

write.csv(finalpredictions, "FinalPredictions.csv")


################# Make and run Probit Regression Models ###########################

probit_model_1 <- glm(Foreclosed ~ GPTPZP + TotalUnits + B_FOIR + C_Income + RoI + LTV + EMI + CIBIL  + PrimaryEmpType + B_C_MasterEmpCat + B_C_Education + RiskCat, family = binomial(link=probit), data = train_smote)

# Print the parameter estimates 
probit_model_1
summary(probit_model_1)

#Make Predictions using the logistic regression model
pred_probit_1 <- predict(probit_model_1, test_original, type = "response")

# Look at the predictions range
range(pred_probit_1)

# Make a binary predictions-vector using a cut-off of 50%
pred_probit_1_bin <- ifelse(pred_probit_1 > 0.5, 1, 0)

# Construct a confusion matrix
table(true_val, pred_probit_1_bin)

################# Make and run cloglog Regression Models ###########################

cloglog_model_1 <- glm(Foreclosed ~ GPTPZP + TotalUnits + B_FOIR + C_Income + RoI + LTV + EMI + CIBIL  + PrimaryEmpType + B_C_MasterEmpCat + B_C_Education + RiskCat, family = binomial(link=cloglog), data = train_smote)

# Print the parameter estimates 
cloglog_model_1
summary(cloglog_model_1)

#Make Predictions using the logistic regression model
pred_cloglog_1 <- predict(cloglog_model_1, test_original, type = "response")

# Look at the predictions range
range(pred_cloglog_1)

# Make a binary predictions-vector using a cut-off of 40%
pred_cloglog_1_bin <- ifelse(pred_cloglog_1 > 0.4, 1, 0)

# Construct a confusion matrix
table(true_val, pred_cloglog_1_bin)

################# Make and run a single rpart decision tree to see what's happening ###########################

tree_loss_matrix_1 <- rpart(Foreclosed ~  Vintage + CIBIL + TotalUnits + LTV, method = "class",
                            data =  train_smote, control = rpart.control(cp = 0.005), parms = list(loss = matrix(c(0, 10, 100, 0))))

write.csv(train_smote, "smoted.csv")

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_loss_matrix_1)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized.
printcp(tree_loss_matrix_1)

# Create an index for of the row with the minimum number of splits for easy visualisation
#index <- which.min(tree_loss_matrix_1$cptable[, "xerror"])

index <- 2

# Store the cp value in a variable
tree_lossmatrix <- tree_loss_matrix_1$cptable[index, "CP"]

# Prune the tree to the size corresponding to the chosen CP value
ptree_loss_matrix <- prune(tree_loss_matrix_1,tree_lossmatrix)

# Use prp() and argument extra = 1 to plot the pruned tree
prp(ptree_loss_matrix, extra = 1)

# Make predictions for the probability of default using the pruned tree and the test set
prob_default_loss_matrix <- predict(ptree_loss_matrix, newdata = test)[ ,2]

# Obtain the cutoff for acceptance rate 80%
cutoff_loss_matrix <- quantile(prob_default_loss_matrix,0.8)

# Obtain the binary predictions.
bin_pred_loss_matrix_80 <- ifelse(prob_default_loss_matrix > cutoff_loss_matrix, 1, 0)

table(true_val,bin_pred_loss_matrix_80)
