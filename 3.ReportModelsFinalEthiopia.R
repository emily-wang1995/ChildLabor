# This script contains all code to create new data frame for modeling
# as well as all models created for Ethiopia


# Required packages
require(MASS)
require(mosaic)
require(dplyr)
require(tidyr)
require(ggplot2)
require(xtable)
require(lme4)
require(randomForest)
require(InformationValue)
require(ROSE)
require(caret)
select <- dplyr::select

# Loading in Ethiopia datasets
ethiopia <- read.csv("../Data/FinalData/ethiopiaFinal.csv",
                     stringsAsFactors = FALSE)
ethiopiaHousehold <- read.csv("../Data/FinalData/ethiopiaHousehold.csv",
                              stringsAsFactors = FALSE)
ethiopiaIndividual <- read.csv("../Data/FinalData/ethiopiaIndividual.csv",
                               stringsAsFactors = FALSE)

# Loading in Uganda datasets
uganda <- read.csv("../Data/FinalData/ugandaFinal.csv",
                   stringsAsFactors = FALSE)
ugandaHousehold <- read.csv("../Data/FinalData/ugandaHousehold.csv",
                            stringsAsFactors = FALSE)
ugandaIndividual <- read.csv("../Data/FinalData/ugandaIndividual.csv",
                             stringsAsFactors = FALSE)

# Creating new dataframes for modeling

# Ethiopia Household Model Data
ethiopiaHouseholdm <- ethiopiaHousehold %>%
  filter(totalChild517 > 0, headType != "Child (<18)") %>%
  group_by(quesID) %>%
  mutate(childNumDefiniteHazardCat = ifelse(childNumDefiniteHazard > 0, 1, 0),
         childNumDefiniteWorstCat = ifelse(childNumDefiniteWorst > 0, 1, 0),
         totalIncomeLog = log(totalIncome + 1),
         homeOwner = ifelse(homeOwner == "Owned by the household", "Owned", 
                            ifelse(homeOwner == "Rented", "Rented", "Other")),
         hazardCat = ifelse(childNumDefiniteHazardCat == 1, "Yes", "No"),
         worstCat = ifelse(childNumDefiniteWorstCat == 1, "Yes", "No")) %>%
  ungroup()
# Ethiopia Household Model Data Complete Cases
ethiopiaHouseholdmComplete <- ethiopiaHouseholdm %>%
  select(quesID, childNumDefiniteHazardCat, childNumDefiniteWorstCat,
         avg517Age, avgAdultAge, headType, totalChild517, residenceType,
         homeOwner, homeRooms, totalIncome, totalIncomeLog, totalAdults) %>%
  na.omit()

# Ethiopia Individual Model Data
ethiopiaIndividualm <- ethiopiaIndividual %>%
  filter(age >= 5 & age <= 17, headType != "Child (<18)") %>%
  group_by(X) %>%
  mutate(anyDefiniteHazardIndCat = ifelse(anyDefiniteHazardInd > 0, 1, 0),
         anyDefiniteWorstIndCat = ifelse(anyDefiniteWorstInd > 0, 1, 0),
         totalIncomeLog = log(totalIncome + 1),
         homeOwner = ifelse(homeOwner == "Owned by the household", "Owned", 
                            ifelse(homeOwner == "Rented", "Rented", "Other")),
         hazardCat = ifelse(anyDefiniteHazardInd == 1, "Yes", "No"),
         worstCat = ifelse(anyDefiniteWorstInd == 1, "Yes", "No")) %>%
  ungroup()
# Ethiopia Individual Model Data Complete Cases
ethiopiaIndividualmComplete <- ethiopiaIndividualm %>%
  select(X, anyDefiniteHazardIndCat, anyDefiniteWorstIndCat,
         sex, age, readWrite, avg517Age, avgAdultAge,
         headType, totalChild517, residenceType, homeOwner, homeRooms, 
         totalIncome, totalIncomeLog, totalAdults) %>%
  na.omit() 

# Uganda Household Model Data
ugandaHouseholdm <-ugandaHousehold %>%
  filter(totalChild517 > 0, headType != "Child (<18)") %>%
  group_by(quesID) %>%
  mutate(childNumDefiniteHazardCat = ifelse(childNumDefiniteHazard > 0, 1, 0),
         childNumDefiniteWorstCat = ifelse(childNumDefiniteWorst > 0, 1, 0),
         hazardCat = ifelse(childNumDefiniteHazardCat == 1, "Yes", "No"),
         worstCat = ifelse(childNumDefiniteWorstCat == 1, "Yes", "No")) %>%
  ungroup()
# Uganda Household Model Data Complete Cases
ugandaHouseholdmComplete <- ugandaHouseholdm %>%
  select(quesID, childNumDefiniteHazardCat, childNumDefiniteWorstCat,
         avg517Age, avgAdultAge, headType,
         totalChild517, residence, totalAdults) %>%
  na.omit()

# Uganda Individual Model Data
ugandaIndividualm <- ugandaIndividual %>%
  filter(age >= 5 & age <= 17, headType != "Child (<18)") %>%
  group_by(X) %>%
  mutate(anyDefiniteHazardIndCat = ifelse(anyDefiniteHazardInd > 0, 1, 0),
         anyDefiniteWorstIndCat = ifelse(anyDefiniteWorstInd > 0, 1, 0),
         educAccess = ifelse(educAccess == 1, "Yes", "No"),
         healthAccess = ifelse(healthAccess == 1, "Yes", "No"),
         hazardCat = ifelse(anyDefiniteHazardIndCat == 1, "Yes", "No"),
         worstCat = ifelse(anyDefiniteWorstIndCat == 1, "Yes", "No")) %>%
  ungroup()
# Uganda Individual Model Data Complete Cases
ugandaIndividualmComplete <- ugandaIndividualm %>%
  select(X, anyDefiniteHazardIndCat, anyDefiniteWorstIndCat,
         avg517Age, avgAdultAge, headType,
         totalChild517, residence, totalAdults,
         sex, age, educAccess, healthAccess) %>%
  na.omit()

#####################################################################################
# ETHIOPIA HOUSEHOLD MODELS


# Ethiopia Household Hazard
set.seed(10)

#Create train and test for Ethiopia Household dataset.

Percentage <- nrow(ethiopiaHouseholdmComplete)*3/4
numtrain <- sample(1:dim(ethiopiaHouseholdmComplete)[1], round(Percentage), replace = FALSE, prob = NULL)
numtest <- -numtrain
EHouseholdtrainset <- ethiopiaHouseholdmComplete[sort(numtrain),]
EHouseholdtestset <- ethiopiaHouseholdmComplete[numtest,]


#Run full Generalized linear model (family = binomial) on training set for Ethiopia Household dataset using 
#if there was at least one child (5-17) in hazardous labor in the household as the dependent variable .

ehFullModelHazard <- glm(childNumDefiniteHazardCat ~ avg517Age + avgAdultAge +
                           headType + totalChild517 + residenceType + homeOwner +
                           homeRooms + totalIncomeLog + totalAdults,
                         data = EHouseholdtrainset, family = "binomial")

#Use backwards step-wise regression, each model was reduced to only include variables that helped minimize the model’s AIC. 
step(ehFullModelHazard, direction = "backward")

#Run reduced generalized linear model(family = binomial) on training set from the backwards step- wise regression.
ehReducedModelHazard <- glm(formula = childNumDefiniteHazardCat ~ avg517Age + headType + 
                              totalChild517 + residenceType + totalIncomeLog , 
                            family = "binomial", data = EHouseholdtrainset)

#Predict using full generalized linear model on test set
pred_glmFull <- predict(ehFullModelHazard , EHouseholdtestset)

#Categorize predictions into yes or no if there was at least one child (5-17) in hazardous labor in the household .
optCutOff <- optimalCutoff(EHouseholdtestset$childNumDefiniteHazardCat, pred_glmFull)[1]
pred_glm_factorFull <- ifelse(pred_glmFull > optCutOff, 1, 0)
head(data.frame(pred_glmFull,pred_glm_factorFull),30)

#Code to see confusion matrix and statistics.Sensitivity is low. Accuracy is .77761
caret::confusionMatrix(as.factor(pred_glm_factorFull), as.factor(EHouseholdtestset$childNumDefiniteHazardCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset$childNumDefiniteHazardCat,pred_glmFull)

#Predict using reduced generalized linear model (family = binomial) on test set
pred_glm <- predict(ehReducedModelHazard, EHouseholdtestset)

#Categorize predictions into yes or no if there was at least one child (5-17) in hazardous labor in the household .
optCutOff <- optimalCutoff(EHouseholdtestset$childNumDefiniteHazardCat, pred_glm)[1]
pred_glm_factor <- ifelse(pred_glm > optCutOff, 1, 0)
head(data.frame(pred_glm,pred_glm_factor),30)

#Code to see confusion matrix and statistics. Sensitivity is low. Accuracy
caret::confusionMatrix(as.factor(pred_glm_factor), as.factor(EHouseholdtestset$childNumDefiniteHazardCat))
 
#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset$childNumDefiniteHazardCat,pred_glm)

#choose the reduced model as it has a higher accuracy and area under the curve for the ROC using less variables in comparison to the full model. 


#Get coefficient (odds ratio) for variables, confidence intervals, and p-value from reduced model
coef <- data.frame(val = round(exp(ehReducedModelHazard$coefficients), 2))
ciTemp <- as.data.frame(exp(confint(ehReducedModelHazard)))
ci <- data.frame(lower = format(round(ciTemp[,1], 2), nsmall = 2), 
                 upper = format(round(ciTemp[,2], 2), nsmall = 2))
p <- data.frame(pval = format(round(coef(summary(ehReducedModelHazard))[,4], 3), 
                              nsmall = 3), stringsAsFactors = FALSE) %>%
  mutate(pval = ifelse(pval == "0.000", "< 0.001", pval))



eHouseTableHazard <- data.frame(a = c("Intercept",
                                      "Average age of household children between ages 5 and 17",
                                      "Married adult female heads (baseline: adult male)",
                                      "Single adult female heads (baseline: adult male)",
                                      "Total children in household between ages 5 and 17",
                                      "Urban residence type (baseline: rural)",
                                      "Log of total household income"),
                                b = coef$val, c = ci[,1],
                                d = ci[,2], e = p$pval) %>%
  select("Reduced Model Variables" = a, "Odds Ratio" = b, "CI 2.5%" = c, "CI 97.5%" = d, "p-value" = e)


#make all the characters into factors
EHouseholdtrainset_fac= EHouseholdtrainset %>% mutate_if(is.character, as.factor)
EHouseholdtestset_fac= EHouseholdtestset %>% mutate_if(is.character, as.factor)

#Random Forest model
EHModelHazard.forest <- randomForest(as.factor(childNumDefiniteHazardCat) ~ avg517Age + avgAdultAge +
                                headType + totalChild517 + residenceType + homeOwner +
                                homeRooms + totalIncomeLog + totalAdults, EHouseholdtrainset_fac, ntree=100, importance = TRUE)
#find variable importance measures
importance <- importance(EHModelHazard.forest)

#Prediction from random forest 
pred_rf <- predict(EHModelHazard.forest, EHouseholdtestset_fac)

#Confusion Matrix and Statistics
caret::confusionMatrix(pred_rf, as.factor(EHouseholdtestset_fac$childNumDefiniteHazardCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset$childNumDefiniteHazardCat,pred_rf)

#area under the curve and accuracy is lower for the random forest model in comparison to the reduced glm model.

#variable plot of most important features
varImpPlot(EHModelHazard.forest, sort = T, n.var = 5, main = 'Top 5 Feature Importance')

#plot the errors percentages for different amount of trees
plot(EHModelHazard.forest)


#make trainset into a data frame
responseCol <- match("childNumDefiniteHazardCat", names(EHouseholdtrainset_fac))
EHouseholdtrainset_fac <- as.data.frame(EHouseholdtrainset_fac)

#find the optimal mtry for random forest with tuned parameters
t <- tuneRF(EHouseholdtrainset_fac[, -responseCol], as.factor(EHouseholdtrainset_fac[, responseCol]), stepFactor = 0.5, plot = TRUE, ntreeTry = 25, trace = TRUE, improve = 0.05)

#create 2nd random forest with tuned parameters
rfModel_new <- randomForest(as.factor(childNumDefiniteHazardCat) ~ avg517Age + avgAdultAge +
                              headType + totalChild517 + residenceType + homeOwner +
                              homeRooms + totalIncomeLog + totalAdults, EHouseholdtrainset_fac, ntree = 20, mtry = 3, importance = TRUE, proximity = TRUE)

#predict using random forest with tuned parameters on test set 
pred_rf2 <- predict(rfModel_new, EHouseholdtestset_fac)

#use confusion Matrix to see accuract and sensitivity
caret::confusionMatrix(pred_rf2, as.factor(EHouseholdtestset_fac$childNumDefiniteHazardCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset_fac$childNumDefiniteHazardCat,pred_rf2)




# Ethiopia Household Worst


#Create train and test for Ethiopia Household dataset.

Percentage <- nrow(ethiopiaHouseholdmComplete)*3/4
numtrain <- sample(1:dim(ethiopiaHouseholdmComplete)[1], round(Percentage), replace = FALSE, prob = NULL)
numtest <- -numtrain
EHouseholdtrainset <- ethiopiaHouseholdmComplete[sort(numtrain),]
EHouseholdtestset <- ethiopiaHouseholdmComplete[numtest,]



#Run full Generalized linear model (family = binomial) on training set for Ethiopia Household dataset using 
#if there was at least one child (5-17) in worst forms of labor in the household as the dependent variable .
ehFullModelWorst <- glm(childNumDefiniteWorstCat ~ avg517Age + avgAdultAge +
                          headType + totalChild517 + residenceType + homeOwner +
                          homeRooms + totalIncomeLog + totalAdults,
                        data = ethiopiaHouseholdmComplete, family = "binomial")


#Use backwards step-wise regression, each model was reduced to only include variables that helped minimize the model’s AIC. 
step(ehFullModelWorst, direction = "backward")

#Run reduced generalized linear model(family = binomial) on training set from the backwards step- wise regression.
ehReducedModelWorst <- glm(childNumDefiniteWorstCat ~ avg517Age + avgAdultAge +
                             headType + totalChild517 + residenceType + homeRooms +
                             totalIncomeLog + totalAdults,
                           data = ethiopiaHouseholdmComplete, family = "binomial")

#Predict using full generalized linear model on test set
pred_glmFull <- predict(ehFullModelWorst , EHouseholdtestset)

#Categorize predictions into yes or no if there was at least one child (5-17) in worst forms of labor in the household .
optCutOff <- optimalCutoff(EHouseholdtestset$childNumDefiniteWorstCat, pred_glmFull)[1]
pred_glm_factorFull <- ifelse(pred_glmFull > optCutOff, 1, 0)
head(data.frame(pred_glmFull,pred_glm_factorFull),30)

#Code to see confusion matrix and statistics.Sensitivity is low. 
caret::confusionMatrix(as.factor(pred_glm_factorFull), as.factor(EHouseholdtestset$childNumDefiniteWorstCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset$childNumDefiniteWorstCat,pred_glmFull)

#Predict using reduced generalized linear model (family = binomial) on test set
pred_glm1 <- predict(ehReducedModelWorst, EHouseholdtestset)

#Categorize predictions into yes or no if there was at least one child (5-17) in worst forms of labor in the household .
optCutOff <- optimalCutoff(EHouseholdtestset$childNumDefiniteWorstCat, pred_glm1)[1]
pred_glm_factor <- ifelse(pred_glm1 > optCutOff, 1, 0)
head(data.frame(pred_glm1,pred_glm_factor),30)

#Code to see confusion matrix and statistics. Sensitivity is low. 
caret::confusionMatrix(as.factor(pred_glm_factor), as.factor(EHouseholdtestset$childNumDefiniteWorstCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset$childNumDefiniteWorstCat,pred_glm1)

#choose the reduced model as it has a higher accuracy and area under the curve for the ROC using less variables in comparison to the full model. 

#Get coefficient (odds ratio) for variables, confidence intervals, and p-value from reduced model
coef <- data.frame(val = round(exp(ehReducedModelWorst$coefficients), 2))
ciTemp <- as.data.frame(exp(confint(ehReducedModelWorst)))
ci <- data.frame(lower = format(round(ciTemp[,1], 2), nsmall = 2), 
                 upper = format(round(ciTemp[,2], 2), nsmall = 2))
p <- data.frame(pval = format(round(coef(summary(ehReducedModelWorst))[,4], 3), 
                              nsmall = 3), stringsAsFactors = FALSE) %>%
  mutate(pval = ifelse(pval == "0.000", "< 0.001", pval))

#create a dataframe for analysis of the reduced model and variables created above
eHouseTableWorst <- data.frame(a = c("Intercept",
                                     "Average age of household children between ages 5 and 17",
                                     "Average age of household adults",
                                     "Married adult female heads (baseline: adult males)",
                                     "Single adult female heads (baseline: adult males)",
                                     "Total children in household between ages 5 and 17",
                                     "Urban residence type (baseline: rural)",
                                     "Number of rooms in the household",
                                     "Log of total household income",
                                     "Total adults in household"),
                               b = coef$val, c = ci[,1],
                               d = ci[,2], e = p$pval) %>%
  select("Reduced Model Variables" = a, "Odds Ratio" = b, "CI 2.5%" = c, "CI 97.5%" = d, "p-value" = e)

#####################################################################################
# ETHIOPIA INDIVIDUAL MODELS

#Create train and test for Ethiopia Individual dataset.

Percentage <- nrow(ethiopiaIndividualmComplete)*3/4
numtrain <- sample(1:dim(ethiopiaIndividualmComplete)[1], round(Percentage), replace = FALSE, prob = NULL)
numtest <- -numtrain
EIndividualtrainset <- ethiopiaIndividualmComplete[sort(numtrain),]
EIndividualtestset <- ethiopiaIndividualmComplete[numtest,]

# Ethiopia Individual Hazard

#Run full Generalized linear model (family = binomial) on training set for Ethiopia Individual dataset using 
#if there was at least one indication of a harzardous type of labor at the individual level as the dependent variable .
eiFullModelHazard <- glm(anyDefiniteHazardIndCat ~ sex + age + readWrite + 
                           avg517Age + avgAdultAge + headType + totalChild517 + 
                           residenceType + homeOwner + homeRooms + totalIncomeLog + 
                           totalAdults,
                         data = EIndividualtrainset , family = "binomial")

#Use backwards step-wise regression, each model was reduced to only include variables that helped minimize the model’s AIC. 
step(eiFullModelHazard, direction = "backward")

#Run reduced generalized linear model(family = binomial) on training set from the backwards step- wise regression.
eiReducedModelHazard <- glm(anyDefiniteHazardIndCat ~ sex + age + readWrite + 
                              avgAdultAge + totalChild517 + 
                              residenceType + totalIncomeLog + totalAdults,
                            data = EIndividualtrainset , family = "binomial")

#Predict using full generalized linear model on test set
pred_glmFull <- predict(eiFullModelHazard , EIndividualtestset)

#Categorize predictions into yes or no if there was at least one indication of a harzardous type of labor at the individual level .
optCutOff <- optimalCutoff(EIndividualtestset$anyDefiniteHazardIndCat, pred_glmFull)[1]
pred_glm_factorFull <- ifelse(pred_glmFull > optCutOff, 1, 0)
head(data.frame(pred_glmFull,pred_glm_factorFull),30)

#Code to see confusion matrix and statistics.
caret::confusionMatrix(as.factor(pred_glm_factorFull), as.factor(EIndividualtestset$anyDefiniteHazardIndCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EIndividualtestset$anyDefiniteHazardIndCat,pred_glmFull)

#Predict using reduced generalized linear model (family = binomial) on test set
pred_glm1 <- predict(eiReducedModelHazard, EIndividualtestset)

#Categorize predictions into yes or no if an individual showed any indications that they experienced one type of hazardous forms of labor in the household .
optCutOff <- optimalCutoff(EIndividualtestset$anyDefiniteHazardIndCat, pred_glm1)[1]
pred_glm_factor <- ifelse(pred_glm1 > optCutOff, 1, 0)
head(data.frame(pred_glm1,pred_glm_factor),30)

#Code to see confusion matrix and statistics. Sensitivity is low. 
caret::confusionMatrix(as.factor(pred_glm_factor), as.factor(EIndividualtestset$anyDefiniteHazardIndCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EIndividualtestset$anyDefiniteHazardIndCat,pred_glm1)

#choose the reduced model as it has a higher accuracy and area under the curve for the ROC using less variables in comparison to the full model. 



#Get coefficient (odds ratio) for variables, confidence intervals, and p-value from reduced model
coef <- data.frame(val = round(exp(eiReducedModelHazard$coefficients), 2))
ciTemp <- as.data.frame(exp(confint(eiReducedModelHazard)))
ci <- data.frame(lower = format(round(ciTemp[,1], 2), nsmall = 2), 
                 upper = format(round(ciTemp[,2], 2), nsmall = 2))
p <- data.frame(pval = format(round(coef(summary(eiReducedModelHazard))[,4], 3), 
                              nsmall = 3), stringsAsFactors = FALSE) %>%
  mutate(pval = ifelse(pval == "0.000", "< 0.001", pval))

#create a dataframe for analysis of the reduced model and variables created above
eIndTableHazard <- data.frame(a = c("Intercept",
                                    "Male (baseline: female)",
                                    "Age",
                                    "Literate (baseline: illiterate)",
                                    "Average age of household adults",
                                    "Total children in household between ages 5 and 17",
                                    "Urban residence type (baseline: rural)",
                                    "Log of total household income",
                                    "Total adults in household"),
                              b = coef$val, c = ci[,1],
                              d = ci[,2], e = p$pval) %>%
  select("Reduced Model Variables" = a, "Odds Ratio" = b, "CI 2.5%" = c, "CI 97.5%" = d, "p-value" = e)

