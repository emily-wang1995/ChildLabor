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

# Ethiopia Household Model Data Complete Cases - get rid of missing data
ethiopiaHouseholdmComplete <- ethiopiaHouseholdm %>%
  select(quesID, childNumDefiniteHazardCat, childNumDefiniteWorstCat,
         avg517Age, avgAdultAge, headType, totalChild517, residenceType,
         homeOwner, homeRooms, totalIncome, totalIncomeLog, totalAdults) %>%
  na.omit()



#####################################################################################

# Ethiopia Household Hazard
set.seed(10)

#Create train and test for Ethiopia Household dataset.

Percentage <- nrow(ethiopiaHouseholdmComplete)*3/4
numtrain <- sample(1:dim(ethiopiaHouseholdmComplete)[1], round(Percentage), replace = FALSE, prob = NULL)
numtest <- -numtrain
EHouseholdtrainset <- ethiopiaHouseholdmComplete[sort(numtrain),]
EHouseholdtestset <- ethiopiaHouseholdmComplete[numtest,]

###Train both full and reduced generalized linear model (binomial)

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

###Testing on Full Model

#Predict using full generalized linear model on test set
pred_glmFull <- predict(ehFullModelHazard , EHouseholdtestset)

#Categorize predictions into yes or no if there was at least one child (5-17) in hazardous labor in the household .
optCutOff <- optimalCutoff(EHouseholdtestset$childNumDefiniteHazardCat, pred_glmFull)[1]
pred_glm_factorFull <- ifelse(pred_glmFull > optCutOff, 1, 0)
head(data.frame(pred_glmFull,pred_glm_factorFull),30)

#Code to see confusion matrix and statistics.Sensitivity is low. 
caret::confusionMatrix(as.factor(pred_glm_factorFull), as.factor(EHouseholdtestset$childNumDefiniteHazardCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset$childNumDefiniteHazardCat,pred_glmFull)

###Testing on Reduced Model

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

###Training on Random Forest

#make all the characters into factors
EHouseholdtrainset_fac= EHouseholdtrainset %>% mutate_if(is.character, as.factor)
EHouseholdtestset_fac= EHouseholdtestset %>% mutate_if(is.character, as.factor)

#Random Forest model
EHModelHazard.forest <- randomForest(as.factor(childNumDefiniteHazardCat) ~ avg517Age + avgAdultAge +
                                headType + totalChild517 + residenceType + homeOwner +
                                homeRooms + totalIncomeLog + totalAdults, EHouseholdtrainset_fac, ntree=100, importance = TRUE)
#find variable importance measures
importance <- importance(EHModelHazard.forest)

###Testing on Random Forest
#Prediction from random forest 
pred_rf <- predict(EHModelHazard.forest, EHouseholdtestset_fac)

#Confusion Matrix and Statistics
caret::confusionMatrix(pred_rf, as.factor(EHouseholdtestset_fac$childNumDefiniteHazardCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset$childNumDefiniteHazardCat,pred_rf)

#area under the curve and accuracy is lower for the random forest model in comparison to the reduced glm model.

###Tune Random Forest parameters
#variable plot of most important features
varImpPlot(EHModelHazard.forest, sort = T, n.var = 5, main = 'Top 5 Feature Importance')

#plot the errors percentages for different amount of trees
plot(EHModelHazard.forest)


#make trainset into a data frame
responseCol <- match("childNumDefiniteHazardCat", names(EHouseholdtrainset_fac))
EHouseholdtrainset_fac <- as.data.frame(EHouseholdtrainset_fac)

#find the optimal mtry for random forest with tuned parameters
t <- tuneRF(EHouseholdtrainset_fac[, -responseCol], as.factor(EHouseholdtrainset_fac[, responseCol]), stepFactor = 0.5, plot = TRUE, ntreeTry = 25, trace = TRUE, improve = 0.05)

###Training tuned random forest
#create 2nd random forest with tuned parameters
rfModel_new <- randomForest(as.factor(childNumDefiniteHazardCat) ~ avg517Age + avgAdultAge +
                              headType + totalChild517 + residenceType + homeOwner +
                              homeRooms + totalIncomeLog + totalAdults, EHouseholdtrainset_fac, ntree = 20, mtry = 3, importance = TRUE, proximity = TRUE)

###Testing tuned random forest
#predict using random forest with tuned parameters on test set 
pred_rf2 <- predict(rfModel_new, EHouseholdtestset_fac)

#use confusion Matrix to see accuract and sensitivity
caret::confusionMatrix(pred_rf2, as.factor(EHouseholdtestset_fac$childNumDefiniteHazardCat))

#ROC curve - obtain the Area under the curve 
par(mfrow = c(1, 1))
roc.curve(EHouseholdtestset_fac$childNumDefiniteHazardCat,pred_rf2)


## Reduced generalized linear model (binomial) is chosen since it has the highest accuracy and AUC. It is also the most interpretable.


###Interpretation of reduced model.

#Get coefficient (odds ratio) for variables, confidence intervals, and p-value from reduced model
coef <- data.frame(val = round(exp(ehReducedModelHazard$coefficients), 2))
ciTemp <- as.data.frame(exp(confint(ehReducedModelHazard)))
ci <- data.frame(lower = format(round(ciTemp[,1], 2), nsmall = 2), 
                 upper = format(round(ciTemp[,2], 2), nsmall = 2))
p <- data.frame(pval = format(round(coef(summary(ehReducedModelHazard))[,4], 3), 
                              nsmall = 3), stringsAsFactors = FALSE) %>%
  mutate(pval = ifelse(pval == "0.000", "< 0.001", pval))


#create a dataframe for analysis of the reduced model and variables created above
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

