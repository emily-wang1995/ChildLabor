# Run this file to create new csv files for Ethiopia, which is a subset of EthiopiaFinal

require(dplyr)
require(tidyr)

subsetData <- function(country){
  if(country == "Ethiopia"){
    # ETHIOPIA HOUSEHOLD DATASET
    ethiopia <- read.csv("../FinalData/ethiopiaFinal.csv", stringsAsFactors = FALSE)
    
    # select desired continuous variables for household level characteristics
    tempHouseCont <- ethiopia %>%
      select(quesID, relationHead, age, highestSchoolLevel, totalMale, totalFemale,
             headType, homeRooms, CLHouseNum, worstCLHouseNum, childTraffic,
             hazardLaborHouseNum, expendMonthHealth, expendYearHealth, totalIncome) %>%
      mutate(homeRooms = ifelse(is.na(homeRooms) | homeRooms == 0, 1, homeRooms)) %>%
      group_by(quesID) %>%
      summarise(ageMin = min(age), ageMax = max(age), ageAvg = mean(age),
                totalMale = mean(totalMale), totalFemale = mean(totalFemale),
                totalMembers = sum(totalMale, totalFemale), 
                homeRooms = mean(homeRooms),
                CLHouseNum = mean(CLHouseNum),
                worstCLHouseNum = mean(worstCLHouseNum), 
                hazardLaborHouseNum = mean(hazardLaborHouseNum),
                expendMonthHealth = mean(expendMonthHealth),
                expendYearHealth = mean(expendYearHealth),
                totalIncome = mean(totalIncome)) %>%
      mutate(worstCLHouseCat = ifelse(worstCLHouseNum > 0, 1, 0),
             hazardLaborHouseCat = ifelse(hazardLaborHouseNum > 0, 1, 0),
             CLHouseCat = ifelse(CLHouseNum > 0, 1, 0)) 
    
    # select desired categorical variables for household level characteristics
    # there are 2231 unique households (quesID)
    temp1 <- ethiopia %>%
      select(quesID, relationHead) %>%
      group_by(quesID) %>%
      mutate(total = n()) %>%
      count(quesID, relationHead, total) %>%
      mutate(propRelationHead = n/total) %>%
      slice(which.max(propRelationHead)) %>%
      select(quesID, relationHead, propRelationHead)
    
    # creating proportion that can't read and write adults
    temp2 <- ethiopia %>%
      select(quesID, readWrite, age) %>%
      group_by(quesID) %>%
      filter(age >= 18) %>%
      mutate(total = n()) %>%
      count(quesID, readWrite, total) %>%
      mutate(adultIlliterateProp = ifelse(readWrite == "Illiterate", n/total, 1-n/total)) %>%
      select(quesID, adultIlliterateProp) %>%
      filter(row_number(adultIlliterateProp) == 1)
    
    # creating proportion that can't read and write for children between 5 and 17
    temp3 <- ethiopia %>%
      select(quesID, readWrite, age) %>%
      group_by(quesID) %>%
      filter(age <= 17 & age >= 5) %>%
      mutate(total = n()) %>%
      count(quesID, readWrite, total) %>%
      mutate(child517IlliterateProp = ifelse(readWrite == "Illiterate", n/total, 1-n/total)) %>%
      select(quesID, child517IlliterateProp) %>%
      filter(row_number(child517IlliterateProp) == 1)
    
    # creating head type per household
    temp4 <- ethiopia %>%
      select(quesID, headType) %>%
      group_by(quesID, headType) %>%
      filter(row_number(quesID) == 1)
    
    # creating if child trafficking exists in household
    temp5 <- ethiopia %>%
      select(quesID, childTraffic) %>%
      group_by(quesID, childTraffic) %>%
      filter(row_number(quesID) == 1)
    
    # creating who owns home for household
    temp6 <- ethiopia %>%
      select(quesID, homeOwner) %>%
      group_by(quesID, homeOwner) %>%
      filter(row_number(quesID) == 1) %>%
      na.omit()
    
    # creating residency type per household
    temp7 <- ethiopia %>%
      select(quesID, residenceType = residence_urban_rural) %>%
      group_by(quesID, residenceType) %>%
      filter(row_number(quesID) == 1)
    
    # creating how many children and adults exist in household
    # as well as how many children between 5 to 17 exist in household
    temp8 <- ethiopia %>%
      select(quesID, age) %>%
      group_by(quesID) %>%
      mutate(totalChildren = ifelse(age < 18, 1, 0),
             totalAdults = ifelse(age >= 18, 1, 0),
             totalChild517 = ifelse(age <= 17 & age >= 5, 1, 0)) %>%
      summarize(totalChildren = sum(totalChildren), 
                totalAdults = sum(totalAdults),
                totalChild517 = sum(totalChild517))
    
    # creating average child and adult age per household
    temp9 <- ethiopia %>%
      select(quesID, age) %>%
      group_by(quesID) %>%
      mutate(ageCat = ifelse(age < 18, "child", "adult")) %>%
      group_by(quesID, ageCat) %>%
      summarize(avgAge = mean(age)) %>%
      ungroup() %>%
      spread(ageCat, avgAge) %>%
      select(quesID, avgChildAge = child, avgAdultAge = adult) %>%
      mutate(avgChildAge = ifelse(is.na(avgChildAge), 0, avgChildAge),
             avgAdultAge = ifelse(is.na(avgAdultAge), 0, avgAdultAge))
    
    # creating new head type variable
    temp10 <- ethiopia %>%
      select(quesID, age, sex, headType, relationHead, maritalStatus) %>%
      mutate(newHeadType = ifelse(headType == "Adult female" & sex == "Female" & maritalStatus == "Married" & 
                                    relationHead == "Household Head" & age >= 18,
                                  "Married Adult Female", 
                                  ifelse(headType == "Adult female" & sex == "Female" &
                                           maritalStatus != "Married" & 
                                           relationHead == "Household Head" & age >= 18,
                                         "Single Adult Female",
                                         ifelse(headType == "Adult female" & age < 18 & 
                                                  relationHead == "Household Head" & maritalStatus != "Married", 
                                                "Child (<18)", 
                                                ifelse(headType == "Adult female" & relationHead == "Household Head" &
                                                         sex == "Male" & age >= 18,
                                                       "Adult Male", 
                                                       ifelse(headType == "Adult female" & relationHead == "Household Head" &
                                                                sex == "Male" & age < 18, "Child (<18)", NA)))))) %>%
      mutate(newHeadType = ifelse(headType == "Adult male" & relationHead == "Household Head" &
                                    age < 18 & sex == "Male", "Child (<18)", 
                                  ifelse(headType == "Adult male" &
                                           relationHead == "Household Head" & 
                                           age >= 18 & sex == "Male", "Adult Male",
                                         newHeadType))) %>%
      filter(relationHead == "Household Head") %>%
      mutate(newHeadType = ifelse(is.na(newHeadType),
                                  ifelse(sex == "Male" & age >= 18, "Adult Male",
                                         ifelse(age < 18, "Child (<18)",
                                                ifelse(sex == "Female" & maritalStatus != "Married", "Single Adult Female", 
                                                       ifelse(sex == "Female" & maritalStatus == "Married", "Married Adult Female", 0)))), 
                                  newHeadType)) %>%
      select(quesID, newHeadType)
    
    # average child age for children eligible for child labor
    temp11 <- ethiopia %>%
      select(quesID, age) %>%
      mutate(age517 = ifelse(age <= 17 & age >= 5, "CLAge", "notCLAge")) %>%
      group_by(quesID, age517) %>%
      summarize(avgAge = mean(age)) %>%
      ungroup() %>%
      spread(age517, avgAge) %>%
      select(quesID, avg517Age = CLAge)
    
    # merging all temporary datasets for Ethiopia household data together
    allQuesID <- data.frame(quesID = unique(ethiopia$quesID))
    tempHouseCat <- full_join(allQuesID, temp1, by = "quesID") %>%
      full_join(temp2, by = "quesID") %>%
      full_join(temp3, by = "quesID") %>%
      full_join(temp4, by = "quesID") %>%
      full_join(temp5, by = "quesID") %>%
      full_join(temp6, by = "quesID") %>%
      full_join(temp7, by = "quesID") %>%
      full_join(temp8, by = "quesID") %>%
      full_join(temp9, by = "quesID") %>%
      full_join(temp10, by = "quesID") %>%
      full_join(temp11, by = "quesID")

    # final Ethiopia household predictors (and CLHouseNum and HazardLaborHouseNum)
    ethiopiaHousePredictors <- full_join(tempHouseCont, tempHouseCat, by = "quesID") %>%
      select(quesID, headType = newHeadType,
             ageMin, ageMax, ageAvg, avgChildAge, avg517Age, avgAdultAge, totalMale, totalFemale, 
             totalAdults, totalChildren, 
             totalChild517, totalMembers,
             adultIlliterateProp, child517IlliterateProp, residenceType,
             homeOwner, homeRooms, totalIncome, CLHouseNum, CLHouseCat, 
             worstCLHouseNum, worstCLHouseCat,
             hazardLaborHouseNum, hazardLaborHouseCat)
    
    # final Ethiopia outcome variables
    
    # definite indicators of hazardous labor
    outcomeDefiniteHazard <- ethiopia %>%
      select(quesID, X, age, poorPhysHarass, 
             probSexualAbuse, probExtremeFatigue, activityAgrProd, 
             activityHunt, activityMining, activityPrepFood, 
             activityCraft, activitySmallBusiness, activityRepair, 
             activityCarShoe, activityTransportGoods, activityConstruct, 
             activityFetch, activityServeFood, activityDomesticAnim, 
             activityProstitution, selfDACare, selfDATransportMemb, 
             workCondHandCont, workCondHandNonCont, workCondHandHillCont, 
             workCondPullDeepSite, operateMachEquip, expDustFume,
             expFireGasFlames, expNoiseVibration, expDangerousTools,
             expExposedChem, subjNightWorkForce, subjForceDangerousEquip) %>%
      mutate(ageCat = ifelse(age >= 5 & age <= 17, "child517", "notCLage")) %>%
      select(-age) %>%
      mutate(poorPhysHarass = ifelse(poorPhysHarass == "Yes", 1, ifelse(poorPhysHarass == "No", 0, poorPhysHarass)), 
             probSexualAbuse = ifelse(probSexualAbuse == "Yes", 1, ifelse(probSexualAbuse == "No", 0, probSexualAbuse)), 
             probExtremeFatigue = ifelse(probExtremeFatigue == "Yes", 1, ifelse(probExtremeFatigue == "No", 0, probExtremeFatigue)), 
             activityAgrProd = ifelse(activityAgrProd == "Yes", 1, ifelse(activityAgrProd == "No", 0, activityAgrProd)), 
             activityHunt = ifelse(activityHunt == "Yes", 1, ifelse(activityHunt == "No", 0, activityHunt)), 
             activityMining = ifelse(activityMining == "Yes", 1, ifelse(activityMining == "No", 0, activityMining)), 
             activityPrepFood = ifelse(activityPrepFood == "Yes", 1, ifelse(activityPrepFood == "No", 0, activityPrepFood)), 
             activityCraft = ifelse(activityCraft == "Yes", 1, ifelse(activityCraft == "No", 0, activityCraft)), 
             activitySmallBusiness = ifelse(activitySmallBusiness == "Yes", 1, ifelse(activitySmallBusiness == "No", 0, activitySmallBusiness)), 
             activityRepair = ifelse(activityRepair == "Yes", 1, ifelse(activityRepair == "No", 0, activityRepair)), 
             activityCarShoe = ifelse(activityCarShoe == "Yes", 1, ifelse(activityCarShoe == "No", 0, activityCarShoe)), 
             activityTransportGoods = ifelse(activityTransportGoods == "Yes", 1, ifelse(activityTransportGoods == "No", 0, activityTransportGoods)), 
             activityConstruct = ifelse(activityConstruct == "Yes", 1, ifelse(activityConstruct == "No", 0, activityConstruct)), 
             activityFetch = ifelse(activityFetch == "Yes", 1, ifelse(activityFetch == "No", 0, activityFetch)), 
             activityServeFood = ifelse(activityServeFood == "Yes", 1, ifelse(activityServeFood == "No", 0, activityServeFood)), 
             activityDomesticAnim = ifelse(activityDomesticAnim == "Yes", 1, ifelse(activityDomesticAnim == "No", 0, activityDomesticAnim)), 
             activityProstitution = ifelse(activityProstitution == "Yes", 1, ifelse(activityProstitution == "No", 0, activityProstitution)), 
             selfDACare = ifelse(selfDACare == "Yes", 1, ifelse(selfDACare == "No", 0, selfDACare)), 
             selfDATransportMemb = ifelse(selfDATransportMemb == "Yes", 1, ifelse(selfDATransportMemb == "No", 0, selfDATransportMemb)), 
             workCondHandCont = ifelse(workCondHandCont > 0, 1, workCondHandCont), 
             workCondHandNonCont = ifelse(workCondHandNonCont > 0, 1, workCondHandNonCont), 
             workCondHandHillCont = ifelse(workCondHandHillCont > 0, 1, workCondHandHillCont), 
             workCondPullDeepSite = ifelse(workCondPullDeepSite > 0, 1, workCondPullDeepSite), 
             operateMachEquip = ifelse(operateMachEquip == "Yes", 1, ifelse(operateMachEquip == "No", 0, operateMachEquip)), 
             expDustFume = ifelse(expDustFume == "Yes", 1, ifelse(expDustFume == "No", 0, expDustFume)),
             expFireGasFlames = ifelse(expFireGasFlames == "Yes", 1, ifelse(expFireGasFlames == "No", 0, expFireGasFlames)), 
             expNoiseVibration = ifelse(expNoiseVibration == "Yes", 1, ifelse(expNoiseVibration == "No", 0, expNoiseVibration)), 
             expDangerousTools = ifelse(expDangerousTools == "Yes", 1, ifelse(expDangerousTools == "No", 0, expDangerousTools)),
             expExposedChem = ifelse(expExposedChem == "Yes", 1, ifelse(expExposedChem == "No", 0, expExposedChem)), 
             subjNightWorkForce = ifelse(subjNightWorkForce == "Yes", 1, ifelse(subjNightWorkForce == "No", 0, subjNightWorkForce)), 
             subjForceDangerousEquip = ifelse(subjForceDangerousEquip == "Yes", 1, ifelse(subjForceDangerousEquip == "No", 0, subjForceDangerousEquip))) %>%
      group_by(quesID, X, ageCat) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(childNumDefiniteHazard = sum(poorPhysHarass, 
                                        probSexualAbuse, probExtremeFatigue, activityAgrProd, 
                                        activityHunt, activityMining, activityPrepFood, 
                                        activityCraft, activitySmallBusiness, activityRepair, 
                                        activityCarShoe, activityTransportGoods, activityConstruct, 
                                        activityFetch, activityServeFood, activityDomesticAnim, 
                                        activityProstitution, selfDACare, selfDATransportMemb, 
                                        workCondHandCont, workCondHandNonCont, workCondHandHillCont, 
                                        workCondPullDeepSite, operateMachEquip, expDustFume,
                                        expFireGasFlames, expNoiseVibration, expDangerousTools,
                                        expExposedChem, subjNightWorkForce, subjForceDangerousEquip)) %>%
      ungroup() %>%
      mutate(childNumDefiniteHazard = ifelse(childNumDefiniteHazard > 0, 1, 0)) %>%
      select(-X) %>%
      group_by(quesID, ageCat, childNumDefiniteHazard) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      filter(ageCat == "child517", childNumDefiniteHazard == 1) %>%
      select(quesID, childNumDefiniteHazard = n)
      
    # definite indicators of worst form of child labor
    outcomeDefiniteWorst <- ethiopia %>%
      select(quesID, X, age, poorEmotionHarass, poorPhysHarass, 
             probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
             expDustFume, expFireGasFlames, expNoiseVibration, 
             expExtremeTemp, expDangerousTools, expWorkUnderground, 
             expWorkHeights, expWorkWater, expDarkConfined, expNoVent, 
             expExposedChem, expExplosives, expAlcoholDrug, subjConstantShout, 
             subjRepeatedInsults, subjPhysAbuse, subjSexAbuse, subjTraffick, 
             subjBondage, subjCommercialSex, subjNightWorkForce, subjUnwillingWork, 
             subjDeniedEdu, subjDeniedHealthCare, subjForceDangerousEquip, 
             subjInsuffFoodDrink, subjUnsuitableLiving, 
             subjDeniedRest, subjDeniedParentContact, subjDeniedPeerContact, 
             subjDeniedSalary, subjForceReligion, subjForceWalk30Min) %>%
      mutate(ageCat = ifelse(age >= 5 & age <= 17, "child517", "notCLage")) %>%
      select(-age) %>%
      mutate(poorEmotionHarass = ifelse(poorEmotionHarass == "Yes", 1, ifelse(poorEmotionHarass == "No", 0, poorEmotionHarass)), 
             poorPhysHarass = ifelse(poorPhysHarass == "Yes", 1, ifelse(poorPhysHarass == "No", 0, poorPhysHarass)), 
             probSexualAbuse = ifelse(probSexualAbuse == "Yes", 1, ifelse(probSexualAbuse == "No", 0, probSexualAbuse)), 
             probExtremeFatigue = ifelse(probExtremeFatigue == "Yes", 1, ifelse(probExtremeFatigue == "No", 0, probExtremeFatigue)), 
             probNoSchoolTime = ifelse(probNoSchoolTime == "Yes", 1, ifelse(probNoSchoolTime == "No", 0, probNoSchoolTime)), 
             expDustFume = ifelse(expDustFume == "Yes", 1, ifelse(expDustFume == "No", 0, expDustFume)), 
             expFireGasFlames = ifelse(expFireGasFlames == "Yes", 1, ifelse(expFireGasFlames == "No", 0, expFireGasFlames)), 
             expNoiseVibration = ifelse(expNoiseVibration == "Yes", 1, ifelse(expNoiseVibration == "No", 0, expNoiseVibration)), 
             expExtremeTemp = ifelse(expExtremeTemp == "Yes", 1, ifelse(expExtremeTemp == "No", 0, expExtremeTemp)), 
             expDangerousTools = ifelse(expDangerousTools == "Yes", 1, ifelse(expDangerousTools == "No", 0, expDangerousTools)), 
             expWorkUnderground = ifelse(expWorkUnderground == "Yes", 1, ifelse(expWorkUnderground == "No", 0, expWorkUnderground)), 
             expWorkHeights = ifelse(expWorkHeights == "Yes", 1, ifelse(expWorkHeights == "No", 0, expWorkHeights)), 
             expWorkWater = ifelse(expWorkWater == "Yes", 1, ifelse(expWorkWater == "No", 0, expWorkWater)), 
             expDarkConfined = ifelse(expDarkConfined == "Yes", 1, ifelse(expDarkConfined == "No", 0, expDarkConfined)), 
             expNoVent = ifelse(expNoVent == "Yes", 1, ifelse(expNoVent == "No", 0, expNoVent)), 
             expExposedChem = ifelse(expExposedChem == "Yes", 1, ifelse(expExposedChem == "No", 0, expExposedChem)), 
             expExplosives = ifelse(expExplosives == "Yes", 1, ifelse(expExplosives == "No", 0, expExplosives)), 
             expAlcoholDrug = ifelse(expAlcoholDrug == "Yes", 1, ifelse(expAlcoholDrug == "No", 0, expAlcoholDrug)), 
             subjConstantShout = ifelse(subjConstantShout == "Yes", 1, ifelse(subjConstantShout == "No", 0, subjConstantShout)), 
             subjRepeatedInsults = ifelse(subjRepeatedInsults == "Yes", 1, ifelse(subjRepeatedInsults == "No", 0, subjRepeatedInsults)), 
             subjPhysAbuse = ifelse(subjPhysAbuse == "Yes", 1, ifelse(subjPhysAbuse == "No", 0, subjPhysAbuse)), 
             subjSexAbuse = ifelse(subjSexAbuse == "Yes", 1, ifelse(subjSexAbuse == "No", 0, subjSexAbuse)), 
             subjTraffick = ifelse(subjTraffick == "Yes", 1, ifelse(subjTraffick == "No", 0, subjTraffick)), 
             subjBondage = ifelse(subjBondage == "Yes", 1, ifelse(subjBondage == "No", 0, subjBondage)), 
             subjCommercialSex = ifelse(subjCommercialSex == "Yes", 1, ifelse(subjCommercialSex == "No", 0, subjCommercialSex)), 
             subjNightWorkForce = ifelse(subjNightWorkForce == "Yes", 1, ifelse(subjNightWorkForce == "No", 0, subjNightWorkForce)), 
             subjUnwillingWork = ifelse(subjUnwillingWork == "Yes", 1, ifelse(subjUnwillingWork == "No", 0, subjUnwillingWork)), 
             subjDeniedEdu = ifelse(subjDeniedEdu == "Yes", 1, ifelse(subjDeniedEdu == "No", 0, subjDeniedEdu)), 
             subjDeniedHealthCare = ifelse(subjDeniedHealthCare == "Yes", 1, ifelse(subjDeniedHealthCare == "No", 0, subjDeniedHealthCare)), 
             subjForceDangerousEquip = ifelse(subjForceDangerousEquip == "Yes", 1, ifelse(subjForceDangerousEquip == "No", 0, subjForceDangerousEquip)), 
             subjInsuffFoodDrink = ifelse(subjInsuffFoodDrink == "Yes", 1, ifelse(subjInsuffFoodDrink == "No", 0, subjInsuffFoodDrink)), 
             subjUnsuitableLiving = ifelse(subjUnsuitableLiving == "Yes", 1, ifelse(subjUnsuitableLiving == "No", 0, subjUnsuitableLiving)), 
             subjDeniedRest = ifelse(subjDeniedRest == "Yes", 1, ifelse(subjDeniedRest == "No", 0, subjDeniedRest)), 
             subjDeniedParentContact = ifelse(subjDeniedParentContact == "Yes", 1, ifelse(subjDeniedParentContact == "No", 0, subjDeniedParentContact)), 
             subjDeniedPeerContact = ifelse(subjDeniedPeerContact == "Yes", 1, ifelse(subjDeniedPeerContact == "No", 0, subjDeniedPeerContact)), 
             subjDeniedSalary = ifelse(subjDeniedSalary == "Yes", 1, ifelse(subjDeniedSalary == "No", 0, subjDeniedSalary)), 
             subjForceReligion = ifelse(subjForceReligion == "Yes", 1, ifelse(subjForceReligion == "No", 0, subjForceReligion)), 
             subjForceWalk30Min = ifelse(subjForceWalk30Min == "Yes", 1, ifelse(subjForceWalk30Min == "No", 0, subjForceWalk30Min))) %>%
      group_by(quesID, X, ageCat) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(childNumDefiniteWorst = sum(poorEmotionHarass, poorPhysHarass, 
                                       probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
                                       expDustFume, expFireGasFlames, expNoiseVibration, 
                                       expExtremeTemp, expDangerousTools, expWorkUnderground, 
                                       expWorkHeights, expWorkWater, expDarkConfined, expNoVent, 
                                       expExposedChem, expExplosives, expAlcoholDrug, subjConstantShout, 
                                       subjRepeatedInsults, subjPhysAbuse, subjSexAbuse, subjTraffick, 
                                       subjBondage, subjCommercialSex, subjNightWorkForce, subjUnwillingWork, 
                                       subjDeniedEdu, subjDeniedHealthCare, subjForceDangerousEquip, 
                                       subjInsuffFoodDrink, subjUnsuitableLiving, 
                                       subjDeniedRest, subjDeniedParentContact, subjDeniedPeerContact, 
                                       subjDeniedSalary, subjForceReligion, subjForceWalk30Min)) %>%
      ungroup() %>%
      mutate(childNumDefiniteWorst = ifelse(childNumDefiniteWorst > 0, 1, 0)) %>%
      select(-X) %>%
      group_by(quesID, ageCat, childNumDefiniteWorst) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      filter(ageCat == "child517", childNumDefiniteWorst == 1) %>%
      select(quesID, childNumDefiniteWorst = n)
      
    allQuesID <- data.frame(quesID = unique(ethiopia$quesID))
    
    ethiopiaHouseOutcomes <- full_join(allQuesID, outcomeDefiniteHazard, by = "quesID") %>%
      full_join(outcomeDefiniteWorst, by = "quesID") %>%
      replace(is.na(.), 0)
    
    # final Ethiopia household data
    ethiopiaHousehold <- full_join(ethiopiaHousePredictors, ethiopiaHouseOutcomes, by = "quesID")
    
    
    # writing household level data
    write.csv(ethiopiaHousehold, file = paste("../FinalData/ethiopiaHousehold.csv"), row.names=FALSE)
  }
  
  if(country != "Ethiopia" ){
    return(paste("Please pick Ethiopia"))
  }

}
