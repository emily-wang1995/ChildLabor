# tests for SubsetData

source("SubsetData.R")

subsetData("Ethiopia")


ethiopia <- read.csv("../../Data/FinalData/ethiopiaFinal.csv",
                     stringsAsFactors = FALSE)
ethiopiaHousehold <- read.csv("../../Data/FinalData/ethiopiaHousehold.csv",
                              stringsAsFactors = FALSE)
ethiopiaIndividual <- read.csv("../../Data/FinalData/ethiopiaIndividual.csv",
                               stringsAsFactors = FALSE)


nrow(ethiopiaHousehold); length(unique(ethiopia$quesID))

nrow(ethiopiaIndividual); nrow(filter(ethiopia, age >= 5 & age <= 17))
