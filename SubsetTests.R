# tests for SubsetData

source("SubsetData.R")

subsetData("Ethiopia")
subsetData("Uganda")

ethiopia <- read.csv("../../Data/FinalData/ethiopiaFinal.csv",
                     stringsAsFactors = FALSE)
ethiopiaHousehold <- read.csv("../../Data/FinalData/ethiopiaHousehold.csv",
                              stringsAsFactors = FALSE)
ethiopiaIndividual <- read.csv("../../Data/FinalData/ethiopiaIndividual.csv",
                               stringsAsFactors = FALSE)

# Loading in Uganda datasets
uganda <- read.csv("../../Data/FinalData/ugandaFinal.csv",
                   stringsAsFactors = FALSE)
ugandaHousehold <- read.csv("../../Data/FinalData/ugandaHousehold.csv",
                            stringsAsFactors = FALSE)
ugandaIndividual <- read.csv("../../Data/FinalData/ugandaIndividual.csv",
                             stringsAsFactors = FALSE)


nrow(ethiopiaHousehold); length(unique(ethiopia$quesID))

nrow(ethiopiaIndividual); nrow(filter(ethiopia, age >= 5 & age <= 17))


nrow(ugandaHousehold); length(unique(uganda$quesID))

nrow(ugandaIndividual); nrow(filter(uganda, age >= 5 & age <= 17))

