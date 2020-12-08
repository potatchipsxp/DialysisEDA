library(tidyverse)
library(skimr)
library(visdat)
library(dplyr)
library(here)

#Ireally want to use the here function but my R is configured stupidly and I dont have time to fix it right now
here()
here::here("DialysisEDA", "data", 'PatientSurvey.csv')

dataS = read_csv(file.path('DialysisEDA/data', 'PatientSurvey.csv'))
dataR = read_csv(file.path('DialysisEDA/data', 'FacilityReview.csv'))

skimr::skim(dataS)
skimr::skim(dataR)

head(dataS)
colnames(dataS)
colnames(dataR)

vis_miss(dataS, warn_large_data = FALSE)

smallDataS <- dataS %>%  
  select(`Profit or Non-Profit`, `Chain Owned`, `Chain Organization`, `Linearized score of nephrologists' communication and caring`, `Star rating of quality of dialysis center care and operations`, `Star rating of quality of dialysis center care and operations`, yellowCards, ties) 

