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

Linear_smallDataS = dataS %>%  
  select(`Provider Number`, `Profit or Non-Profit`, `Chain Owned`, `Chain Organization`, `Linearized score of nephrologists' communication and caring`, `Linearized score of quality of dialysis center care and operations`, `Linearized score of providing information to patients`, `Linearized score of rating of the nephrologist`, `Linearized score of rating of the dialysis center staff`, `Linearized score of rating of the dialysis facility`) %>% 
  rename(PoNP = `Profit or Non-Profit`, Chain = `Chain Owned`,Org = `Chain Organization`, CnC = `Linearized score of nephrologists' communication and caring`,Operations = `Linearized score of quality of dialysis center care and operations`, info = `Linearized score of providing information to patients`, Neph =  `Linearized score of rating of the nephrologist`, Staff = `Linearized score of rating of the dialysis center staff`, Facility = `Linearized score of rating of the dialysis facility`)

smallDataR = dataR %>% 
  select(!c("Network","Facility Name","Five Star Date","Five Star","Five Star Data Availability Code","Address Line 1","Address Line 2","City","State","Zip","County","Phone Number",))

ourData = Linear_smallDataS %>% 
  left_join(smallDataR, by = "Provider Number") %>% 
  filter(!is.na(Facility))

ourData = left_join(Linear_smallDataS, smallDataR, "Provider Number")

ourDataN = select(ourData, where(is.numeric), where(is.factor))

cor_matrix = ourDataN %>%
  mutate_if(is.factor, as.numeric) %>%
  cor(method = 'spearman')

cor_df = as_tibble(cor_matrix, rownames = 'covar')

cor_df %>% 
  select(covar, Facility) %>% 
  top_n(10,Facility)

cor_df %>% 
  select(covar, Staff) %>% 
  top_n(10,Staff)

#run t test
forProfit_df = Linear_smallDataS %>% 
  filter(PoNP == "Profit", !is.na(Facility)) 

NonProfit_df = Linear_smallDataS %>% 
  filter(PoNP == "Non-Profit", !is.na(Facility)) 

t.test(forProfit_df$Facility, NonProfit_df$Facility)

