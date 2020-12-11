library(tidyverse)
library(skimr)
library(visdat)
library(dplyr)
library(here)
library(rerddap)


dataD = read_csv(file.path('DialysisEDA/data', 'cleanDemogData.csv'))

dataM = read_csv(file.path('DialysisEDA/data', 'cleanMedData.csv'))

skimr::skim(dataD)
skimr::skim(dataM)

s_miss(dataS, warn_large_data = FALSE)



dataM_N = select(dataM, where(is.numeric), where(is.factor))

cor_matrix = dataM_N %>%
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
forProfit_df = dataM %>% 
  filter(PoNP == "Profit", !is.na(Facility)) 

NonProfit_df = dataM %>% 
  filter(PoNP == "Non-Profit", !is.na(Facility)) 

t.test(forProfit_df$Facility, NonProfit_df$Facility)


smalldataM = dataM %>% 
  select("Provider Number", "State", "County", "Facility", "PoNP", "Fistula Rate (Facility)")

fullData = left_join(smalldataM, dataD, by = c("State", "County"))

fullData2 = fullData %>% 
  select(!c("Provider Number",                                                         
            "State",                                                                   
            "County",                                                                  
            "PoNP",
            "FIPStxt",
            "FIPS")) %>% 
  na.omit()

cor_matrixDemo = fullData2 %>%
  mutate_if(is.factor, as.numeric) %>%
  cor(method = 'spearman')

corDemo_df = as_tibble(cor_matrixDemo, rownames = 'covar')

#scatter.smooth(x=fullData2$MEDHHINC_2018, y=fullData2$`Fistula Rate (Facility)`, main="Dist ~ Speed")  # scatterplot

fullData2$avgEdu = fullData2$`Percent of adults with a high school diploma only, 2014-18` + fullData2$`Percent of adults completing some college or associate's degree, 2014-18` * 2 + fullData2$`Percent of adults with a bachelor's degree or higher, 2014-18` * 3

#scatter.smooth(x=fullData2$`Percent of adults with a high school diploma only, 2014-18` , y=fullData2$`Fistula Rate (Facility)`, main="Dist ~ Speed")  # scatterplot

cor_matrixDemo = fullData2 %>%
  mutate_if(is.factor, as.numeric) %>%
  cor(method = 'spearman')

corDemo_df = as_tibble(cor_matrixDemo, rownames = 'covar')


Fist_by_Econ = lm(`Fistula Rate (Facility)` ~ `MEDHHINC_2018`, data = fullData2)

summary(Fist_by_Econ)


corEcondf2 = corDemo_df %>% 
  select(covar, `Fistula Rate (Facility)`)

corEcon_df %>% 
  select(covar, `Fistula Rate (Facility)`) %>% 
  top_n(10, `Fistula Rate (Facility)`)


