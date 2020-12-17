library(tidyverse)
library(skimr)
library(visdat)
library(dplyr)
library(here)
library(rerddap)

#import demographics and economic for county data
dataD = read_csv(file.path('DialysisEDA/data', 'cleanDemogData.csv'))

#import state and pateint review data of dialysis facility
dataM = read_csv(file.path('DialysisEDA/data', 'cleanMedData.csv'))

#take a quick peek at the data
skimr::skim(dataD)
skimr::skim(dataM)

s_miss(dataS, warn_large_data = FALSE)

#notice that there are several columns with many Na, these seem to be columns where many of these facilites dont provide such services, such as pediatric services.

#create a numeric only data set to make a correlation matrix
dataM_N = select(dataM, where(is.numeric), where(is.factor))

#create correlation amtrix
cor_matrix = dataM_N %>%
  mutate_if(is.factor, as.numeric) %>%
  cor(method = 'spearman')

#turn correlation matrix into a tibble so we can select different parts because otherwise its effing huge
cor_df = as_tibble(cor_matrix, rownames = 'covar')

#look at how the top ten relate to facility
cor_df %>% 
  select(covar, Facility) %>% 
  top_n(10,Facility)

#noticed that staff and facility are very related so lets look at the top ten things that predict staff
cor_df %>% 
  select(covar, Staff) %>% 
  top_n(10,Staff)

#run t test on profit vs non profit to show capitalism in healthcare sucks
forProfit_df = dataM %>% 
  filter(PoNP == "Profit", !is.na(Facility)) 

NonProfit_df = dataM %>% 
  filter(PoNP == "Non-Profit", !is.na(Facility)) 

t.test(forProfit_df$Facility, NonProfit_df$Facility)

#lets make a small dataset to look at Avfistula and how it relates to demographics (reasoning in paper)
smalldataM = dataM %>% 
  select("Provider Number", "State", "County", "Facility", "PoNP", "Fistula Rate (Facility)")

#combine with demographics
fullData = left_join(smalldataM, dataD, by = c("State", "County"))

#remove all non quantitative variables, and any other problem vars
fullData2 = fullData %>% 
  select(!c("Provider Number",                                                         
            "State",                                                                   
            "County",                                                                  
            "PoNP",
            "FIPStxt",
            "FIPS")) %>% 
  na.omit()

#make new corr amtrix to look at relationships between fistual and demographics
cor_matrixDemo = fullData2 %>%
  mutate_if(is.factor, as.numeric) %>%
  cor(method = 'spearman')

#income seemed to be related lets look at that really quick
corDemo_df = as_tibble(cor_matrixDemo, rownames = 'covar')

#scatter.smooth(x=fullData2$MEDHHINC_2018, y=fullData2$`Fistula Rate (Facility)`, main="Dist ~ Speed")  # scatterplot

#education didnt seem related, but maybe if we change wrangle that variable a bit it will become more clear
fullData2$avgEdu = fullData2$`Percent of adults with a high school diploma only, 2014-18` + fullData2$`Percent of adults completing some college or associate's degree, 2014-18` * 2 + fullData2$`Percent of adults with a bachelor's degree or higher, 2014-18` * 3

#remake corr matrix
cor_matrixDemo = fullData2 %>%
  mutate_if(is.factor, as.numeric) %>%
  cor(method = 'spearman')

corDemo_df = as_tibble(cor_matrixDemo, rownames = 'covar')

#^, edu not looking help still

#plot new edu varuable just to be sure
#scatter.smooth(x=fullData2$`Percent of adults with a high school diploma only, 2014-18` , y=fullData2$`Fistula Rate (Facility)`, main="Dist ~ Speed")  # scatterplot


#lets amke a regression to analyze income though since that seemed promising

Fist_by_Econ = lm(`Fistula Rate (Facility)` ~ `MEDHHINC_2018`, data = fullData2)

summary(Fist_by_Econ)

#whaddaya know, income is related, my wife will be so happy because her clininc is always viewed poorly becuase of how many !AV fistula they have, but her clinic is in merced so maybe income is playing a role


