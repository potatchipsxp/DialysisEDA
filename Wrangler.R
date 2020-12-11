library(tidyverse)
library(skimr)
library(visdat)
library(dplyr)
library(here)
library(rerddap)

#Ireally want to use the here function but my R is configured stupidly and I dont have time to fix it right now
here()


fixingFIPS = function(arg_1) {
  return_val = ifelse(arg_1 < 10000, 
                      paste0('0', as.character(arg_1)), 
                      as.character(arg_1))
  return(return_val)
}

getCounty = function(fips){
  
  fips = fixingFIPS(fips)
  
  my_vector = vector("numeric", length(fips))
  
  for (i in 1:length(fips)) {
    #ERROR HANDLING
    possibleError <- tryCatch(
      fipscounty(code = fips[i]),
      error=function(e) e
    )
    
    if(inherits(possibleError, "error")) next
    
    #REAL WORK
    my_vector[i] = fipscounty(code = fips[i])
    
  }
  statesVec <- vector("numeric", length(fips))
  
  countiesVec <- vector("numeric", length(fips))
  
  for(i in 1:length(my_vector)){
    statesVec[i] = toupper(unlist(strsplit(my_vector[i], ","))[1])
    countiesVec[i] = toupper(substring(unlist(strsplit(my_vector[i], ","))[2], 2))
  }
  
  return(list(countiesVec,statesVec))
}


dataS = read_csv(file.path('DialysisEDA/data', 'PatientSurvey.csv'))




dataR = read_csv(file.path('DialysisEDA/data', 'FacilityReview.csv'))





dataP = read_csv(file.path('DialysisEDA/data', 'PovertyEstimates.csv'))
smalldataP = dataP %>% 
  select("FIPStxt", "POVALL_2018", "MEDHHINC_2018")

testList = getCounty(dataP$FIPStxt)


dataP = dataP[as.numeric(dataP$FIPStxt) > 1,]

dataP$FIPS = fixingFIPS(dataP$FIPStxt)


Pvector = vector("numeric", length(dataP$FIPS))


for (i in 1:length(dataP$FIPS)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    fipscounty(code = dataP$FIPS[i]),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  Pvector[i] = fipscounty(code = dataP$FIPS[i])
  
}

statesVec <- vector("numeric", length(dataP$FIPS))
countiesVec <- vector("numeric", length(dataP$FIPS))

for(i in 1:length(Pvector)){
  statesVec[i] = toupper(unlist(strsplit(Pvector[i], ","))[1])
  countiesVec[i] = toupper(substring(unlist(strsplit(Pvector[i], ","))[2], 2))
}

dataP$County = countiesVec
dataP$State = statesVec







cleandata = read_csv(file.path('DialysisEDA/data', 'cleanData.csv'))

dataP

skimr::skim(dataS)
skimr::skim(dataR)

head(dataS)
colnames(dataS)
colnames(dataR)

vis_miss(dataS, warn_large_data = FALSE)

Linear_smallDataS = dataS %>%  
  select(`Provider Number`, `State`, `County`, `Profit or Non-Profit`, `Chain Owned`, `Chain Organization`, `Linearized score of nephrologists' communication and caring`, `Linearized score of quality of dialysis center care and operations`, `Linearized score of providing information to patients`, `Linearized score of rating of the nephrologist`, `Linearized score of rating of the dialysis center staff`, `Linearized score of rating of the dialysis facility`) %>% 
  rename(PoNP = `Profit or Non-Profit`, Chain = `Chain Owned`,Org = `Chain Organization`, CnC = `Linearized score of nephrologists' communication and caring`,Operations = `Linearized score of quality of dialysis center care and operations`, info = `Linearized score of providing information to patients`, Neph =  `Linearized score of rating of the nephrologist`, Staff = `Linearized score of rating of the dialysis center staff`, Facility = `Linearized score of rating of the dialysis facility`)

smallDataR = dataR %>% 
  select(!c("Network","Facility Name","Five Star Date","Five Star","Five Star Data Availability Code","Address Line 1","Address Line 2","City","Zip","Phone Number",))

ourData = Linear_smallDataS %>% 
  left_join(smallDataR, by = c("Provider Number", "State", "County")) %>% 
  filter(!is.na(Facility))

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


my_vector <- vector("numeric", length(dataP2$FIPS))


for (i in 1:length(dataP2$FIPS)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    fipscounty(code = dataP2$FIPS[i]),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  #REAL WORK
  my_vector[i] = fipscounty(code = dataP2$FIPS[i])
  
}

getCounty = function(Df, column){
  
  df = Df
  
  df$FIPS = fixingFIPS(df$column)
  
  my_vector = vector("numeric", length(df$FIPS))
  
  for (i in 1:length(df$FIPS)) {
    #ERROR HANDLING
    possibleError <- tryCatch(
      fipscounty(code = df$FIPS[i]),
      error=function(e) e
    )
    
    if(inherits(possibleError, "error")) next
    
    #REAL WORK
    my_vector[i] = fipscounty(code = dataP2$FIPS[i])
    
  }
  statesVec <- vector("numeric", length(df$FIPS))
  
  countiesVec <- vector("numeric", length(df$FIPS))
  
  for(i in 1:length(my_vector)){
    statesVec[i] = toupper(unlist(strsplit(my_vector[i], ","))[1])
    countiesVec[i] = toupper(substring(unlist(strsplit(my_vector[i], ","))[2], 2))
  }
  dataP2$County = countiesVec
  dataP2$State = statesVec
}

my_vector

statesVec <- vector("numeric", length(dataP2$FIPS))

countiesVec <- vector("numeric", length(dataP2$FIPS))

for(i in 1:length(my_vector)){
  statesVec[i] = toupper(unlist(strsplit(my_vector[i], ","))[1])
  countiesVec[i] = toupper(substring(unlist(strsplit(my_vector[i], ","))[2], 2))
}



toupper(substring(unlist(strsplit(my_vector[2], ","))[[2]], 2))

dataP2$County = countiesVec
dataP2$State = statesVec



fullData = left_join(ourData, dataP2, by = c("State", "County"))

colnames(ourData)

write.csv(fullData,"DialysisEDA/data\\cleanData.csv", row.names = FALSE)

cleandata = cleandata %>% 
  select(!c("POV04_2018",	"CI90LB04_2018",	"CI90UB04_2018",	"PCTPOV04_2018",	"CI90LB04P_2018",	"CI90UB04P_2018"))


cleandata_Num = select(cleandata, where(is.numeric), where(is.factor))

cleandata_Num = cleandata_Num[, colSums(is.na(cleandata_Num)) < nrow(cleandata_Num) * 0.5] 

cleandata_Num = na.omit(cleandata_Num)


cor_matrixEcon = cleandata_Num %>%
  mutate_if(is.factor, as.numeric) %>%
  cor(method = 'spearman')

corEcon_df = as_tibble(cor_matrixEcon, rownames = 'covar')

corEcondf2 = corEcon_df %>% 
  select(covar, `Fistula Rate (Facility)`)

corEcon_df %>% 
  select(covar, `Fistula Rate (Facility)`) %>% 
  top_n(10, `Fistula Rate (Facility)`)
















