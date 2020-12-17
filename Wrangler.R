library(tidyverse)
library(skimr)
library(visdat)
library(dplyr)
library(here)
library(rerddap)

#Ireally want to use the here function but my R is configured stupidly and I dont have time to fix it right now
here()

#created a function to fix fips codes to be all five digits when necessary
fixingFIPS = function(arg_1) {
  return_val = ifelse(arg_1 < 10000, 
                      paste0('0', as.character(arg_1)), 
                      as.character(arg_1))
  return(return_val)
}

#import survey data
dataSurv = read_csv(file.path('DialysisEDA/data', 'PatientSurvey.csv'))

#create smaller more useful data set and keep the numerical versions of the redundant variables (many variables have multiple versions (linear vs onehot encoding etc)
Linear_smalldataS = dataSurv %>%  
  select(`Provider Number`, `State`, `County`, `Profit or Non-Profit`, `Chain Owned`, `Chain Organization`, `Linearized score of nephrologists' communication and caring`, `Linearized score of quality of dialysis center care and operations`, `Linearized score of providing information to patients`, `Linearized score of rating of the nephrologist`, `Linearized score of rating of the dialysis center staff`, `Linearized score of rating of the dialysis facility`) %>% 
  rename(PoNP = `Profit or Non-Profit`, Chain = `Chain Owned`,Org = `Chain Organization`, CnC = `Linearized score of nephrologists' communication and caring`,Operations = `Linearized score of quality of dialysis center care and operations`, info = `Linearized score of providing information to patients`, Neph =  `Linearized score of rating of the nephrologist`, Staff = `Linearized score of rating of the dialysis center staff`, Facility = `Linearized score of rating of the dialysis facility`)

#import stat review data
dataRev = read_csv(file.path('DialysisEDA/data', 'FacilityReview.csv'))
#drop unnecessary columns
smallDataR = dataRev %>% 
  select(!c("Network","Facility Name","Five Star Date","Five Star","Five Star Data Availability Code","Address Line 1","Address Line 2","City","Zip","Phone Number",))

#create one data set of all clininc data
dataMed = Linear_smalldataS %>% 
  left_join(smallDataR, by = c("Provider Number", "State", "County")) %>% 
  filter(!is.na(Facility))

#import poverty data for counties in us
dataPov = read_csv(file.path('DialysisEDA/data', 'PovertyEstimates.csv'))
#drop the rows that correspond to whole states or the whole country
dataPov = dataPov[as.numeric(dataPov$FIPStxt) > 1,]
dataPov = dataPov[(as.numeric(dataPov$FIPStxt) %% 1000 != 0),]
smalldataPov = dataPov %>% 
  select("FIPStxt", "POVALL_2018", "MEDHHINC_2018")

#import education levle data for counties in us
dataEdu = read_csv(file.path('DialysisEDA/data', 'Education.csv'))
#grab a few useful columns for describing education at the county level
smalldataEdu = dataEdu %>% 
  select("FIPS Code",
         "Percent of adults with less than a high school diploma, 2014-18",
         "Percent of adults with a high school diploma only, 2014-18",
         "Percent of adults completing some college or associate's degree, 2014-18",
         "Percent of adults with a bachelor's degree or higher, 2014-18")

#create one demographics dataset combing poverty and education data
dataDemog = left_join(smalldataPov, smalldataEdu, by = c("FIPStxt" = "FIPS Code"))

#fix any fips codes that arent 5 digits. For example alabama fips code is 01000 but in this data set it is just 1000 so we have to fix it
dataDemog$FIPS = fixingFIPS(dataDemog$FIPStxt)


#in order to match the county to the clinic data we have to get each county name. This code will go to the website and check each fips code against the list there to find the county names. I could have done this an easier way by download that info ahead of time but I wanted to learn how to do this.
#be warned, this will take a long time to run


my_vector <- vector("numeric", length(dataDemog$FIPS))


for (i in 1:length(dataDemog$FIPS)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    fipscounty(code = dataDemog$FIPS[i]),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  #REAL WORK
  my_vector[i] = fipscounty(code = dataDemog$FIPS[i])
  
}

my_vector

statesVec <- vector("numeric", length(dataDemog$FIPS))

countiesVec <- vector("numeric", length(dataDemog$FIPS))

for(i in 1:length(my_vector)){
  statesVec[i] = toupper(unlist(strsplit(my_vector[i], ","))[1])
  countiesVec[i] = toupper(substring(unlist(strsplit(my_vector[i], ","))[2], 2))
}

dataDemog$County = countiesVec
dataDemog$State = statesVec

#save our two final data frames, there is an option below to join them, but I decided against it because i wanted to show some of the steps of the eda in the paper too.
write.csv(dataDemog,"DialysisEDA/data\\cleanDemogData.csv", row.names = FALSE)

write.csv(dataMed,"DialysisEDA/data\\cleanMedData.csv", row.names = FALSE)












#finalData = left_join(smalldataMed, dataDemog, by = c("State", "County"))
