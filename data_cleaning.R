library(readxl)
library(readr)
library(tidyverse)
library(ExcelFunctionsR)
library(haven)
library(writexl)
library(janitor)
library(sf)
library(tmap)
library(rmapshaper)
library(arsenal)


#Setup files & filter vectors

NInaID<-c("N06000018",	"N06000001",	"N06000002",	"N06000003",	"N06000004",	"N06000005",	"N06000006",	"N06000007",	"N06000008",	"N06000009",	"N06000010",	"N06000011",	"N06000012",	"N06000013",	"N06000014",	"N06000015",	"N06000016",	"N06000017") #Northern Ireland Constituencies ID

WPCname_2016<-read_xlsx("raw_files/WPCname_2016.xlsx") #names of all UK parliamentary constituencies
colnames(WPCname_2016)[1]  <- "ONSConstID"
colnames(WPCname_2016)[2]  <- "ConstituencyName"
WPCname_2016<- WPCname_2016%>%filter(!(ONSConstID %in% NInaID)) #exclude all NI results
WPCname_2016_v<-as.vector(WPCname_2016$ConstituencyName) #turning the above dataframe to a vector to filter NI strings in other datasets

#load the map
WPC_map_full <- read_sf("WPC_map")
WPC_map_shfl <- st_simplify(WPC_map_full, preserveTopology = FALSE, dTolerance = 1000)

#load the datasets

age_raw<-read_excel(
  "raw_files/age2.xls", sheet=4) #age demographic in England, Northern Ireland & Wales constituencies in 2016

age_raw_scotland<-read_excel(
  "raw_files/Scotland_data_UKPC_2016.xlsx") #age demographic in Scotland constituencies in 2016

ethnicity_raw<-read_excel(
  "raw_files/ethnicity.xlsx", sheet=4) #ethnicity demographic in all UK constituencies based on 2011 UK census

income_raw<-read_excel(
  "raw_files/NS_Table_3_15_1617.xlsx", sheet=1) #income data in all UK constituencies in 2016

vote_result_2016_raw<-read_excel(
  "raw_files/vote_result_2016.xlsx", sheet=2) #Brexit Referendum result in 2016 

political_orientation_raw<-read_excel(
  "raw_files/BES-2015-General-Election-results-file-v2.21.xlsx") #2015 UK parliamentary election result


#clean data

##Brexit Referendum result in 2016 cleaning
colnames(vote_result_2016_raw)[1]  <- "ONSConstID"
colnames(vote_result_2016_raw)[2]  <- "ConstituencyName"
colnames(vote_result_2016_raw)[6]  <- "vote_for_leave_percentage"
vote_result_2016_raw<-vote_result_2016_raw[-c(1:7),]

vote_result_2016_clean<-vote_result_2016_raw%>%filter(!(ONSConstID%in%NInaID))%>%
  select(ONSConstID,ConstituencyName,"vote_for_leave_percentage") %>%left_join(WPCname_2016)%>%select(-FID) #exclude Northern Ireland results; confined with ONSConstID, ConstituencyName and vote_for_leave_percentage.

#age 
age_raw2 <-age_raw[-c(1:3),]
age_raw2<-age_raw2 %>%row_to_names(row_number = 1)

age_combine_voting_age <- rbind(age_raw2,age_raw_scotland)%>% select(-c(3:21),-("90+")) #To combine England,Wales and Scotland Data, plus excluding non voters age(i.e<18)

age_combine_voting_age_tidied<-age_combine_voting_age %>%
  pivot_longer(cols = "18":"89",
               names_to= "age",
               values_to="population") 

colnames(age_combine_voting_age_tidied)[2]  <- "ConstituencyName"

age_combine_voting_age_tidied$age<-as.numeric(age_combine_voting_age_tidied$age)

age_mean <- age_combine_voting_age_tidied %>%
  group_by(ConstituencyName)%>%summarise(age_mean=weighted.mean(age,population))

age_mean_clean<-left_join(age_mean,WPCname_2016,by="ConstituencyName")%>%select(-(c(ConstituencyName,FID)))


#etnicity 
ethnicity_clean <- ethnicity_raw %>% 
  select (ONSConstID,ConstituencyName,"PopTotalConst%","PopWhiteConst%")%>% #exclude Northern Ireland results;select total and white population precentage
  filter(!(ONSConstID%in%NInaID))%>%mutate("PopNonWhiteConst%"=`PopTotalConst%`-
                                             `PopWhiteConst%`)%>%
  select(-(`PopTotalConst%`))%>%select(-(ConstituencyName))  #compute non-white population %

#income #DONE
colnames(income_raw)[1]  <- "ConstituencyName"
colnames(income_raw)[18]  <- "income_median"
income_clean <- income_raw%>%filter(ConstituencyName %in% WPCname_2016_v)%>%
  select(ConstituencyName,income_median) %>%left_join(WPCname_2016)%>%select(-(c(ConstituencyName,FID))) #select all income_median results in England, Scotland and Wales

#political_orientation 
political_orientation_clean<-political_orientation_raw%>%
  select(ONSConstID,ConstituencyName,Winner15)%>%select(-(c(ConstituencyName))) #select the Party of the elected MP in each constituencies

colnames(political_orientation_clean)[2] <-"MP_party"

#map_clean(withoutNI) 
WPC_map_clean<-WPC_map_full%>%filter(!(pcon15cd%in%NInaID)) #exclude all Northern Ireland results

####combine the files
dataset_2016<-left_join(age_mean_clean,income_clean,by=c("ONSConstID"))

dataset_2016<-left_join(dataset_2016,political_orientation_clean,by=c("ONSConstID"))

dataset_2016<-left_join(dataset_2016,ethnicity_clean,
                        by=c("ONSConstID"))

dataset_2016<-left_join(dataset_2016,vote_result_2016_clean,
                        by=c("ONSConstID"))

dataset_2016$income_median<-as.numeric(dataset_2016$income_median)
dataset_2016$vote_for_leave_percentage<-as.numeric(dataset_2016$vote_for_leave_percentage)

lm(formula = vote_for_leave_percentage ~ MP_party + relevel(dataset_2016_reg, ref = "MP_party"),
   data=dataset_2016_reg)

