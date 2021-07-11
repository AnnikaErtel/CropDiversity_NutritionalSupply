#####Affiliance

#Annika Ertel
#Universität Leipzig/ Institut für Geographie
#Matrikelnummer: 3710313 

#SKRIPT 1: Country Selection and split of large Dataframes

####Setting up

setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")
rm(list=ls())

library(tidyverse)
library(readxl)
library(countrycode)

#************************************************************************************************************************************************
####country selection####
#************************************************************************************************************************************************
##Criteria for basis selection: just takes the 100 most populus nations (representing 89% of the global 2010 population and 91% of the global crop production)
FAO_pop_2010 <-read.csv("data/pop_data/FAO_pop_2010.csv") #population data

FAO_pop_2010$ISO <-countrycode(FAO_pop_2010$Area.Code, "fao","iso3c") #Some values were not matched unambiguously:
   #41: China, mainland, 151: Netherlands Antilles (former), 206: Sudan (former), 259: Channel Islands, 299: Palestine

#remove countries which had changing borders 
#USSR and Russian federation (RUS and SUN) 
#Czechoslovakia (CSK) : dissolution into the Czech Republic (CZE) and Slovakia (SVK) on 1 January 1993
#The republics of Serbia and Montenegro (SCG) together established a federation in 1992 as the Federal Republic of Yugoslavia (FR Yugoslavia or FRY). 
#Became independent in 2006 in Serbia (SRB), Montenegro (MNE)
#Yugoslav SFR:Socialist Federal Republic of Yugoslavia not to be confunded with the Federal Republic of Yugoslavia
#South Sudan (SSD) and Sudan (SDN), Ethiopa and Ethiopia PDR (ETH)
FAO_pop_2010<- droplevels(subset(FAO_pop_2010, !(FAO_pop_2010$ISO %in% c("RUS", "SUN" ,"CSK", "CZE", "SVK", "SCG", "SRB", "MNE",
                                                "SSD", "SDN", "SFR", "PRK", "ETH"))))

#most populous countries
FAO_pop_2010 <-FAO_pop_2010[order(FAO_pop_2010$Value, decreasing = T),] #sort df
#China
FAO_pop_2010 <-FAO_pop_2010[!FAO_pop_2010$Area=="China, mainland",] #df contains doubled listing: "china, mainland" and "china"
FAO_pop_2010 <-FAO_pop_2010[!FAO_pop_2010$Area=="Sudan (former)",]
FAO_pop_2010 <-head(FAO_pop_2010,100) #take first 100 rows, thus most populous countries

##Some countries are additionally removed from analysis (following Renard and Tilmann 2019):
#Because of high rates of estimated FAO data: North Korea (PRK), Guinea (GIN), Kenya (KEN), Mozambique (MOZ), Zambia (ZMB)
#Because of uncertaintys concerning fertilizer use: Ireland (IRL), Netherland (NLD), New Zealand (NZL) 
#Because has  100% of cropland as area equipped for irrigation: Egypt (EGY)
FAO_pop_2010 <-FAO_pop_2010[!(FAO_pop_2010$Area == "Egypt"| FAO_pop_2010$Area == "Netherlands" | FAO_pop_2010$Area ==  "North Korea" |
                              FAO_pop_2010$Area == "Guinea" | FAO_pop_2010$Area == "Kenya" | FAO_pop_2010$Area ==  "Mozambique"|
                              FAO_pop_2010$Area == "Zambia" | FAO_pop_2010$Area ==  "New Zealand" | FAO_pop_2010$Area ==  "Ireland"),]
  #here 94 countries (not 91) as New Zealand, Ireland and North Korea are not under the 100 most populous countries 

#Change FAO code to ISO code
#FAO_pop_2010 <-FAO_pop_2010$Area.Code
FAO_pop_2010$ISO<-countrycode(FAO_pop_2010$Area.Code, "fao","iso3c") #no country matched unambigously 
target_country<-subset(FAO_pop_2010, select = c("Area","Area.Code","ISO"))


###### Save #### 
write_csv(target_country, file ="data/target_country.csv")

##Criteria for selection: All variables need to be available for every year of each decade
#That is done in "06_variables.R" 



#*******************************************************************************************************************************************
##### Split fbs data ####
#*******************************************************************************************************************************************
#load fbs data
fbs<-"data/FAO_fbs/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).zip"%>%
  unz("FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv")%>%
  read_csv
target_country<-read.csv("data/target_country.csv")

#take only target countries
fbs<-fbs%>%
  filter(fbs$`Area Code`%in%target_country$Area.Code)

#FAO_pop_2010 <-FAO_pop_2010$Area.Code
fbs$ISO<-countrycode(fbs$`Area Code`, "fao","iso3c") #no country matched unambigously 

#check if population data include all countries
n_distinct(fbs$ISO) == n_distinct(target_country$ISO)  
uniqueIso<-unique(fbs$ISO) 
dif<-setdiff(target_country$ISO,uniqueIso)
# "COD"(democratic republic of kongo), "SYR"(Syria), "SOM" (Somalia), "BDI" (Burundi), "PNG" (Papua New Guinea) not listed in fbs data. -> will be removed in final_data.R

# Save
fbs2<-fbs    #Make copy of fbs
area2<-split(fbs2, fbs2$ISO) #split by country

setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/FAO_fbs/fbs_ISO")

lapply(names(area2), function(x){
  write_csv(area2[[x]], file = paste0(x , ".csv"))
})
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")

#*******************************************************************************************************************************************
#### Split pop data ####
#*******************************************************************************************************************************************
#load data
target_country<-read.csv("data/target_country.csv")
pop<-read_csv("data/pop_data/worldbank/Worldbank_Pop.csv")

#rename country code to ISO
names(pop)[names(pop) == "Country Code"]<- "ISO"

#keep all countries existing in population AND in target_country
pop<-pop[which(pop$ISO %in% target_country$ISO),]
n_distinct(pop$ISO) == n_distinct(target_country$ISO)   #check if population data include all countries -> No
uniqueIso<-unique(pop$ISO) 
dif<-setdiff(target_country$ISO,uniqueIso) #Which country?->Taiwan -> Taiwan 
    # data for China does not include data for taiwan. FAO data doesn't include it as well, as it is listed seperately. 
    #(https://datahelpdesk.worldbank.org/knowledgebase/articles/114933-where-are-your-data-on-taiwan)
    # Taiwan will be removed from entire dataset in "final_data.R"

#split pop data by country
pop2<-pop   #Make copy of pop
split_pop<-split(pop2, pop2$ISO) #split by country

# Save
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/pop_data/worldbank/pop_ISO")

lapply(names(split_pop), function(x){
  write_csv(split_pop[[x]], file = paste0(x , "_pop.csv"))
})
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")

#*******************************************************************************************************************************************
#### Split birth_rate data ####
#*******************************************************************************************************************************************
#load data
target_country<-read.csv("data/target_country.csv")
birth_rate<-read_csv("data/pop_data/worldbank/Birth_rate.csv")

#rename country code to ISO
names(birth_rate)[names(birth_rate) == "Country Code"]<- "ISO"

#keep all countries existing in birthrate AND in target_country
birth_rate<-birth_rate[which(birth_rate$ISO %in% target_country$ISO),]
n_distinct(birth_rate$ISO) == n_distinct(target_country$ISO)   #check if population data include all countries -> No
uniqueIso<-unique(birth_rate$ISO) 
dif<-setdiff(target_country$ISO,uniqueIso) #Which country?->Taiwan -> Taiwan 
# data for China does not include data for taiwan. FAO data doesn't include it as well, as it is listed seperately. 
#(https://datahelpdesk.worldbank.org/knowledgebase/articles/114933-where-are-your-data-on-taiwan)
# Taiwan will be removed from entire dataset in "final_data.R"

#split birth_rate data by country
birth_rate2<-birth_rate   #Make copy of birth_rate
split_birth_rate<-split(birth_rate2, birth_rate2$ISO) #split by country

# Save
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/pop_data/worldbank/birth_rate_ISO")

lapply(names(split_birth_rate), function(x){
  write_csv(split_birth_rate[[x]], file = paste0(x , "_birth_rate.csv"))
})
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")

#*******************************************************************************************************************************************
####Split Crop-Production Data####
#*******************************************************************************************************************************************

#load data
crop_prod<-read.csv("data/FAO_crop_prod/Production_Crops_E_All_Data_(Normalized).csv")
target_country<-read.csv("data/target_country.csv")

#take only target countries
crop_prod<-crop_prod%>%
  filter(crop_prod$`Area.Code`%in%target_country$Area.Code)

#FAO_pop_2010 <-FAO_pop_2010$Area.Code
crop_prod$ISO<-countrycode(crop_prod$`Area.Code`, "fao","iso3c") #no country matched unambigously 

#check if population data include all countries
n_distinct(crop_prod$ISO) == n_distinct(target_country$ISO)  #Yes 

crop_prod2<-crop_prod    #Make copy of crop_prod
area2<-split(crop_prod2, crop_prod2$ISO) #split by country

# Save
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/FAO_crop_prod/crop_prod_ISO")

lapply(names(area2), function(x){
  write_csv(area2[[x]], file = paste0(x , ".csv"))
})
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")

#*******************************************************************************************************************************************
