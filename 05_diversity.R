#####Affiliance

#Annika Ertel
#Universität Leipzig/ Institut für Geographie
#Matrikelnummer: 3710313 

#Date of Preperation:20.02.21-
#SKRIPT 5: Crop Diversity Metrics (incl. Crop Asynchrony)

####SETTING UP####
#setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")
library(plyr)#CAREFUL plyr masks the dplyr function "summarise"--> summarize exists only in dplyr!
library(tidyverse)
library(readxl)
library(vegan)
library(codyn)


####LOAD DATA####

#list of files in folder (needed for the loop)
crop_prod_ISO<-list.files("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/FAO_crop_prod/crop_prod_ISO")

for (i in 1:length(crop_prod_ISO)){
  
  crop_dat<-read_csv(paste0("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/FAO_crop_prod/crop_prod_ISO/",crop_prod_ISO[i]))
  
  ####Data Preparation####
  #keep important
  crop_dat <- crop_dat[,c("ISO","Item", "Element","Year", "Value")]  
  crop_dat <- crop_dat[which(!crop_dat$Item=="Mushrooms and truffles"),]  # not plant but fungi
  
  #Select target years
  crop_dat <-crop_dat[which(crop_dat$Year%in%1961:2010),]
  
  #crop_dat <- crop_dat[which(crop_dat$Element == "Production"|crop_dat$Element=="Area harvested"),] #prod
  crop_dat<-crop_dat%>%pivot_wider(names_from = Element, values_from = Value) #combined
  
  #sort dataframe
  crop_dat<-crop_dat[order(crop_dat$Year),]
  
  #keep just production
  crop_prod <- crop_dat[,c("ISO", "Item", "Year", "Production")] #prod, because has much more values than harv
  
  #remove rows containing na
  crop_prod<-na.omit(crop_prod)
  
  #Assign time Periods in 10 year intervals
  crop_prod$timePeriod=0
  crop_prod[crop_prod$Year%in%c(1961:1970),"timePeriod"] = 1961
  crop_prod[crop_prod$Year%in%c(1971:1980),"timePeriod"] = 1971
  crop_prod[crop_prod$Year%in%c(1981:1990),"timePeriod"] = 1981
  crop_prod[crop_prod$Year%in%c(1991:2000),"timePeriod"] = 1991
  crop_prod[crop_prod$Year%in%c(2001:2010),"timePeriod"] = 2001
  
  # #only keep complete timePeriod (10yrs)
  # distinct_years<-crop_prod%>%
  #   group_by(timePeriod)%>%
  #   dplyr::summarise(rowCount= n(), distinctID=n_distinct(Year))
  
  
  ####Richness####
  #from long to wide
  crop_prod<-crop_prod%>%pivot_wider(names_from = Item, values_from = `Production`)
  
  #replace NA with 0 for vegan
  crop_prod[is.na(crop_prod)]<- 0
  
  #richness
  crop_prod$richness_R_prod<-specnumber(crop_prod[,4:ncol(crop_prod)]) #richness per year
  
  #average richness per interval
  mean_Richness_R<-crop_prod%>%
    group_by(timePeriod)%>%
    dplyr::summarise(Richness= mean(richness_R_prod))
  
  ####inverse Simpson####
  crop_prod$inv_Simpson_D_prod<-diversity(crop_prod[,4:ncol(crop_prod)], index= "invsimpson") #inv.Simpson per year
  
  #average inverse Simpson per interval
  mean_invSimp_D_prod<-crop_prod%>%
    group_by(timePeriod)%>%
    dplyr::summarise(mean_invSimp_D_prod= mean(inv_Simpson_D_prod))
  
  #from wide to long 
  crop_prod<-crop_prod%>%pivot_longer(cols=names(crop_prod[4:(ncol(crop_prod)-2)]), names_to = "Item", values_to = "Production") #all colums but the first 3 and the last two (R and D) to long again 
  
  
  ####Crop Asynchrony####
  
  #Detrending Production for Asynchrony metrics
  for (j in unique(crop_prod$Item)){
    crop_prod[which(crop_prod$Item==j),"Production_Det"] = resid(lm(Production ~ Year^2,data=crop_prod[which(crop_prod$Item==j),]))
  }  
  
  
  ## asynchrony cacluation
  #Residuals of the lm as abundance.var- as it measures the deviation from the long-term average (higher deviation, higher temp. diversity)
  # try() is for not stopping the loop if empty df is returned
  tryCatch({
    time_interval61<-crop_prod[crop_prod$Year%in%1961:1970,]#slice df into time interval; 
    time_interval61$productionAsynchrony <- ifelse(empty(time_interval61),
                                                   NULL,
                                                   1-synchrony(time_interval61,time.var="Year", species.var="Item", abundance.var="Production_Det")) #calculate asynchrony (ifelse: for the case country data is not available "make" nothing)
    
    time_interval71<-try(crop_prod[crop_prod$Year%in%1971:1980,]) #slice df into time interval; 
    time_interval71$productionAsynchrony <- ifelse(empty(time_interval71),
                                                   NULL,
                                                   1-synchrony(time_interval71,time.var="Year", species.var="Item", abundance.var="Production_Det")) #calculate asynchrony (ifelse: for the case country data is not available)
    
    time_interval81<-try(crop_prod[crop_prod$Year%in%1981:1990,]) #slice df into time interval;
    time_interval81$productionAsynchrony <- ifelse(empty(time_interval81),
                                                   NULL,
                                                   1-synchrony(time_interval81,time.var="Year", species.var="Item", abundance.var="Production_Det")) #calculate asynchrony (ifelse: for the case country data is not available)
    
    time_interval91<-try(crop_prod[crop_prod$Year%in%1991:2000,]) #slice df into time interval; 
    time_interval91$productionAsynchrony <- ifelse(empty(time_interval91),
                                                   NULL,
                                                   1-synchrony(time_interval91,time.var="Year", species.var="Item", abundance.var="Production_Det")) #calculate asynchrony (ifelse: for the case country data is not available)
    
    time_interval01<-try(crop_prod[crop_prod$Year%in%2001:2010,]) #slice df into time interval; 
    time_interval01$productionAsynchrony <- ifelse(empty(time_interval01),
                                                   NULL,
                                                   1-synchrony(time_interval01,time.var="Year", species.var="Item", abundance.var="Production_Det")) #calculate asynchrony (ifelse: for the case country data is not available)
  }, error=function(e){})
  
  #create vector containing time interval specific asynchrony
  timePeriod<-c(1961,1971,1981,1991,2001) 
  # timePeriod<-c(ifelse(empty(time_interval61),NA,1961),
  #                               ifelse(empty(time_interval71),NA,1971),
  #                               ifelse(empty(time_interval81),NA,1981),
  #                               ifelse(empty(time_interval91),NA,1991),
  #                               ifelse(empty(time_interval01),NA,2001))  #create vector containing timePeriod in right order
  
  Asynchrony<- c(ifelse(empty(time_interval61),NA,unique(time_interval61$productionAsynchrony)),
                 ifelse(empty(time_interval71),NA,unique(time_interval71$productionAsynchrony)),
                 ifelse(empty(time_interval81),NA,unique(time_interval81$productionAsynchrony)),
                 ifelse(empty(time_interval91),NA,unique(time_interval91$productionAsynchrony)),
                 ifelse(empty(time_interval01),NA,unique(time_interval01$productionAsynchrony)))  #create vector containing Asynchrony values in the right order
  
  Asynchrony<-data.frame(timePeriod, Asynchrony)
  Asynchrony<-na.omit(Asynchrony)
  
  ########summarized df ######
  ISO<-rep(unique(crop_dat$ISO),5) #extract ISO (takes the right one depending on ISO country)
  df_div<-data.frame(ISO,timePeriod)
  df_div<-df_div%>%
    full_join(mean_Richness_R)%>%
    full_join(mean_invSimp_D_prod)%>%
    full_join(Asynchrony)
  
  
  #write .csv
  write.csv(df_div, paste0("data/FAO_crop_prod/div_ISO/div_", crop_prod_ISO[i]))
  
}


####all dataframes in one big###
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/FAO_crop_prod/div_ISO")
file_list <- list.files("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/FAO_crop_prod/div_ISO")

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("df_div_all")){
    df_div_all <- read.csv(file)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("df_div_all")){
    temp_df_div_all <-read.csv(file)
    df_div_all<-rbind(df_div_all, temp_df_div_all)
    rm(temp_df_div_all)
  }
}
#remove first col X
df_div_all$X<-NULL

#first file of fulfilled_ISO is doubled, remove duplicate
df_div_all<-unique(df_div_all)

#save
write.csv(df_div_all, file="~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/data_for_analysis/diversity.csv")
