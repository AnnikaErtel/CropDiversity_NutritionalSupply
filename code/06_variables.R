#####Affiliance

#Annika Ertel
#Universität Leipzig/ Institut für Geographie
#Matrikelnummer: 3710313 

#SKRIPT 6: Preparation of other variables 

####Setting up####
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")
rm(list=ls())

library(tidyverse)
library(readxl)
library(countrycode)

####Load data
target_country<-read.csv("data/target_country.csv")
fertilizer<-read.csv("data/variables/FAOSTAT_fertilizer.csv")
irrigation<-read_csv("data/variables/Area_equ_Irrigation.csv")
warfare<-read_xls("data/variables/warefare.xls")
gdp<-read_csv("data/pop_data/worldbank/GDP_per_capita.csv")
#final_ISO<-read_csv("data/final_ISO.csv")       #final Country selection (done at the end of this skript)
agriculture<-read.csv("data/FAOstat/FAO_Agriculture.csv")
livestock<-read.csv("data/FAOstat/FAO_Livestock.csv")

####GDP PPP per capita####
# only take target country
gdp<-gdp[gdp$`Country Code` %in% target_country$ISO,]

#rename col
colnames(gdp)[5:65]<-colnames(gdp)[5:65]%>%
  str_sub(start = 1L, end = 4L)

# only 1961-2010 and and country code
gdp<-gdp[,c(2,6:55)]

#from wide to long
gdp<-pivot_longer(gdp, cols = "1961":"2010", names_to = "Year", values_to = "gdp_per_capita_USD")

#Assign time Periods in 10 year intervals
gdp$timePeriod=0
gdp[gdp$Year%in%c(1961:1970),"timePeriod"] = 1961
gdp[gdp$Year%in%c(1971:1980),"timePeriod"] = 1971
gdp[gdp$Year%in%c(1981:1990),"timePeriod"] = 1981
gdp[gdp$Year%in%c(1991:2000),"timePeriod"] = 1991
gdp[gdp$Year%in%c(2001:2010),"timePeriod"] = 2001

# ".." to NA
gdp[gdp==".."]<-NA
sum(is.na(gdp)) #104

#count per country: which timePeriods are how insecure? 
gdp_count<-na.omit(gdp)
gdp_count<-gdp_count%>%
  group_by(`Country Code`)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise(rowCount= n())

#calculate mean per decade
gdp$gdp_per_capita_USD<-as.numeric(gdp$gdp_per_capita_USD)
gdp<-gdp%>%
  group_by(`Country Code`)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise("gdp_per_capita_USD"= mean(`gdp_per_capita_USD`, na.rm= T))

#rename countrycode for later join
colnames(gdp)[1]<-"ISO"
  
# I will add all data to final data even if there are some NA`s values
# -> have to be adressed before analysis!

####Agriculture area####
# only take target country
agriculture<-agriculture[agriculture$Area.Code %in% target_country$Area.Code,]

# #### adapt region names (fao.code-> iso)
agriculture$ISO <- countrycode(agriculture$`Area.Code`, 'fao', 'iso3c')   # no important regions missing

# only keep target year
agriculture <- agriculture[which(agriculture$Year%in%1961:2010),]

#only important info
agriculture$Value<-names(agriculture)[names(agriculture)=="Value"]<-"agriculture_area_ha"  #rename column
agriculture<-agriculture[c("ISO", "Year", "agriculture_area_ha")]

#Assign time Periods in 10 year intervals
agriculture$timePeriod=0
agriculture[agriculture$Year%in%c(1961:1970),"timePeriod"] = 1961
agriculture[agriculture$Year%in%c(1971:1980),"timePeriod"] = 1971
agriculture[agriculture$Year%in%c(1981:1990),"timePeriod"] = 1981
agriculture[agriculture$Year%in%c(1991:2000),"timePeriod"] = 1991
agriculture[agriculture$Year%in%c(2001:2010),"timePeriod"] = 2001

#count per country
agriculture_count<-agriculture%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise(rowCount= n())

#only those timePeriods were time series is complete
agriculture_country<-agriculture_count%>%filter(rowCount==10) #find out which is complete
agriculture_country<-agriculture_country[,c("ISO", "timePeriod")] #selects info for filter
agriculture_merge<-merge(agriculture_country,agriculture) #takes only those timePeriods were series is complete
agriculture_merge<-agriculture_merge[,c("ISO", "timePeriod","agriculture_area_ha")]

#calculate mean per decade
agriculture_mean<-agriculture_merge%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise("agriculture_area_ha"= mean(`agriculture_area_ha`))


#### Livestock #### 
livestock<-read.csv("data/FAOstat/FAO_Livestock.csv")

# only take target country
livestock<-livestock[livestock$Area %in% target_country$Area,]

# #### adapt region names (fao.code-> iso)
livestock$ISO <- countrycode(livestock$`Area.Code`, 'fao', 'iso3c')   # no important regions missing

# only keep target year
livestock <- livestock[which(livestock$Year%in%1961:2010),]

#only important info
livestock$Value<-names(livestock)[names(livestock)=="Value"]<-"Livestock_LSU"
livestock<-livestock[c("ISO", "Year", "Livestock_LSU")]

#summarize all livestocks per year and country (doesn't mind, different kinds of animals LSU references bodyweight)
livestock<-livestock%>%
  group_by(ISO)%>%
  group_by(Year, .add = T)%>%
  dplyr::summarise(Livestock_LSU =sum(Livestock_LSU))
  
#Assign time Periods in 10 year intervals
livestock$timePeriod=0
livestock[livestock$Year%in%c(1961:1970),"timePeriod"] = 1961
livestock[livestock$Year%in%c(1971:1980),"timePeriod"] = 1971
livestock[livestock$Year%in%c(1981:1990),"timePeriod"] = 1981
livestock[livestock$Year%in%c(1991:2000),"timePeriod"] = 1991
livestock[livestock$Year%in%c(2001:2010),"timePeriod"] = 2001

#count per country
livestock_count<-livestock%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise(rowCount= n())

#only those timePeriods were time series is complete
livestock_country<-livestock_count%>%filter(rowCount==10) #find out which is complete
livestock_country<-livestock_country[,c("ISO", "timePeriod")] #selects info for filter
livestock_merge<-merge(livestock_country,livestock) #takes only those timePeriods were series is complete
livestock_merge<-livestock_merge[,c("ISO", "timePeriod","Livestock_LSU")]

#calculate mean per decade
livestock_mean<-livestock_merge%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise("Livestock_LSU"= mean(`Livestock_LSU`))


#### Fertilizer data #####

# only take target country
fertilizer<-fertilizer[fertilizer$Area %in% target_country$Area,]

# #### adapt region names (fao.code-> iso)
fertilizer$ISO <- countrycode(fertilizer$`Area.Code`, 'fao', 'iso3c')   # no important regions missing

# only keep target year
fertilizer <- fertilizer[which(fertilizer$Year%in%1961:2010),]

#only important info
fertilizer$Value<-names(fertilizer)[names(fertilizer)=="Value"]<-"N_use/croparea_in_kg/ha"
fertilizer<-fertilizer[c("ISO", "Year", "N_use/croparea_in_kg/ha")]

#Assign time Periods in 10 year intervals
fertilizer$timePeriod=0
fertilizer[fertilizer$Year%in%c(1961:1970),"timePeriod"] = 1961
fertilizer[fertilizer$Year%in%c(1971:1980),"timePeriod"] = 1971
fertilizer[fertilizer$Year%in%c(1981:1990),"timePeriod"] = 1981
fertilizer[fertilizer$Year%in%c(1991:2000),"timePeriod"] = 1991
fertilizer[fertilizer$Year%in%c(2001:2010),"timePeriod"] = 2001

#count per country
fertilizer_count<-fertilizer%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise(rowCount= n())

#only those timePeriods were time series is complete
fertilizer_country<-fertilizer_count%>%filter(rowCount==10) #find out which is complete
fertilizer_country<-fertilizer_country[,c("ISO", "timePeriod")] #selects info for filter
fertilizer_merge<-merge(fertilizer_country,fertilizer) #takes only those timePeriods were series is complete
fertilizer_merge<-fertilizer_merge[,c("ISO", "timePeriod","N_use/croparea_in_kg/ha")]

#calculate mean per decade
fertilizer_mean<-fertilizer_merge%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise("N_use/croparea_in_kg/ha"= mean(`N_use/croparea_in_kg/ha`))

####Irrigation data##### 
#-> Land Area equipped for Irrigation
# only take target country
irrigation<-irrigation[irrigation$`Area Code` %in% target_country$Area.Code,]

# adapt region names (fao.code-> iso)
irrigation$ISO <- countrycode(irrigation$`Area Code`, 'fao', 'iso3c')   # no important regions missing

# only keep target year
irrigation <- irrigation[which(irrigation$Year%in%1961:2010),]

#only important info
names(irrigation)[names(irrigation)=="Value"]<-"Land_area_equ._for-Irrigation_%"
irrigation<-irrigation[c("ISO", "Year", "Land_area_equ._for-Irrigation_%")]

#Assign time Periods in 10 year intervals
irrigation$timePeriod=0
irrigation[irrigation$Year%in%c(1961:1970),"timePeriod"] = 1961
irrigation[irrigation$Year%in%c(1971:1980),"timePeriod"] = 1971
irrigation[irrigation$Year%in%c(1981:1990),"timePeriod"] = 1981
irrigation[irrigation$Year%in%c(1991:2000),"timePeriod"] = 1991
irrigation[irrigation$Year%in%c(2001:2010),"timePeriod"] = 2001

#count per country
irrigation_count<-irrigation%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise(rowCount= n())

#only those timePeriods were time series is complete
irrigation_country<-irrigation_count%>%filter(rowCount==10) #find out which is complete
irrigation_country<-irrigation_country[,c("ISO", "timePeriod")] #selects info for filter
irrigation_merge<-merge(irrigation_country,irrigation) #takes only those timePeriods were series is complete
irrigation_merge<-irrigation_merge[,c("ISO", "timePeriod","Land_area_equ._for-Irrigation_%")]

#calculate mean per decade
irrigation_mean<-irrigation_merge%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise("Land_area_equ._for-Irrigation_%"= mean(`Land_area_equ._for-Irrigation_%`))

####Warefare####
#->number of armed conflicts!

# adapt region names (country.name-> iso)
warfare$ISO<- countrycode(warfare$country, 'country.name', 'iso3c')   #von country zu iso 
#Some values were not matched unambiguously: Czechoslovakia, Germany East, Kosovo, Serbia and Montenegro, Vietnam South, Yemen North, Yemen South, Yugoslavia

# only keep target year
warfare <- warfare[which(warfare$year%in%1961:2010),]

#target column (number of armed conflicts)
warfare <- warfare[,c("ISO","country","year","actotal")]

#remove na's and countries with no armed conflicts
warfare <- warfare[!warfare$actotal==0,]

# Check wich countries need to be prepared/translated
warfare$ISO<- countrycode(warfare$country, 'country.name', 'iso3c')
#Some values were not matched unambiguously: Czechoslovakia, Vietnam South, Yemen North, Yemen South, Yugoslavia

# combine north and south yemen
Yemen_N <- warfare[which(warfare$country=="Yemen North" & warfare$year%in%1961:1990),]
Yemen_N$ISO <- "YEM"
Yemen_S <- warfare[which(warfare$country=="Yemen South" & warfare$year%in%1961:1990),]
Yemen_S$ISO <- "YEM"
Yemen_comb <-merge(Yemen_N, Yemen_S, all= T)
Yemen_comb <-Yemen_comb %>% group_by(year) %>% dplyr::summarise(actotal=sum(actotal))
Yemen_comb$ISO<-"YEM"

#combine North and South Vietnam
Viet_N <- warfare[which(warfare$country=="Vietnam North" & warfare$year%in%1961:1975),]
Viet_N$ISO <- "VNM"
Viet_S <- warfare[which(warfare$country=="Vietnam South" & warfare$year%in%1961:1975),]
Viet_S$ISO <- "VNM"
Viet_comb <-merge(Viet_N, Viet_S, all= T)
Viet_comb <-Viet_comb %>% group_by(year) %>% dplyr::summarise(actotal=sum(actotal))
Viet_comb$ISO<-"VNM"

#Czechoslovakia
#prager frühling 1968 in territory of Czechoslovakia, which does not exist anymore. 
#-> I remove the conflict as this period is not assessed for that region

#remove formaly devided countries and Czechoslovakia
warfare<-rbind(warfare[-which(warfare$country%in% c("Yemen North", "Yemen South", "Vietnam North","Vietnam South","Czechoslovakia")),])

#remove country col (so that dfs merge)
warfare$country<-NULL

#add aggregate countries instead
warfare<-rbind(warfare,Viet_comb,Yemen_comb)

# only take target country
warfare<-warfare[warfare$ISO %in% target_country$ISO,]

#Assign time Periods in 10 year intervals
warfare$timePeriod=0
warfare[warfare$year%in%c(1961:1970),"timePeriod"] = 1961
warfare[warfare$year%in%c(1971:1980),"timePeriod"] = 1971
warfare[warfare$year%in%c(1981:1990),"timePeriod"] = 1981
warfare[warfare$year%in%c(1991:2000),"timePeriod"] = 1991
warfare[warfare$year%in%c(2001:2010),"timePeriod"] = 2001

#calculate mean per decade
warfare<-warfare%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise("actotal"= mean(`actotal`))

names(warfare)[names(warfare) == 'year'] <- 'Year'


#####final variable data together#####
final_data<-fertilizer_mean%>%
  full_join(irrigation_mean)%>%
  full_join(warfare)%>%
  full_join(gdp)%>%
  full_join(agriculture_mean)%>%
  full_join(livestock_mean)
#no conflicts whenever no reported
final_data$actotal[is.na(final_data$actotal)]<-0

#remove where not all information is available
n_distinct(final_data$ISO) #94
final_data<-na.omit(final_data)
n_distinct(final_data$ISO) 


write.csv(final_data,"data/data_for_analysis/variables.csv")


####ALL DATA TOGETHER####
#### LOAD DATA ####
diversity <- read_csv("data/data_for_analysis/diversity.csv")
diversity$X1<-NULL
fulfilled_nutr <- read_csv("data/data_for_analysis/fulfilled_nutr.csv")
fulfilled_nutr$X1<-NULL
variables <- read_csv("data/data_for_analysis/variables.csv")
variables$X1<-NULL
climate<- read_csv("data/data_for_analysis/egli_climate_national.csv")
target_country <- read_csv("data/target_country.csv")
country_info<- read_csv("data/spatial/UNSD - Methodology.csv")
selfsuf_food_basket<-read_csv("data/data_for_analysis/selfsuf_food_basket.csv")
selfsuf_food_basket$X1<-NULL
# sd_fulfilled_nutr<-read_csv("data/data_for_analysis/fulfilled_nutr_sd.csv")
# sd_fulfilled_nutr$X1<-NULL

# #Not really used in analysis 
# self_suffiency<-read_csv("data/data_for_analysis/fulfilled_selfsuffiency.csv")
# self_suffiency$X1<-NULL
# self_suf_2<-read_csv("data/data_for_analysis/share_prod_in_fulfilled_sup_ISO.csv")
# self_suf_2$X1<-NULL


#### country information ####
#####country grouping####
#grouping with Groupings the UN uses for SDG's
final_ISO<-read_csv("data/final_ISO.csv")
final_ISO$X1<-NULL
country_info<- read_csv("data/spatial/UNSD - Methodology.csv")

#only those countries which will be assessed
country_info<-country_info%>%
  filter(`ISO-alpha3 Code`%in% target_country$ISO)%>%
  dplyr::select(c(`ISO-alpha3 Code`, `Developed / Developing Countries`, `Sub-region Name`)) #select cols

#country grouping; regional groups: https://unstats.un.org/sdgs/indicators/regional-groups
#assigning regions in extra col
country_info$Region1<-ifelse(country_info$`Sub-region Name`=='Sub-Saharan Africa', "Sub_Saharan_Africa", "")
country_info$Region2<-ifelse(country_info$`Sub-region Name`%in% c("Northern Africa", "Western Asia"), "Northern_Africa_and_Western_Asia", "")
country_info$Region3<-ifelse(country_info$`Sub-region Name`%in% c("Central Asia", "Southern Asia"), "Central_and_Southern_Asia", "")
country_info$Region4<-ifelse(country_info$`Sub-region Name`%in% c("Eastern Asia", "South-eastern Asia"), "Eastern_and_South_Eastern_Asia", "")
country_info$Region5<-ifelse(country_info$`Sub-region Name` == "Latin America and the Caribbean", "Latin_America_and_the_Caribean", "")
country_info$Region6<-ifelse(country_info$`Sub-region Name` == "Oceania", "Oceania", "")
country_info$Region7<-ifelse(country_info$`Sub-region Name` == "Australia and New Zealand", "Australia_and_New_Zealand", "")
country_info$Region8<-ifelse(country_info$`Sub-region Name` %in% c("Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", "Northern America"), "Europe_and_Northern_America", "")

#to one col
country_info$Region<-with(country_info, paste0(Region1, Region2, Region3 , Region4, Region5, Region6, Region7, Region8))
country_info<-country_info%>%dplyr::select(!4:11)

#rename for later join
names(country_info)[names(country_info) == 'ISO-alpha3 Code'] <- 'ISO'

#### Preparation for Analysis####
#climate: decided for sd, to measure climatic stability
# only take target country
names(climate)[names(climate) == 'Level'] <- 'ISO'
climate<-climate[climate$ISO %in% target_country$ISO,]

# only keep target year
climate <- climate[which(climate$Year%in%1961:2010),]

#Assign time Periods in 10 year intervals
climate$timePeriod=0
climate[climate$Year%in%c(1961:1970),"timePeriod"] = 1961
climate[climate$Year%in%c(1971:1980),"timePeriod"] = 1971
climate[climate$Year%in%c(1981:1990),"timePeriod"] = 1981
climate[climate$Year%in%c(1991:2000),"timePeriod"] = 1991
climate[climate$Year%in%c(2001:2010),"timePeriod"] = 2001

#calculate mean per decade
sd_Temp<-climate%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise("sd_Temp"= mean(`sdTemp`))

sd_Prec<-climate%>%
  group_by(ISO)%>%
  group_by(timePeriod, .add=T)%>%
  dplyr::summarise("sd_Prec"= mean(`sdPrec`))


#adjust naming selfsuf_food_basket
colnames(selfsuf_food_basket) <- paste0( "self_suf_food_basket_", colnames(selfsuf_food_basket))
names(selfsuf_food_basket)[names(selfsuf_food_basket) == 'self_suf_food_basket_timePeriod'] <- 'timePeriod'
names(selfsuf_food_basket)[names(selfsuf_food_basket) == 'self_suf_food_basket_ISO'] <- 'ISO'

##### one final timePeriod/country selection####
dat<-selfsuf_food_basket%>%
  full_join(fulfilled_nutr)%>%
  full_join(variables)%>%
  full_join(sd_Prec)%>%
  full_join(sd_Temp)%>%
  full_join(diversity)%>%
  #full_join(self_suffiency)%>%
  #full_join(self_suf_2)%>%
  full_join(country_info)
  #full_join(sd_fulfilled_nutr)

n_distinct(dat$ISO) #94
dat$actotal[is.na(dat$actotal)] <- 0
dat<-na.omit(dat)
n_distinct(dat$ISO) #65

#just those countrys where time series is complete!
distinct_countries<-dat%>%
  group_by(ISO, .add= T)%>%
  dplyr::summarise(rowCount= n()) #count entries per country
distinct_countries<-filter(distinct_countries,distinct_countries$rowCount==5) #only take, when time series complete
countrycode(distinct_countries$ISO, "iso3c", "country.name") #shows which countries 
n_distinct(dat$ISO) #65

dat<-dat[dat$ISO %in% distinct_countries$ISO,]
n_distinct(dat$ISO) #57
#write.csv(distinct_countries$ISO, "data/final_ISO.csv") # save final data selection!

names(dat)[names(dat) == 'mean_invSimp_D_prod'] <- 'Simp_Div'


#### FULL BASKET / NUTRITIOUS ADEQUACY ####
# Criteria: demand of the lowest fulfilliation counts- as it shows which demographic percentage is fully nourished by supply
# Accounts for: High supply for one nutrient does not substitute other requirements

col_ful_nutr<-colnames(fulfilled_nutr)[-c(1:2)]  # extract colnames all but timePeriod and ISO 
dat$full_basket<-apply(dat[,names(dat)[names(dat) %in% col_ful_nutr]], 1, FUN= min)


# col_ful_nutr_sd<-colnames(sd_fulfilled_nutr)[-c(1:2)]  # extract colnames
# dat$sd_full_basket<-apply(dat[,names(dat)[names(dat) %in% col_ful_nutr_sd]], 1, FUN= "mean" )


#### SELF SUFFICIENT FOOD BASKET ####
# Criteria: demand of the lowest fulfilliation (of nutrients) counts- as it shows which demographic percentage is depependent on trade over all nutrients
# Accounts for: High supply for one nutrient does not substitute other requirements


col_selfsuf_bas<-colnames(selfsuf_food_basket)[-c(1:2)]  # extract colnames
dat$selfsuf_food_basket<-apply(dat[,names(dat)[names(dat) %in% col_selfsuf_bas]], 1, FUN= min)


# #### SELF SUFFIENCY #####
# dat$full_sufficiency<-apply(dat[,19:26], 1, FUN= min)
# 
# pdf("Plots/boxplot_SelfSuffiency_with_time_Region.pdf")
# ggplot(data = dat, aes(y = full_sufficiency, x = timePeriod, group= timePeriod, colour = Region))+ # structure of the graph
#   geom_boxplot() +                                # add the boxplot
#   geom_jitter() +                                 # show the points distribution 
#   geom_hline(yintercept=100, col= "red")+
#   labs(x = '', y = "full_sufficiency [%]") +                # add labels to the axis
#   theme_classic()                                 # make it pretty
# dev.off()
# 
# #### SELF SUFFIENCY 2 ####
# 
# dat$self_suf_2<-apply(dat[,27:34], 1, FUN= min)
# pdf("Plots/boxplot_SelfSuffiency_2_with_time_Region.pdf")
# ggplot(data = dat, aes(y = self_suf_2, x = timePeriod, group= timePeriod, colour = Region))+ # structure of the graph
#   geom_boxplot() +                                # add the boxplot
#   geom_jitter() +                                 # show the points distribution 
#   geom_hline(yintercept=100, col= "red")+
#   labs(x = '', y = "share of national production in nutritional supply [%]") +                # add labels to the axis
#   theme_classic()                                 # make it pretty
# dev.off()

#### save csv####

write.csv(dat,"data/final_dataset.csv")

#write.csv(dat,"data/final_dataset_inkl_sd.csv")

