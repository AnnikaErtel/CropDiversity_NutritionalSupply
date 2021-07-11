#####Affiliance

#Annika Ertel
#Universität Leipzig/ Institut für Geographie
#Matrikelnummer: 3710313 

#SKRIPT 2: Nutritional Supply / 
#Self Suffiency Var (% of Pop nourished through production-exports) /  
#Saudia Arabia saved speacially!

####Setting up

setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")
rm(list=ls())

library(tidyverse)
library(readxl)

#split data & country selection#### SEE skript:"01_country_selection_&_split.R"

#####load data####
fbs <-list.files(path="~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/FAO_fbs/fbs_ISO/",  pattern = "*.csv")   #list of files (needed for loop)
USDA_Nutrients1<-read_xlsx("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability/data/USDA_Nutrients.xlsx", sheet=1) #Aggregated Nutrients
#USDA_Nutrients2<-read_xlsx("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability/data/USDA_Nutrients.xlsx", sheet=2) #Composite Items

#leerer Vektor, wird im loop gefüllt, Test ob alle elements in allen Ländern vorhanden sind
number_columns <- c()

#for(i in 1:length(fbs)){
  #load csv from list of files
  fbs_ISO <- read.csv(paste0("data/FAO_fbs/fbs_ISO/", fbs[69]))
  
  #keep necessary Elements and cols
  element<-c("Feed","Seed","Production","Other uses","Import Quantity","Export Quantity","Stock Variation") # without "Losses": no account for loss due to transport and storage (no need to: we can't assess distribution anyway) refuse fraction is assessed in nutr. supply later 
  fbs_ISO<-filter(fbs_ISO, fbs_ISO$Element %in% element)
  fbs_ISO<-fbs_ISO[,c("ISO", "Area","Item", "Item.Code", "Element", "Year", "Unit", "Value")]
  
  #join tables
  nutr_ISO<-fbs_ISO%>%
    left_join(USDA_Nutrients1, by = c("Item.Code" = "FAO Code"))
  
  # Items Wood et al sorted out
  NA_nutr_ISO<-nutr_ISO%>%
    filter(is.na(nutr_ISO$item))
  item_exclude<-unique(NA_nutr_ISO$Item.Code)
  
  #exclude beverages, spices (incl. all sugar stuff) and some others
  #selection following wood et al
  nutr_ISO<-filter(nutr_ISO,!nutr_ISO$Item.Code %in% item_exclude)
  
  
  ####Item*refuse fraction####
  
  #convert all values to "in 100g", so that the nutritional values fit
  #10 000 000g*100g= 1 000 000 000g --> 1 000t
  unique(nutr_ISO$Unit) #check if all values have 1 000t
  nutr_ISO$Value<-nutr_ISO$Value*10000000
  nutr_ISO$Unit<-"in 100g"
  
  #Item*refuse fraction
  nutr_ISO$`Refuse fraction`[nutr_ISO$`Refuse fraction`== 0]<- NA
  nutr_ISO$Value<- ifelse(is.na(nutr_ISO$`Refuse fraction`), nutr_ISO$Value*1, nutr_ISO$`Refuse fraction`*nutr_ISO$Value)
  
  ####edible amount*nutrients#### 
  nutr_ISO$Total_Energy_kcal<-nutr_ISO$Value*nutr_ISO$`Energy (kcal/100g)`
  nutr_ISO$Total_Protein_g<-nutr_ISO$Value*nutr_ISO$`Protein (g/100g)`
  nutr_ISO$Total_Lipid_g<-nutr_ISO$Value*nutr_ISO$`Total Lipid (g/100g)`
  nutr_ISO$Total_Carbs_g<-nutr_ISO$Value*nutr_ISO$`Carbs (g/100g)`
  nutr_ISO$Total_Fiber_g<-nutr_ISO$Value*nutr_ISO$`Fiber (g/100g)`
  nutr_ISO$Total_Sugars_g<-nutr_ISO$Value*nutr_ISO$`Sugars (g/100g)`
  nutr_ISO$Total_Ca_mg<-nutr_ISO$Value*nutr_ISO$`Ca (mg/100g)`
  nutr_ISO$Total_Fe_mg<-nutr_ISO$Value*nutr_ISO$`Fe (mg/100g)`
  nutr_ISO$Total_Mg_mg<-nutr_ISO$Value*nutr_ISO$`Mg (mg/100g)`
  nutr_ISO$Total_P_mg<-nutr_ISO$Value*nutr_ISO$`P (mg/100g)`
  nutr_ISO$Total_K_mg<-nutr_ISO$Value*nutr_ISO$`K (mg/100g)`
  nutr_ISO$Total_Na_mg<-nutr_ISO$Value*nutr_ISO$`Na (mg/100g)`
  nutr_ISO$Total_Zn_mg<-nutr_ISO$Value*nutr_ISO$`Zn (mg/100g)`
  nutr_ISO$Total_Vit_C_mg<-nutr_ISO$Value*nutr_ISO$`Vitamin C (mg/100g)`
  nutr_ISO$Total_Thiamin_mg<-nutr_ISO$Value*nutr_ISO$`Thiamin (mg/100g)`
  nutr_ISO$Total_Riboflavin_mg<-nutr_ISO$Value*nutr_ISO$`Riboflavin (mg/100g)`
  nutr_ISO$Total_Niacin_mg<-nutr_ISO$Value*nutr_ISO$`Niacin (mg/100g)`
  nutr_ISO$Total_Vit_B6_mg<-nutr_ISO$Value*nutr_ISO$`Vitamin B-6 (mg/100g)`
  nutr_ISO$Total_Folate_ug<-nutr_ISO$Value*nutr_ISO$`Folate (ug/100g)`
  nutr_ISO$Total_Vit_B12_ug<-nutr_ISO$Value*nutr_ISO$`Vitamin B-12 (ug/100g)`
  nutr_ISO$Total_Vit_A_ug<-nutr_ISO$Value*nutr_ISO$`Vitamin A, RAE (ug/100g)`
  nutr_ISO$Total_Vit_E_ug<-nutr_ISO$Value*nutr_ISO$`Vitamin E (mg/100g)`
  nutr_ISO$Total_Vit_D_ug<-nutr_ISO$Value*nutr_ISO$`Vitamin D (ug/100g)`
  
  
  #clear data
  nutr_ISO<-nutr_ISO[,c("ISO", "Area", "Element", "Year", "item", 
                        "Total_Energy_kcal","Total_Protein_g","Total_Lipid_g",
                        "Total_Carbs_g","Total_Fiber_g","Total_Sugars_g",
                        "Total_Sugars_g", "Total_Ca_mg", "Total_Fe_mg",
                        "Total_Mg_mg", "Total_P_mg", "Total_K_mg", "Total_Na_mg", 
                        "Total_Zn_mg", "Total_Vit_C_mg", "Total_Thiamin_mg",
                        "Total_Riboflavin_mg", "Total_Niacin_mg", "Total_Vit_B6_mg",
                        "Total_Folate_ug", "Total_Vit_B12_ug", "Total_Vit_A_ug", 
                        "Total_Vit_E_ug", "Total_Vit_D_ug")]
  
  
  ####nutritional supply####
  #nutr_ISO<-read_csv("data/nutr_ISO.csv")
  
  #select important nutrients
  nutr_ISO<-nutr_ISO[,c("ISO", "Area", "Element",
                        "Year", "item", "Total_Energy_kcal", 
                        "Total_Protein_g", "Total_Zn_mg","Total_Ca_mg", 
                        "Total_Fe_mg", "Total_Vit_B12_ug", "Total_Folate_ug", 
                        "Total_Vit_A_ug")]
  
  #Element from long to wide for summing up rowwise 
  nutr_ISO<-nutr_ISO%>%
    pivot_wider(names_from =Element, values_from = Total_Energy_kcal:Total_Vit_A_ug)
  
  #number_columns[i] <- length(nutr_ISO) - 3
  
  #Total Nutrition per Element
  level<-unique(nutr_ISO$ISO)
  
  nutr_ISO<-nutr_ISO%>%
    replace(is.na(.),0)%>%
    group_by(Year)%>%
  dplyr:: summarise(across(starts_with("Total"), sum))%>%
    add_column(.before=1, Area =level)
  
  #Nutrient form wide to long for calculating rowwise
  nutr_ISO<-nutr_ISO%>%
    pivot_longer(cols = -c(Area,Year),
                 names_to = c("nutrient",".value"),
                 values_to = "element per nutrient",
                 names_pattern = "(.*)_(\\w+)")
  
  #save as .csv for example with Saudia Arabia
  #write.csv(nutr_ISO, "nutr_sup_SAU_all_elements.csv")
  
  ######Nutritional supply######
  nutr_ISO$Total_Supply<-nutr_ISO$Production+nutr_ISO$Import+nutr_ISO$Stock-nutr_ISO$Export-nutr_ISO$Feed-nutr_ISO$Seed-nutr_ISO$Other #- nutr_ISO$Losses #no account for losses: we do not know how nutrients are distributed! and refuse fraction is in wood et. al vorhanden!
  

  
  #save as .csv
   write.csv(nutr_ISO, paste0("data/nutrients/nutr_supply_ISO/nutr_sup_", fbs[i]))
}

# #test if all countrys have all elements       #djibuti: has no seed information
# number_columns <- data.frame(number_columns)
# rownames(number_columns) <- fbs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Explore trade
rm(list = ls())
setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/nutrients/nutr_supply_ISO")
#list of files in folder (needed for the loop)
fbs_ISO<-list.files(path ="~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/nutrients/nutr_supply_ISO", pattern = "*.csv")

for (file in fbs_ISO){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("fbs")){
    fbs <- read_csv(file)
  }
  
  # if the merged fbs does exist, append to it
  if (exists("fbs")){
    temp_fbs <-read_csv(file)
    fbs<-rbind(fbs, temp_fbs)
    rm(temp_fbs)
  }
  
}

#first file of fulfilled_ISO is doubled, remove duplicate
fbs<-unique(fbs)

#summarize per decade
#Assign time Periods in 10 year intervals
fbs$timePeriod=0
fbs[fbs$Year%in%c(1961:1970),"timePeriod"] = 1961
fbs[fbs$Year%in%c(1971:1980),"timePeriod"] = 1971
fbs[fbs$Year%in%c(1981:1990),"timePeriod"] = 1981
fbs[fbs$Year%in%c(1991:2000),"timePeriod"] = 1991
fbs[fbs$Year%in%c(2001:2010),"timePeriod"] = 2001

#remove year info for summarise
fbs$Year<-NULL

#calculate mean per decade
mean_fbs<-fbs%>%
  group_by(timePeriod)%>%
  group_by(Area, .add=T)%>%
  dplyr:: summarise(across(everything(), list(mean)))


mean_fbs$ratio<-mean_fbs$Import_1/mean_fbs$Production_1

