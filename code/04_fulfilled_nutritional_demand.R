#####Affiliance

#Annika Ertel
#Universität Leipzig/ Institut für Geographie
#Matrikelnummer: 3710313 

#SKRIPT 4: fulfilled nutritional demand 
#          % of achieved food basket that is self-sufficiently produced

####SETTING UP####
library(tidyverse)
library(readr)
rm(list = ls())
#### load data ####

#list of files in folder (needed for the loop)
nutr_sup<-list.files(path ="~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/nutrients/nutr_supply_ISO", pattern = "*.csv")
nutr_needs<-list.files(path="~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/nutrients/nutr_demand_ISO", pattern = "*.csv")

#take only those countrys available for both datasets
index_nutr_sup<-str_extract(nutr_sup, "[A-Z]{3}") #extract just ISO name
index_nutr_needs<-str_extract(nutr_needs, "[A-Z]{3}") #extract just ISO name
intersect<-intersect(index_nutr_needs, index_nutr_sup) # gives only matching patterns (ISOs)!
nutr_sup<-nutr_sup[index_nutr_sup %in% intersect] 
nutr_needs<-nutr_needs[index_nutr_needs %in% intersect]


#####fulfilled nutritional demand#####
#load data in loop
for (i in 1:length(nutr_sup)){
  
  nutr_sup_ISO_orig<-read_csv(paste0("data/nutrients/nutr_supply_ISO/", nutr_sup[69]), col_names = T) 
  nutr_sup_ISO_orig$X1<-NULL
  nutr_needs_ISO_orig<-read_csv(paste0("data/nutrients/nutr_demand_ISO/", nutr_needs[69]), col_names = T)
  nutr_needs_ISO_orig$X1<-NULL
  
  #Select target years
  nutr_sup_ISO_orig <-nutr_sup_ISO_orig[which(nutr_sup_ISO_orig$Year%in%1961:2010),]
  nutr_needs_ISO_orig <-nutr_needs_ISO_orig[which(nutr_needs_ISO_orig$Year%in%1961:2010),]
  
  #duplicates to minimise loop conflicts
  nutr_sup_ISO<-nutr_sup_ISO_orig
  nutr_needs_ISO<-nutr_needs_ISO_orig
  
  #extra df just with supply
  nutr_sup_ISO<-subset(nutr_sup_ISO, select= c("Area", "Year", "nutrient", "Total_Supply"))

  
  #nutr_sup from long to wide
  nutr_sup_ISO<-nutr_sup_ISO%>%
    group_by(Year)%>%
    pivot_wider(names_from = "nutrient", values_from = "Total_Supply")
  
  #match up rows 
  nutr_sup_ISO<-nutr_sup_ISO%>%
    group_by(Year,Area)%>%
    summarize_all(sum, na.rm=T)
  
  #Convert MJ -> Kcal -> 1 Megajoules = 238.85 Kilocalories
  nutr_needs_ISO$Energy_MJ<-nutr_needs_ISO$Energy_MJ*238.85
  names(nutr_needs_ISO)[names(nutr_needs_ISO)=="Energy_MJ"]<- "Energy_kcal" #adjust unitname
  
  #Exclude Mg from nutr_needs df. calculated by mistake
  nutr_needs_ISO$Mg_mg<-NULL
  
  #Adjust colnames (need the same name for match)
  names(nutr_sup_ISO)<-gsub("Total_", "", names(nutr_sup_ISO))
  names(nutr_sup_ISO)<-gsub("ug", "μg", names(nutr_sup_ISO))    #adjust unitname
  names(nutr_needs_ISO)<-gsub("_DFE", "", names(nutr_needs_ISO))    #Folate-> is usda=DFE? 
  names(nutr_sup_ISO)<-gsub("Vit_A_μg", "Vit_A_μg_RE", names(nutr_sup_ISO))     #Vitamin A both measured in RE
  names(nutr_sup_ISO)[names(nutr_sup_ISO)=="Area"]<- "ISO"      #adjust name for country
  
  #safe ISO for later
  ISO<-nutr_sup_ISO$ISO
  
  #fulfilled demand per nutrient
  Energy<-nutr_sup_ISO$Energy_kcal[nutr_sup_ISO$Year%in%1961:2010]*100/nutr_needs_ISO$Energy_kcal[nutr_needs_ISO$Year%in%1961:2010]
  Protein<-nutr_sup_ISO$Protein_g[nutr_sup_ISO$Year%in%1961:2010]*100/nutr_needs_ISO$Protein_g[nutr_needs_ISO$Year%in%1961:2010]
  Ca<-nutr_sup_ISO$Ca_mg[nutr_sup_ISO$Year%in%1961:2010]*100/nutr_needs_ISO$Ca_mg[nutr_needs_ISO$Year%in%1961:2010]
  Fe<-nutr_sup_ISO$Fe_mg[nutr_sup_ISO$Year%in%1961:2010]*100/nutr_needs_ISO$Fe_mg[nutr_needs_ISO$Year%in%1961:2010]
  Zn<-nutr_sup_ISO$Zn_mg[nutr_sup_ISO$Year%in%1961:2010]*100/nutr_needs_ISO$Zn_mg[nutr_needs_ISO$Year%in%1961:2010]
  Vit_B12<-nutr_sup_ISO$Vit_B12_μg[nutr_sup_ISO$Year%in%1961:2010]*100/nutr_needs_ISO$Vit_B12_μg[nutr_needs_ISO$Year%in%1961:2010]
  Folate<-nutr_sup_ISO$Folate_μg[nutr_sup_ISO$Year%in%1961:2010]*100/nutr_needs_ISO$Folate_μg[nutr_needs_ISO$Year%in%1961:2010]
  Vit_A<-nutr_sup_ISO$Vit_A_μg_RE[nutr_sup_ISO$Year%in%1961:2010]*100/nutr_needs_ISO$Vit_A_μg_RE[nutr_needs_ISO$Year%in%1961:2010]
  
  Year<-c(1961:2010)
  
  fulfilled_nutr_ISO<-cbind(ISO,Year,Energy,Protein,Ca,Fe,Zn, Vit_B12,Folate,Vit_A)
  
  write.csv(fulfilled_nutr_ISO, paste0("data/nutrients/fulfilled_nutr_ISO/fulfilled_", nutr_sup[i]))
}
  
##### one dataframe together fulfilled_nutr.csv #####
  setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/nutrients/fulfilled_nutr_ISO")
  #list of files in folder (needed for the loop)
  fulfilled_ISO<-list.files(path ="~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/nutrients/fulfilled_nutr_ISO", pattern = "*.csv")
  

  for (file in fulfilled_ISO){
    
    # if the merged dataset doesn't exist, create it
    if (!exists("fulfilled_nutr")){
      fulfilled_nutr <- read_csv(file)
    }
    
    # if the merged fulfilled_nutr does exist, append to it
    if (exists("fulfilled_nutr")){
      temp_fulfilled_nutr <-read_csv(file)
      fulfilled_nutr<-rbind(fulfilled_nutr, temp_fulfilled_nutr)
      rm(temp_fulfilled_nutr)
    }
    
  }
  
  #first file of fulfilled_ISO is doubled, remove duplicate
  fulfilled_nutr<-unique(fulfilled_nutr)
  
  #summarize per decade
  #Assign time Periods in 10 year intervals
  fulfilled_nutr$timePeriod=0
  fulfilled_nutr[fulfilled_nutr$Year%in%c(1961:1970),"timePeriod"] = 1961
  fulfilled_nutr[fulfilled_nutr$Year%in%c(1971:1980),"timePeriod"] = 1971
  fulfilled_nutr[fulfilled_nutr$Year%in%c(1981:1990),"timePeriod"] = 1981
  fulfilled_nutr[fulfilled_nutr$Year%in%c(1991:2000),"timePeriod"] = 1991
  fulfilled_nutr[fulfilled_nutr$Year%in%c(2001:2010),"timePeriod"] = 2001
  
  #remove year info for summarise
  fulfilled_nutr$Year<-NULL
  
  #calculate mean per decade
  mean_fulfilled_nutr<-fulfilled_nutr%>%
    group_by(timePeriod)%>%
    group_by(ISO, .add=T)%>%
    dplyr::summarise(across(everything(), list(mean)))

  #renaming
  names(mean_fulfilled_nutr) <- gsub(x = names(mean_fulfilled_nutr), pattern = "_1", replacement = "")  
  mean_fulfilled_nutr$X1<-NULL
  
  #write csv
  write.csv(mean_fulfilled_nutr, "~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/data_for_analysis/fulfilled_nutr.csv")
  
  # #calculate sd per decade
  # fulfilled_nutr$X1<-NULL #remove ID (no sd calc)
  # sd_fulfilled_nutr<-fulfilled_nutr%>%
  #   group_by(timePeriod)%>%
  #   group_by(ISO, .add=T)%>%
  #   summarise(across(everything(), list(sd)))
  # 
  
  #first scale sd_fulfilled_nutr to make mean sd later!
  #scale needs to be rowwise (i want comparison of nutrients!)
  # library(scrime)
  # sd_fulfilled_nutr_scaled<- rowScales(sd_fulfilled_nutr[,-c(1:2)])
  # 
  # 
  # 
  # #renaming
  # names(sd_fulfilled_nutr_scaled) <- gsub(x = names(sd_fulfilled_nutr_scaled), pattern = "_1", replacement = "_sd_scaled")  
  # sd_fulfilled_nutr_scaled$X1_sd_scaled<-NULL
  # 
  # 
  # #write csv
  # write.csv(sd_fulfilled_nutr, "~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/data_for_analysis/fulfilled_nutr_sd.csv")
  # #write.csv(sd_fulfilled_nutr_scaled, "~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/data_for_analysis/fulfilled_nutr_sd_scaled.csv", )
  # 
 
  ####self sufficiant food basket ####
  
 #"% of achieved food basket that is self-sufficiently produced"
  setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")
  
  final_ISO<-read.csv("data/final_ISO.csv") #final country selection (finally done later in 06_variables!)
  final_ISO$X<-NULL

  #take only the final final data selection! (finally done later in 06_variables!)

  nutr_sup<-paste0("nutr_sup_",final_ISO$x)  #build back name of file
  nutr_sup<-paste0(nutr_sup, ".csv")
      # Zeigt den total supply in einem land,so wie alle elemente der nutrients aus den fbs pro nutrient; Einheiten: MJ/mg/μg...
  fulfilled_nutr_sup<-paste0("fulfilled_nutr_sup_", final_ISO$x)      #build back name of file
  fulfilled_nutr_sup<-paste0(fulfilled_nutr_sup, ".csv")
  # Produkt aus beidem: wie gut wird der Bedarf gedeckt durch Supply? Einheit: %
  nutr_needs<-paste0("nutr_needs_", final_ISO$x)                      #build back name of file
  nutr_needs<-paste0(nutr_needs, "_pop.csv")
      # Zeigt die Nährstoffe die Gesammt pro Land benötigt werden; Einheiten: kcal/mg/μg...
  
  for (i in 1:length(nutr_sup)){
  #load data
    nutr_sup_ISO<-read_csv(paste0("data/nutrients/nutr_supply_ISO/", nutr_sup[48])) 
    nutr_sup_ISO$X1<-NULL
    nutr_needs_ISO<-read_csv(paste0("data/nutrients/nutr_demand_ISO/", nutr_needs[48]))
    nutr_needs_ISO$X1<-NULL
    fulfilled_nutr_sup_ISO<-read_csv(paste0("data/nutrients/fulfilled_nutr_ISO/", fulfilled_nutr_sup[48]))
    fulfilled_nutr_sup_ISO$X1<-NULL
    
  #Select target years
    nutr_sup_ISO <-nutr_sup_ISO[which(nutr_sup_ISO$Year%in%1961:2010),]
    nutr_needs_ISO <-nutr_needs_ISO[which(nutr_needs_ISO$Year%in%1961:2010),]
    fulfilled_nutr_sup_ISO<-fulfilled_nutr_sup_ISO[which(nutr_needs_ISO$Year%in%1961:2010),]
  
#Was möchte ich? 
  # ich möchte für Welthandel accounten:  Modelle mit Handel und  Modelle mit (hypothetischem) Ausbleiben des Außenhandels gegenüberstellen
  # Brauche eine Variable, welche das theoretische Ausbleiben des Außenhandels bewertet
  # % of achieved food basket that is self-sufficiently produced
    # dann drei mal drei modelle ausprobieren
        # Model 1-3: lm(full_basket ~ full_basket_without_trade + Asynchrony + Irrigation + N_use + warfare + precepitation_instability + temperature_instability + timePeriod, data = datCenter_trans)
        # Model 4-6: lm(full_basket_without_trade ~ Asynchrony + Irrigation + N_use + warfare + precepitation_instability + temperature_instability + timePeriod, data = datCenter_trans)
        # Model 7-9: lm(full_basket ~ Asynchrony + Irrigation + N_use + warfare + precepitation_instability + temperature_instability + timePeriod, data = datCenter_trans)
    
  
#### Nutritions available without trade per nutr #####
  # Nutritions available without trade per nutr
  nutr_sup_ISO$nutr_without_trade<-nutr_sup_ISO$Production-nutr_sup_ISO$Export-nutr_sup_ISO$Feed-nutr_sup_ISO$Seed-nutr_sup_ISO$Other     #-nutr_sup_ISO$Losses               
    
  #Select target years
  nutr_sup_ISO <-nutr_sup_ISO[which(nutr_sup_ISO$Year%in%1961:2010),]
  nutr_needs_ISO <- nutr_needs_ISO%>%filter(between(Year, 1961,2010))
  
  
  #from wide to long
  nutr_without_trade_ISO<-nutr_sup_ISO%>%
    dplyr::select(c("Area", "Year", "nutrient","nutr_without_trade"))%>%
    group_by(Year)%>%
    pivot_wider(names_from = "nutrient", values_from = "nutr_without_trade")
  
  #Convert MJ -> Kcal -> 1 Megajoules = 238.85 Kilocalories
  nutr_needs_ISO$Energy_MJ<-nutr_needs_ISO$Energy_MJ*238.85
  names(nutr_needs_ISO)[names(nutr_needs_ISO)=="Energy_MJ"]<- "Energy_kcal" #adjust unitname
  
  #Exclude Mg from nutr_needs df. calculated by mistake
  nutr_needs_ISO$Mg_mg<-NULL
  
  #save ISO for later
  ISO<-nutr_without_trade_ISO$Area
  
  #Percentage of pop nourished by nutritions available without trade  per nutrient
  Energy<-nutr_without_trade_ISO$Total_Energy_kcal*100/nutr_needs_ISO$Energy_kcal
  Protein<-nutr_without_trade_ISO$Total_Protein_g*100/nutr_needs_ISO$Protein_g
  Ca<- nutr_without_trade_ISO$Total_Ca_mg*100/nutr_needs_ISO$Ca_mg
  Fe<-nutr_without_trade_ISO$Total_Fe_mg*100/nutr_needs_ISO$Fe_mg
  Zn<-nutr_without_trade_ISO$Total_Zn_mg*100/nutr_needs_ISO$Zn_mg
  Vit_B12<-nutr_without_trade_ISO$Total_Vit_B12_ug*100/nutr_needs_ISO$Vit_B12_μg
  Folate<-nutr_without_trade_ISO$Total_Folate_ug*100/nutr_needs_ISO$Folate_μg_DFE
  Vit_A<-nutr_without_trade_ISO$Total_Vit_A_ug*100/nutr_needs_ISO$Vit_A_μg_RE
  
  #make dataframe
  full_nutr_without_trade_ISO <-cbind(Energy,Protein,Ca,Fe,Zn, Vit_B12,Folate,Vit_A)
  full_nutr_without_trade_ISO <- data.frame(full_nutr_without_trade_ISO)
  full_nutr_without_trade_ISO$Year<-c(1961:2010) #save timespan for cbind
  full_nutr_without_trade_ISO$ISO<-ISO
  
####self-sufficient food basket per nutrient ##### 
  #(mind small letters!><above)
  energy<-full_nutr_without_trade_ISO$Energy/fulfilled_nutr_sup_ISO$Energy
  protein<-full_nutr_without_trade_ISO$Protein/fulfilled_nutr_sup_ISO$Protein
  ca<-full_nutr_without_trade_ISO$Ca/fulfilled_nutr_sup_ISO$Ca
  fe<-full_nutr_without_trade_ISO$Fe/fulfilled_nutr_sup_ISO$Fe
  zn<-full_nutr_without_trade_ISO$Zn/fulfilled_nutr_sup_ISO$Zn
  vit_B12<-full_nutr_without_trade_ISO$Vit_B12/fulfilled_nutr_sup_ISO$Vit_B12
  folate<-full_nutr_without_trade_ISO$Folate/fulfilled_nutr_sup_ISO$Folate
  vit_A<-full_nutr_without_trade_ISO$Vit_A/fulfilled_nutr_sup_ISO$Vit_A
  
  #make dataframe
  selfsuf_food_basket_nutr <-cbind(energy,protein,ca,fe,zn, vit_B12,folate,vit_A)
  selfsuf_food_basket_nutr <- data.frame(selfsuf_food_basket_nutr)
  selfsuf_food_basket_nutr$Year<-c(1961:2010) #save timespan for cbind
  selfsuf_food_basket_nutr$ISO<-ISO
  
  
  
#save csv
 write.csv(selfsuf_food_basket_nutr, paste0("data/nutrients/selfsuf_food_basket_nutr_ISO/selfsuf_", fulfilled_nutr_sup[i]))
  }    
 
  
  ##### one dataframe together selfsuf_foodbasket.csv#####
  rm(list = ls())
  setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/nutrients/selfsuf_food_basket_nutr_ISO")
  #list of files in folder (needed for the loop)
  selfsuf_ISO<-list.files(path ="~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/nutrients/selfsuf_food_basket_nutr_ISO", pattern = "*.csv")
  
  for (file in selfsuf_ISO){
    
    # if the merged dataset doesn't exist, create it
    if (!exists("selfsuf_food_basket")){
      selfsuf_food_basket <- read_csv(file)
    }
    
    # if the merged selfsuf_food_basket does exist, append to it
    if (exists("selfsuf_food_basket")){
      temp_selfsuf_food_basket <-read_csv(file)
      selfsuf_food_basket<-rbind(selfsuf_food_basket, temp_selfsuf_food_basket)
      rm(temp_selfsuf_food_basket)
    }
    
  }
  
  #first file of fulfilled_ISO is doubled, remove duplicate
  selfsuf_food_basket<-unique(selfsuf_food_basket)
  
  #summarize per decade
  #Assign time Periods in 10 year intervals
  selfsuf_food_basket$timePeriod=0
  selfsuf_food_basket[selfsuf_food_basket$Year%in%c(1961:1970),"timePeriod"] = 1961
  selfsuf_food_basket[selfsuf_food_basket$Year%in%c(1971:1980),"timePeriod"] = 1971
  selfsuf_food_basket[selfsuf_food_basket$Year%in%c(1981:1990),"timePeriod"] = 1981
  selfsuf_food_basket[selfsuf_food_basket$Year%in%c(1991:2000),"timePeriod"] = 1991
  selfsuf_food_basket[selfsuf_food_basket$Year%in%c(2001:2010),"timePeriod"] = 2001
  
  #remove year info for summarise
  selfsuf_food_basket$Year<-NULL
  
  #calculate mean per decade
  mean_selfsuf_food_basket<-selfsuf_food_basket%>%
    group_by(timePeriod)%>%
    group_by(ISO, .add=T)%>%
   dplyr:: summarise(across(everything(), list(mean)))
  
  #renaming
  names(mean_selfsuf_food_basket) <- gsub(x = names(mean_selfsuf_food_basket), pattern = "_1", replacement = "")  
  mean_selfsuf_food_basket$X1<-NULL
  
  #write csv
  write.csv(mean_selfsuf_food_basket, "~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/data_for_analysis/selfsuf_food_basket.csv")
  
