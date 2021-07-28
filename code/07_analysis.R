#####Affiliance

#Annika Ertel
#Universität Leipzig/ Institut für Geographie
#Matrikelnummer: 3710313 

#SKRIPT 07: Analysis and Plotting

####Setting up#####

setwd("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new")
rm(list=ls())

library(readr)
library(lme4)
library(lmtest)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(broom) #for plotting worldmap
library(formattable)
library(magrittr)
library(multipanelfigure)
library(corrplot)
library(colorRamps)
library(countrycode)
library(tidyverse)
library(rgdal)
library(jtools)

####Load data####
dat <- read_csv("data/final_dataset.csv")
dat$X1<-NULL


####Transform Variables####
#transform colnames (sometimes cause problems, sometimes because of better naming)
names(dat)[names(dat) ==  "N_use/croparea_in_kg/ha"] <- 'N_use'
names(dat)[names(dat) == "Land_area_equ._for-Irrigation_%"] <- 'irrigation'
names(dat)[names(dat) == "actotal"] <- 'warfare'
names(dat)[names(dat) == "sd_Prec"] <- 'precepitation instability'
names(dat)[names(dat) == "sd_Temp"] <- 'temperature instability'
names(dat)[names(dat) == "selfsuf_food_basket"] <- "self-sufficient full basket" #changed name from selfsuf food basket to selfsuf full basket. See Thesis: self suf food basket is per nutrient- self suf full basket is the minimum of all  
names(dat)[names(dat) == "gdp_per_capita_USD"] <- "GDP per capita (USD)"
names(dat)[names(dat) == "Livestock_LSU" ] <- "livestock"
names(dat)[names(dat) == "agriculture_area_ha"] <- "agricultural area"
names(dat)[names(dat) == "timePeriod"] <-"time"
names(dat)[names(dat) == "Simp_Div"] <-"inverse Simpson diversity"
names(dat)[names(dat) == "Richness"] <-"richness"
names(dat)[names(dat) == "Asynchrony"] <-"asynchrony"

#dataset with fixed names
dat_fix_names<-dat
names(dat_fix_names)[names(dat_fix_names) == "self-sufficient full basket"] <- "self_sufficient_full_basket"
names(dat_fix_names)[names(dat_fix_names) == "GDP per capita (USD)"] <- "GDP per capita"
names(dat_fix_names) <-str_replace_all(names(dat_fix_names), c(" " = "." , "," = "" ))

####Descriptive####

#get how many countries per region
dat%>%group_by(Region)%>%count()%>%mutate(n_countries= n/5)

####___ worldmap of regions ####

# Load Data
countries     <-  readOGR(dsn="data/spatial/countries_global.shp")

# Unique data
dat_worldmap           <- dat[,c("ISO", "Region")]
dat_worldmap           <- unique(dat_worldmap)

countries$ISO <- countrycode(countries$Area, "country.name", "iso3c")
countries@data <- full_join(countries@data, dat_worldmap, by = "ISO")

countries$colour  <- ifelse(countries$Region == "Northern_Africa_and_Western_Asia", "#A58AFF",
                            ifelse(countries$Region == "Latin_America_and_the_Caribean", "#00B6EB",
                                   ifelse(countries$Region == "Australia_and_New_Zealand", "#F8766D",
                                          ifelse(countries$Region == "Europe_and_Northern_America", "#00C094",
                                                 ifelse(countries$Region == "Central_and_Southern_Asia", "#C49A00",
                                                        ifelse(countries$Region == "Eastern_and_South_Eastern_Asia", "#53B400",
                                                               ifelse(countries$Region == "Sub_Saharan_Africa", "#FB61D7", "#FFFFFF")))))))

#jpeg("Plots/worldmap_of_regions.jpg", units = "in", width= 8.5, height= 5, res= 200)
plot(countries, col = countries$colour)
legend("bottomleft", legend = c("Australia and New Zealand (n=1)",
                                "Central and Southern Asia (n=6)",
                                "Eastern and South Eastern Asia (n=8)",
                                "Europe and Northern America (n=12)",
                                "Latin America and the Caribean (n=14)", 
                                "Northern Africa and Western Asia (n=7)",
                                "Sub Saharan Africa (n=10)"),
       col = countries$colour,
       fill =  c("#F8766D","#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7"),
       cex = 0.65)
#dev.off()

#increased full basket globally? # 31% between 1961 and 2010
full_basket61<-filter(dat, time=="1961")
full_basket01<-filter(dat, time=="2001")

glob_increase<-mean(full_basket01$full_basket)-mean(full_basket61$full_basket)


####Data Exploration####
####___Histogram full basket####


#in modeling i transformed full basket to log to normalize
hist(dat_trans$full_basket,
     xlab="log(full basket)",
     main = NULL,
     breaks = 12
)

#not log transformed-> original
hist(dat$full_basket,
     xlab="full basket [%]",
     main = NULL,
     breaks = 12,
)

####___CorrPlot####
#select variables to correlate
dat_var<-dat%>% dplyr::select('inverse Simpson diversity',
                              richness,
                              asynchrony,
                              'self-sufficient full basket',
                              'GDP per capita (USD)',
                              irrigation,
                              N_use,
                              'precepitation instability',
                              'temperature instability',
                              warfare,
                              'livestock',
                              'agricultural area',
                              'time')


correl<-cor(dat_var)

#corrplot first impression
res1 <- cor.mtest(dat_var, conf.level = .95)
res2 <- cor.mtest(dat_var, conf.level = .99)


#Corrplot (Fig. 2) showing correlation strength!
#png("Plots/corrplot_r.png", width = 800, height = 800)
corrplot(correl, type = "upper", order = "alphabet",
         tl.col = "black", tl.srt = 45,  p.mat = res1$p,insig = "label_sig", pch.cex= 1)
#dev.off()

#Corrplot (Appendix) showing R2 (how much of the variation of one variable is explained by the "model")
col_r2 = hcl.colors(10, "viridis", rev = TRUE)
#png("Plots/corrplot_r2.png", width = 800, height = 800)
corrplot(correl*correl, type = "upper", order = "alphabet", col= col_r2, is.corr = F,
         tl.col = "black", tl.srt = 45,  pch.cex= 1)
#dev.off()

####___Explore distribution####

#minimise df
dat_fix_names=with(dat_fix_names, data.frame(ISO,
                         time,
                         full_basket,
                         richness,
                         inverse.Simpson.diversity,
                         asynchrony,
                         self_sufficient.food.basket, 
                         GDP.per.capita,
                         N_use,  
                         irrigation, 
                         precepitation.instability,
                         temperature.instability,
                         warfare,
                         livestock,
                         agricultural.area,
                         Region
))

dat_sin<-dat_fix_names[,-c(1,2)] # just the assessed vars

#loop to build plotlist
plot_dat <- lapply(names(dat_sin), function(var_x){
  p <- 
    ggplot(dat_fix_names) +
    aes_string(var_x)
  
  if(is.numeric(dat_sin[[var_x]])) {
    p <- p + geom_density()
    
  } else {
    p <- p + geom_bar()
  } 
})                  

#plot distribution
cowplot::plot_grid(plotlist = plot_dat) 


####___Explore selfsuf food basket####
lm_selfsuf<-lm(full_basket~self_sufficient_full_basket, data = dat_fix_names)
plot(dat_fix_names$full_basket, dat_fix_names$self_sufficient_full_basket)
abline(lm(scale(dat_fix_names$full_basket)~scale(dat_fix_names$self_sufficient_full_basket), data= dat_fix_names))
summary(lm_selfsuf)
# has significant correlation, but variation remains
# By not using absolute values, but using the share instead it is no circular
#   argument as then selfsuf full basket is not limited to the fact how big full basket is

####___explore irrigation~full Basket####
ggplot(dat) +
  aes(x = irrigation, y = full_basket, colour = Region) +
  geom_point(shape = "circle", size = 2.5) +
  scale_x_log10()+
  geom_smooth(method= lm , se = F)+
  scale_color_hue(direction = 1) +
  theme_minimal()

#model irrig global
model_irrig<-dat %>% 
  do(model = lm(full_basket ~ irrigation, data = .))%>%
  transmute(RegionCoef = map (model, tidy)) %>%
  unnest(RegionCoef)

#lm full_basket~irrigation per region!
models_irrigation<-dat %>% 
  group_by(Region) %>% 
  do(model = lm(full_basket ~ irrigation, data = .))%>%
  ungroup %>%
  transmute(Region, RegionCoef = map (model, tidy)) %>%
  unnest(RegionCoef)

##make datatable 
table_irrig_reg<-models_irrigation%>%
  add_row(model_irrig)%>%
  filter(term != "(Intercept)")%>%
  select(c(Region, estimate, p.value))

#set "World" as did not have a region name
table_irrig_reg$Region <-as.character(table_irrig_reg$Region) 
table_irrig_reg$Region[8]<-"World"

formattable(table_irrig_reg) #make table

#irrigation effect on full basket for most regions despite:
#almost no effect in Latin America and the Caribean and Australia


####___Explore trade ####
#rm(list = ls())
#!!!!!load data from nutr supply as seperate elements are interesting!!!!!

#just final country selection
final_ISO<-read_csv("data/final_ISO.csv")
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
fbs<-filter(fbs, Area %in% final_ISO$x)
#summarize per decade
#Assign time Periods in 10 year intervals
fbs$timePeriod=0
fbs[fbs$Year%in%c(1961:1970),"timePeriod"] = 1961
fbs[fbs$Year%in%c(1971:1980),"timePeriod"] = 1971
fbs[fbs$Year%in%c(1981:1990),"timePeriod"] = 1981
fbs[fbs$Year%in%c(1991:2000),"timePeriod"] = 1991
fbs[fbs$Year%in%c(2001:2010),"timePeriod"] = 2001
#all yearPeriod showing 0 are not in framework time!
fbs<-filter(fbs, timePeriod!=0)
#remove year info for summarise
fbs$Year<-NULL
#calculate mean per decade
mean_fbs<-fbs%>%
  group_by(timePeriod)%>%
  group_by(Area, .add=T)%>%
  dplyr:: summarise(across(everything(), list(mean)))


#get ratio of Imports to Production
mean_fbs$ratio_imp_prod<-mean_fbs$Import_1/mean_fbs$Production_1
#low values: low Imports compared to Production: ARG; NPL; AUS; TUR 1961-1990
#high values: high Imports compared to Production: JOR; SAU; DZA (Algeria) 1961-1990

#get ratio of feed to export: 
#because of bias: As the elements "feed" and "seed" include 
#imported and locally produced nutrients the self-sufficient full basket tends to underestimate
#the actual degree of self sufficiency especially for feed importing countries, 
#thus explaining negative values. 

mean_fbs$ratio_feed_Imp<-mean_fbs$Feed_1/mean_fbs$Import_1
#low values: low conversion to feed compared to Imports: LKA; SEN; DZA 1961-2010
#high values: high conversion to feed compared to Imports:ARG; NPL; AUS; TUR 1961-1980

#-> Does this correlate with low self_suf_foodbasket values? 
plot(mean_fbs$ratio_feed_Imp~dat$'self-sufficient full basket')
summary(lm(mean_fbs$ratio_feed_Imp~dat$'self-sufficient full basket'))
#YES!

####___explore gdp and selfsufvar ####
plot(dat$selfsuf_food_basket, dat$gdp_per_capita_USD)
summary(lm(formula= scale(gdp_per_capita_USD, center = T)~ scale(selfsuf_food_basket, center = T), dat))
#significant correlation between degree of globalization (selfsuf_food_basket) and gdp!

####___explore richness and warfare ####
# bivariate worldmap #

#use strg+F to change variables
## read shape
ctryMapOriginal <- readOGR(dsn="data/spatial/countries_global.shp")
ctryMapOriginal$Country <-  countrycode(ctryMapOriginal$Area, 'country.name', 'iso3c')

## extract period
dfGroups <- dat[which(dat$time==2001),]
sort(as.character(setdiff(dfGroups$ISO,ctryMapOriginal$Country)))

## calculate quantiles
hist(dfGroups$Richness)
hist(dfGroups$actotal)

grd <- rbind(data.frame(dim2=3,dim1=3,color="#3F2949"),
             data.frame(dim2=2,dim1=3,color="#435786"),
             data.frame(dim2=1,dim1=3,color="#4885C1"),
             data.frame(dim2=3,dim1=2,color="#77324C"),
             data.frame(dim2=2,dim1=2,color="#806A8A"),
             data.frame(dim2=1,dim1=2,color="#89A1C8"),
             data.frame(dim2=3,dim1=1,color="#AE3A4E"),
             data.frame(dim2=2,dim1=1,color="#BC7C8F"),
             data.frame(dim2=1,dim1=1,color="#CABED0"))   #set colours: all 9 possibilities of combinations of 
#Richness and full basket (dim1 and dim2)

grd$color <- as.character(grd$color)

trintRichness <- as.numeric(quantile(dfGroups$Richness,probs=seq(0,1,length.out = 4))) #quantile festlegen
trintactotal <- as.numeric(quantile(dfGroups$actotal,probs=seq(0,1,length.out = 4)))

dfGroups$dim1 <-car::recode(dfGroups$Richness,"trintRichness[1]:trintRichness[2]=1; trintRichness[2]:trintRichness[3]=2; trintRichness[3]:trintRichness[4]=3;") #1-2quantil ist 1. "quadrat" etc
dfGroups$dim2 <-car::recode(dfGroups$actotal,"trintactotal[1]:trintactotal[2]=1; trintactotal[2]:trintactotal[3]=2; trintactotal[3]:trintactotal[4]=3;")

## join to map
dfGroupsFinal <- merge(dfGroups[,c("ISO", "dim1","dim2")],grd)
head(dfGroupsFinal)
dfGroupsFinal$id <- as.character(dfGroupsFinal$ISO)
mapsBivariate <- broom::tidy(ctryMapOriginal, region = "Country")
mapsBivariate = join(mapsBivariate, dfGroupsFinal[,c("id","color")], by="id")

## create legend
g.legend <- ggplot(grd, aes(dim1,dim2,fill=factor(1:9)))+
  geom_tile()+
  scale_fill_manual(values=grd$color)+
  # theme_void()+
  theme(legend.position="none",axis.title=element_text(size=5),
        panel.background=element_blank(),plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black",size=8),
        axis.title.y = element_text(angle = 90),axis.text.y=element_text(angle=90))+
  labs(x="Richness",y="warfare") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=6),
        axis.ticks = element_blank(),axis.text.x = element_text(vjust = 3),axis.text.y = element_text(vjust = -1))+
  scale_x_continuous(breaks=c(1,3),labels=c("low","high"))+
  scale_y_continuous(breaks=c(1,3),labels=c("low","high"))

vp<-viewport(width=0.24,height=0.4,x=0.12,y=0.3)

## plot
#pdf("Plots/Bivariat_map_selsuf_fullbasket.jpeg", width = 7.204724, height = 7.204724*0.625)

ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none", plot.title = element_text(size=12), plot.subtitle = element_text(size=10))
#http://lenkiefer.com/2017/04/24/bivariate-map/

print(g.legend,vp=vp)

#dev.off()

####___explore instability area ####
# bivariate worldmap #

#use strg+F to change variables
## read shape
ctryMapOriginal <- readOGR(dsn="data/spatial/countries_global.shp")
ctryMapOriginal$Country <-  countrycode(ctryMapOriginal$Area, 'country.name', 'iso3c')

## extract period
dfGroups <- dat[which(dat$time==2001),]
sort(as.character(setdiff(dfGroups$ISO,ctryMapOriginal$Country)))

## calculate quantiles
hist(dfGroups$sd_Temp)
hist(dfGroups$agriculture_area_ha)

grd <- rbind(data.frame(dim2=3,dim1=3,color="#3F2949"),
             data.frame(dim2=2,dim1=3,color="#435786"),
             data.frame(dim2=1,dim1=3,color="#4885C1"),
             data.frame(dim2=3,dim1=2,color="#77324C"),
             data.frame(dim2=2,dim1=2,color="#806A8A"),
             data.frame(dim2=1,dim1=2,color="#89A1C8"),
             data.frame(dim2=3,dim1=1,color="#AE3A4E"),
             data.frame(dim2=2,dim1=1,color="#BC7C8F"),
             data.frame(dim2=1,dim1=1,color="#CABED0"))   #set colours: all 9 possibilities of combinations of 
#sd_Temp and full basket (dim1 and dim2)

grd$color <- as.character(grd$color)

trintsd_Temp <- as.numeric(quantile(dfGroups$sd_Temp,probs=seq(0,1,length.out = 4))) #quantile festlegen
trintagriculture_area_ha <- as.numeric(quantile(dfGroups$agriculture_area_ha,probs=seq(0,1,length.out = 4)))

dfGroups$dim1 <-car::recode(dfGroups$sd_Temp,"trintsd_Temp[1]:trintsd_Temp[2]=1; trintsd_Temp[2]:trintsd_Temp[3]=2; trintsd_Temp[3]:trintsd_Temp[4]=3;") #1-2quantil ist 1. "quadrat" etc
dfGroups$dim2 <-car::recode(dfGroups$agriculture_area_ha,"trintagriculture_area_ha[1]:trintagriculture_area_ha[2]=1; trintagriculture_area_ha[2]:trintagriculture_area_ha[3]=2; trintagriculture_area_ha[3]:trintagriculture_area_ha[4]=3;")

## join to map
dfGroupsFinal <- merge(dfGroups[,c("ISO", "dim1","dim2")],grd)
head(dfGroupsFinal)
dfGroupsFinal$id <- as.character(dfGroupsFinal$ISO)
mapsBivariate <- broom::tidy(ctryMapOriginal, region = "Country")
mapsBivariate = join(mapsBivariate, dfGroupsFinal[,c("id","color")], by="id")

## create legend
g.legend <- ggplot(grd, aes(dim1,dim2,fill=factor(1:9)))+
  geom_tile()+
  scale_fill_manual(values=grd$color)+
  # theme_void()+
  theme(legend.position="none",axis.title=element_text(size=5),
        panel.background=element_blank(),plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black",size=8),
        axis.title.y = element_text(angle = 90),axis.text.y=element_text(angle=90))+
  labs(x="sd_Temp",y="agricultural area") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=6),
        axis.ticks = element_blank(),axis.text.x = element_text(vjust = 3),axis.text.y = element_text(vjust = -1))+
  scale_x_continuous(breaks=c(1,3),labels=c("low","high"))+
  scale_y_continuous(breaks=c(1,3),labels=c("low","high"))

vp<-viewport(width=0.24,height=0.4,x=0.12,y=0.3)

## plot
#pdf("Plots/Bivariat_map_selsuf_fullbasket.jpeg", width = 7.204724, height = 7.204724*0.625)

ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none", plot.title = element_text(size=12), plot.subtitle = element_text(size=10))
#http://lenkiefer.com/2017/04/24/bivariate-map/

print(g.legend,vp=vp)

#dev.off()


####Plots####
####___ single variable_Plot_settings ####
#legend
regions <- c("Australia and New Zealand",
             "Central and Southern Asia",
             "Eastern and South Eastern Asia",
             "Europe and Northern America",
             "Latin America and the Caribean", 
             "Northern Africa and Western Asia",
             "Sub Saharan Africa")

colors<- c("#F8766D","#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7") #same order as for regions

region_colors<-data.frame(colors,regions)

#make dataframe without Australia as it should not be printet as lm!
dat_min_AUS<-filter(dat,ISO!="AUS")
dat_min_AUS_fix_names<-filter(dat_fix_names,ISO!="AUS")

####___full basket with time####
ggplot(data = dat, aes(y = full_basket, x = time, colour = Region))+ # structure of the graph
  geom_boxplot(aes(group= time), outlier.shape = NA) +                                # add the boxplot
  scale_y_continuous (breaks = seq(0,1000,100))+
  scale_x_continuous(breaks = c(1961,1971,1981,1991,2001), labels = c("1961-1970", "1971-1980", "1981-1990", "1991-2000","2001-2010"))+ 
  geom_hline(yintercept=100, col= "red", alpha= 0.9, size = 0.8)+         # 100% mark
  geom_jitter(alpha = 0.7) +
  geom_line(data= dat_min_AUS, stat='smooth', method = "lm", alpha=0.85, size = 1) +  # linear regression lines per group without australia 
  labs(x = '', y = "full basket [%]", color = "Legend") +                # add labels to the axis
  theme_classic() +                               # make it pretty
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  stat_regline_equation(data= dat_min_AUS, show.legend = T, label.x.npc = 0, label.y.npc = 1, size = 4)#+  #adds equation: out of ggpubr
#ggsave("Plots/boxplot_full_basket_with_time_Region.jpg", width = 20, height = 15, units = "cm", dpi = 300)



####___selfsuf_food_basket####
ggplot(data = dat_fix_names, aes(y = self_sufficient.food.basket, x = time, colour = Region))+ # structure of the graph
  geom_boxplot(aes(group= time), outlier.shape = NA) +                                # add the boxplot
  scale_y_continuous (breaks = seq(-1,1,0.2))+
  scale_x_continuous(breaks = c(1961,1971,1981,1991,2001), labels = c("1961-1970", "1971-1980", "1981-1990", "1991-2000","2001-2010"))+ 
  geom_jitter(alpha = 0.7) +
  geom_line(data= dat_min_AUS_fix_names, stat='smooth', method = "lm", alpha=0.85, size = 1) +  # linear regression lines per group    
  labs(x = '', y = "self-sufficient full basket") +                # add labels to the axis
  theme_classic()+# make it pretty
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  stat_regline_equation(data= dat_min_AUS_fix_names, show.legend = T, label.x.npc = 0, label.y.npc = 0.28, size = 4)+
 ggsave("Plots/selfsuf_food_basket_with_time_Region.jpg", width = 20, height = 15, units = "cm", dpi = 300)


####___Combi Figure Subsahara Africa and East & Southeast Asia#### 
dat_just_sea_subafr<-filter(dat, Region %in% c("Eastern_and_South_Eastern_Asia" , "Sub_Saharan_Africa"))

#Left:
#get gdp for all years!
GDP_per_capita <- read_csv("~/data/MAS-group-share/04_personal/Annika/CropDiversity_NutritionalStability_new/data/pop_data/worldbank/GDP_per_capita.csv")
gdp<- dat%>%
  select(c(ISO,Region))%>%
  full_join(GDP_per_capita, by= c("ISO"="Country Code"))%>%
  filter(Region %in% c("Eastern_and_South_Eastern_Asia" , "Sub_Saharan_Africa"))

n_distinct(gdp$ISO) #check number of countries (right translation of ISO and countrycode)
gdp<-unique(gdp)

#table transformation
#from wide to long
gdp<-pivot_longer(gdp,cols = "1960 [YR1960]":"2020 [YR2020]", names_to = 'Year', values_to = 'gdp_per_capita(current USD)')
gdp$Year<-gdp$Year%>%str_sub( 1L, 4L)%>%as.numeric() #year to numeric
gdp$`gdp_per_capita(current USD)`<-as.numeric(gdp$`gdp_per_capita(current USD)`)
#select timePeriod
gdp<-gdp %>% filter(Year %in% 1961:2010)
#summ up per region 
gdp<-gdp%>%group_by(Region)%>%
  group_by(Year, .add = T)%>%
  dplyr::summarise(regional_gdp_sum= mean( `gdp_per_capita(current USD)`, na.rm = T))
#ACHTUNG: Mali und indonesien haben beide bis 1966 kein gdp!

region_colors_gdp<-region_colors %>% filter(regions %in% c("Eastern_and_South_Eastern_Asia" , "Sub_Saharan_Africa"))

plot_gdp<-ggplot(gdp) +
aes(x = Year, y = regional_gdp_sum, colour = Region) +
  geom_point(shape = "circle", size = 1) +
  scale_color_manual(values=c ("#53B400", "#FB61D7")) + #right colors specific regions!
  labs(
    y = "average regional gdp per capita (current USD)"
    #,
    #    title = "Average GDP per capita for Sub-Saharan Africa and Eastern & Southeastern Asia"
  ) +
  theme(legend.position = "none") #no legend (added manually afterwards)

#Right: Full_basket with time
plot1<-ggplot(data = dat_just_sea_subafr, aes(y = full_basket, x = time, colour = Region))+ # structure of the graph
  geom_boxplot(aes(group= time), outlier.shape = NA, data= dat) +                                # add the boxplot
  scale_y_continuous (breaks = seq(0,160,20))+
  scale_x_continuous(breaks = c(1961,1971,1981,1991,2001), labels = c("1961-1970", "1971-1980", "1981-1990", "1991-2000","2001-2010"))+ 
  geom_hline(yintercept=100, col= "red", alpha= 0.7, size = 0.8)+         # 100% mark
  geom_jitter(alpha = 0.7) +
  geom_line(data= dat_just_sea_subafr, stat='smooth', method = "lm", alpha=0.85, size = 1) +  # linear regression lines per group without australia 
  labs(x = '', y = "full basket [%]", color = "Legend") +                # add labels to the axis
  scale_color_manual(values=c ("#53B400", "#FB61D7"))+
  theme_classic() +                               # make it pretty
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

 #Plot together (using multipanelfigure package):
figure1 <- multi_panel_figure(columns = 6, rows = 3, panel_label_type = "none")
# show the layout
figure1
figure1 %<>%
  fill_panel(plot_gdp, column = 1:4, row = 1:3) %<>%
  fill_panel(plot1, column = 5:6, row = 1:2)
figure1 #manual export #added legend manually




#####___bivariate worldmap selfsuf_full_basket ####

#use strg+F to change variables
## read shape
ctryMapOriginal <- readOGR(dsn="data/spatial/countries_global.shp")
ctryMapOriginal$Country <-  countrycode(ctryMapOriginal$Area, 'country.name', 'iso3c')

## extract period
dfGroups <- dat[which(dat$time==2001),]
sort(as.character(setdiff(dfGroups$ISO,ctryMapOriginal$Country)))

## calculate quantiles
hist(dfGroups$`self-sufficient full basket`)
hist(dfGroups$full_basket)

grd <- rbind(data.frame(dim2=3,dim1=3,color="#3F2949"),
             data.frame(dim2=2,dim1=3,color="#435786"),
             data.frame(dim2=1,dim1=3,color="#4885C1"),
             data.frame(dim2=3,dim1=2,color="#77324C"),
             data.frame(dim2=2,dim1=2,color="#806A8A"),
             data.frame(dim2=1,dim1=2,color="#89A1C8"),
             data.frame(dim2=3,dim1=1,color="#AE3A4E"),
             data.frame(dim2=2,dim1=1,color="#BC7C8F"),
             data.frame(dim2=1,dim1=1,color="#CABED0"))   #set colours: all 9 possibilities of combinations of 
#selfsuf_food_basket and full basket (dim1 and dim2)

grd$color <- as.character(grd$color)

trintselfsuf_food_basket <- as.numeric(quantile(dfGroups$`self-sufficient full basket`,probs=seq(0,1,length.out = 4))) #quantile festlegen
trintfull_basket <- as.numeric(quantile(dfGroups$full_basket,probs=seq(0,1,length.out = 4)))

dfGroups$dim1 <-car::recode(dfGroups$`self-sufficient full basket`,"trintselfsuf_food_basket[1]:trintselfsuf_food_basket[2]=1; trintselfsuf_food_basket[2]:trintselfsuf_food_basket[3]=2; trintselfsuf_food_basket[3]:trintselfsuf_food_basket[4]=3;") #1-2quantil ist 1. "quadrat" etc
dfGroups$dim2 <-car::recode(dfGroups$full_basket,"trintfull_basket[1]:trintfull_basket[2]=1; trintfull_basket[2]:trintfull_basket[3]=2; trintfull_basket[3]:trintfull_basket[4]=3;")

## join to map
dfGroupsFinal <- merge(dfGroups[,c("ISO", "dim1","dim2")],grd)
head(dfGroupsFinal)
dfGroupsFinal$id <- as.character(dfGroupsFinal$ISO)
mapsBivariate <- broom::tidy(ctryMapOriginal, region = "Country")
mapsBivariate = full_join(mapsBivariate, dfGroupsFinal[,c("id","color")], by="id")

## create legend
g.legend <- ggplot(grd, aes(dim1,dim2,fill=factor(1:9)))+
  geom_tile()+
  scale_fill_manual(values=grd$color)+
  # theme_void()+
  theme(legend.position="none",axis.title=element_text(size=7),
        panel.background=element_blank(),plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black",size=8),
        axis.title.y = element_text(angle = 90),axis.text.y=element_text(angle=90))+
  labs(x="self-sufficient full basket",y="full basket") +
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8),
        axis.ticks = element_blank(),axis.text.x = element_text(vjust = 3),axis.text.y = element_text(vjust = -1))+
  scale_x_continuous(breaks=c(1,3),labels=c("low","high"))+
  scale_y_continuous(breaks=c(1,3),labels=c("low","high"))

vp<-viewport(width=0.24,height=0.4,x=0.12,y=0.3)


#jpeg("Plots/Bivariat_map_selsuf_fullbasket_.jpeg", width = 7.204724, height = 7.204724*0.625)

ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none", plot.title = element_text(size=12), plot.subtitle = element_text(size=10))
#http://lenkiefer.com/2017/04/24/bivariate-map/

print(g.legend,vp=vp)
#dev.off()


####Modelling####
####___Transformation/Scaling####

#/////////////////Transform variables to normalize//////////
#transform variables
dat_trans=with(dat_fix_names, data.frame(ISO,
                               time,
                               Region,
                               full_basket = log(full_basket),
                               richness,
                               inverse.Simpson.diversity,
                               asynchrony,
                               self_sufficient_full_basket = self_sufficient_full_basket ^2, 
                               GDP.per.capita = log(GDP.per.capita),
                               N_use  = sqrt(N_use),  
                               irrigation = sqrt(irrigation), 
                               agricultural.area = log(agricultural.area),
                               livestock = log1p(livestock), #how to transform livestock (as it has neg values?!)-> log1p() 
                               precepitation.instability = sqrt(precepitation.instability),
                               temperature.instability = log(temperature.instability),
                               warfare
))



####____Explore adjusted distribution
dat_trans_sin<-dat_trans[,-c(1,2,14)] # just the continious and assessed vars
plot_transform <- lapply(names(dat_trans_sin), function(var_x){
  p <- 
    ggplot(dat_trans_sin) +
    aes_string(var_x)
  
  if(is.numeric(dat_trans_sin[[var_x]])) {
    p <- p + geom_density()
    
  } else {
    p <- p + geom_bar()
  } 
})                  

cowplot::plot_grid(plotlist = plot_transform) 

#looks better
# actually it's more important that residuals of the model are normally distributed!


#////////scale predictors for standardized regression/////////
datPredictors_trans=sapply(dat_trans[,-c(1,2,3)],function(x)scale(x,center = T,scale=T)[,1])  
datCenter_trans=data.frame(ISO= dat_trans$ISO,
                           time= dat_trans$time,
                           Region=dat_trans$Region,
                           datPredictors_trans)
head(datCenter_trans)

####Model fitting####
####___richness#####

####___without fixed effect####
#starting with most basic model, then adding predictors, keep them when they have impact (significance or rising r2)
mod1_rich<-lm(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time, data = datCenter_trans)
mod2_rich<-lm(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + agricultural.area , data = datCenter_trans)
mod3_rich<-lm(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + livestock, data = datCenter_trans)
mod4_rich<-lm(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + self_sufficient_full_basket , data = datCenter_trans)
mod5_rich<-lm(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + self_sufficient_full_basket + livestock, data = datCenter_trans)
mod6_rich<-lm(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + self_sufficient_full_basket + livestock + GDP.per.capita, data = datCenter_trans)
mod7_rich<-lm(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare + self_sufficient_full_basket + livestock, data = datCenter_trans)


mod_2_richness<-lm(full_basket~ richness +
                     self_sufficient_full_basket +
                     GDP.per.capita +
                     irrigation +
                     N_use +
                     precepitation.instability +
                     temperature.instability  +
                     warfare +
                     livestock+
                     agricultural.area +
                     time,
                   data = datCenter_trans)

# Explore Model
summary(mod_2_richness)
#qqPlot(datCenter_trans$livestock) # try each variable: not opitmal: Livestock, agriculture area, 
vif(mod_2_richness) #vif ~ moderate!
AIC(mod_2_richness)

summ(mod_2_richness)

#shows residuals of model!
effect_plots<-list(effect_plot(mod_2_richness, pred = richness, intervall = T, plot.points = T, y.label = "log(full_basket)"),
                   effect_plot(mod_2_richness, pred = self_sufficient_full_basket, intervall = T, plot.points = T,y.label = "log(full_basket)", x.label ="sq(self_sufficient_full_basket)" ),
                   effect_plot(mod_2_richness, pred = GDP.per.capita , intervall = T, plot.points = T, y.label = "log(full_basket)", x.label =  "log(GDP.per.capita)" ),
                   effect_plot(mod_2_richness, pred = irrigation, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label = "sqrt(irrigation)" ),
                   effect_plot(mod_2_richness, pred = N_use, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label = "sqrt(N_use)"),
                   effect_plot(mod_2_richness, pred = precepitation.instability, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label =  "sqrt(precepitation.instability)"),
                   effect_plot(mod_2_richness, pred = temperature.instability, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label=  "sqrt(temperature.instability)"),
                   effect_plot(mod_2_richness, pred = warfare, intervall = T, plot.points = T, y.label = "log(full_basket)"),
                   effect_plot(mod_2_richness, pred = livestock, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label = "log1p(livestock)"),
                   effect_plot(mod_2_richness, pred = agricultural.area, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label= "log(agricultural.area)"),
                   effect_plot(mod_2_richness, pred = time, intervall = T, plot.points = T, y.label = "log(full_basket)"))

cowplot::plot_grid(plotlist = effect_plots, ncol = 3)

#interactions? 
mod_2_1_richness<-lm(full_basket~ richness +
                       self_sufficient_full_basket +
                       GDP.per.capita  +
                       irrigation +
                       irrigation *GDP.per.capita +
                       N_use +
                       precepitation.instability +
                       temperature.instability  +
                       warfare +
                       livestock +
                       agricultural.area +
                       irrigation +
                       time,
                     data = datCenter_trans)


summary(mod_2_1_richness)

#Interactions
#irrigation *GDP.per.capita  ** --> estimate: -0.09447; r2: 0.6987
#N_use *GDP.per.capita * --> * --> estimate: -0.098823; r2: 0.6965   


####___with fixed effect- richness####
# (1 | g) s the simplest possible mixed-model formula, where each level of the grouping factor g, has its own random intercept (not slope!). 
# The mean and standard deviation of these intercepts are parameters to be estimated.
#https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

####____Null model####
fix_zero<-lmer(full_basket~1+(1|Region), data = datCenter_trans, REML = F) 
# full_basket~1 refers to the intercept of the model 
# +(1|Region) -> allows the intercept to vary randomly per region
summary(fix_zero)
summ(fix_zero)
#ICC-> Interclass value correlation: 0.56 indicates clustering!
ranova(fix_zero)
#  Pr(>Chisq): 2.2e-16 *** --> shows significant variance in intercept variation ("differently behavior per Region")

fmod1_rich<-lmer(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + (1|Region), data = datCenter_trans, REML = F)
fmod1_2rich<-lmer(full_basket~irrigation+precepitation.instability+time + (1|Region), data = datCenter_trans, REML = F)

fmod2_rich<-lmer(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + agricultural.area + (1|Region), data = datCenter_trans, REML = F) #agricult area has pos
fmod2_2rich<-lmer(full_basket~irrigation+precepitation.instability+time + agricultural.area + (1|Region), data = datCenter_trans, REML = F)

fmod3_rich<-lmer(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + livestock + (1|Region), data = datCenter_trans, REML = F) #livestock no pos

fmod4_rich<-lmer(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + self_sufficient_full_basket + (1|Region), data = datCenter_trans, REML = F)#self.suf.food has pos effect
fmod4_2rich<-lmer(full_basket~irrigation+precepitation.instability + self_sufficient_full_basket + (1|Region), data = datCenter_trans, REML = F)

fmod5_rich<-lmer(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + self_sufficient_full_basket + livestock + (1|Region), data = datCenter_trans, REML = F)
fmod5_2_rich<-lmer(full_basket~irrigation + precepitation.instability+temperature.instability+ self_sufficient_full_basket + (1|Region), data = datCenter_trans, REML = F)

fmod6_rich<-lmer(full_basket~richness+irrigation+N_use+precepitation.instability+temperature.instability+warfare+time + self_sufficient_full_basket +agricultural.area + livestock + GDP.per.capita + (1|Region), data = datCenter_trans, REML = F)

fmod7_rich<-lmer(full_basket ~ richness + GDP.per.capita + N_use + irrigation + precepitation.instability+temperature.instability + self_sufficient_full_basket + (1|Region), data = datCenter_trans, REML = F)

fmod8_rich<-lmer(full_basket ~ richness + GDP.per.capita + N_use + irrigation + precepitation.instability+temperature.instability + self_sufficient_full_basket + (1|Region), data = datCenter_trans, REML = F)


#fmod5_2_rich best AIC! but include richness as this is variable of intrest!
fmod5_3_rich<-lmer(full_basket~richness+irrigation + precepitation.instability+temperature.instability+ self_sufficient_full_basket + (1|Region), data = datCenter_trans)
fmod5_4rich<- lmer(full_basket~richness+irrigation + precepitation.instability+temperature.instability+ GDP.per.capita + (1|Region), data = datCenter_trans)

model.names<-c("fmod1_rich","fmod1_2rich","fmod2_rich","fmod2_2rich","fmod3_rich","fmod4_rich", "fmod4_2rich","fmod5_rich","fmod5_2_rich", "fmod5_3_rich")
model.list<-c( fmod1_rich , fmod1_2rich , fmod2_rich , fmod2_2rich , fmod3_rich , fmod4_rich ,  fmod4_2rich , fmod5_rich , fmod5_2_rich, fmod5_3_rich )


mod_renard_rich<-lmer(full_basket~richness+irrigation+N_use+precepitation.instability+ temperature.instability+ warfare + time  + (1|Region), data=datCenter_trans)
mod_renard_simp<-lmer(full_basket~inverse.Simpson.diversity+irrigation+N_use+precepitation.instability+ temperature.instability+ warfare + time  + (1|Region), data=datCenter_trans)
mod_renard_async<-lmer(full_basket~asynchrony+irrigation+N_use+precepitation.instability+ temperature.instability+ warfare + time  + (1|Region), data=datCenter_trans)


#####___Model from Scratch!#####
mod_full_final_rich<-lmer(full_basket~ richness + irrigation + N_use + precepitation.instability + temperature.instability+ warfare + self_sufficient_full_basket + livestock + GDP.per.capita + time +(1|Region), data = datCenter_trans, REML = F)
summary(mod_full_final_rich) 
summ(mod_full_final_rich)
vif(mod_full_final_rich)
#AIC:428.7; R2: 0.73; vif: < 2.7

mod_1<-lmer(full_basket~ richness + irrigation + precepitation.instability + temperature.instability+ warfare + self_sufficient_full_basket + livestock + GDP.per.capita + time + (1|Region), data = datCenter_trans, REML = F)
summary(mod_1) #delete precep instab; temp instab; warfare and Livestock as it shows no significance!

mod_select_final_rich<-lmer(full_basket~ richness + irrigation + self_sufficient_full_basket + GDP.per.capita + time + (1|Region), data = datCenter_trans, REML = F)
summary(mod_select_final_rich)
summ(mod_select_final_rich)
vif(mod_select_final_rich)
#AIC: 429.9; R2: 0.72; vif < 2.2

model_table<-export_summs(mod_full_final_rich, mod_select_final_rich, scale = TRUE, plot.distributions = TRUE, error_format = "p = {p.value})", digits= 4, model.names = c("full model","minimum model"))
tab_model(mod_full_final_rich, mod_select_final_rich, show.ci = F, show.stat = F, show.aic = T, show.r2 = T, dv.labels = c("full model", "minimal model"), show.re.var = F, title= "Response Variable: full_basket")

# library(AICcmodavg)
# aictab_fmod <-aictab(model.list, modnames = model.names) 
# do.call(anova,model.names)

effect_plots_fix<-list(effect_plot(mod_renard_rich, pred = richness, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label =  "log(GDP.per.capita)" ),
                       effect_plot(mod_renard_rich, pred = irrigation, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label = "sqrt(irrigation)"),
                       effect_plot(mod_renard_rich, pred = precepitation.instability, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label =  "sqrt(precepitation.instability)"),
                       effect_plot(mod_renard_rich, pred = temperature.instability, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label =  "sqrt(temperature.instability)"),
                       effect_plot(mod_renard_rich, pred = time, intervall = T, plot.points = T, y.label = "log(full_basket)"),
                       effect_plot(mod_renard_rich, pred = N_use, intervall = T, plot.points = T, y.label = "log(full_basket)", x.label = "sqrt(N_use)"),
                       effect_plot(mod_renard_rich, pred = warfare, intervall = T, plot.points = T, y.label = "log(full_basket)",))


cowplot::plot_grid(plotlist = effect_plots_fix)

#####___FINAL MODELs-all div! + Plot #####
mod_select_final_rich<-lmer(full_basket~ richness + irrigation + self_sufficient_full_basket + GDP.per.capita + time + (1|Region), data = datCenter_trans, REML = F)
mod_select_final_async<-lmer(full_basket~ asynchrony + irrigation + self_sufficient_full_basket + GDP.per.capita + time + (1|Region), data = datCenter_trans, REML = F)
mod_select_final_Simp<-lmer(full_basket~ inverse.Simpson.diversity + irrigation + self_sufficient_full_basket + GDP.per.capita + time + (1|Region), data = datCenter_trans, REML = F)


export_summs(mod_select_final_rich, mod_select_final_Simp,mod_select_final_async, scale = TRUE, plot.distributions = TRUE, error_format = "p = {p.value})", digits= 4, model.names = c("Richness", "inverse Simpson", "Asynchrony"))
tab_model(mod_select_final_rich, mod_select_final_Simp,mod_select_final_async, show.ci = F, show.stat = F, show.aic = T, show.r2 = T, dv.labels = c("Richness", "inverse Simpson", "Asynchrony"), show.re.var = F, title= "linear mixed effect model with response variable: full_basket")

####___lm vs. lmer
mod_lm_select_final_rich<-lm(full_basket~ richness + irrigation + self_sufficient_full_basket + GDP.per.capita + time,  data = datCenter_trans)
tab_model(mod_select_final_rich, mod_lm_select_final_rich, show.ci = F, show.stat = F, show.aic = T, show.r2 = T, dv.labels = c("with random effect", "without random effect"), show.re.var = F, title= "Response variable: full_basket")


dfrichness <- data.frame(summary(mod_select_final_rich)$coefficients)[2:6,c(1,2,4)]
names(dfrichness) <- c("Effect","SE","pVal")
colnames(dfrichness)
dfrichness$nam <- c("richness", "sqrt(irrigation)",
                    "sq(self-sufficient Full Basket)",
                    "log(gdp per capita)","Time")
#dfrichness <- rbind(dfrichness[1,],data.frame(Effect=0,SE=0,pVal=NA,nam="richness"),dfrichness)
dfrichness$Model <- "richness"

dfinv.Simp_Diversity <- data.frame(summary(mod_select_final_Simp)$coefficients)[2:6,c(1,2,4)]
names(dfinv.Simp_Diversity) <- c("Effect","SE","pVal")
colnames(dfinv.Simp_Diversity)
dfinv.Simp_Diversity$nam <- c("inv. Simp. Diversity", "sqrt(irrigation)",
                              "sq(self-sufficient Full Basket)",
                              "log(gdp per capita)","Time")
dfinv.Simp_Diversity$Model <-  "inv. Simp. Diversity"

dfasynchrony <- data.frame(summary(mod_select_final_async)$coefficients)[2:6,c(1,2,4)]
names(dfasynchrony) <- c("Effect","SE","pVal")
colnames(dfasynchrony)
dfasynchrony$nam <- c("asynchrony", "sqrt(irrigation)",
                      "sq(self-sufficient Full Basket)",
                      "log(gdp per capita)","Time")
dfasynchrony$Model <- "asynchrony"

dfCombined <- rbind(dfrichness,dfinv.Simp_Diversity,dfasynchrony)
dfCombined$Model <- factor(dfCombined$Model, levels = unique(dfCombined$Model))
dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
dfCombined$labHeight <- dfCombined$Effect + 0.05
dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"] - 0.05
dfCombined$lab <- ""
dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))

vgl_dfCombined <- dfCombined[unlist(lapply(1:11,function(i)seq(i,33,11))),] #sort 


#positions of the *
# dfText <- data.frame(xpos=c(1.0, 1.7, 1.75, 2.0, 2.15, 2.30, 2.50, 2.75, 3, 3.25, 3.7, 4.05, 4.15, 4.3, 6.0,
#                             6.15, 6.85, 7.0, 7.15, 7.85, 8.0, 8.15, 9, 10)
#                        #sort(c(1:8-0.3,1:8,1:8+0.3))
#                      ,
#                      ypos=dfCombined$labHeight,
#                      lab=dfCombined$lab,
#                      Model=dfCombined$Model)

#dfText <- data.frame(xpos=sort(c(1:8-0.3,1:8,1:8+0.3)),ypos=dfCombined$labHeight,lab=dfCombined$lab,Model=dfCombined$Model)

#jpeg("Plots/barplot_coeff_pre.jpeg", width = 7.204724, height = 7.204724*2/3)



#dfText <- data.frame(xpos=sort(c(1:8-0.3,1:8,1:8+0.3)),ypos=dfCombined$labHeight,lab=dfCombined$lab,Model=dfCombined$Model)

jpeg("Plots/barplot_coeff_pre.jpg", width = 7.204724, height = 7.204724*2/3)

ggplot(data=dfCombined, aes(x=nam, y=Effect, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  scale_fill_brewer(palette="RdYlGn") +
  # geom_text(data=dfText,aes(x=xpos,y=ypos, label=lab),size=3)+  
  theme_bw() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.4,1, by = 0.1),1),limits=c(-0.4,1)) +
  #scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10))+
  theme(axis.text.y = element_text(size=10))+
  #  scale_fill_manual(name = "Model",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 

dev.off()
#************************************************************************************************



# fix_mod_2_richness<-lmer(full_basket~ richness +
#                                               self_sufficient_full_basket +
#                                               GDP.per.capita +
#                                               irrigation +
#                                               N_use +
#                                               precepitation.instability +
#                                               temperature.instability  +
#                                               warfare +
#                                               livestock +
#                                               agricultural.area +
#                                               time +
#                                               (1|Region),
#                                             data = datCenter_trans)
# summary(fix_mod_2_richness)
# summ(fix_mod_2_richness, digits= 4)
# AIC(fix_mod_2_richness) 
# vif(fix_mod_2_richness)
# 
# 
# tab_model(fix_mod_2_richness)
# tab_model(mod_2_richness)
# 
# effect_plots_fix<-list(effect_plot(fix_mod_2_richness, pred = richness, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = self_sufficient_full_basket, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = GDP.per.capita , intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = irrigation, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = N_use, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = precepitation.instability, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = temperature.instability, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = warfare, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = livestock, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = agricultural.area, intervall = T, plot.points = T),
#                    effect_plot(fix_mod_2_richness, pred = time, intervall = T, plot.points = T))
# 
# cowplot::plot_grid(plotlist = effect_plots)
# 
# 
# fix_mod_2_richness_2<-lmer(full_basket~ richness +
#                            self_sufficient_full_basket +
#                            GDP.per.capita +
#                            irrigation +
#                            N_use +
#                            precepitation.instability +
#                            temperature.instability  +
#                            warfare +
#                            livestock +
#                            agricultural.area +
#                            time +
#                            (1+time|Region),
#                          data = datCenter_trans)
# 
# anova(fix_mod_2_richness, fix_mod_2_richness_2) #second model fits better 
# summary(fix_mod_2_richness_2) #but has singularity # don't take it!



##### test for interactions
fix_2_mod_2_richness<-lmer(full_basket~ richness +
                             richness +
                             richness *agricultural.area + 
                             self_sufficient_full_basket +
                             GDP.per.capita +
                             irrigation +
                             N_use +
                             precepitation.instability +
                             temperature.instability  +
                             warfare +
                             livestock +
                             agricultural.area +
                             time +
                             (1|Region),
                           data = datCenter_trans)

summary(fix_2_mod_2_richness)

effect_plot(fix_2_mod_2_richness, pred = richness *agricultural.area, intervall = T, plot.points = T)
#irrigation *GDP.per.capita & N_use *GDP.per.capita 
#self_sufficient_full_basket*GDP.per.capita 
# precepitation.instability*agricultural.area 
# temperature.instability*agricultural.area 
# livestock * GDP.per.capita
# richness * agricultural.area
#... interaction effect not significant 

summ(fix_2_mod_2_richness, digits= 4) 
simple_slopes(fix_2_mod_2_richness)
graph_model(fix_2_mod_2_richness, y= full_basket, x= irrigation, lines=GDP.per.capita)
# if lines tend to be parallel, no interaction is going on!


####Final Conclusion: 
# Best Model without interaction and with fixed effect! 
# use for explanation of model choice: fix_zero (ICC indicates that!)
# richness is significantly important! 
# gdp is important!
# agricultural area is important & Correlation Agricultural area and richness!?
# gdp correlated with 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####____Other Divs (NS)####
#without fixed effect#
#taking same structure as model fitted for richness
fmod5_3_simp<-lmer(full_basket~ Simp_Div +irrigation + precepitation.instability+temperature.instability+ self_sufficient_full_basket + (1|Region), data = datCenter_trans)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#_____Asynchrony
#taking same structure as model fitted for richness
fmod5_3_async<-lmer(full_basket~ asynchrony +irrigation + precepitation.instability+temperature.instability+ self_sufficient_full_basket + (1|Region), data = datCenter_trans)


mod_2_Asynchr<-lm(full_basket~ asynchrony +
                    self_sufficient_full_basket +
                    GDP.per.capita +
                    irrigation +
                    N_use +
                    precepitation.instability +
                    temperature.instability  +
                    warfare +
                    livestock +
                    agricultural.area +
                    time,
                  data = datCenter_trans)

summary(mod_2_Asynchr)
summ(mod_2_Asynchr)

#with fixed effects
fix_mod_2_asynchrony<-lmer(full_basket~ asynchrony +
                             self_sufficient_full_basket +
                             GDP.per.capita +
                             irrigation +
                             N_use +
                             precepitation.instability +
                             temperature.instability  +
                             warfare +
                             livestock +
                             agricultural.area +
                             time +
                             (1|Region),
                           data = datCenter_trans)

summary(fix_mod_2_asynchrony)
summ(fix_mod_2_asynchrony, digits= 4)
simple_slopes(fix_mod_2_asynchrony)





