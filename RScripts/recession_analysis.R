###################################################################################
#Title: Recession Analysis
#Date: 10/11/2018
#Update: 2/20/2019
#Coder: C. Nathan Jones (njones@sesync.org)
#Purpose: Determine the date when peak inundation occurred for indiviudal counties
#         affected by Hurricane Harvey on the Texas Gulf Coast
###################################################################################

###################################################################################
#Setup workspace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################################################################
#Download required libraries
library(raster)          #spatial analysis
library(sf)              #spatial analysis
library(rgdal)           #spatial analysis
library(rgeos)           #spatial analysis
library(dataRetrieval)   #USGS data download
library(dplyr)           #enter into the tidyverse
library(magrittr)        #more tidyverse
library(tidyr)           #because the tidyverse is strong in you
library(lubridate)       #datetime formatting is a PITA

#Define data directory
data_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/recession_analysis_data"

#Pull required data into R Environment
df<-read.csv(paste0(data_dir,"/locations.csv"))
counties<-st_read(paste0(data_dir,"/spatial_data/."), "counties_texas")
gages<-st_read(paste0(data_dir,"/spatial_data/NHDPlus12/."), "StreamGageEvent")

#reproject
p<-"+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
counties<-st_transform(counties, p)
gages<-st_transform(gages, p)

#remove m coordinates
counties<-st_zm(counties)
gages<-st_zm(gages)

###################################################################################
#Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################################################################
#Define counties of interest
county_list<-c(levels(df$Sample_County), "Bee", "Childress", "Colorado", "La Salle", "Montgomery", "Washington")
counties<-counties[counties$NAME %in% county_list,]
counties %<>% select(NAME)

#Clip gages to counties in question
gages<-st_intersection(gages, counties)
gages<-gages %>% select(SOURCE_FEA)

#download gage data 
gage_data<-readNWISdv(siteNumbers = gages$SOURCE_FEA, parameterCd ="00065", startDate="2017-08-01", endDate = "2017-10-10")[,2:4]
colnames(gage_data)<-c("SOURCE_FEA", "date", "stage")
gage_data$date<-yday(strptime(gage_data$date, format="%Y-%m-%d"))
gage_data<-na.omit(gage_data)

#determine peak flow day for each gage
gage_data %<>% 
  group_by(SOURCE_FEA) %>% 
  summarise(max_date = date[max(stage, na.rm = T)]) %>%
  filter(max_date>230)  #This assumes max streamflow occurs after Aug 17th.

#join gage_data with gage sf
gages$SOURCE_FEA<-as.numeric(paste(gages$SOURCE_FEA))
gage_data$SOURCE_FEA<-as.numeric(paste(gage_data$SOURCE_FEA))
gages<-left_join(gages,gage_data, by="SOURCE_FEA")
gages<-na.omit(gages)

#merge gage data with county data
gages<-st_intersection(counties, gages) %>% group_by(NAME) %>% summarise(mean_peak=mean(max_date))
counties<-st_join(counties, gages)
rownames(counties)<-seq(1,  length(counties$mean_peak))

#create function to estimate NA
fun<-function(m){
  
  #Define county
  county<-counties[m,]
  
  #Define neighboring counties
  neighbors<-st_intersects(counties[m,], counties)
  neighbors<-counties[neighbors[[1]],]
  
  #Estimate mean value of neighbors
  mean(neighbors$mean_peak,na.rm=T)
}

#run function
m<-as.numeric(rownames(counties[is.na(counties$mean_peak),]))
for(i in 1:length(m)){
  counties$mean_peak[m[i]]<-fun(m[i])
}

#spit out list of coutnies and peak date
counties<-data.frame(county=counties$NAME.x,
                     peak_date= as.Date(counties$mean_peak, origin=as.Date("2017-01-01")))

counties<-counties %>% arrange(county)
#export csv
write.csv(counties, paste0(data_dir,"Hurricane_Harvey_Peak_Flow_Date.csv"))
