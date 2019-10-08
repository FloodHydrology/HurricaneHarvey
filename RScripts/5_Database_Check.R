#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Database Check
#Date: 10/8/2019
#Coder: Nate Jones (cnjones7@ua.edu)
#Purpose: Check for missing zip codes in current well "database"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear Memory 
remove(list=ls())

#Download required libraries
library(mapsapi)
library(readxl)
library(parallel)
library(sf)
library(tidyverse)


#Define data directory
data_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/database_check/"
spatial_data_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/spatial_data/"

#Download Tabular Data
df<-read_xlsx(paste0(data_dir, "zip_codes_to_check.xlsx"))

#Download Spatial Data
p<-"+proj=utm +zone=15 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
counties<-st_read(paste0(spatial_data_dir, "counties_tx/counties_texas.shp"))
counties<-st_transform(counties, crs=p)

#Api Key  (Do not leave this here or upload to github)
api_key <- NULL  #Fill in here 
  #Steps for obtaining API
  #https://console.cloud.google.com/google/maps-apis/overview
  #Select appropriate "project" in header 
  #Select "APIs and Services" from menu
  #Select "Credentials" from menu
  #Copy API Key
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Check Lat Long Info--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create function to go from address to lat long ----------------------------
google_api<-function(UID=NULL, address = NULL, api_key = NULL){
  
  #Download xml file
  doc<-mp_geocode(address, key=api_key)
  
  #Convert to points
  pnts<-mp_get_points(doc)
  
  #Convert to coordinates
  xy<-st_coordinates(pnts)
  
  #export coordinates with Unique ID
  data.frame(UID, xy)
}

#2.2 Apply function-------------------------------------------------------------
#Create wrapper function with error handling
fun<-function(n){
  #Create Address for Lookup
  sample_address <- paste0(df$Street[n], ", ", df$Town[n], ", TX ", df$Zip[n])
  
  #Run google_api fun with trycatch
  tryCatch(google_api(UID     = df$Key[n],
                      address = sample_address, 
                      api_key = api_key), 
           error=function(e) data.frame(UID=df$Key[n], 
                                        X=0, 
                                        Y=0))
}

#run wrapper function
locations<-mclapply(X=seq(  1, nrow(df)),FUN=fun, mc.cores = 8) %>% bind_rows(.)

#2.3 Check for errors-----------------------------------------------------------
#Create function
fun<-function(n){
  #select row in locations
  new<-locations[n,]
  
  #Select row in df
  old<-df %>% filter(Key == new$UID)
  
  #Ask: Are the x and y the same
  output<-tibble(
    Key=old$Key,
    Location_Check = if_else(
      new$X == as.numeric(paste(old$Long)) &
        new$Y == as.numeric(paste(old$Lat)), 
      1, 0)
  )
  
  #Output
  output
}

#Execute Functin
x<-mclapply(X=seq(  1, nrow(df)),FUN=fun, mc.cores = 8) %>% bind_rows(.)
df<-left_join(df, x)

#Save Image 
save.image(paste0(data_dir,"backup_after_google_download.RData"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Check County Info----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Overlay Points with County Data
pnts<-df %>% 
  #select collumn of interest
  select(Key, Lat, Long) %>% 
  #Remove NA
  mutate(Long = as.numeric(paste(Long)), 
         Lat = as.numeric(paste(Lat))) %>% 
  filter(!is.na(Long)) %>% 
  #Convert to sf object
  st_as_sf(., coords = c("Long", "Lat"), crs = 4326, remove=F) %>% 
  #project
  st_transform(., crs=p) %>% 
  #select collumn of interest
  select(Key) %>% 
  #Overlay with county data
  st_join(., counties) %>% 
  #Select Collumns of interest
  select(Key, NAME) %>% 
  #Rename for later
  rename(new_county_name = NAME) %>% 
  #Drop Geometry Data
  st_drop_geometry(.)

#Compare new county names to old
comparison<- df %>% 
  #Select Collumns of interest
  select(Key, County) %>% 
  #Join with new points layer
  left_join(., pnts) %>% 
  #Compare!
  mutate(County_Check = if_else(County == paste(new_county_name), 1, 0)) %>% 
  #Select Collumns of interet
  select(Key, County_Check)

#Left Join with master dataframe
df<-left_join(df, comparison)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Gather Missing Data--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create Updated Output
update<-df %>% 
  filter(Location_Check != 1 |
         County_Check   != 1) %>% 
  select(Key, Street, Town, Zip) %>% 
  left_join(., locations %>% rename(Key=UID, Lat = Y, Long = X)) %>% 
  left_join(., pnts %>% rename(County=new_county_name))

#Add NA's to "No address" 
update<-update %>% 
  mutate(Zip =  ifelse(Street == "No address", NA, Zip), 
         Long = ifelse(Street == "No address", NA, Long), 
         Lat  = ifelse(Street == "No address", NA, Lat)) 

#Add NA's to "No address" 
update<-update %>% 
  mutate(Zip =  ifelse(Street == "NA", NA, Zip), 
         Long = ifelse(Street == "NA", NA, Long), 
         Lat  = ifelse(Street == "NA", NA, Lat)) 
  
#Add Bianary Cues 
update<- update %>% 
  #Are there still missing locations
  mutate(Location_Check = if_else(!is.na(Long) & !is.na(Lat), 1, 0)) %>% 
  #Are there still missing Counties
  mutate(County_Check = if_else(!is.na(County), 1, 0))

#Bind with master df
df<-df %>% 
 #filter out updated keys
 filter(!(Key %in% update$Key)) %>% 
 #bind rows
 rbind(., update)

#Report
# 1 Unreadable Address
# 9 Missing County Information
#60 Missing Location Info
