#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Intext calculations
#Date: 3/24/2020  
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Purpose: The goal of this RScript is three fold: 
#   1.) Update geolocations
#   2.) Fill in intext calculations
#   3.) Create map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup workspace----------------------------------------------------------------
#Clear memory
rm(list=ls(all=TRUE))

#Load Required Packages
library(mapsapi)
library(sf)
library(parallel)
library(readxl)
library(tidyverse)


#Define working directory and database location
working_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/intext_calculations/"

#Api Key  (Do not leave this here or upload to github)
api_key <-   #Fill in here
  #Steps for obtaining API
  #https://console.cloud.google.com/google/maps-apis/overview
  #Select appropriate "project" in header 
  #Select "APIs and Services" from menu
  #Select "Credentials" from menu
  #Copy API Key
  
#Create function to interact with API function----------------------------------
#Create function to access Google API
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

#Execute function for well water samples----------------------------------------
#Read well sample location .csv
df<-read_xlsx(paste0(working_dir,"TWON_3.17.2020.xlsx"), sheet = "Sample Coordinates")

#Create wrapper function with error handling
fun<-function(n){
  tryCatch(google_api(UID     = df$Key[n],
                      address = paste0(df$Street[n], ", ", df$Town[n], ", TX ", df$Zip[n]),
                      api_key = api_key), 
           error=function(e) data.frame(UID=df$Key[n], 
                                        X=0, 
                                        Y=0))
}

#run wrapper function
x<-mclapply(X=seq(  1, nrow(df)),FUN=fun, mc.cores = 8)
save.image(paste0(working_dir,"output.RData"))

#Collect data
output<-bind_rows(x)
colnames(output)<-c("Key", "Long_new","Lat_new")

#Merge output with df
df<-left_join(df, output, by="Key")

#Clean up 
df<-df %>% 
  mutate(Long_new = ifelse(is.na(Long_new), paste(Long), Long_new), 
         Lat_new  = ifelse(is.na(Lat_new),  paste(Lat),  Lat_new))

#Write.csv
write.csv(df, paste0(working_dir, "address_output.csv")) 

# output
save.image(paste0(working_dir,"output.RData"))
