#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Geolocate Addresses
#Date: 6/7/2019
#Coder: C. Nathan Jones (njones@sesync.org)
#Purpose: The goal of this R Notebook is to Geolocate addresses from a well 
#         water sampling campaign. To do this, we will access the Google Earth 
#         API using an the mapsapi R package. 
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
working_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/geolocation/"

#Api Key  (Do not leave this here or upload to github)
api_key <-  #Fill in here
  
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
df<-read_xlsx(paste0(working_dir,"TWON.xlsx"), sheet = "Sample_Coordinates")

#Find samples without xyz coordinates
df<-df %>%
  filter(Lat=="-") %>%
  filter(Sample_Address!="No address")

#Create wrapper function with error handling
fun<-function(n){
  tryCatch(google_api(UID     = df$Key[n],
                      address = df$Sample_Address[n], 
                      api_key = api_key), 
           error=function(e) data.frame(UID=df$Key[n], 
                                        X=0, 
                                        Y=0))
}

#run wrapper function
x<-mclapply(X=seq(  1, nrow(df)),FUN=fun, mc.cores = 8)
save.image(working_dir,"output.RData")

#Collect data
output<-bind_rows(x)
colnames(output)<-c("Key", "X","Y")

#Merge output with df
df<-left_join(df, output, by="Key")
df<- df %>% select(-Lat, -Long)

#Write.csv
write.csv(df, paste0(working_dir, "address_output.csv")) 

# output
save.image(paste0(working_dir,"output.RData"))