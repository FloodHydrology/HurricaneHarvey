#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: County & Zip Code
#Date: 6/7/2019
#Coder: C. Nathan Jones (njones@sesync.org)
#Purpose: Determine county and zipcode for each sample.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup workspace----------------------------------------------------------------
#Clear memory
rm(list=ls(all=TRUE))

#Load Required Packages
library(tidyverse)
library(readxl)
library(sf)

#Define working directory and database location
spatial_data_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/spatial_data/"
working_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/county_zip_data/"

#Download zipcode and county spatial data
zip_codes<-st_read(paste0(spatial_data_dir, "zip_codes/tl_2015_us_zcta510.shp"))
counties<-st_read(paste0(spatial_data_dir, "counties_tx/counties_texas.shp"))

#create points from address coordinates
pnts<-read_xlsx(paste0(working_dir,"zip_codes_needed.xlsx"))
pnts<-st_as_sf(pnts, coords = c("Long", "Lat"), crs = 4326, remove=F)

#reproject data to UTM coordinates
p<-"+proj=utm +zone=15 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
pnts<-st_transform(pnts, crs=p)
zip_codes<-st_transform(zip_codes, crs=p)
counties<-st_transform(counties, crs=p)

#Deinfe county and zip codes for points-----------------------------------------
#limit zip codes and counties to location of points
zip_codes<-zip_codes[pnts,]
counties<-counties[pnts,]

#limit infor in zip and counties
zip_codes<- zip_codes %>% select(ZCTA5CE10) %>% rename(zip = ZCTA5CE10)
counties<- counties %>% select(NAME) %>% rename(name=NAME)

#grab information
pnts<-st_intersection(pnts, zip_codes)
pnts<-st_intersection(pnts, counties)

#Convert to tibble and export
pnts<-as_tibble(pnts)
write_csv(pnts, paste0(working_dir,"lat_long_zip.csv"))




