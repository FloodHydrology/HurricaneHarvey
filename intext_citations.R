#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Intext Calculations
#Date: 7/20/2020
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Purpose: Calculations for values discussed intext
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0: Setup workspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Load Required Packages
library(tmap)
library(fasterize)
library(sf)
library(raster)
library(readxl)
library(tidyverse)

#Define working directory and database location
spatial_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Private Wells\\Harvey\\spatial_data\\"
data_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Private Wells\\Harvey\\database\\"
output_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Private Wells\\Harvey\\output\\"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Compare reported flooding to modeled flooding------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Assemble Inundation raster
#Step 2: Download and Tidy Survey Data
#Step 3: Convert to spatial data
#Step 4: Overlay and compare reported flooding with modeldued inundation

#2.1 Assemble Inundation raster-------------------------------------------------
#List shape files from Dartmouth Flood Observatory
files<-list.files(paste0(spatial_dir, "DFO_Inundation")) %>% 
  tibble::enframe(name = NULL) %>% 
  filter(str_detect(value,".shp")) %>%
  as_vector()

#Load well raster and reproject relevant data (https://doi.org/10.1016/j.scitotenv.2017.07.018)
wells<-raster(paste0(spatial_dir, "Private_Wells/REM_map1990.tif"))
p<-wells@crs

#Load municipal boundaries (source: http://gis-txdot.opendata.arcgis.com/datasets/09cd5b6811c54857bd3856b5549e34f0_0)
counties<-st_read(paste0(spatial_dir, "counties_tx/counties_texas.shp")) %>%
  st_transform(., crs=p)

#Define disaster counties (https://www.fema.gov/disaster/4332)
disaster<-c('Aransas', 'Austin', 'Bastrop', 'Bee', 'Brazoria', 
            'Caldwell', 'Calhoun', 'Chambers', 'Colorado', 'DeWitt', 
            'Fayette', 'Fort Bend', 'Galveston', 'Goliad', 'Gonzales', 
            'Grimes', 'Hardin', 'Harris', 'Jackson', 'Jasper', 'Jefferson', 
            'Karnes', 'Kleberg', 'Lavaca', 'Lee', 'Liberty', 'Matagorda', 
            'Montgomery', 'Newton', 'Nueces', 'Orange', 'Polk', 'Refugio', 
            'Sabine', 'San Jacinto', 'San Patricio', 'Tyler', 'Victoria', 
            'Walker', 'Waller', 'Wharton')
disaster<-tibble(NAME = disaster) %>% mutate(NAME = as_factor(NAME))

#limit counties to disaster areas
counties<-counties %>% 
  dplyr::select(NAME) %>% 
  filter(NAME %in% disaster$NAME)

#Add population information
counties<-counties %>% left_join(., tx_pop)

#crop wells to county extent
wells<-crop(wells, counties)

#Create blank inundation raster
inundation<-wells*0

#Create loop to download and rasterize each 
for(i in 1:length(files)){
  
  #Read file  
  temp<-st_read(paste0(spatial_dir, "DFO_Inundation/", files[i])) %>%
    st_transform(., crs=p) 
  
  #rasterize
  temp<-fasterize(sf=temp, raster= inundation, background=0)
  
  #Add to inundation
  inundation<-inundation+temp
  
  #Remove temp
  remove(temp)
}

#Make inundation raster bianary
inundation[inundation==0]<-NA
inundation<-inundation*0+1 

#2.2 Download and Tidy Survey Data----------------------------------------------
#Load VT database
#Spatial Data
pnts<-read_xlsx(
    path = paste0(data_dir,'VT-TAMU\\TWON_4.9.2020.xlsx'),
    sheet = 'Sample Coordinates') %>% 
  mutate(Lat = as.numeric(Lat), 
         Long = as.numeric(Long)) %>% 
  na.omit()

#Questionaire Data
flood<-read_xlsx(
    path = paste0(data_dir,'VT-TAMU\\TWON_4.9.2020.xlsx'),
    sheet = 'VT Survey (n=631)') %>% 
  select(Key, flood = `Q11A-height_home`) %>% 
  mutate(flood = if_else(flood=='Yes',1,0))

#Join together 
pnts<-left_join(pnts, flood)

#Create sf object
pnts<-pnts %>% 
  st_as_sf(., 
    coords = c("Long", "Lat"), 
    crs = crs('+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs '))

#reproject
pnts<-st_transform(pnts, crs = st_crs(paste(inundation@crs)))

#2.3 Overlay--------------------------------------------------------------------
#Spatial Overlay
pnts$flood_modeled_100<-raster::extract(inundation, pnts, buffer=100, fun=max, na.rm=T)
pnts$flood_modeled_250<-raster::extract(inundation, pnts, buffer=250, fun=max, na.rm=T)
pnts$flood_modeled_500<-raster::extract(inundation, pnts, buffer=500, fun=max, na.rm=T)
pnts$flood_modeled_1000<-raster::extract(inundation, pnts, buffer=1000, fun=max, na.rm=T)

#Tidy data
pnts<-pnts %>% 
  st_drop_geometry() %>% 
  select(Key, flood_modeled_100, flood_modeled_250, flood_modeled_500, flood_modeled_1000)

#Summarise data
flood_survey<-pnts %>% filter(flood == 1) %>% count %>% pull()
flood_model<-pnts %>% filter(flood == 1 & flood_modeled ==1) %>% count %>% pull()
flood_model/flood_survey
pnts %>% filter(flood == 1 & flood_modeled ==0) %>% count %>% pull()

