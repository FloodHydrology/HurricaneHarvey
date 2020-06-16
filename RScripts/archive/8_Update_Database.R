#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Update Database
#Date: 4/9/2020  
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Purpose: Check 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Request from Kelsey Peiper~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (1) Make maps of all binary data by zip (maybe?) and county 
#       (a) testing rates
#       (b) TC positivity
#       (c) EC posiviity 
# (2) Make data layer with disaster counties and update text in Section 2 
# (3) Section 1 think about one sentence maybe about % and # of well users impacted and how it variable by county (for example, high % but low # and low # but high %)
# (4) Do you want to do something spatially for Section 2?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Setup workspace----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Load Required Packages
library(tmap)
library(sf)
library(readxl)
library(tidyverse)

#Define working directory and database location
spatial_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/spatial_data/"
data_dir<-   "//nfs/njones-data/Research Projects/Private Wells/Harvey/database/"

#Gather data
df<-read_xlsx(paste0(data_dir,"VT-TAMU/TWON_4.9.2020.xlsx"), sheet='Sample Coordinates')
zip_shp<-st_read(paste0(spatial_dir, "zip_codes/tl_2015_us_zcta510.shp"))
county_shp<-st_read(paste0(spatial_dir, "counties_tx/counties_texas.shp"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Find Counties------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define common projection
p<-c('+proj=utm +zone=15 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

#reproject shapefiles
zip_shp <- zip_shp %>% st_transform(.,p) %>% select(zip=GEOID10) 
county_shp <- county_shp %>% st_transform(.,p) %>% select(county=NAME)

#Create shapefile of TWON data
pnts<-df %>% 
  #Subset to relevant cols
  select(Key, Lat, Long) %>% 
  #Turn data into numeric data type
  mutate(
    Lat=as.numeric(paste(Lat)),
    Long = as.numeric(paste(Long))
  ) %>% 
  #Convert NA to 0
  mutate(
    Lat = replace_na(Lat, 0), 
    Long = replace_na(Long, 0)
  ) %>%
  #Convert to sf feature
  st_as_sf(.,coords = c("Long", "Lat"), crs=4269) %>% 
  #reproject
  st_transform(., p)

#creat output table
output<-
  #Intersect sample points with county
  st_intersection(pnts, county_shp) %>% 
  #drop geometry
  st_drop_geometry() %>% as_tibble() %>% 
  #rename county collumn
  rename(new_county=county) %>% 
  #join to df
  left_join(df, .) %>% 
  #arrange by key
  arrange(Key)
    
#Write output
write_csv(output, paste0(data_dir,"20200409_samplecoords.csv"))
