#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Inundated Well Users
#Date: 6/26/2019
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Purpose: Estimate the number of well user impacted by Hurricane Harvey. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup workspace----------------------------------------------------------------
#Clear memory
rm(list=ls(all=TRUE))

#Load Required Packages
library(tidyverse)
library(raster)
library(sf)
library(fasterize)
library(tmap)

#Define working directory and database location
spatial_data_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/spatial_data/"
working_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/inundated_wells/"

#Download well and reproject relevant data
wells<-raster(paste0(spatial_data_dir, "Private_Wells/REM_map1990.tif"))
p<-wells@crs

#Download Municipal Boundaries (source: http://gis-txdot.opendata.arcgis.com/datasets/09cd5b6811c54857bd3856b5549e34f0_0)
cities<-st_read(paste0(spatial_data_dir, "TxDOT_City_Boundaries/TxDOT_City_Boundaries.shp")) %>%
  st_transform(., crs=p)
zip_codes<-st_read(paste0(spatial_data_dir, "zip_codes/tl_2015_us_zcta510.shp")) %>%
  st_transform(., crs=p)
counties<-st_read(paste0(spatial_data_dir, "counties_tx/counties_texas.shp")) %>%
  st_transform(., crs=p)

#Define counties sampled in this study------------------------------------------
#Make list of counties in the study
county_names<-read_csv("//nfs/njones-data/Research Projects/Private Wells/Harvey/geolocation_data/locations.csv") %>%
  dplyr::select(Sample_County) %>%
  distinct(.) %>% na.omit() %>%
  rename(NAME = Sample_County)

#Limit Counties
counties_sampled<-counties %>% right_join(.,county_names)
counties<-counties[counties_sampled,]
remove(county_names)
remove(counties_sampled)

#Crop wells
wells<-crop(wells, counties)

#Create raster of inundation extent---------------------------------------------
#List shape files from Dartmouth Flood Observatory
files<-list.files(paste0(spatial_data_dir, "DFO_Inundation")) %>% 
  tibble::enframe(name = NULL) %>% 
  filter(str_detect(value,".shp")) %>%
  as_vector()

#Create blank inundation raster
inundation<-wells*0
inundation[is.na(inundation)]<-0

#Create loop to download and rasterize each 
for(i in 1:length(files)){

  #Read file  
  temp<-st_read(paste0(spatial_data_dir, "DFO_Inundation/", files[i])) %>%
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

#Create Summary Stats (County)--------------------------------------------------
#Create function to sum by county
fun<-function(n){
  
  #Select county
  county<-counties[n,]
  
  #crop inundation and wells to counties
  wells      <- crop(wells,      county)
  inundation <- crop(inundation, county)
  wells      <- mask(wells,      county)
  inundation <- mask(inundation, county)
  
  #Create output tibble
  output<-tibble(
    NAME = county$NAME,
    total_area = st_area(county), 
    inun_area  = cellStats(inundation, sum)*(res(inundation)[1]^2),
    prop_inun_area = inun_area/total_area,
    total_well_users = cellStats(wells, sum),
    inun_well_users  = cellStats(wells*inundation, sum), 
    prop_inun_wells  = inun_well_users/total_well_users)
  
  #Export
  output
}

#apply function
output<-lapply(seq(1, nrow(counties)), fun) %>% bind_rows()

#Left Join to counties sf
counties<-counties %>%
  dplyr::select(NAME) %>%
  dplyr::left_join(., output)

#Create Summary Stats (Zip Code)------------------------------------------------
#Limit zip codes to county extent
zip_codes<-zip_codes[counties,] 
zip_codes %<>%
  dplyr::rename(zip = ZCTA5CE10) %>%
  dplyr::select(zip)

#Create function to sum by county
fun<-function(n){
  
  #Select county
  zip<-zip_codes[n,]
  
  #crop inundation and wells to counties
  wells      <- crop(wells,      zip)
  inundation <- crop(inundation, zip)
  wells      <- mask(wells,      zip)
  inundation <- mask(inundation, zip)
  
  #Create output tibble
  output<-tibble(
    zip_code = zip$zip,
    total_area = st_area(zip), 
    inun_area  = cellStats(inundation, sum)*(res(inundation)[1]^2),
    prop_inun_area = inun_area/total_area,
    total_well_users = cellStats(wells, sum),
    inun_well_users  = cellStats(wells*inundation, sum), 
    prop_inun_wells  = inun_well_users/total_well_users)
  
  #Export
  output
}

#apply function
output<-lapply(seq(1, nrow(zip_codes)), fun) %>% bind_rows() %>% rename(zip = zip_code)

#Left Join to counties sf
zip_codes<-zip_codes %>%
  dplyr::left_join(., output)

#Creat Initial County Plots-----------------------------------------------------
#Turn plotting device on
png(paste0(working_dir, "inundated_wells_county.png"), width=7,height=7, units = "in", res=300)
tmap_mode("plot")

#Create plots
tm1<-tm_shape(counties) + 
  tm_polygons("total_well_users", palette = "BuGn", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
tm2<-tm_shape(counties) + 
  tm_polygons("inun_well_users", palette = 'PuRd', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)      
tm3<-tm_shape(counties) + 
  tm_polygons("prop_inun_wells", palette = "YlOrBr", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
tm4<-tm_shape(counties) + 
  tm_polygons("prop_inun_area", palette = 'PuBu', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    

#plot
tmap_arrange(tm4, tm1, tm2, tm3)

#Turn plotting device off
dev.off()

#Export csv for good measure
output<-counties %>% 
  as_tibble() %>%
  dplyr::select(NAME, total_area, inun_area, prop_inun_area, total_well_users,
                inun_well_users, prop_inun_wells)

write_csv(output, paste0(working_dir, "inundated_wells_county.csv"))

#Create Initial Zip Code Plots--------------------------------------------------
#Turn plotting device on
png(paste0(working_dir, "inundated_wells_zip.png"), width=7,height=7, units = "in", res=300)
tmap_mode("plot")

#Create plots
tm1<-tm_shape(zip_codes) + 
  tm_polygons("total_well_users", palette = "BuGn", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
tm2<-tm_shape(zip_codes) + 
  tm_polygons("inun_well_users", palette = 'PuRd', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)      
tm3<-tm_shape(zip_codes) + 
  tm_polygons("prop_inun_wells", palette = "YlOrBr", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
tm4<-tm_shape(zip_codes) + 
  tm_polygons("prop_inun_area", palette = 'PuBu', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    

#plot
tmap_arrange(tm4, tm1, tm2, tm3)

#Turn plotting device off
dev.off()

#Export csv for good measure
output<-zip_codes %>%
  as_tibble() %>%
  dplyr::select(zip,total_area,inun_area,prop_inun_area,total_well_users,inun_well_users,prop_inun_wells)
write_csv(output, paste0(working_dir, "inundated_wells_zip.csv"))
