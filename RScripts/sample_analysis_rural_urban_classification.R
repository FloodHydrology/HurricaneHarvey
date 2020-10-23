#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Sample Analysis: Rural-Urban Classification
#Date: 10/23/2020
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Purpose: Examine spatial extent of sampling effort accross rural and urban counties
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
#2.0 Urban/Rural Classification-------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Load CDC country rural/urban classifications (https://www.cdc.gov/nchs/data_access/urban_rural.htm#2013_Urban-Rural_Classification_Scheme_for_Counties)
cdc<-read_xlsx(paste0(data_dir,"Other datasets\\NCHSURCodes2013.xlsx")) %>% 
  filter(`State Abr.` == 'TX') %>% 
  select(NAME = `County name`, 
         cdc_code = `2013 code`) %>% 
  mutate(NAME = str_remove_all(NAME, " County")) %>% 
  mutate(cdc_code = if_else(cdc_code>4, 'Rural', 'Urban')) 

#Read VT-TAMU data
pnts<-read_xlsx(
  path = paste0(data_dir,'VT-TAMU\\TWON_4.9.2020.xlsx'),
  sheet = 'Sample Coordinates') %>% 
  mutate(Lat = as.numeric(Lat), 
         Long = as.numeric(Long)) %>% 
  na.omit()s

#Left join
cdc<-cdc %>% 
  rename(County = NAME) %>% 
  left_join(pnts, .) %>% 
  select(Key, cdc_code)

#Export cdc
write_csv(cdc, paste0(output_dir,"sample_locations_cdc_code.csv"))
