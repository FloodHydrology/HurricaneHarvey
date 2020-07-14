#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Inundated well users by county
#Date: 6/14/2020
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Purpose: Estimate the number of well user impacted by Hurricane Harvey by county 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Gather data on 
#   1) population
#   2) well owners
#   3) testing results

#Make plots:

#Figure 1
#A) Proportion of well owners (by population)
#B) Proportion of wells flooded
#C) # positivte EC
#D) # positive TC

#Figure 2 histogram of rural vs urban



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace--------------------------------------------------------
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

#Load well raster and reproject relevant data (https://doi.org/10.1016/j.scitotenv.2017.07.018)
wells<-raster(paste0(spatial_dir, "Private_Wells/REM_map1990.tif"))
  p<-wells@crs
  
#Load municipal boundaries (source: http://gis-txdot.opendata.arcgis.com/datasets/09cd5b6811c54857bd3856b5549e34f0_0)
counties<-st_read(paste0(spatial_dir, "counties_tx/counties_texas.shp")) %>%
  st_transform(., crs=p)

#Load zip code boundaries
zip<-st_read(paste0(spatial_dir, "zip_codes/tl_2015_us_zcta510.shp"))
  #Note, we transform these later!

#Load USDA county rural/urban classifications (https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx)
usda<-read_csv(paste0(data_dir,"Other datasets\\ruralurbancodes2013.csv"))

#Load CDC country rural/urban classifications (https://www.cdc.gov/nchs/data_access/urban_rural.htm#2013_Urban-Rural_Classification_Scheme_for_Counties)
cdc<-read_xlsx(paste0(data_dir,"Other datasets\\NCHSURCodes2013.xlsx")) 

#Load County population info (https://demographics.texas.gov/Data/TPEPP/Estimates/)
tx_pop<-read_csv(paste0(data_dir,"Other datasets\\2018_txpopest_county.csv")) %>% 
  dplyr::select(county,july1_2018_pop_est) %>% 
  rename(NAME=county, pop = july1_2018_pop_est)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Define counties of interest--------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#Add USDA designations
# usda<-usda %>% 
#   #Filter to Texas
#   filter(State=='TX') %>% 
#   #Select collumns of interest
#   dplyr::select(NAME=County_Name, 
#                 USDA = RUCC_2013) %>% 
#   #remove "County" in name
#   mutate(NAME = substr(NAME, 1, (nchar(NAME)-7)))
# counties<-counties %>% left_join(., usda)

#Add CDC designations
cdc<-cdc %>% 
  filter(`State Abr.` == 'TX') %>% 
  select(NAME = `County name`, 
         cdc_code = `2013 code`) %>% 
  mutate(NAME = str_remove_all(NAME, " County"))
counties<-counties %>% left_join(., cdc)


#crop wells to county extent
wells<-crop(wells, counties)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Inundation-------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Create raster of inundation extent-----------------------------------------
#List shape files from Dartmouth Flood Observatory
files<-list.files(paste0(spatial_dir, "DFO_Inundation")) %>% 
  tibble::enframe(name = NULL) %>% 
  filter(str_detect(value,".shp")) %>%
  as_vector()

#Create blank inundation raster
inundation<-wells*0
inundation[is.na(inundation)]<-0

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

#3.2 Create Summary Stats (County)----------------------------------------------
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
counties<-counties %>% dplyr::left_join(., output)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 4: Bianary contamination data---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Organize data by zip-------------------------------------------------------
#Brazoria Samples~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
brazoria<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'Brazoria', skip=1) %>% 
  dplyr::select('...1', 'TOTAL...13', 'TOTAL...27') %>% 
  rename(zip='...1', n_pos = 'TOTAL...13', n_samples = 'TOTAL...27') %>% 
  mutate(test='TC') %>% 
  ungroup(zip) %>%  mutate(zip=as.numeric(paste(zip)))

#Houston Samples~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
h_1<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'Houston', skip=1) %>% 
  dplyr::select('Zip Code...1',"Total Coliform...2", "E. coli...3" ) %>% 
  rename(zip='Zip Code...1', TC="Total Coliform...2", EC="E. coli...3") %>% 
  pivot_longer(-zip) %>% rename(test=name) %>% 
  group_by(zip, test) %>% summarise(n_pos=sum(value), n_samples = n()) 

h_2<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'Houston', skip=1) %>% 
  dplyr::select('Zip Code...4',"Total Coliform...5", "E. coli...6" ) %>% 
  rename(zip='Zip Code...4', TC="Total Coliform...5", EC="E. coli...6") %>% 
  pivot_longer(-zip) %>% rename(test=name) %>% 
  group_by(zip, test) %>% summarise(n_pos=sum(value), n_samples = n())  

h_3<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'Houston', skip=1) %>% 
  dplyr::select('Zip Code...7',"Total Coliform...8", "E. coli...9" ) %>% 
  rename(zip='Zip Code...7', TC="Total Coliform...8", EC="E. coli...9") %>% 
  pivot_longer(-zip) %>% rename(test=name) %>% 
  group_by(zip, test) %>% summarise(n_pos=sum(value), n_samples = n())

houston<-bind_rows(h_1, h_2) %>% bind_rows(., h_3) %>% 
  group_by(zip, test) %>% summarise(n_pos = sum(n_pos), n_samples=sum(n_samples))

remove(h_1, h_2, h_3)

#RAPID Samples~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rapid<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'RAPID') %>% 
  dplyr::select('Zip', 'TC_Detect', 'EC_Detect') %>% 
  mutate(TC = if_else(TC_Detect == 'Positive', 1,0), 
         EC = if_else(TC_Detect == 'Positive', 1,0)) %>% 
  dplyr::select(-TC_Detect, -EC_Detect) %>% 
  pivot_longer(-Zip) %>% rename(test=name, zip=Zip) %>%
  group_by(zip, test) %>% summarise(n_pos=sum(value), n_samples = n()) %>% 
  ungroup(zip) %>%  mutate(zip=as.numeric(paste(zip)))

#FEMA Samples~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fema<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'FEMA') %>% 
  dplyr::select('Zip', 'TC_Detect', 'EC_Detect') %>% 
  mutate(TC = if_else(TC_Detect == 'Positive', 1,0), 
         EC = if_else(TC_Detect == 'Positive', 1,0)) %>% 
  dplyr::select(-TC_Detect, -EC_Detect) %>% 
  pivot_longer(-Zip) %>% rename(test=name, zip=Zip) %>%
  group_by(zip, test) %>% summarise(n_pos=sum(value),  n_samples = n()) %>% 
  ungroup(zip) %>%  mutate(zip=as.numeric(paste(zip)))

#Combine Datasets~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_zip<-bind_rows(brazoria, houston, rapid, fema) %>% 
  group_by(zip, test) %>% 
  summarise(n_pos=sum(n_pos), 
            n_samples = sum(n_samples))

#4.2 Combine zip/county data----------------------------------------------------
#Define common projection
p_utm<-c('+proj=utm +zone=15 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

#reproject shapefiles
zip_shp <- zip %>% st_transform(.,p_utm) %>% select(zip=GEOID10) 
county_shp <- counties %>% st_transform(.,p_utm) %>% select(county=NAME)

#Define intersecting zip codes
zip_shp<-zip_shp[county_shp,]

#Create zip-county conversiont table
df_county<- zip_shp %>% 
  #spatial join
  st_intersection(., county_shp) %>% 
  #Remove spatial information
  st_drop_geometry() %>% as_tibble()

#Estiamte proportion of area in each zip
df_county<-
  lapply(
    #Counter for function
    seq(1,nrow(df_county)), 
    #Function         
    function(n){
      #Isolate zip and county
      temp<-df_county[n,]
      zip_temp<-zip_shp %>% dplyr::filter(zip == temp$zip)
      county_temp<-county_shp %>% filter(county==temp$county)
      
      #Estimate proportion of zip in county
      prop<-st_area(st_intersection(zip_temp, county_temp))/st_area(zip_temp)
      temp<-temp %>% mutate(prop = prop)
      
      #Export
      temp}
    #Number of cores
    #mc.cores = detectCores()
  ) %>% 
  #Bind rows and select distinct rows
  bind_rows() %>% distinct()

#Add county info to df
df_county<-df_county %>% 
  mutate(zip = as.numeric(paste(zip))) %>% 
  left_join(., df_zip)

#Distribute samples accross counties [assume equal distribution of samples accross zip]
df_county<-df_county %>% 
  #Estimate number of samples
  mutate(
    n_samples=n_samples*prop, 
    n_pos = n_pos*prop
  ) %>% 
  #Summarize by county and test
  group_by(county, test) %>% 
  summarise(n_pos = round(sum(n_pos)), 
            n_samples = round(sum(n_samples))) 

#Make wider for join
df_county<-df_county %>% 
  pivot_wider(names_from='test', 
              values_from = c('n_pos', 'n_samples')) %>% 
  select(-c('n_pos_NA', 'n_samples_NA'))

#Join to master dataframe
counties<-counties %>% left_join(., df_county %>% dplyr::rename(NAME = county)) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 5: Products---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.1 Output table (modeled data)------------------------------------------------
output<-counties %>% 
  #Drop geometry
  st_drop_geometry() %>% 
  #Select col of interest
  select(NAME, prop_inun_area, pop, total_well_users, inun_well_users, prop_inun_wells) %>% 
  #Convert proportions to %
  mutate(prop_well_users = round(total_well_users/pop*100,1),
         prop_inun_wells = round(prop_inun_wells*100,1),  
         prop_inun_area = round(prop_inun_area*100,1),
         total_well_users = round(total_well_users), 
         inun_well_users = round(inun_well_users)) %>% 
  #Rename
  rename('County' = NAME, 
         'Area Inundated [%]' = prop_inun_area, 
         'Total population' = pop, 
         'Estimated well users'= total_well_users, 
         '% popultion on well water' = prop_well_users, 
         'Well users impacted by inundation' = inun_well_users,
         '% of well users impacted by inundation' = prop_inun_wells) %>% 
  #Arrange by county
  arrange(County)

#Export 
write_csv(output, paste0(output_dir, "county_pop_summary.csv"))

#5.2 Output table (sampled data)------------------------------------------------
write_csv(df_county, paste0(output_dir, "county_sample_summary.csv"))

#5.3 County plot----------------------------------------------------------------
export<-counties %>% 
  mutate(prop_inun_area = prop_inun_area*100, 
         prop_well_users = total_well_users/pop*100, 
         prop_pos_EC = n_pos_EC/n_samples_EC*100,
         prop_pos_TC = n_pos_TC/n_samples_TC*100) %>%  
  select(pop, prop_inun_area, prop_well_users, prop_inun_wells, prop_pos_EC, prop_pos_TC) %>% 
  rename('Total population' = pop, 
         '% Area inundated' = prop_inun_area,
         '% Popultion on well water' = prop_well_users, 
         '% Wells inundated' = prop_inun_wells, 
         '% Samples EC Positive' = prop_pos_EC, 
         '% Samples TC Positive' = prop_pos_TC)


#Turn plotting device on
png(paste0(output_dir, "county_map.png"), width=7,height=9, units = "in", res=300)
tmap_mode("plot")

#Create plots
pop<-tm_shape(export) + 
  tm_polygons('Total population', palette = "Greys", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)  
prop_inun_area<-tm_shape(export) + 
  tm_polygons('% Area inundated', palette = "Blues", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)  
prop_well_users<-tm_shape(export) + 
  tm_polygons('% Popultion on well water', palette = "BuPu", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)  
prop_inun_wells<-tm_shape(export) + 
  tm_polygons('% Wells inundated', palette = "Reds", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T) 
prop_pos_EC<-tm_shape(export) + 
  tm_polygons('% Samples EC Positive', palette = 'Greens', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
prop_pos_TC<-tm_shape(export) +
  tm_polygons('% Samples TC Positive', palette = "YlOrBr", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)

#plot
tmap_arrange(pop, 
             prop_inun_area,
             prop_well_users,
             prop_inun_wells,
             prop_pos_EC, 
             prop_pos_TC,
             ncol=2)

#Turn device off
dev.off()

#5.4 Rural vs city polots-------------------------------------------------------
#Intall packages
library(ggpubr)

#Edit matrix of interest
m<-counties %>% 
  #Drop geometry
  st_drop_geometry() %>% 
  #select collumns of interest
  dplyr::select(cdc_code, inun_well_users, prop_inun_wells) %>% 
  #Urban/rural classfication
  mutate(cdc_code = if_else(cdc_code>4, 'Rural', 'Urban')) %>% 
  #Convert to %
  mutate(prop_inun_wells = prop_inun_wells*100) %>% as_tibble() %>% 
  #Change cdc_code to County Type for legend
  rename('County Type' = 'cdc_code') 

ggscatterhist(m,
  #Scatter plot Data
  x = 'inun_well_users', 
  y = 'prop_inun_wells', 
  color = "County Type",
  #font sizes
  font.x = c(14), 
  font.xtickslab = c(11),
  font.ytickslab = c(11),
  font.y=c(14),
  #Scatterplot options
  palette = c('#E57200','#232D4B'),
  xlab = "Total well users per county",
  ylab = "Wells users impacted\nby inundation [%]",
  size = 6, alpha = 0.7,
  #histogram options
  margin.plot.size = 1.75,
  margin.params = list(
    fill = "County Type", 
    color= c('black'),
    size=0.2),
  #Legend
  legend = 'bottom', 
  legend.title = c("County Type"),
  font.legend = 14,
  #Margin Plots
  # margin.plot = "boxplot",
  # #Plot Options
  # ggtheme = theme_bw()
  )


