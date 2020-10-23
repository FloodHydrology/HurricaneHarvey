#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: County Level Analysis
#Date: 10/23/2020
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Purpose: Examine spatial extent of (i) publically available well user and inundation
#            datasets, and (ii) spatial extent of contamination from well water sampling
#            campaigns. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
#Read in Brazoria county bianary data
brazoria<-
  read_xlsx(
    paste0(data_dir, "All labs/All binary_v2.xlsx"), 
    sheet = 'Brazoria', 
    skip=1) 

#Rename cols manually
colnames(brazoria)<-c("zip_1", "AUG_1",	"SEPT_1", "OCT_1", "NOV_1",	"DEC_1",
                      "JAN_1", "FEB_1",	"MAR_1",  "APR_1", "MAY_1", "JUN_1",	
                      "TOTAL_1", "blank","zip_2","AUG_2",	"SEPT_2",	"OCT_2", "NOV_2", "DEC_2",
                      "JAN_2", "FEB_2",	"MAR_2", "APR_2",	"MAY_2", "JUN_2", "TOTAL_2")
#Tidy
brazoria <- brazoria %>% 
  rename(zip='zip_1', n_pos = 'TOTAL_1', n_samples = 'TOTAL_2') %>% 
  mutate(test='TC') %>% 
  #ungroup(zip) %>% 
  mutate(zip=as.numeric(paste(zip))) 

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

#Something weird is happening -- a [1] is showing up in the col type? 
df_county$prop<-as.numeric(paste0(df_county$prop))

#Add county info to df
df_county<-df_county %>% 
  mutate(zip = as.numeric(paste(zip))) %>% 
  left_join(., df_zip) %>% 
  drop_na()

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
              values_from = c('n_pos', 'n_samples')) 

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
  mutate(prop_inun_area = as.numeric(paste(prop_inun_area)),
         prop_inun_area = prop_inun_area*100, 
         prop_inun_wells = prop_inun_wells*100,
         prop_well_users = total_well_users/pop*100, 
         prop_pos_EC = n_pos_EC/n_samples_EC*100,
         prop_pos_TC = n_pos_TC/n_samples_TC*100) %>%  
  select(total_well_users, prop_inun_area, prop_well_users, prop_inun_wells,
         n_samples_EC, n_samples_TC, prop_pos_EC, prop_pos_TC, inun_well_users) %>% 
  rename("Well users flooded" = inun_well_users,
         'Well users' = total_well_users, 
         'Area flooded (%)' = prop_inun_area,
         'Prop well users (%)' = prop_well_users, 
         'Well users flooded (%)' = prop_inun_wells, 
         'EC Samples' = n_samples_EC, 
         'TC Samples' = n_samples_TC,
         'EC Pos (%)' = prop_pos_EC, 
         'TC Pos (%)' = prop_pos_TC)

#A Plot data from geospatial analysis-------------------------------------------
#Turn plotting device on
png(paste0(output_dir, "county_well_map.png"), width=7,height=6, units = "in", res=300)
tmap_mode("plot")

#Create plots
w1<-tm_shape(export) + 
  tm_polygons('Well users', palette = "RdPu", style = 'fixed', breaks=c(1500,3000,6000,9000,16000,110000)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="A)", title.size = 14/12)  
w2<-tm_shape(export) + 
  tm_polygons('Area flooded (%)', palette = "GnBu", style = 'fixed', breaks=c(0,1,5,10,25,66)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="B)", title.size = 14/12)  
w3<-tm_shape(export) + 
  tm_polygons('Well users flooded', palette = "BuPu",style = 'fixed', breaks=c(0,10,50, 250,500,3000)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="C)", title.size = 14/12)  
w4<-tm_shape(export) + 
  tm_polygons('Well users flooded (%)', palette = "Blues", style = 'fixed', breaks=c(0,1,2, 5,10,20)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="D)", title.size = 14/12) 

#plot
tmap_arrange(w1,w2,w3,w4, ncol=2)

#Turn device off
dev.off()

#B Plot sampling efforts by county----------------------------------------------
#Turn plotting device on
png(paste0(output_dir, "county_testing_map.png"), width=7,height=6, units = "in", res=300)
tmap_mode("plot")

#Create Indivudal maps
s1<-tm_shape(export) +
  tm_polygons("TC Samples", palette = 'YlOrBr', style = 'fixed', breaks=c(0,5,10,50,200,4300)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="E)", title.size = 14/12) 
s2<-tm_shape(export) +
  tm_polygons("EC Samples", palette = 'Greens', style = 'fixed', breaks=c(0,5,10,50,100,1500))+
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="F)", title.size = 14/12)  
s3<-prop_pos_TC<-tm_shape(export) +
  tm_polygons('TC Pos (%)', palette = "Reds", style = 'fixed', breaks=c(0,10,25,50,75,100)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="G)", title.size = 14/12) 
s4<-tm_shape(export) + 
  tm_polygons('EC Pos (%)', palette = 'YlGn', style = 'fixed', breaks=c(0,10,25,50,75,100)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="H)", title.size = 14/12)     

#Print
tmap_arrange(s1,s2,s3,s4,ncol=2)

#Turn printing device off
dev.off()

#5.3c Combine county plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(paste0(output_dir, "county_map_a.png"), 
    width=7,height=12, units = "in", res=300)
tmap_mode("plot")
tmap_arrange(w1, w2, w3, w4,s1,s2,   s3,s4,  ncol=2)
dev.off()

png(paste0(output_dir, "county_map_b.png"), 
    width=14,height=6, units = "in", res=300)
tmap_mode("plot")
tmap_arrange(w1, w2, s1,s2, w3, w4, s3,s4,ncol=4)
dev.off()

#5.4 Rural vs city plots-------------------------------------------------------_
#Intall packages
library(ggpubr)
library(patchwork)

#A. Plot well users impacted by inundation by total well users per county
#Edit matrix of interest
m<-counties %>% 
  #Drop geometry
  st_drop_geometry() %>% 
  #select collumns of interest
  dplyr::select(cdc_code, inun_well_users, total_well_users) %>% 
  #Urban/rural classfication
  mutate(cdc_code = if_else(cdc_code>4, 'Rural', 'Urban')) %>% 
  as_tibble() %>% 
  #Change cdc_code to County Type for legend
  rename('County Type' = 'cdc_code') 

m1<-ggscatterhist(m,
  #Scatter plot Data
  y = 'inun_well_users', 
  x = 'total_well_users', 
  color = "County Type",
  #font sizes
  font.x = c(14), 
  font.xtickslab = c(11),
  font.ytickslab = c(11),
  font.y=c(14),
  #Scatterplot options
  palette = c('#E57200','#232D4B'),
  xlab = "Total well users per county",
  ylab = "Total well users\nimpacted by inundation",
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

#B. Plot well users impacted by inundation by proporotion of well users
#Edit matrix of interest
m<-counties %>% 
  #Drop geometry
  st_drop_geometry() %>% 
  #Estimate proportion of population on well
  mutate(prop_well_users=total_well_users/pop*100) %>% 
  #select collumns of interest
  dplyr::select(cdc_code, prop_well_users, prop_inun_wells) %>% 
  #Urban/rural classfication
  mutate(cdc_code = if_else(cdc_code>4, 'Rural', 'Urban')) %>% 
  #Convert to %
  mutate(prop_inun_wells = prop_inun_wells*100) %>% as_tibble() %>% 
  #Change cdc_code to County Type for legend
  rename('County Type' = 'cdc_code') 

m2<-ggscatterhist(m,
                  #Scatter plot Data
                  x = 'prop_well_users', 
                  y = 'prop_inun_wells', 
                  color = "County Type",
                  #font sizes
                  font.x = c(14), 
                  font.xtickslab = c(11),
                  font.ytickslab = c(11),
                  font.y=c(14),
                  #Scatterplot options
                  palette = c('#E57200','#232D4B'),
                  xlab = "Well users per county [%]",
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

#C.) Export plot
png(paste0(output_dir, "county_impacts_total.png"), width=7,height=6, units = "in", res=300)
m1
dev.off()

png(paste0(output_dir, "county_impacts_percent.png"), width=7,height=6, units = "in", res=300)
m2
dev.off()

#5.5 Abstract Art!-------------------------------------------------------------
m<-counties %>% 
  #Drop geometry
  st_drop_geometry() %>%
  #Urban/rural classfication
  mutate(cdc_code = if_else(cdc_code>4, 'Rural', 'Urban')) %>% 
  #select cols of interest
  select(cdc_code, total_population = pop, total_well_users, inun_well_users) %>% 
  #pivot long
  pivot_longer(-cdc_code) %>% 
  #summarise
  group_by(name, cdc_code) %>% 
  summarise(value=sum(value))

m<-m %>% 
  group_by(name) %>% 
  summarise(value = sum(value)) %>% 
  mutate(cdc_code = 'total') %>% 
  bind_rows(m,.) %>% 
  arrange(name)

write_csv(m, paste0(output_dir,'abstract_art.csv'))
  
#Calcs
tot_pop<-m %>% filter(cdc_code == 'total', name=='total_population') %>% select(value) %>% pull()
tot_well<-m %>% filter(cdc_code=='total', name=='total_well_users') %>% select(value) %>% pull()
1-tot_well/tot_pop

#5.6 SI Figures ----------------------------------------------------------------
#A. sampled data ------------------------------------------------------------------
#Convert df_zip results into wider format
export<-df_zip %>% 
  #Pivot longer
  pivot_longer(
    -c(zip, test)
  ) %>% 
  #combine test/name cols
  mutate(name = paste0(test,"_", name)) %>% 
  select(-test) %>% 
  #Pivot wider
  pivot_wider(names_from = name, 
              values_from = value) %>% 
  #Estimate percent pos
  select(
    'TC Pos (%)' = TC_n_pos/TC_n_samples, 
    'EC Pos (%)' = EC_n_pos/EC_n_samples,
    'TC Samples' = TC_n_samples,
    'EC Samples' = EC_n_samples)

#Export csv 
write_csv(export, paste0(output_dir,'zip_sampling.csv'))

#Joint to spatial data
export<-zip_shp %>% 
  #Convert zip col to double
  mutate(zip = as.numeric(zip)) %>% 
  #join to df_zip with sampling data
  left_join(., export)

#Plot
#Turn plotting device on
png(paste0(output_dir, "zip_testing_map.png"), width=7,height=6, units = "in", res=300)
tmap_mode("plot")

#Create Indivudal maps
s1<-tm_shape(export) +
  tm_polygons("TC Samples", palette = 'YlOrBr', style = 'fixed', breaks=c(0,5,10,50,200,4300)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="E)", title.size = 14/12) 
s2<-tm_shape(export) +
  tm_polygons("EC Samples", palette = 'Greens', style = 'fixed', breaks=c(0,5,10,50,100,1500))+
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="F)", title.size = 14/12)  
s3<-prop_pos_TC<-tm_shape(export) +
  tm_polygons('TC Pos (%)', palette = "Reds", style = 'fixed', breaks=c(0,10,25,50,75,100)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="G)", title.size = 14/12) 
s4<-tm_shape(export) + 
  tm_polygons('EC Pos (%)', palette = 'YlGn', style = 'fixed', breaks=c(0,10,25,50,75,100)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="H)", title.size = 14/12)     

#Print
tmap_arrange(s1,s2,s3,s4,ncol=2)

#Turn printing device off
dev.off()

#B. modeled data-------------------------------------------------------------------
#Project zip_shp
zip_shp<-st_transform(zip_shp, crs = st_crs(p))

#Create function to sum by zip
fun<-function(n){
  
  #Select county
  zip_shp<-zip_shp[n,]
  
  x<-tryCatch(crop(wells, zip_shp), error = function(e) 1)
  
  if(is(x)[2]=='Raster'){
    #crop inundation and wells to counties
    wells      <- crop(wells,      zip_shp)
    inundation <- crop(inundation, zip_shp)
    wells      <- mask(wells,      zip_shp)
    inundation <- mask(inundation, zip_shp)
    
    #Create output tibble
    output<-tibble(
      NAME = zip_shp$zip,
      total_area = st_area(zip_shp), 
      inun_area  = cellStats(inundation, sum)*(res(inundation)[1]^2),
      prop_inun_area = inun_area/total_area,
      total_well_users = cellStats(wells, sum),
      inun_well_users  = cellStats(wells*inundation, sum), 
      prop_inun_wells  = inun_well_users/total_well_users)
  }else{
    #if Extents don't overlap
    output<-NA
  }
  
  #Export
  output
}

#apply function
output<-lapply(seq(1, nrow(zip_shp)), fun) %>% bind_rows()
output<-output %>% mutate(zip = NAME)

#Left Join to counties sf
zip_shp<-zip_shp %>% dplyr::left_join(., output)
zip_shp <- zip_shp %>% 
  mutate(
    total_area = as.numeric(paste(total_area)), 
    prop_inun_area = as.numeric(paste(prop_inun_area))
  )

#Define cols for plotting
zip_shp<-zip_shp %>% 
  mutate(
     zip,
    'Well Users' = total_well_users,
    'Area flooded (%)' = inun_area/total_area*100, 
    'Well users flooded' = inun_well_users,
    'Well users flooded (%)' = inun_well_users/total_well_users*100
  ) %>% 
  select(zip, 'Well Users','Area flooded (%)',
         'Well users flooded','Well users flooded (%)')

#Start plot
png(paste0(output_dir, "zip_well_map.png"), width=7,height=6, units = "in", res=300)
tmap_mode("plot")

#Create plots
w1<-tm_shape(zip_shp) + 
  tm_polygons('Well Users', palette = "RdPu", style = 'fixed', breaks=c(0,100,500,1500,15000)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="A)", title.size = 14/12)  
w2<-tm_shape(zip_shp) + 
  tm_polygons('Area flooded (%)', palette = "GnBu", style = 'fixed', breaks=c(0,1,5,10,25,100)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="B)", title.size = 14/12)  
w3<-tm_shape(zip_shp) + 
  tm_polygons('Well users flooded', palette = "BuPu",style = 'fixed', breaks=c(0,5,10,15, 20, 1200)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="C)", title.size = 14/12)  
w4<-tm_shape(zip_shp) + 
  tm_polygons('Well users flooded (%)', palette = "Blues", style = 'fixed', breaks=c(0,1,2, 5,10,20)) +
  tm_layout(frame=F, 
            legend.show = T, legend.text.size = 8/12, 
            title="D)", title.size = 14/12) 

#plot
tmap_arrange(w1,w2,w3,w4, ncol=2)

#Turn device off
dev.off()

#Export csv
zip_shp %>% st_drop_geometry() %>% write_csv(., paste0(output_dir,"zip_wells.csv"))

#5.4 Intext calculations -------------------------------------------------------
#Isolate County Data
df<-counties %>% 
  #Drop geometry
  st_drop_geometry() %>% as_tibble() %>% 
  #Urban/rural classfication
  mutate(cdc_code = if_else(cdc_code>4, 'Rural', 'Urban')) 

#Proportion of county reliant on well water
df %>% 
  mutate(prop_well_users = total_well_users/pop*100) %>% 
  group_by(cdc_code) %>% 
  summarise(
    mean = mean(prop_well_users), 
    min  = min(prop_well_users),
    max  = max(prop_well_users))

#Percent of counties well pop inundated <0.05
(df %>% filter(prop_inun_wells<0.05) %>% summarise(n()))/(df %>% summarise(n()))  
  

#Averge number of well users in counties
df %>% 
  group_by(cdc_code) %>% 
  summarise(mean = mean(total_well_users),
            min = min(total_well_users),
            max = max (total_well_users))

#% of counties with <750 users inundated
(df %>% filter(inun_well_users<750) %>% count())/(df %>% count())

#Count of rural and urban counties with >750 wells inundated
df %>% 
  filter(inun_well_users>750) %>% 
  group_by(cdc_code) %>% 
  summarise(n())

#Summary of proportion of well users in counties with >750 wells inudnated
df %>% 
  #filter(inun_well_users>750) %>% 
  summarise(total_well_users = sum(total_well_users[inun_well_users>750])/sum(total_well_users)*100,
            inun_well_users  = sum(inun_well_users[inun_well_users>750])/sum(inun_well_users)*100)
