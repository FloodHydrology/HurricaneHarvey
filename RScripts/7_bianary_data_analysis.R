#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Intext calculations
#Date: 4/8/2020  
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Purpose: The goal of this RScript is three fold: 
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
working_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/intext_calculations/"
spatial_dir<-"//nfs/njones-data/Research Projects/Private Wells/Harvey/spatial_data/"
data_dir<-   "//nfs/njones-data/Research Projects/Private Wells/Harvey/database/"

#Gather data
zip_shp<-st_read(paste0(spatial_dir, "zip_codes/tl_2015_us_zcta510.shp"))
county_shp<-st_read(paste0(spatial_dir, "counties_tx/counties_texas.shp"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Zip Data-----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Brazoria Samples~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
brazoria<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'Brazoria', skip=1) %>% 
  select('...1', 'TOTAL...13', 'TOTAL...27') %>% 
  rename(zip='...1', n_pos = 'TOTAL...13', n_samples = 'TOTAL...27') %>% 
  mutate(test='TC') %>% 
  ungroup(zip) %>%  mutate(zip=as.numeric(paste(zip)))

#Houston Samples~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
h_1<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'Houston', skip=1) %>% 
  select('Zip Code...1',"Total Coliform...2", "E. coli...3" ) %>% 
  rename(zip='Zip Code...1', TC="Total Coliform...2", EC="E. coli...3") %>% 
  pivot_longer(-zip) %>% rename(test=name) %>% 
  group_by(zip, test) %>% summarise(n_pos=sum(value), n_samples = n()) 

h_2<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'Houston', skip=1) %>% 
  select('Zip Code...4',"Total Coliform...5", "E. coli...6" ) %>% 
  rename(zip='Zip Code...4', TC="Total Coliform...5", EC="E. coli...6") %>% 
  pivot_longer(-zip) %>% rename(test=name) %>% 
  group_by(zip, test) %>% summarise(n_pos=sum(value), n_samples = n())  

h_3<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'Houston', skip=1) %>% 
  select('Zip Code...7',"Total Coliform...8", "E. coli...9" ) %>% 
  rename(zip='Zip Code...7', TC="Total Coliform...8", EC="E. coli...9") %>% 
  pivot_longer(-zip) %>% rename(test=name) %>% 
  group_by(zip, test) %>% summarise(n_pos=sum(value), n_samples = n())

houston<-bind_rows(h_1, h_2) %>% bind_rows(., h_3) %>% 
  group_by(zip, test) %>% summarise(n_pos = sum(n_pos), n_samples=sum(n_samples))

remove(h_1, h_2, h_3)

#RAPID Samples~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rapid<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'RAPID') %>% 
  select('Zip', 'TC_Detect', 'EC_Detect') %>% 
  mutate(TC = if_else(TC_Detect == 'Positive', 1,0), 
         EC = if_else(TC_Detect == 'Positive', 1,0)) %>% 
  select(-TC_Detect, -EC_Detect) %>% 
  pivot_longer(-Zip) %>% rename(test=name, zip=Zip) %>%
  group_by(zip, test) %>% summarise(n_pos=sum(value), n_samples = n()) %>% 
  ungroup(zip) %>%  mutate(zip=as.numeric(paste(zip)))
  
#FEMA Samples~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fema<-read_xlsx(paste0(data_dir, "All labs/All binary_v2.xlsx"), sheet = 'FEMA') %>% 
  select('Zip', 'TC_Detect', 'EC_Detect') %>% 
  mutate(TC = if_else(TC_Detect == 'Positive', 1,0), 
         EC = if_else(TC_Detect == 'Positive', 1,0)) %>% 
  select(-TC_Detect, -EC_Detect) %>% 
  pivot_longer(-Zip) %>% rename(test=name, zip=Zip) %>%
  group_by(zip, test) %>% summarise(n_pos=sum(value),  n_samples = n()) %>% 
  ungroup(zip) %>%  mutate(zip=as.numeric(paste(zip)))
  
#Combine Datasets~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_zip<-bind_rows(brazoria, houston, rapid, fema) %>% 
  group_by(zip, test) %>% 
  summarise(n_pos=sum(n_pos), 
            n_samples = sum(n_samples))
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#County Data--------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define common projection
p<-c('+proj=utm +zone=15 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

#reproject shapefiles
zip_shp <- zip_shp %>% st_transform(.,p) %>% select(zip=GEOID10) 
county_shp <- county_shp %>% st_transform(.,p) %>% select(county=NAME)

#Create zip-county conversiont table
df_county<-zip_shp %>% 
  #Limit zip to zips in this study
  mutate(zip = as.numeric(paste0(zip))) %>% 
  right_join(.,df_zip %>% select(zip)) %>% 
  #spatial join
  st_intersection(., county_shp) %>% 
  #Remove spatial information
  st_drop_geometry() %>% as_tibble()

#Estiamte proportion of area in each zip
df_county<-
  mclapply(
    #Counter for function
    seq(1,nrow(df_county)), 
    #Function         
    function(n){
      #Isolate zip and county
      t<-df_county[n,]
      zip<-zip_shp %>% filter(zip == t$zip)
      county<-county_shp %>% filter(county==t$county)
               
      #Estimate proportion of zip in county
      prop<-st_area(st_intersection(zip, county))/st_area(zip)
      t<-t %>% mutate(prop = prop)
      
      #Export
      t},
    #Number of cores
    mc.cores = detectCores()
  ) %>% 
  #Bind rows and select distinct rows
  bind_rows() %>% distinct()

#Add county info to df
df_county<-df_county %>% left_join(., df_zip)

#Distribute samples accross counties [assume equal distribution of samples accross zip]
df_county<-df_county %>% 
  #Estimate number of samples
  mutate(
    n_samples=n_samples*prop, 
    n_pos = n_pos*prop
  ) %>% 
  #Summarize by county and test
  group_by(county, test) %>% 
  summarise(n_pos = sum(n_pos), 
            n_samples = sum(n_samples))
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add disaster data--------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download disaster county designations
dz<-read_xlsx(paste0(data_dir, "Other datasets/FEMA declarations.xlsx"))

#Clean data
dz<-dz %>% 
  select(county = 'FEMA declarations') %>% 
  mutate(disaster = 1)

#Add to master dataframe
df_county<-df_county %>% 
  left_join(., dz) %>% 
  mutate(disaster = if_else(is.na(disaster), 0, disaster)) %>% distinct()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Intext calculations------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Number of counties
df_county %>% filter(n_samples>=1) %>%  select(county) %>% distinct() %>% nrow()

#Total # of samples
n_samples<-df_county %>% 
  ungroup(county) %>% 
  filter(test=='TC') %>%  
  select(n_samples) %>% 
  summarise(sum = sum(n_samples, na.rm=T))

#Number of samples from disaster counties
n_disaster<-df_county %>% 
  ungroup(county) %>% 
  filter(test=='TC') %>%  
  filter(disaster==1) %>% 
  select(n_samples) %>% 
  summarise(sum = sum(n_samples, na.rm=T))

#Proportion of samples
n_disaster/n_samples
n_disaster
n_samples

#Number of positive samples from undeclared counties
df_county %>% 
  filter(disaster==0) %>% 
  filter(test=='TC') %>% 
  ungroup(county) %>% 
  summarise(TC_pos = sum(n_pos),
            n_samples=sum(n_samples, na.rm=T))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Maps---------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Prep spatial data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create wide format tables
#Zip
tbl_zip<-df_zip %>% 
  #Pivot
  pivot_wider(id_cols=c('zip'), 
              names_from='test', 
              values_from = c('n_pos','n_samples')) %>% 
  #Estimate proportions
  mutate(
    prop_pos_EC = n_pos_EC/n_samples_EC,
    prop_pos_TC = n_pos_EC/n_samples_TC
  )

#County
tbl_county<-df_county %>% 
  pivot_wider(id_cols=c('county', 'disaster'), 
              names_from='test', 
              values_from = c('n_pos','n_samples'))%>% 
  #Estimate proportions
  mutate(
    prop_pos_EC = n_pos_EC/n_samples_EC,
    prop_pos_TC = n_pos_EC/n_samples_TC
  )

#Join tables to shape files
#Zip
zip_shp<-zip_shp %>% 
  mutate(zip=as.numeric(paste(zip))) %>% 
  right_join(.,tbl_zip)

#County
county_shp<-county_shp %>% 
  mutate(county=paste(county)) %>% 
  right_join(.,tbl_county)

#Creat Initial County Plots-----------------------------------------------------
#Turn plotting device on
#png(paste0(working_dir, "inundated_wells_county.png"), width=7,height=7, units = "in", res=300)
tmap_mode("plot")

#Create plots
county_total_EC<-tm_shape(county_shp) + 
  tm_polygons("n_samples_EC", palette = "BuGn", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)  
county_total_TC<-tm_shape(county_shp) + 
  tm_polygons("n_samples_TC", palette = "BuGn", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T) 
county_n_EC<-tm_shape(county_shp) + 
  tm_polygons("n_pos_EC", palette = 'PuRd', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
county_n_TC<-tm_shape(county_shp) +
  tm_polygons("n_pos_TC", palette = "PuRd", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)
county_prop_EC<-tm_shape(county_shp) + 
  tm_polygons("prop_pos_EC", palette = 'YlOrBr', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
county_prop_TC<-tm_shape(county_shp) +
  tm_polygons("prop_pos_TC", palette = "YlOrBr", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)

#plot
tmap_arrange(county_total_EC,
             county_total_TC,
             county_n_EC, 
             county_n_TC, 
             county_prop_EC, 
             county_prop_TC, 
             ncol=2)

# #Turn plotting device off
# dev.off()

#Creat Initial Zip Plots-------------------------------------------------------
#Turn plotting device on
#png(paste0(working_dir, "inundated_wells_zip.png"), width=7,height=7, units = "in", res=300)
tmap_mode("plot")

#Create plots
zip_total_EC<-tm_shape(zip_shp) + 
  tm_polygons("n_samples_EC", palette = "BuGn", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)  
zip_total_TC<-tm_shape(zip_shp) + 
  tm_polygons("n_samples_TC", palette = "BuGn", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T) 
zip_n_EC<-tm_shape(zip_shp) + 
  tm_polygons("n_pos_EC", palette = 'PuRd', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
zip_n_TC<-tm_shape(zip_shp) +
  tm_polygons("n_pos_TC", palette = "PuRd", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)
zip_prop_EC<-tm_shape(zip_shp) + 
  tm_polygons("prop_pos_EC", palette = 'YlOrBr', style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)    
zip_prop_TC<-tm_shape(zip_shp) +
  tm_polygons("prop_pos_TC", palette = "YlOrBr", style = 'quantile', breaks=10) +
  tm_layout(frame=F, legend.show = T)

#plot
tmap_arrange(zip_total_EC,
             zip_total_TC,
             zip_n_EC, 
             zip_n_TC, 
             zip_prop_EC, 
             zip_prop_TC, 
             ncol=2)

# #Turn plotting device off
# dev.off()