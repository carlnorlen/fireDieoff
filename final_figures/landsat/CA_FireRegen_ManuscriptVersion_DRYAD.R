# Hemes et al 2023
# PNAS

    ######## PREP environment ######
data_read='.../DRYAD/Data/'
FRAP_path = '.../CalFIRE_FRAP/FRAP_shp/firep18_1.shp' # FRAP polygons: https://frap.fire.ca.gov/mapping/gis-data
USFS_path = '.../USFS/VegBurnSeverity18_1_shp/VegBurnSeverity18.shp' # USFS polygons: https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/fseprd596279.zip

# Load packages
packages <- c("dplyr", "ggplot2", "tidyr", "stringi","varhandle","zoo","lubridate","reshape2","sf","elevatr","raster","broom","tidyverse","vroom","scales","data.table",'yardstick','patchwork','mgcv','mgcViz','RColorBrewer','fs','readxl');
lapply(packages, library, character.only = TRUE);
rm('packages')

# Define function for number of month differencing
num_mnths <- function(end_date, start_date) { # build a function to do the differencing
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

# Define function to get mode - unused right now
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Define function to scale factor function
scale1000 <- function(v) {
  v/1000
}

# FRAP perimeters, centroid, lat lon
CAfire.spatial.add_FRAP_perimeters <- function(df) { 
  global_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  FRAP_polygons <- st_read(FRAP_path) %>% 
    st_transform(global_crs) %>%
    dplyr::select(OBJECTID=OBJECTID_1, geometry) # add geometry perimeters
  FRAP_geo <- FRAP_polygons %>% # add centroid lat and lon
    mutate(centroid = st_centroid(geometry),
           OBJECTID=as.character(OBJECTID)) %>%
    unnest(centroid) %>% 
    group_by(OBJECTID) %>% 
    mutate(col=seq_along(OBJECTID)) %>% #add a column indicator
    spread(key=col, value=centroid) %>%
    rename('centroid_lat'= '2','centroid_lon'='1') 
  df <- df %>% left_join(FRAP_geo, by='OBJECTID') %>% # merge with df function input by OBJECTID
    mutate(intersects=lengths(st_intersects(geometry,geometry))-1) #,
  return(data.frame(df))
}

# USFS perimeters, centroid, lat lon
CAfire.spatial.add_USFS_perimeters <- function(df) { 
  global_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  USFS_polygons <- st_read(USFS_path, quiet=T, promote_to_multi=T) %>%
    st_transform(global_crs) %>%
    #st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>% #similifies polygons with ~1km tolerance
    filter(BURNSEV<=4 & BURNSEV>0) %>% # burnsev == 4 means high severity and burnsev == 255 means masked
    unite('OBJECTID', VB_ID, BURNSEV, sep='_',remove = FALSE) %>%
    dplyr::select(OBJECTID,geometry)
  USFS_geo <- USFS_polygons %>% # add centroid lat and lon
    mutate(centroid = st_centroid(geometry),
           OBJECTID=as.character(OBJECTID)) %>%
    unnest(centroid) %>% 
    group_by(OBJECTID) %>% 
    mutate(col=seq_along(OBJECTID)) %>% #add a column indicator
    spread(key=col, value=centroid) %>%
    rename('centroid_lat'= '2','centroid_lon'='1') %>% dplyr::select(-'3',-'4') %>%
    ungroup() %>% 
    mutate(intersects=lengths(st_intersects(geometry,geometry))-1)  %>%
    st_drop_geometry() 
  df <- df %>% left_join(USFS_geo, by='OBJECTID') #%>% # merge with df function input by OBJECTID
  return(data.frame(df))
}

# spatial attributes - state and counties
global_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
CA_boundary <- map_data("state",region='california') %>%
  st_as_sf(coords = c("long", "lat"), crs = global_crs) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

    ######## FRAP data frames ########
    # IMPORT
files = dir_ls(path=paste0(data_read,'FRAP/'),glob = '*csv') 
CAfire.annual_FRAP_LS578 <- vroom(files,col_select=list(-'system:index',-'.geo',-'time'))

    # DATA transform
CAfire.data <- CAfire.annual_FRAP_LS578 %>%  # data
  mutate(firestartdate=ymd(YEAR_,truncated = 2L),
         firestartyear = year(firestartdate)) %>%
  mutate(date=ymd(date),
         year = year(date),
         yrs_sincefire = year-firestartyear, 
         area = Shape_Area) %>%
  filter(!is.na(firestartyear)) %>% 
  mutate(emaprlc_mean = round(emaprlc_mode,0),
         CA_Own = round(CA_Own_mode,0),
         EPAzone = round(EPA_l3zone_mode,0),
         Burn1=FRAP_1burn_sum/FRAP_1burn_count, Burn2=FRAP_2burn_sum/FRAP_2burn_count,
         Burn3=FRAP_3burn_sum/FRAP_3burn_count, Burn4=FRAP_4burn_sum/FRAP_4burn_count, Burn5=FRAP_5burn_sum/FRAP_5burn_count,
         pptanom_mean = pptsum_mean/clm_precip_sum_mean) %>%
  dplyr::select(OBJECTID,FIRE_NAME,AGENCY,firestartdate,firestartyear,date,year,yrs_sincefire,area, # fire metadata stuff
                GPP_mean=GPPgf_mean,GPP_std = GPPgf_stdDev,
                NBR_mean,NBR_std = NBR_stdDev,
                ET_mean, ET_std = ET_stdDev,
                Burn1,Burn2,Burn3,Burn4,Burn5,
                emaprlc_mean,CA_Own,EPAzone,emaprbm_mean=emapr_biomass_mean,emaprbm_std=emapr_biomass_stdDev,#cancover_mean,cancover_std=cancover_stdDev, # derived biomass and can cover products
                lemmabm_mean=lemma_biomass_mean,lemmabm_std=lemma_biomass_stdDev,
                elevation_mean,aspect_mean,slope_mean,clm_temp_mean_mean,clm_precip_sum_mean,pptanom_mean) %>% # geo and climate 
  mutate_at(vars(-OBJECTID,-FIRE_NAME,-AGENCY,-firestartdate,-firestartyear,-date,-year,-yrs_sincefire,-area,-Burn1,-Burn2,-Burn3,-Burn4,-Burn5,-pptanom_mean),scale1000) %>%
  pivot_longer(cols=c(GPP_mean,GPP_std,
                      ET_mean, ET_std,
                      NBR_mean,NBR_std, # VIs
                      emaprlc_mean,emaprbm_mean,emaprbm_std,lemmabm_mean,lemmabm_std,pptanom_mean),names_to='Index',values_to='Index_value') %>%
  separate(col=Index,into=c('Index','stat'),sep='_') %>% 
  pivot_wider(names_from='stat',values_from='Index_value',names_prefix = "Index_value_") %>% # new columns with std
  mutate(OBJECTID=as.character(OBJECTID),Index=as.character(Index),
                             CA_Own = as.factor(recode_factor(CA_Own,'0'='Local','1'='Non-Profit','2'='CA State',
                                                              '3'='CA State','4'='CA State','5'='CA State',
                                                              '6'='Federal','7'='Federal','8'='Federal',
                                                              '9'='Federal','10'='Federal','11'='Federal',
                                                              '12'='Federal','13'='Federal','14'='Federal','20'='Private')),
                             EPAzone = as.factor(recode_factor(EPAzone,'1'='CoastRange','4'='Cascades','5'='SierraNevada',
                                                               '6'='CentCAfthills','7'='CentralValley',
                                                               '8'='SouthCAmtns','9'='EasternCascdSlopes',
                                                               '13'='CentralBasinRange','14'='MojaveBasinRange',
                                                               '78'='Klamath','81'='SonoranBasinRange','85'='SoCAcoast'))) %>%
  dplyr::select(OBJECTID,FIRE_NAME,AGENCY,firestartdate,firestartyear,year,yrs_sincefire,Index,Index_value_mean,Index_value_std,everything())

rm(CAfire.annual_FRAP_LS578)

    # prefire_emapr_lc: find pre-fire land cover emapr for each fire
CAfire.data.date_pre_emapr_lc <- filter(CAfire.data,Index=='emaprlc') %>%    
  group_by(OBJECTID) %>%              
  summarize(prefire_emapr_lc = case_when(min(yrs_sincefire)<0 ~ Index_value_mean[(yrs_sincefire>-4)], 
                                      min(yrs_sincefire)>=0 ~ Index_value_mean[which.min(yrs_sincefire)])) %>%
  distinct(OBJECTID,.keep_all=T)
CAfire.data <- left_join(CAfire.data,CAfire.data.date_pre_emapr_lc,by='OBJECTID') %>% 
  mutate(isforest = case_when(prefire_emapr_lc%in%c(3,4,5) ~ 'forest', 
                              prefire_emapr_lc%in%c(6) ~ 'shrub',
                              prefire_emapr_lc%in%c(7) ~ 'herb',
                              TRUE ~ NA_character_))
rm(CAfire.data.date_pre_emapr_lc)

  # postfire_ppt_anom: post-fire 3 or 5 yr precip anomaly
CAfire.data.postfire_pptanom <- filter(CAfire.data,Index=='pptanom') %>%    # subset for the 3 yrs before fire
  group_by(OBJECTID) %>%              # group by Fire_ID, Index, actual year of the image
  summarize(postfire3_pptanom = mean(Index_value_mean[(yrs_sincefire>0&yrs_sincefire<4)]),
            postfire5_pptanom = mean(Index_value_mean[(yrs_sincefire>0&yrs_sincefire<6)])) %>% # when we have pre-fire data (fire occured after 1984), get the mode of the three years pre-fire
  mutate(postfire3_pptanom_bin=cut(postfire3_pptanom,breaks=c(-Inf,0.5,0.75,1,1.25,1.5,Inf),include.lowest=TRUE),
         postfire5_pptanom_bin=cut(postfire5_pptanom,breaks=c(-Inf,0.5,0.75,1,1.25,1.5,Inf),include.lowest=TRUE))
CAfire.data <- left_join(CAfire.data,CAfire.data.postfire_pptanom,by='OBJECTID') 

      # METADATA
CAfire.metadata <- CAfire.data %>% # metadata
  dplyr::select(OBJECTID,FIRE_NAME,AGENCY,firestartdate,firestartyear,prefire_emapr_lc,isforest,area,Burn1,Burn2,Burn3,Burn4,Burn5,CA_Own,EPAzone,elevation_mean,aspect_mean,slope_mean,clm_temp_mean_mean,clm_precip_sum_mean,postfire3_pptanom_bin,postfire5_pptanom_bin) %>% #elevation_p50,aspect_p50,slope_p50) %>%
  distinct(OBJECTID,.keep_all=TRUE) %>%
  CAfire.spatial.add_FRAP_perimeters() %>% # add county to this?
  mutate(area_bin = cut_number(area,5,labels=c('smallest','small','mid','large','largest'),ordered_result=FALSE),
         elevation_bin = cut_number(elevation_mean,5,labels=c('lowest','low','mid','high','highest'),ordered_result=FALSE),
         slope_bin = cut_number(slope_mean,5,labels=c('flattest','flat','mid','steep','steepest'),ordered_result=FALSE),
         aspect_bin = cut(aspect_mean,breaks=c(0,90,180,270,360),labels=c('NE','SE','SW','NW'),ordered_result=FALSE),
         intersects_bin = cut(intersects,breaks=c(-Inf,0,2,5,10,Inf),include.lowest=TRUE),
         OBJECTID=as.character(OBJECTID),
         firestartdate_bin = cut(firestartyear,breaks=seq(1920,2020,10),labels=seq(1920,2010,10),ordered_result=TRUE,include.lowest=TRUE),
         Burn1_bin=cut(1-Burn1,breaks=seq(0,1,0.2),labels=c('0-20%','20-40%','40-60%','60-80%','80-100%'),ordered_result=TRUE,include.lowest=TRUE)) %>%
  group_by(isforest) %>%
  mutate(temp_bin = cut_number(clm_temp_mean_mean,5,labels=c('coldest','cold','mid','hot','hottest'),ordered_result=FALSE),
         precip_bin = cut_number(clm_precip_sum_mean,5,labels=c('driest','dry','mid','wet','wettest'),ordered_result=FALSE)) %>%
  ungroup() 

CAfire.metadata_nogeo <- dplyr::select(CAfire.metadata,-geometry) 

  #### CONTROL MATCHING

 # prep for matching
elev_bin_size=100
precip_bin_size=200
temp_bin_size=5
lat_bin_size=0.25*100
CAfire.controls.to_be_matched <- CAfire.metadata_nogeo %>% 
  dplyr::select(all_of(c('OBJECTID','clm_precip_sum_mean', 'clm_temp_mean_mean', 'elevation_mean','centroid_lat'))) %>%
  mutate(elevbin = as.numeric((ceiling(elevation_mean/elev_bin_size))*elev_bin_size),
         precipbin = as.numeric((ceiling(clm_precip_sum_mean/precip_bin_size)-1)*precip_bin_size),
         tempbin = as.numeric((ceiling(clm_temp_mean_mean/temp_bin_size)-1)*temp_bin_size),
         latbin = as.numeric(as.character(cut(centroid_lat*100, breaks=seq(32.5*100, 42.5*100, lat_bin_size),labels=seq(32.50*100, 42.25*100, lat_bin_size))))) %>%
  mutate_at(vars(elevbin,precipbin,tempbin,latbin),as.integer) %>%
  dplyr::select(-starts_with('clm'),-elevation_mean,-centroid_lat)  

 # ingest matched controls
controls <- read_csv(file=paste0(data_read,'Controls/firesMatchedToControls_200pts_20220623.csv')) %>%
  mutate(latbin=latbin*100) %>%
  rename('N' = '2_NBR_N') %>%
  dplyr::select(-V1) %>%
  pivot_longer(cols=-c(latbin,elevbin,precipbin,tempbin,OBJECTID,N),names_to='Index',values_to='Index_value') %>%
  separate(col=Index,into=c('year','Index','stat'),sep='_') %>% 
  mutate(year=as.integer(as.numeric(year)+1984)) %>%
  mutate_at(vars(elevbin,precipbin,tempbin,latbin),as.integer) %>%
  pivot_wider(names_from='stat',values_from='Index_value') %>% # new columns with std
  mutate(se_mean=visd/sqrt(N)) %>% # standard error
  mutate(Index=recode(Index,GPPgf='GPP')) %>%
  rename(spconmean='vimean',spconmed='vimedian',spconsd='visd',spconn='N',spconse_mean='se_mean') 

  # calculate prefire mean for data.diff (diff_mean) 
prefire_mean <- CAfire.data %>%
  filter(yrs_sincefire<0) %>%
  group_by(OBJECTID,Index,firestartyear) %>%
  summarize(prefire_fullmean=mean(Index_value_mean,na.rm=TRUE)) %>% # what happens to NA's ? should this be na.rm=TRUE?
  ungroup() 

  #### CREATE DATA.DIFF
CAfire.data.diff <- CAfire.data %>% 
  filter(Index%in%c('GPP','NBR')) %>%
  dplyr::select(OBJECTID,firestartyear,year,yrs_sincefire,Index,Index_value_mean,Index_value_std,isforest,area) %>%
  left_join(prefire_mean,by=c('OBJECTID','Index','firestartyear')) %>%
  left_join(controls,by = c('OBJECTID','Index','year')) %>%
  dplyr::select(-c(latbin,elevbin,tempbin,precipbin)) %>%
  mutate(diff_mean = case_when(yrs_sincefire>0 ~ Index_value_mean-prefire_fullmean,  # only for post-fire years
                               yrs_sincefire<=0 ~ NA_real_),
         diff_mean_perc = case_when(yrs_sincefire>0 ~ (Index_value_mean-prefire_fullmean)/prefire_fullmean*100,  # only for post-fire years
                                    yrs_sincefire<=0 ~ NA_real_),
         diff_spatial = case_when(yrs_sincefire>0 ~ Index_value_mean-spconmean,  # any year now with spatial controls
                                  yrs_sincefire<=0 ~ NA_real_),
         diff_spatial_perc = case_when(yrs_sincefire>0 ~ (Index_value_mean-spconmean)/spconmean*100,
                                       yrs_sincefire<=0 ~ NA_real_),
         diff_spatial_stderror = case_when(yrs_sincefire>0 ~ sqrt(((Index_value_std/sqrt(area/30^2))^2)+(spconse_mean^2)),  # propogate SE's: sqrt(SE^2 + SE^2) = sqrt((SD/sqrt(n))^2 + (SD/sqrt(n))^2)... https://www.dummies.com/article/academics-the-arts/science/biology/simple-error-propagation-formulas-for-simple-expressions-149357/
                                           yrs_sincefire<=0 ~ NA_real_),
         diff_spatial_stderror_wmodfor = case_when(yrs_sincefire>0 ~ sqrt(((Index_value_std/sqrt(area/30^2))^2)+(spconse_mean^2)+(134.0147^2)+(281.4931^2)),  # propogate SE's: sqrt(SE^2 + SE^2) = sqrt((SD/sqrt(n))^2 + (SD/sqrt(n))^2)... https://www.dummies.com/article/academics-the-arts/science/biology/simple-error-propagation-formulas-for-simple-expressions-149357/
                         yrs_sincefire<=0 ~ NA_real_)) %>%
  left_join(dplyr::select(CAfire.metadata_nogeo,-isforest,-area), by = c('OBJECTID','firestartyear'))  # merge with metadata_nogeo

rm(prefire_mean)

# fire severity metric
dNBR_prefire <- filter(CAfire.data,Index=='NBR',yrs_sincefire%in%c(-1,1)) %>%
  pivot_wider(id_cols=c(OBJECTID,firestartyear,isforest,prefire_emapr_lc),names_from=yrs_sincefire,values_from=c(Index_value_mean,Index_value_std)) %>%
  mutate(dNBR_prefire=(`Index_value_mean_1`-`Index_value_mean_-1`), #dNBR based on year after - year before fire
         dNBR_prefire_perc = (`Index_value_mean_1`-`Index_value_mean_-1`)/`Index_value_mean_-1`*100)
# difference based on spatial control conditions
dNBR_spatcon <- subset(CAfire.data.diff,Index=='NBR'&yrs_sincefire%in%c(1)) %>% 
  mutate(dNBR_spcon = diff_spatial,
         dNBR_spcon_perc = diff_spatial_perc)

# left join with CAfire.data.diff, and bin
CAfire.data.diff <- left_join(CAfire.data.diff,dplyr::select(dNBR_prefire,c(OBJECTID,dNBR_prefire,dNBR_prefire_perc)),by='OBJECTID') %>%
  left_join(dplyr::select(dNBR_spatcon,c(OBJECTID,dNBR_spcon,dNBR_spcon_perc)),by='OBJECTID') %>%
  mutate(dNBR_prefire_bin = cut_number(dNBR_prefire,5,labels=c('high_severity','more_severe','moderate','less_severe','low_severity'),ordered_result=FALSE),
         dNBR_spcon_bin = cut_number(dNBR_spcon,5, labels = c('high_severity','more_severe','moderate','less_severe','low_severity'),ordered_result = FALSE),
         dNBR_prefire_perc_bin = cut_number(dNBR_prefire_perc,5, labels = c('high_severity','more_severe','moderate','less_severe','low_severity'),ordered_result = FALSE),
         dNBR_spcon_perc_bin = cut_number(dNBR_spcon_perc,5, labels = c('high_severity','more_severe','moderate','less_severe','low_severity'),ordered_result = FALSE))

# dIXyr1
dIXyr1 <- filter(CAfire.data.diff,yrs_sincefire==1) %>% 
  mutate(dIXyr1 = diff_spatial[yrs_sincefire==1]) %>% dplyr::select(OBJECTID,Index,dIXyr1)
CAfire.data.diff <- left_join(CAfire.data.diff,dIXyr1,by=c('OBJECTID','Index'))


    ######## USFS data frames ########
# IMPORT
USFSfiles = dir_ls(path=paste0(data_read,'USFS/'),glob = '*csv')
CAfire.annual_USFS_LS578 <- vroom(USFSfiles,col_select=list(-'system:index',-'.geo',-'time'))

CAfireUSFS.data <- CAfire.annual_USFS_LS578 %>%  # USFS data
  distinct() %>% 
  filter(BURNSEV<=4 & BURNSEV>0) %>% # burnsev == 4 means high severity and burnsev == 255 means masked
  mutate(firestartdate=ymd(FIRE_YEAR, truncated = 2L),
         firestartyear = year(firestartdate)) %>%
  mutate(date=ymd(date),
         year = year(date),
         yrs_sincefire = year-firestartyear, 
         area = Shape_Area) %>%
  filter(!is.na(firestartyear)) %>% # none
  mutate(emaprlc_mean = round(emaprlc_mode,0),
         CA_Own = round(CA_Own_mode,0),
         EPAzone = round(EPA_l3zone_mode,0),
         Burn1=FRAP_1_sum/FRAP_1_count, 
         pptanom_mean = pptsum_mean/clm_precip_sum_mean) %>%
  mutate(AGENCY='NA', FIRE_NAME=str_sub(VB_ID,5,-1)) %>%
  unite('OBJECTID', VB_ID, BURNSEV, sep='_',remove = FALSE) %>% # specific to USFS
  dplyr::select(OBJECTID,FIRE_NAME,AGENCY,firestartdate,firestartyear,date,year,yrs_sincefire,area, 
                ASSESS_TYP, BEST_ASSES, BURNSEV, # special USFS stuff
                GPP_mean=GPPgf_mean, GPP_std = GPPgf_stdDev,
                NBR_mean,NBR_std = NBR_stdDev,
                ET_mean, ET_std = ET_stdDev,
                Burn1,
                emaprlc_mean,CA_Own,EPAzone,emaprbm_mean=emapr_biomass_mean,emaprbm_std=emapr_biomass_stdDev, 
                lemmabm_mean=lemma_biomass_mean,lemmabm_std=lemma_biomass_stdDev,
                elevation_mean,aspect_mean,slope_mean,clm_temp_mean_mean,clm_precip_sum_mean,pptanom_mean) %>%  
  mutate_at(vars(-OBJECTID,-FIRE_NAME,-AGENCY,-firestartdate,-firestartyear,-date,-year,-yrs_sincefire,-area,-ASSESS_TYP, -BEST_ASSES, -BURNSEV, -pptanom_mean, -Burn1),scale1000) %>%
  pivot_longer(cols=c(GPP_mean,GPP_std,
                      ET_mean, ET_std,
                      NBR_mean,NBR_std, # VIs
                      emaprlc_mean,emaprbm_mean,emaprbm_std,lemmabm_mean,lemmabm_std,pptanom_mean),names_to='Index',values_to='Index_value') %>%
  separate(col=Index,into=c('Index','stat'),sep='_') %>% 
  pivot_wider(names_from='stat',values_from='Index_value',names_prefix = "Index_value_") %>%
  mutate(OBJECTID=as.character(OBJECTID),Index=as.character(Index),
         CA_Own = as.factor(recode_factor(CA_Own,'0'='Local','1'='Non-Profit','2'='CA State',
                                          '3'='CA State','4'='CA State','5'='CA State',
                                          '6'='Federal','7'='Federal','8'='Federal',
                                          '9'='Federal','10'='Federal','11'='Federal',
                                          '12'='Federal','13'='Federal','14'='Federal','20'='Private')),
         EPAzone = as.factor(recode_factor(EPAzone,'1'='CoastRange','4'='Cascades','5'='SierraNevada',
                                           '6'='CentCAfthills','7'='CentralValley',
                                           '8'='SouthCAmtns','9'='EasternCascdSlopes',
                                           '13'='CentralBasinRange','14'='MojaveBasinRange',
                                           '78'='Klamath','81'='SonoranBasinRange','85'='SoCAcoast'))) %>%
  dplyr::select(OBJECTID,FIRE_NAME,AGENCY,firestartdate,firestartyear,year,yrs_sincefire,ASSESS_TYP, BEST_ASSES, BURNSEV, Index,Index_value_mean,Index_value_std,everything())

rm(CAfire.annual_USFS_LS578)

# prefire_emapr_lc: find pre-fire land cover emapr for each fire
CAfireUSFS.data.date_pre_emapr_lc <- filter(CAfireUSFS.data,Index=='emaprlc') %>%    # subset for the 3 yrs before fire
  group_by(OBJECTID) %>%              # group by Fire_ID, Index, actual year of the image
  summarize(prefire_emapr_lc = case_when(min(yrs_sincefire)<0 ~ Index_value_mean[(yrs_sincefire>-4)], # when we have pre-fire data (fire occured after 1984), get the mode of the three years pre-fire
                                         min(yrs_sincefire)>=0 ~ Index_value_mean[which.min(yrs_sincefire)])) %>%
  distinct(OBJECTID,.keep_all=T)# when we dont have pre-fire data (fire occured before 1984), assign the earliest year's (min of yrs since fire) lc as the prefire emapr lc
CAfireUSFS.data <- left_join(CAfireUSFS.data,CAfireUSFS.data.date_pre_emapr_lc,by='OBJECTID') %>% # join with data
  mutate(isforest = case_when(prefire_emapr_lc%in%c(3,4,5) ~ 'forest', 
                              prefire_emapr_lc%in%c(6) ~ 'shrub',
                              prefire_emapr_lc%in%c(7) ~ 'herb',
                              TRUE ~ NA_character_))
rm(CAfireUSFS.data.date_pre_emapr_lc)

# postfire_ppt_anom: post-fire 3 or 5 yr precip anomaly
CAfireUSFS.data.postfire_pptanom <- filter(CAfireUSFS.data,Index=='pptanom') %>%    # subset for the 3 yrs before fire
  group_by(OBJECTID) %>%              # group by Fire_ID, Index, actual year of the image
  summarize(postfire3_pptanom = mean(Index_value_mean[(yrs_sincefire>0&yrs_sincefire<4)]),
            postfire5_pptanom = mean(Index_value_mean[(yrs_sincefire>0&yrs_sincefire<6)])) %>% 
  mutate(postfire3_pptanom_bin=cut(postfire3_pptanom,breaks=c(-Inf,0.5,0.75,1,1.25,1.5,Inf),include.lowest=TRUE),
         postfire5_pptanom_bin=cut(postfire5_pptanom,breaks=c(-Inf,0.5,0.75,1,1.25,1.5,Inf),include.lowest=TRUE))
CAfireUSFS.data <- left_join(CAfireUSFS.data,CAfireUSFS.data.postfire_pptanom,by='OBJECTID') 

# METADATA
CAfireUSFS.metadata <- CAfireUSFS.data %>% # metadata
  dplyr::select(OBJECTID,FIRE_NAME,AGENCY,firestartdate,firestartyear,ASSESS_TYP,BEST_ASSES,BURNSEV,prefire_emapr_lc,isforest,area,CA_Own,EPAzone,elevation_mean,aspect_mean,slope_mean,clm_temp_mean_mean,clm_precip_sum_mean,postfire3_pptanom_bin,postfire5_pptanom_bin,Burn1) %>% #elevation_p50,aspect_p50,slope_p50) %>%
  distinct(OBJECTID,.keep_all=T) %>%
        CAfire.spatial.add_USFS_perimeters() %>% 
  drop_na(centroid_lon,centroid_lat) %>% 
  mutate(area_bin = cut_number(area,5,labels=c('smallest','small','mid','large','largest'),ordered_result=FALSE),
         elevation_bin = cut_number(elevation_mean,5,labels=c('lowest','low','mid','high','highest'),ordered_result=FALSE),
         slope_bin = cut_number(slope_mean,5,labels=c('flattest','flat','mid','steep','steepest'),ordered_result=FALSE),
         aspect_bin = cut(aspect_mean,breaks=c(0,90,180,270,360),labels=c('NE','SE','SW','NW'),ordered_result=FALSE),
         intersects_bin = cut(intersects,breaks=c(-Inf,0,2,5,10,Inf),include.lowest=TRUE),
         OBJECTID=as.character(OBJECTID),
         firestartdate_bin = cut(firestartyear,breaks=seq(1920,2020,10),labels=seq(1920,2010,10),ordered_result=TRUE,include.lowest=TRUE),
         Burn1_bin=cut(1-Burn1,breaks=seq(0,1,0.2),labels=c('0-20%','20-40%','40-60%','60-80%','80-100%'),ordered_result=TRUE,include.lowest=TRUE)) %>%
  group_by(isforest) %>%
  mutate(temp_bin = cut_number(clm_temp_mean_mean,5,labels=c('coldest','cold','mid','hot','hottest'),ordered_result=FALSE),
         precip_bin = cut_number(clm_precip_sum_mean,5,labels=c('driest','dry','mid','wet','wettest'),ordered_result=FALSE)) %>%
  ungroup() 

CAfireUSFS.metadata_nogeo <- CAfireUSFS.metadata 

# CONTROL MATCHING
CAfireUSFS.controls.to_be_matched <- CAfireUSFS.metadata_nogeo %>% 
  filter(!is.na(centroid_lat)) %>%
  dplyr::select(all_of(c('OBJECTID','clm_precip_sum_mean', 'clm_temp_mean_mean', 'elevation_mean','centroid_lat'))) %>%
  mutate(elevbin = as.numeric((ceiling(elevation_mean/elev_bin_size))*elev_bin_size),
         precipbin = as.numeric((ceiling(clm_precip_sum_mean/precip_bin_size)-1)*precip_bin_size),
         tempbin = as.numeric((ceiling(clm_temp_mean_mean/temp_bin_size)-1)*temp_bin_size),
         latbin = as.numeric(as.character(cut(centroid_lat*100, breaks=seq(32.5*100, 42.5*100, lat_bin_size),labels=seq(32.50*100, 42.25*100, lat_bin_size))))) %>%
  mutate_at(vars(elevbin,precipbin,tempbin,latbin),as.integer) %>%
  dplyr::select(-starts_with('clm'),-elevation_mean,-centroid_lat) 

prefire_mean <- CAfireUSFS.data %>%
  filter(yrs_sincefire<0) %>%
  group_by(OBJECTID,Index,firestartyear) %>%
  summarize(prefire_fullmean=mean(Index_value_mean,na.rm=TRUE)) %>% 
  ungroup() 

# CREATE DATA.DIFF 
CAfireUSFS.data.diff <- CAfireUSFS.data %>% 
  filter(Index%in%c('GPP','NBR')) %>%
  dplyr::select(OBJECTID,firestartyear,year,yrs_sincefire,Index,Index_value_mean,Index_value_std,isforest,area) %>%
  left_join(prefire_mean,by=c('OBJECTID','Index','firestartyear')) %>%
  left_join(controls,by = c('OBJECTID','Index','year')) %>%
  dplyr::select(-c(latbin,elevbin,tempbin,precipbin)) %>%
  mutate(diff_mean = case_when(yrs_sincefire>0 ~ Index_value_mean-prefire_fullmean, 
                               yrs_sincefire<=0 ~ NA_real_),
         diff_mean_perc = case_when(yrs_sincefire>0 ~ (Index_value_mean-prefire_fullmean)/prefire_fullmean*100,  
                                    yrs_sincefire<=0 ~ NA_real_),
         diff_spatial = case_when(yrs_sincefire>0 ~ Index_value_mean-spconmean,  
                                  yrs_sincefire<=0 ~ NA_real_),
         diff_spatial_perc = case_when(yrs_sincefire>0 ~ (Index_value_mean-spconmean)/spconmean*100,
                                       yrs_sincefire<=0 ~ NA_real_),
         diff_spatial_stderror = case_when(yrs_sincefire>0 ~ sqrt(((Index_value_std/sqrt(area/30^2))^2)+(spconse_mean^2)),  
                                           yrs_sincefire<=0 ~ NA_real_),
         diff_spatial_stderror_wmodfor = case_when(yrs_sincefire>0 ~ sqrt(((Index_value_std/sqrt(area/30^2))^2)+(spconse_mean^2)+(134.0147^2)+(281.4931^2)),
                                        yrs_sincefire<=0 ~ NA_real_)) %>%
  left_join(dplyr::select(CAfireUSFS.metadata_nogeo,-isforest,-area), by = c('OBJECTID','firestartyear'))  

rm(prefire_mean)

# dIXyr1
dIXyr1 <- filter(CAfireUSFS.data.diff,yrs_sincefire==1) %>% 
  mutate(dIXyr1 = diff_spatial[yrs_sincefire==1]) %>% dplyr::select(OBJECTID,Index,dIXyr1)
CAfireUSFS.data.diff <- left_join(CAfireUSFS.data.diff,dIXyr1,by=c('OBJECTID','Index'))

    ######## FIGURES MAIN ########

### Fig 1 - Forest Tile Short ###
fig1_data <- CAfire.data.diff %>%
  arrange(firestartyear) %>%
  mutate(OBJECTID = factor(OBJECTID, unique(OBJECTID))) %>%
  filter(Index%in%c('GPP')&isforest%in%c('forest')&firestartyear>1984)
fig1_a_short <- ggplot(fig1_data) + 
  geom_tile(aes(yrs_sincefire,OBJECTID,fill=Index_value_mean)) +
  scale_fill_gradient(low='white',high='darkgreen',limits=c(0,1800),oob=squish) +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.position="top", legend.box = "horizontal") +
  labs(x='years since fire',y='Fires',
       fill=expression(GPP~(gC~m^-2~year^-1)))
fig1_b_short <- ggplot(fig1_data) + 
  geom_tile(aes(yrs_sincefire,OBJECTID,fill=diff_spatial)) +
  scale_fill_gradient2(low='orange',mid = "white",high='deepskyblue2',limits=c(-500,500),oob=squish) +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),
        legend.position="top", legend.box = "horizontal") +
  labs(x='years since fire',
    subtitle = paste0("Forest, n= ",length(unique(subset(fig1_data)$OBJECTID)),' fires'),
    fill=expression(dGPP~(gC~m^-2~year^-1)))
(fig1_a_short | fig1_b_short) + plot_annotation(tag_levels='a')


### Fig 2 - Recovery Curves  ###
fig2_data <-  subset(CAfire.data.diff,Index%in%c('GPP')&isforest%in%c('forest'))
      # 2a
CAfire.data.diff.annual <- fig2_data %>%
  group_by(yrs_sincefire) %>%
  summarize(dGPP_annual_mean=mean(diff_spatial,na.rm=T),
            dGPP_annual_SE_w_n_modfor = sqrt(sum((diff_spatial_stderror_wmodfor^2),na.rm=T))/sum(!is.na(diff_spatial)), # no divide by n
            dGPP_annual_n = sum(!is.na(diff_spatial)))
fig2a <- ggplot(CAfire.data.diff.annual) + 
  geom_bin2d(data=filter(CAfire.data.diff,isforest%in%'forest'&Index%in%'GPP'),aes(yrs_sincefire,diff_spatial),binwidth=c(1,15)) +
  scale_fill_gradient(low="white", high="red") +
  geom_hline(yintercept = 0, color = "black", size=0.5,linetype = 1) + 
  geom_ribbon(data=CAfire.data.diff.annual,aes(x=yrs_sincefire,ymin=dGPP_annual_mean-1.96*dGPP_annual_SE_w_n_modfor,ymax=dGPP_annual_mean+1.96*dGPP_annual_SE_w_n_modfor),outline.type='full',fill='grey70',alpha=0.6) +
  geom_line(data=CAfire.data.diff.annual,aes(yrs_sincefire,dGPP_annual_mean),color='black',size=0.5) +
  coord_cartesian(ylim=c(-400,200),xlim=c(1,75)) + 
  labs(y=expression(dGPP~(gC~m^-2~yr^-1)),x='years since fire') +
  theme(legend.position = 'right', legend.box = "vertical") +
  theme_bw()
   
  # 2b
CAfire.data.diff.annual.area <- fig2_data %>%
  group_by(yrs_sincefire,area_bin) %>%
  summarize(dGPP_annual_mean=mean(diff_spatial,na.rm=T),
            dGPP_annual_SE_w_n_modfor = sqrt(sum((diff_spatial_stderror_wmodfor^2),na.rm=T))/sum(!is.na(diff_spatial)),
            dGPP_annual_n = sum(!is.na(diff_spatial)))
fig2b <- ggplot(CAfire.data.diff.annual.area) + 
  geom_hline(yintercept = 0, color = "black", size=0.5,linetype = 1) + 
  geom_ribbon(aes(x=yrs_sincefire,ymin=dGPP_annual_mean-1.96*dGPP_annual_SE_w_n_modfor,ymax=dGPP_annual_mean+1.96*dGPP_annual_SE_w_n_modfor,group=area_bin,fill=area_bin),outline.type='full',alpha=0.2) +
  geom_line(aes(yrs_sincefire,dGPP_annual_mean,color=area_bin),size=0.5) + 
  scale_color_manual(name="Area",
                     labels=c("smallest", "small", "mid","large","largest"),
                     values = brewer.pal(6,"PuRd")[2:6]) +
  scale_fill_manual(name="Area",
                     labels=c("smallest", "small", "mid","large","largest"),
                     values = brewer.pal(6,"PuRd")[2:6]) +
  coord_cartesian(ylim=c(-400,200),xlim=c(1,75)) + 
  labs(y=expression(dGPP~(gC~m^-2~yr^-1)),x='',color='Area') +
  theme(axis.title.x=element_blank()) +
  theme_bw()

# 2c
CAfire.data.diff.annual.temp <- fig2_data %>%
  group_by(yrs_sincefire,temp_bin) %>%
  summarize(dGPP_annual_mean=mean(diff_spatial,na.rm=T),
            #dGPP_annual_SE_w_n = sqrt(sum((diff_spatial_stderror^2),na.rm=T))/sum(!is.na(diff_spatial_stderror)), # with divide by n
            dGPP_annual_SE_w_n_modfor = sqrt(sum((diff_spatial_stderror_wmodfor^2),na.rm=T))/sum(!is.na(diff_spatial)), # no divide by n
            dGPP_annual_n = sum(!is.na(diff_spatial)))
fig2c <- ggplot(CAfire.data.diff.annual.temp) + 
  geom_hline(yintercept = 0, color = "black", size=0.5,linetype = 1) + 
  geom_ribbon(aes(x=yrs_sincefire,ymin=dGPP_annual_mean-1.96*dGPP_annual_SE_w_n_modfor,ymax=dGPP_annual_mean+1.96*dGPP_annual_SE_w_n_modfor,group=temp_bin,fill=temp_bin),outline.type='full',alpha=0.2) +
  geom_line(aes(yrs_sincefire,dGPP_annual_mean,color=temp_bin),size=0.5) + 
  scale_color_brewer(name="Temp Normal",
                     palette = "RdYlBu",direction=-1) +
  scale_fill_brewer(name="Temp Normal",
                     palette = "RdYlBu",direction=-1) +
  coord_cartesian(ylim=c(-400,200),xlim=c(1,75)) + 
  labs(y=expression(dGPP~(gC~m^-2~yr^-1)),x='years since fire',color='Temp') +
  theme(axis.title.x=element_blank()) +
  theme_bw()

# 2d
fig2d_data <- fig2_data %>%
  mutate(obs_decade_bin = cut(year,breaks=c(1980,1990,2000,2010,2020),labels=c('1980','1990','2000','2010'),ordered_result=FALSE,include.lowest=TRUE))
CAfire.data.diff.annual.dec <- fig2d_data %>%
  group_by(yrs_sincefire,obs_decade_bin) %>%
  summarize(dGPP_annual_mean=mean(diff_spatial,na.rm=T),
            #dGPP_annual_SE_w_n = sqrt(sum((diff_spatial_stderror^2),na.rm=T))/sum(!is.na(diff_spatial_stderror)), # with divide by n
            dGPP_annual_SE_w_n_modfor = sqrt(sum((diff_spatial_stderror_wmodfor^2),na.rm=T))/sum(!is.na(diff_spatial)), # no divide by n
            dGPP_annual_n = sum(!is.na(diff_spatial)))
fig2d <- ggplot(CAfire.data.diff.annual.dec) + 
  geom_hline(yintercept = 0, color = "black", size=0.5,linetype = 1) + 
  geom_ribbon(aes(x=yrs_sincefire,ymin=dGPP_annual_mean-1.96*dGPP_annual_SE_w_n_modfor,ymax=dGPP_annual_mean+1.96*dGPP_annual_SE_w_n_modfor,group=obs_decade_bin,fill=obs_decade_bin),outline.type='full',alpha=0.2) +
  geom_line(aes(yrs_sincefire,dGPP_annual_mean,color=obs_decade_bin),size=0.5) + 
  scale_color_manual(values = colorRampPalette(brewer.pal(6, "BuPu"))(6)[3:6],name="Observation\nDecade") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(6, "BuPu"))(6)[3:6],name="Observation\nDecade") +
  coord_cartesian(ylim=c(-400,200),xlim=c(1,75)) + 
  labs(y=expression(dGPP~(gC~m^-2~yr^-1)),x='',color='Observation\nDecade') +
  theme(axis.title.x=element_blank()) +
  theme_bw()

# 2e
CAfire.data.diff.annual.precip <- fig2d_data %>%
  group_by(yrs_sincefire,precip_bin) %>%
  summarize(dGPP_annual_mean=mean(diff_spatial,na.rm=T),
            #dGPP_annual_SE_w_n = sqrt(sum((diff_spatial_stderror^2),na.rm=T))/sum(!is.na(diff_spatial_stderror)), # with divide by n
            dGPP_annual_SE_w_n_modfor = sqrt(sum((diff_spatial_stderror_wmodfor^2),na.rm=T))/sum(!is.na(diff_spatial)), # no divide by n
            dGPP_annual_n = sum(!is.na(diff_spatial)))
fig2e <- ggplot(CAfire.data.diff.annual.precip) + 
  geom_hline(yintercept = 0, color = "black", size=0.5,linetype = 1) + 
  geom_ribbon(aes(x=yrs_sincefire,ymin=dGPP_annual_mean-1.96*dGPP_annual_SE_w_n_modfor,ymax=dGPP_annual_mean+1.96*dGPP_annual_SE_w_n_modfor,group=precip_bin,fill=precip_bin),outline.type='full',alpha=0.25) +
  geom_line(aes(yrs_sincefire,dGPP_annual_mean,color=precip_bin),size=0.5) + 
  scale_color_brewer(name="Precip Normal",
                     palette = "BrBG") +
  scale_fill_brewer(name="Precip Normal",
                     palette = "BrBG") +
  coord_cartesian(ylim=c(-400,200),xlim=c(1,75)) + 
  labs(y=expression(dGPP~(gC~m^-2~yr^-1)),x='years since fire',color='Precip') +
  theme(axis.title.x=element_blank()) +
  theme_bw()

(fig2a / ((fig2b / fig2c) | (fig2d / fig2e))) + 
  plot_annotation(tag_levels = 'a') +
  plot_layout(heights = c(2,3))+
  theme_bw()

### Fig 3: BURNSEV USFS recovery ###
fig3_data <-  subset(CAfireUSFS.data.diff,Index%in%c('GPP')&isforest%in%c('forest'))
myred <- brewer.pal(n = 6, "YlOrRd")[3:6] 
  # fig 3a
CAfireUSFS.data.diff.annual <- fig3_data %>%
  group_by(yrs_sincefire) %>%
  summarize(dGPP_annual_mean=mean(diff_spatial,na.rm=T),
            dGPP_annual_SE_w_n_modfor = sqrt(sum((diff_spatial_stderror_wmodfor^2),na.rm=T))/sum(!is.na(diff_spatial)), 
            dGPP_annual_n = sum(!is.na(diff_spatial)))
fig3a <- ggplot(CAfireUSFS.data.diff.annual) + 
  geom_bin2d(data=filter(CAfireUSFS.data.diff,isforest%in%'forest'&Index%in%'GPP'),aes(yrs_sincefire,diff_spatial),binwidth=c(1,50)) +
  geom_hline(yintercept = 0, color = "black", size=0.5,linetype = 1) + 
  scale_fill_gradient(low="white", high="red") +
  geom_ribbon(aes(x=yrs_sincefire,ymin=dGPP_annual_mean-1.96*dGPP_annual_SE_w_n_modfor,ymax=dGPP_annual_mean+1.96*dGPP_annual_SE_w_n_modfor),outline.type='full',fill='grey70',alpha=0.6) +
  geom_line(aes(yrs_sincefire,dGPP_annual_mean),color='black',size=0.5) +
  coord_cartesian(ylim=c(-600,200),xlim=c(1,25)) + 
  labs(y=expression(dGPP~(gC~m^-2~yr^-1)),x='years since fire') +
  theme(legend.position = 'right', legend.box = "vertical") +
  theme_bw()
  # fig 3b
fig3b <- ggplot(fig3_data) + 
  geom_hline(yintercept=0,color='black',size=1) +
  geom_boxplot(data=filter(fig3_data,Index%in%c('GPP')&yrs_sincefire>0&yrs_sincefire<=3&EPAzone%in%c('Klamath','SierraNevada','Cascades','CoastRange','CentCAhills','SouthCAmtns','SoCAcoast')), 
               aes(x=EPAzone,y=diff_spatial,fill=as.factor(BURNSEV)),outlier.shape = NA) +
  scale_fill_manual(name="USFS \nBurn Severity",
                    values=myred,labels=c('unchanged','low','moderate','high')) + 
  coord_cartesian(ylim=c(-1200,200)) +
  labs(y=expression(dGPP~(gC~m^-2~yr^-1)),x='EPA Ecozone',subtitle='Years since fire 1-3')
(fig3a / fig3b) + plot_annotation(tag_levels='a'); 

### Fig 4: Integrated Landscape Scale ###

# 2000-2019 wildfire emissions from CARB: https://ww2.arb.ca.gov/wildfire-emissions
CARB_wildfireEmissions <- read_csv('CARB wildfire emissions/CARB_wildfireEmissions2019.csv')

# all forest fires
fig4a_data <- filter(CAfire.data.diff,Index%in%'GPP'&isforest%in%'forest') %>% 
  mutate(dGPParea=diff_spatial*area*(44/12)*(1/1e12),
         dGPParea_SE=diff_spatial_stderror_wmodfor*area*(44/12)*(1/1e12)) %>% #gC/m2*yr * m2 * gCO2/gC * 1MMT/1e12g = MMT CO2
  group_by(year) %>%
  summarize(dGPP_sum_pos = sum(dGPParea[dGPParea>=0],na.rm=T),
            dGPP_sum_neg =  sum(dGPParea[dGPParea<0],na.rm=T),
            dGPP_sum_all = sum(dGPParea,na.rm=T),
            dGPP_SE_w_n_modfor = sqrt(sum((dGPParea_SE^2),na.rm=T)),
            n=sum(!is.na(dGPParea))) # no divide by n

fig4a <- ggplot(fig4a_data) + 
  geom_bar(aes(x=year,y=dGPP_sum_pos,fill='darkgreen'),stat="identity",alpha=0.75) +
  geom_bar(aes(x=year,y=dGPP_sum_neg,fill='orange3',), stat="identity",alpha=0.75) +
  geom_ribbon(aes(x=year,ymin=dGPP_sum_all-1.96*dGPP_SE_w_n_modfor,ymax=dGPP_sum_all+1.96*dGPP_SE_w_n_modfor),outline.type='full',alpha=0.3) +
  geom_line(aes(x=year,y=dGPP_sum_all),size=1,color='black',alpha=1) +
  geom_hline(yintercept = 0, color = "black", size=0.5) + 
  labs(x='Year',
       y=expression(fire~GPP~legacy~(MMT~CO[2]))) + #,
  scale_fill_identity(name = "",
                      breaks = c("darkgreen", "orange3", 'white'),
                      labels = c("GPP enhancement", "GPP deficit", "CARB wildfire emissions"),
                      guide = "legend") +
  theme_bw() +
  theme(legend.position = "none") 

# all shrub fires
fig4b_data <- filter(CAfire.data.diff,Index=='GPP'&isforest=='shrub') %>% 
  mutate(dGPParea=diff_spatial*area*(44/12)*(1/1e12),
         dGPParea_SE=diff_spatial_stderror_wmodfor*area*(44/12)*(1/1e12)) %>% #gC/m2*yr * m2 * gCO2/gC * 1MMT/1e12g = MMT CO2
  group_by(year) %>%
  summarize(dGPP_sum_pos = sum(dGPParea[dGPParea>=0],na.rm=T),
            dGPP_sum_neg =  sum(dGPParea[dGPParea<0],na.rm=T),
            dGPP_sum_all = sum(dGPParea,na.rm=T),
            dGPP_SE_w_n_modfor = sqrt(sum((dGPParea_SE^2),na.rm=T)),
            n=sum(!is.na(dGPParea))) # no divide by n

fig4b <- ggplot(fig4b_data) + 
  geom_bar(aes(x=year,y=dGPP_sum_pos,fill='darkgreen'),stat="identity",alpha=0.75) +
  geom_bar(aes(x=year,y=dGPP_sum_neg,fill='orange3',), stat="identity",alpha=0.75) +
  geom_ribbon(aes(x=year,ymin=dGPP_sum_all-1.96*dGPP_SE_w_n_modfor,ymax=dGPP_sum_all+1.96*dGPP_SE_w_n_modfor),outline.type='full',alpha=0.3) +
  geom_line(aes(x=year,y=dGPP_sum_all),size=1,color='black',alpha=1) +
  geom_hline(yintercept = 0, color = "black", size=0.5) + 
  labs(x='Year',
       y=expression(fire~GPP~legacy~(MMT~CO[2]))) + #,
  scale_fill_identity(name = "",
                      breaks = c("darkgreen", "orange3", 'white'),
                      labels = c("GPP enhancement", "GPP deficit", "CARB wildfire emissions"),
                      guide = "legend") +
  theme_bw() +
  theme(legend.position = "none") 

# all fires
fig4c_data <- filter(CAfire.data.diff,Index=='GPP') %>% 
  mutate(dGPParea=diff_spatial*area*(44/12)*(1/1e12),
         dGPParea_SE=diff_spatial_stderror_wmodfor*area*(44/12)*(1/1e12)) %>% #gC/m2*yr * m2 * gCO2/gC * 1MMT/1e12g = MMT CO2
  group_by(year) %>%
  summarize(dGPP_sum_pos = sum(dGPParea[dGPParea>=0],na.rm=T),
            dGPP_sum_neg =  sum(dGPParea[dGPParea<0],na.rm=T),
            dGPP_sum_all = sum(dGPParea,na.rm=T),
            dGPP_SE_w_n_modfor = sqrt(sum((dGPParea_SE^2),na.rm=T)),
            n=sum(!is.na(dGPParea))) %>% 
  mutate(dGPP_sum_all_rollmean = rollmean(dGPP_sum_all,3,fill=NA,align='right'),
         dGPP_sum_all_rollSD = rollapply(dGPP_sum_all,3,sd,fill=NA,align='right'),
         dGPP_sum_all_rollSE = dGPP_sum_all_rollSD/sqrt(3))

fig4c <- ggplot(fig4c_data) +
  geom_hline(yintercept = 0, color = "black", size=0.5) +
  geom_bar(aes(x=year,y=dGPP_sum_pos,fill='darkgreen'),stat="identity",alpha=0.75) +
  geom_bar(aes(x=year,y=dGPP_sum_neg,fill='orange3',), stat="identity",alpha=0.75) +
  geom_bar(data=filter(CARB_wildfireEmissions,year<2018),aes(year,-emissions,fill='white'),alpha=0, stat="identity",color='black') +
  geom_line(aes(x=year,y=dGPP_sum_all,color='black'),size=1,alpha=0.0) +
  #geom_ribbon(aes(x=year,ymin=dGPP_sum_all-1.96*dGPP_SE_w_n_modfor,ymax=dGPP_sum_all+1.96*dGPP_SE_w_n_modfor),outline.type='full',alpha=0.4) +
  geom_line(aes(x=year,y=dGPP_sum_all_rollmean,color='5 yr rolling mean'),size=1) +
  geom_ribbon(aes(x=year,ymin=dGPP_sum_all_rollmean-1.96*dGPP_sum_all_rollSE,ymax=dGPP_sum_all_rollmean+1.96*dGPP_sum_all_rollSE),outline.type='full',alpha=0.3) +
  labs(x='Year',
    y=expression(fire~GPP~legacy~(MMT~CO[2])),
    color='') + 
  coord_cartesian(ylim=c(-50,50)) +
  scale_color_manual(name="",values=c('red2','black'),labels=c('3yr rolling mean','Cumulative dGPP')) +
  scale_fill_identity(name = "",
                      breaks = c("darkgreen", "orange3", 'white'),
                      labels = c("GPP enhancement", "GPP reduction", "Wildfire emissions\n(CARB)"),
                      guide = "legend") +
  theme_bw()

((fig4a / fig4b) | fig4c) + plot_annotation(tag_levels='a') + plot_layout(widths = c(2,3)); 


    ######## STATS for the TEXT ######
# Abstract
  CAfire.data.diff.annual
  CAfire.data.diff.annual.area
  fig4c_data

# Results
  # GPP fell X% compared to the year before fire
  CAfire.data.diff %>% filter(Index%in%c('GPP')&yrs_sincefire==1&is.finite(diff_mean_perc)) %>%
    group_by(isforest) %>% 
    summarize(mean=mean(as.numeric(diff_mean_perc)),count=n(),sde=sd(as.numeric(diff_mean_perc))/sqrt(n()))
  # 'a fire's impact on GPP (dGPP) was usually negative'
  CAfire.data.diff %>% filter(Index%in%c('GPP')&yrs_sincefire==1&isforest=='forest') %>%
    count(diff_spatial<=0)
  # mean California forest wildfire reduced
  CAfire.data.diff.annual
  # About half of these fires recovered carbon uptake capacity within the first decade after fire. 
  CAfire.data.diff %>% filter(Index%in%c('GPP')&yrs_sincefire==11&isforest=='forest') %>%
    count(diff_spatial<=0)
  #  following the largest quintile of fires
  CAfire.data.diff.annual.area
  # and fires in the wettest quintile of climates
  CAfire.data.diff.annual.precip
  # The most severe burn areas in this dataset resulted in 
  # fig 3a
  CAfireUSFS.data.diff.annual.sev <- fig3_data %>%
    group_by(yrs_sincefire,BURNSEV) %>%
    summarize(dGPP_annual_mean=mean(diff_spatial,na.rm=T),
              dGPP_annual_SE_w_n_modfor = sqrt(sum((diff_spatial_stderror_wmodfor^2),na.rm=T))/sum(!is.na(diff_spatial)), # no divide by n
              dGPP_annual_n = sum(!is.na(diff_spatial)))
  # By grouping recovery by decade
  CAfire.data.diff.annual.dec
  # 'California's fire-disturbed landscape compared to unburned controls, resulting in '
  fig4c_data

  # Discussion
  # In the year after fire, GPP in forest-dominated ecosystems decreased
  CAfire.diff.annual
  #There was a persistent but small
  CAfire.diff.annual
  # the net GPP deficit of forest fire-affected areas  has increased rapidly
  fig4a_data
  # have produced a statewide GPP deficit of close to 10MMT
  fig4c_data

  
    
  ######## FIGURES SUPPLEMENT ########

### Fig SX: Control bins ###

cont_rand = sample_n(filter(controls,Index=='GPP',year==2010,spconn>25),200)
cont_a<-ggplot(data=cont_rand) + #need to sample a few control bins
  geom_errorbar(aes(y=spconmean, x=latbin, ymax = spconmean + spconse_mean, ymin = spconmean - spconse_mean, color=tempbin),
                position = "dodge",size=0.5) +
  scale_colour_distiller(palette = "Reds",direction=1,limits=c(10,25)) +
  labs(x='lattitude bins', y=expression(GPP~(gC~m^-2~year^-1))) +
  ylim(0,2000)
cont_b<-ggplot(data=cont_rand) + #need to sample a few control bins
  geom_errorbar(aes(y=spconmean, x=latbin, ymax = spconmean + spconse_mean, ymin = spconmean - spconse_mean, color=precipbin),
                position = "dodge",size=0.5) +
  scale_colour_distiller(palette = "Blues",direction=1,limits=c(0,2000)) + 
  labs(x='lattitude bins', y='') +
  ylim(0,2000)
cont_c<-ggplot(data=cont_rand) + #need to sample a few control bins
  geom_errorbar(aes(y=spconmean, x=elevbin, ymax = spconmean + spconse_mean, ymin = spconmean - spconse_mean, color=tempbin),
                position = "dodge",size=0.5) +
  scale_colour_distiller(palette = "Reds",direction=1,limits=c(10,25)) +
  labs(x='elevation bins', y=expression(GPP~(gC~m^-2~year^-1))) +
  ylim(0,2000)
cont_d<-ggplot(data=cont_rand) + #need to sample a few control bins
  geom_errorbar(aes(y=spconmean, x=elevbin, ymax = spconmean + spconse_mean, ymin = spconmean - spconse_mean, color=precipbin),
                position = "dodge",size=0.5) +
  scale_colour_distiller(palette = "Blues",direction=1,limits=c(0,2000)) +
  labs(x='elevation bins', y='') +
  ylim(0,2000)
(cont_a+cont_b)/(cont_c+cont_d)

### Fig SX - forest diff LONG
fig1_a_lng <- ggplot(data=subset(CAfire.data.diff_ro,Index%in%c('GPP')&isforest%in%c('forest'))) + 
  geom_vline(aes(xintercept=0),color ='black',size=1,linetype=2) +
  geom_tile(aes(yrs_sincefire,OBJECTID,fill=Index_value_mean)) +
  scale_fill_gradient(low='white',high='darkgreen',limits=c(0,1800),oob=squish) +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.position="top", legend.box = "horizontal") +
  labs(x='years since fire',y='Fires',#subtitle = paste0("Forest, n= ",length(unique(subset(CAfire.data.diff_ro,Index%in%c('GPP_NIRv')&prefire_emapr_lc%in%c(3,4,5))$OBJECTID)),' fires'),
       fill=expression(GPP~(gC~m^-2~year^-1)))
fig1_b_lng <- ggplot(data=subset(CAfire.data.diff_ro,Index%in%c('GPP')&isforest%in%c('forest'))) + 
  geom_vline(aes(xintercept=0),color ='black',size=1,linetype=2) +
  geom_tile(aes(yrs_sincefire,OBJECTID,fill=diff_spatial)) +
  scale_fill_gradient2(low='orange2',mid = "white",high='deepskyblue2',limits=c(-500,500),oob=squish) +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),
        legend.position="top", legend.box = "horizontal") +
  labs(x='years since fire',
    subtitle = paste0("Forest, n= ",length(unique(subset(CAfire.data.diff_ro,Index%in%c('GPP')&isforest%in%c('forest'))$OBJECTID)),' fires'),
    fill=expression(dGPP~(gC~m^-2~year^-1)))
(fig1_a_lng | fig1_b_lng) + plot_annotation(tag_levels='a')

### Fig SX: coherence between pre-fire GPP and control GPP ###
CAfire.fig.coherence <- ggplot(filter(CAfire.data.diff,Index%in%c('GPP')&yrs_sincefire<0&isforest%in%c('forest'))) +
  geom_bin2d(aes(Index_value_mean,spconmean,fill=log(..count..))) +
  geom_smooth(aes(Index_value_mean,spconmean),method='lm',color='black') +
  scale_fill_gradient(low="white", high="orange2") +
  geom_abline(intercept = 0, slope=1, color = "black", size=0.4,linetype=2) +
  facet_wrap(~isforest,scales='free') + 
  labs(title = 'Spatial control and pre-fire coherence', x=expression(Prefire~GPP~observation~(gC~m^-2~yr^-1)),
       y=expression(Matched~control~GPP~(gC~m^-2~yr^-1))) +
  coord_cartesian(ylim=c(-10,3000),xlim=c(-10,3000))
CAfire.fig.coherence 
modfor = lm(Index_value_mean ~ spconmean, data=filter(CAfire.data.diff,Index%in%c('GPP')&yrs_sincefire>0&isforest%in%c('forest')))
summary(modfor)
modforRMSE <- sqrt(mean(modfor$residuals^2))
modforR2 <- summary(modfor)$r.squared

### Fig SX: Fire map and characteristics ###
a <- filter(distinct(CAfireUSFS.data.diff,OBJECTID,.keep_all=T),isforest%in%c('forest','shrub')) %>%
  mutate(area_ha = area*1e-4) %>% # m2 to ha
  ggplot() + 
  geom_bar(aes(firestartyear,fill=as.factor(BURNSEV),weight=area_ha)) +
  scale_fill_brewer(name="Severity",palette = "YlOrRd",direction=1,na.value = "grey50") +
  labs(x='fire start year',y='USFS Fire Polygon Area (ha)')#Fire Area (ha)')
b <- filter(distinct(CAfire.data.diff,OBJECTID,.keep_all=T),isforest%in%c('forest','shrub')) %>%
  mutate(area_ha = area*1e-4) %>% # m2 to ha
  ggplot() +
  geom_bar(aes(firestartyear,fill=isforest,weight=area_ha)) +
  labs(fill='Landcover',y='FRAP Fire Area (ha)') +
  theme(axis.title.x=element_blank())
mapFRAP <- ggplot(filter(CAfire.metadata,isforest%in%c('forest','shrub')))+ 
  geom_sf(data=CA_boundary, color="black", fill='black') +
  geom_sf(aes(geometry=geometry, fill=as.numeric(firestartyear)),lwd=0) +
  scale_fill_distiller(palette = 'Oranges',name='Fire Year',direction=1)
(mapFRAP+(b/a)) + plot_layout(ncol=2) + plot_annotation(tag_levels='a')

### Fig SX: case study fires ###
case_fires <- filter(CAfire.data.diff,isforest%in%c('forest')&OBJECTID%in%c('15957','11041','18532','11028'))
casetop <- ggplot(case_fires) +
  geom_point(data=filter(case_fires,yrs_sincefire<0&Index%in%c('GPP')),aes(yrs_sincefire, Index_value_mean,color='Pre-fire Obs'),shape = 2) +
  geom_point(data=filter(case_fires,yrs_sincefire>0&Index%in%c('GPP')),aes(yrs_sincefire, Index_value_mean,color='Post-fire Obs'),shape = 1) +
  geom_point(data=filter(case_fires,Index%in%c('GPP')),aes(yrs_sincefire, spconmean,color='Recently undisturbed\ncontrol pixels'),shape = 1) +
  geom_errorbar(data=filter(case_fires,Index%in%c('GPP')),aes(x=yrs_sincefire,ymin=spconmean-(spconsd/sqrt(spconn)),ymax=spconmean+(spconsd/sqrt(spconn))),width=.2,color='green') +
  geom_vline(xintercept=0,size=0.4,color='orange') +
  coord_cartesian(xlim = c(-15, 25)) +
  scale_colour_manual(name='', values=c('Pre-fire Obs'='black','Post-fire Obs' = 'red','Recently undisturbed\ncontrol pixels'='green')) +  #'Prefire trend'='blue' 'Temporal control, trend'='red',
  facet_grid(rows='FIRE_NAME', scales = "free",labeller = label_value) +
  labs(y=expression(GPP~(gC~m^-2~yr^-1)))
casebot <- ggplot(filter(case_fires,Index%in%c('GPP'))) +
  geom_line(aes(yrs_sincefire, diff_spatial, color='Recovery Curve'),size=1.5,alpha=1) +
  scale_colour_manual(name='', values=c('Recovery Curve' = 'green')) +  
  geom_vline(xintercept=0,size=0.4,color='orange') +
  geom_hline(yintercept=0,color='black',size=0.5) +
  coord_cartesian(xlim = c(-2, 25)) +
  facet_grid(rows='FIRE_NAME', scales = "free",labeller = label_value) +
  labs(y=expression(dGPP~(gC~m^-2~yr^-1)))
(casetop + casebot) + plot_layout(widths = c(2,1)) + plot_layout(guides = 'collect')

  ### Fig SX: map the dGPP ###
CAfire.data.diff_GPP_geo <- left_join(filter(CAfire.data.diff,Index=='GPP'&isforest%in%c('forest','shrub')),dplyr::select(CAfire.metadata,OBJECTID,geometry),by='OBJECTID')
SX_map <- ggplot(data=filter(CAfire.data.diff_GPP_geo,yrs_sincefire%in%c(1,5,10,20))) + 
  geom_sf(data=CA_boundary, color="black", fill='black') +
  geom_sf(aes(geometry=geometry, fill=diff_spatial),lwd=0) +
  scale_fill_gradient2(low='orange',mid = "white",high='deepskyblue2',limits=c(-500,500),oob=squish) +
  labs(fill = expression(dGPP~(gC~m^{-2}~yr^{-1})),
       subtitle = paste0(length(unique(filter(CAfire.data.diff_GPP_geo,yrs_sincefire%in%c(1,5,10,20)&isforest%in%c('forest','shrub'))$OBJECTID)),' forest and shrub fires'),
       y='lattitude', x='longitude') +
  facet_wrap(~yrs_sincefire,labeller = label_value) 

### Fig SX: GAM 10 yr ###
GPP_gam1_10yr <- CAfire.data.diff %>% 
  filter(yrs_sincefire%in%c(10)&Index=='GPP'&isforest%in%('forest')) %>% 
  mutate(reburned_perc=1-Burn1) %>%
  gam(diff_spatial ~ 
        s(log(area),bs='cs') + 
        s(elevation_mean,bs='cs') + 
        s(clm_temp_mean_mean,bs='cs') + 
        s(clm_precip_sum_mean,bs='cs') + 
        s(dNBR_prefire_perc,bs='cs') + 
        s(prefire_fullmean,bs='cs') +
        s(reburned_perc,bs='cs') + 
        s(firestartyear,bs='cs'),# +
      data = .,method='REML')
summary(GPP_gam1_10yr)
# set up for plotting
GPP_gam1_perc_plot <- getViz(GPP_gam1_10yr)
gam1_int = coef(GPP_gam1_10yr)[1]
print(plot(GPP_gam1_perc_plot, allTerms = TRUE), pages = 1)
# plot fancy
gam1_severity <- ggplot(plot(sm(GPP_gam1_perc_plot, 5))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  theme(axis.title.y=element_blank()) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x='fire severity (dNBR % change)',y=expression(dGPP~residual~(gC~m^-2~yr^-1)))
gam1_area <- ggplot(plot(sm(GPP_gam1_perc_plot,1))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  theme(axis.title.y=element_blank()) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x='log(area)',
       y=expression(dGPP~residual~(gC~m^-2~yr^-1)))
gam1_elev <- ggplot(plot(sm(GPP_gam1_perc_plot, 2))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x='elevation (m)',y=expression(dGPP~residual~(gC~m^-2~yr^-1)))
gam1_temp <- ggplot(plot(sm(GPP_gam1_perc_plot, 3))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x='normal temp (K)',y=expression(dGPP~residual~(gC~m^-2~yr^-1)))#,y=expression(dGPP~(gC~m^-2~yr^-1)))
gam1_precip <- ggplot(plot(sm(GPP_gam1_perc_plot, 4))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x='normal precip (mm/yr)')#,y=expression(dGPP~(gC~m^-2~yr^-1)))
gam1_prefiremean <- ggplot(plot(sm(GPP_gam1_perc_plot, 6))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  theme(axis.title.y=element_blank()) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x=expression(prefire~mean~GPP~(gC~m^-2~yr^-1)),y=expression(dGPP~residual~(gC~m^-2~yr^-1)))#,y=expression(dGPP~(gC~m^-2~yr^-1)))
gam1_firestartyr <- ggplot(plot(sm(GPP_gam1_perc_plot, 8))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  theme(axis.title.y=element_blank()) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x='fire start year',y='')#,y=expression(dGPP~(gC~m^-2~yr^-1)))
gam1_burn1 <- ggplot(plot(sm(GPP_gam1_perc_plot, 7))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  theme(axis.title.y=element_blank()) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x='% reburned',y='')#,y=expression(dGPP~(gC~m^-2~yr^-1)))
(gam1_area+gam1_severity)/(gam1_temp+gam1_precip)/(gam1_elev+gam1_burn1) / (gam1_prefiremean+gam1_firestartyr) +
  plot_annotation(title='10 years since fire',tag_levels='a') 

### Fig SX, SX: GPP-NIRv model figs ###
    # 0.) Ingest GEE NIRv and GPP data
GEE_GPP <- vroom(file=paste0(data_read,'FluxSites/UCIupwind_pixels_byMonthGPPgf_30m.csv'),   
  col_select=list(-'system:index',-'.geo')) %>%
  mutate(site_ID=Site,date=ymd(date),
         GPP_CAsites = na_if(GPP_CAsites, 0), # exported zeroes are missing NIRv data
         GPP_CAsites_Kcorr = na_if(GPP_CAsites_Kcorr,0),
         GPP_CAsites_Tcorr = na_if(GPP_CAsites_Tcorr,0),
         GPP_CAsites_raw = na_if(GPP_CAsites_raw,0),
         GPP_CAsites_raw_gf = na_if(GPP_CAsites_raw_gf,0),
         NIRv_monthly = na_if(NIRv_monthly,0),
         NIRv_monthly_gf = na_if(NIRv_monthly_gf,0),
         GPPgf_CAsites = na_if(GPPgf_CAsites,0)) %>%
  dplyr::select(-Site) %>%
  filter(!site_ID%in%c('US-SCd'))

# take mean of 9 pixels footprint for each site
GEE_GPP_mean <- GEE_GPP %>% group_by(site_ID,date) %>% # mean and std
  summarise_all("mean") %>%
  dplyr::select(-`Pixel #`)

    # 1.) Ingest flux data
    UCI_tower_good <- read_xlsx(paste0(data_read,'FluxSites/Monthly_towerdata_20200903.xlsx'),sheet='All sites good4') %>% # new gC/m2/day
      dplyr::select(Site,GEEfill,`Mean date`) %>%
      mutate(site_ID = as.factor(Site), date=floor_date(as.Date(`Mean date`),unit='month'), Days_Month = days_in_month(date),
             GPP_gC_m2_d=GEEfill*-1) %>%
      dplyr::select(site_ID,GPP_gC_m2_d,date,Days_Month) %>%
      mutate(GPP_gC_m2_d = case_when(GPP_gC_m2_d<=0 ~ 0,
                                     GPP_gC_m2_d>0 ~ GPP_gC_m2_d)) %>% 
      filter(!site_ID%in%c('US-SCd'))
    
      # combine
    GEE_GPP_tower_all <- left_join(GEE_GPP_mean,UCI_tower_good,by=c("date", "site_ID"))

  # plot scaling factors
    figSX_scalingfactors <- ggplot(GEE_GPP) + geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Tcorr,color='T scalar'),alpha=0.5) + 
      geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr,color='K scalar'),alpha=0.5) + 
      geom_boxplot(aes(as.factor(month(date)),GPP_CAsites_Kcorr*GPP_CAsites_Tcorr,color='Combined scalar')) + 
      geom_hline(yintercept=1,color='black') +
      facet_wrap(~site_ID,scales='free') +
      labs(color='GPP correction',x='month',y='correction scalar')
    
    # ANNUAL
    GEE_GPP_tower_annual <- GEE_GPP_tower_all %>% group_by(site_ID,year=year(date)) %>%
      summarize(GPP_CAscale_annual = sum(GPPgf_CAsites*Days_Month,na.rm=T), 
                GPP_CAscale_annual_n = n(),
                GPP_UCIsites_annual = sum(GPP_gC_m2_d*Days_Month,na.rm=T),
                GPP_UCIsites_annual_n = n())
  
    GPPmod <- lm(GPP_UCIsites_annual ~ GPP_CAscale_annual, GEE_GPP_tower_annual)
    summary(GPPmod)
    GPPmodRMSE <- sqrt(mean(GPPmod$residuals^2))
    GPPmodR2 <- summary(GPPmod)$r.squared
    
    figSX_scaled_observedGPP <- ggplot(filter(GEE_GPP_tower_annual,GPP_UCIsites_annual_n>=12&GPP_CAscale_annual_n>=11)) +
      geom_point(aes(GPP_UCIsites_annual,GPP_CAscale_annual,color=site_ID)) +
      geom_smooth(aes(GPP_UCIsites_annual,GPP_CAscale_annual,color='linear model'),method='lm') +
      geom_abline(intercept = 0, slope = 1) +
      labs(x=expression(Observed~GPP~(gC~m^-2~year^-1)), y=expression(Scaled~GPP~(gC~m^-2~year^-1)),
           #title='Scaled GPP (w T and K scalar correction) vs. measured GPP, UCI sites',
           subtitle=expression(RMSE==134.01))

