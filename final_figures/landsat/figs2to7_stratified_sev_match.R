#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: February 20, 2023
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'MatchIt')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# library(MatchIt)
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the data
# pixel.data <- read.csv(file.path(dir_in, "Stratified_sample_stand_age_2012_no_fire_history_mask_20210629_30m_v2.csv"), header = TRUE, na.strings = "NaN") #v2 is for all of Sierra and Socal
# pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_stand_age_no_fire_history_mask_01242022_30m.csv"), header = TRUE, na.strings = "NaN")
# pixel.data <- read.csv(file.path(dir_in, "frapsev_ecoregion_stratified_sample_100pts_30m_ts8_20220713.csv"), header = TRUE, na.strings = "NaN")
sev.data <- read.csv(file.path(dir_in, "fire_south_sierra_USFS_sevfire_500pt_ts8_300m_20230207.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
sev.data$treatment <- 'Disturb'
summary(sev.data)
# list.files(fire_in)
# list.files(fire_in)
unchanged.control.data <- read.csv(file.path(dir_in, "control_south_sierra_unchanged_sev_2km_buffer_200pt_ts16_300m_20230207.csv"), header = TRUE, na.strings = "NaN")
low.control.data <- read.csv(file.path(dir_in, "control_south_sierra_low_sev_2km_buffer_200pt_ts16_300m_20230210.csv"), header = TRUE, na.strings = "NaN")
med.control.data <- read.csv(file.path(dir_in, "control_south_sierra_med_sev_2km_buffer_200pt_ts16_300m_20230210.csv"), header = TRUE, na.strings = "NaN")
high.control.data <- read.csv(file.path(dir_in, "control_south_sierra_high_sev_2km_buffer_200pt_ts16_300m_20230210.csv"), header = TRUE, na.strings = "NaN")

sev.control.data <- rbind(unchanged.control.data, low.control.data, med.control.data, high.control.data)
#Add Fire Columns
# control.data$fire_sev_2010 <- -9999
# control.data$fire_year_2010 <- -9999
# control.data$fire_ID_2010 <- -9999
sev.control.data$fire_count_2010 <- -9999
sev.control.data$fire_sev_2019 <- -9999
sev.control.data$fire_year_2019 <- -9999
sev.control.data$fire_ID_2019 <- -9999
sev.control.data$fire_count_2019 <- -9999
sev.control.data$fire_sev_2020 <- -9999
sev.control.data$fire_year_2020 <- -9999
sev.control.data$fire_ID_2020 <- -9999
sev.control.data$fire_count_2020 <- -9999

#Add buffer dummy columns for fire.data
# sev.data$buffer_type_2010 <- -9999
# sev.data$buffer_type_2019 <- -9999
# sev.data$buffer_year_2010 <- -9999
# sev.data$buffer_year_2019 <- -9999

# summary(control.data)
#Get a  of the data
# summary(pixel.data)
# pixel.data <- pixel.data %>% filter(fire.year >= 1919 & !is.na(stand.age) & !is.na(NDMI))
# control.data$fire.year <- control.data$perimeter_year
sev.control.data$treatment <- 'Control' #Try making this 1-km versus, 2-km

#Figure out why the two dataframes don't match
# sev.n <- sev.data %>% colnames()
# con.n <- control.data %>% colnames()
# sev.n
# con.n
# sev.n[!(sev.n %in% con.n)]

#Combine the data together
sev.pixel.data <- rbind(sev.data, sev.control.data)
# pixel.data <- rbind(combine.data, control.data.2km)
summary(sev.pixel.data)

`%notin%` <- Negate(`%in%`)

#Convert data to long format
sev.pixel.data <- sev.pixel.data %>% #dplyr::select(-c('latitude', 'longitude')) %>% 
               pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

sev.pixel.data$year <- as.numeric(sev.pixel.data$year) + 1984 

#Convert missing TPA data to NAs
sev.pixel.data[sev.pixel.data$tpa_max < 0,]$tpa_max <- NA

#Convert fire data -9999 to NAs
sev.pixel.data[sev.pixel.data$fire_sev_2010 == -9999,]$fire_sev_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_ID_2010 == -9999,]$fire_ID_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_count_2010 == -9999,]$fire_count_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_sev_2019 == -9999,]$fire_sev_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_ID_2019 == -9999,]$fire_ID_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_count_2019 == -9999,]$fire_count_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_sev_2020 == -9999,]$fire_sev_2020 <- NA
sev.pixel.data[sev.pixel.data$fire_year_2020 == -9999,]$fire_year_2020 <- NA
sev.pixel.data[sev.pixel.data$fire_ID_2020 == -9999,]$fire_ID_2020 <- NA
sev.pixel.data[sev.pixel.data$fire_count_2020 == -9999,]$fire_count_2020 <- NA

#Convert to trees per hectare
sev.pixel.data$tpa_max <- sev.pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
sev.pixel.data$date <- as.Date(as.character(sev.pixel.data$year), format = '%Y')
# sev.pixel.data$vi.year <- format(sev.pixel.data$date , '%Y')
sev.pixel.data$vi.year <- sev.pixel.data$year
#Use the FRAP fire perimeter year
sev.pixel.data$fire.year <- sev.pixel.data$fire_year_2010
sev.pixel.data$stand.age <- as.numeric(sev.pixel.data$year) - as.numeric(sev.pixel.data$fire.year) 

#Update Cover data to 100% scale
sev.pixel.data$Tree_Cover <- sev.pixel.data$Tree_Cover / 100
sev.pixel.data$Shrub_Cover <- sev.pixel.data$Shrub_Cover / 100
sev.pixel.data$Herb_Cover <- sev.pixel.data$Herb_Cover / 100
sev.pixel.data$Bare_Cover <- sev.pixel.data$Bare_Cover / 100

#Convert the SPI48 scale back to decimal
sev.pixel.data$SPI48 <- sev.pixel.data$SPI48 / 100

#Try to fix soil moisture by dividing by 10
sev.pixel.data$Soil_Moisture <- sev.pixel.data$Soil_Moisture / 10

#Rename ppt and Water Stress
sev.pixel.data$Water_Stress <- sev.pixel.data$Water_Stress
sev.pixel.data$ppt <- sev.pixel.data$ppt
sev.pixel.data$AET <- sev.pixel.data$AET
sev.pixel.data$GPP <- sev.pixel.data$GPP
sev.pixel.data$elevation <- sev.pixel.data$elevation
sev.pixel.data$PrET <- sev.pixel.data$ppt - sev.pixel.data$AET
# 

summary(sev.pixel.data %>% filter(fire_sev_2010 != 243))
sev.pixel.data <- sev.pixel.data %>% mutate(treat = case_when(treatment == 'Disturb' ~ 1, treatment == 'Control' ~ 0))
# match0 <- matchit(data = sev.pixel.data %>% filter(fire_sev_2010 != 243), formula = treat ~ Tree_Cover + clm_precip_sum, method = NULL, distance = "glm")
# summary(match0)
# 
# match1 <- matchit(data = sev.pixel.data %>% filter(fire_sev_2010 != 243), formula = treat ~ clm_precip_sum, 
#                   method = "nearest", distance = "glm")
# summary(match1)

sev.pixel.data <- sev.pixel.data %>% mutate(fire.year.bin = case_when(
  # bin >= 1 ~ '1900',
  # bin == 2 ~ '1909-1910',
  # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
  # treatment == 'Control' ~ 'No Fire',
  # fire.year >= 1910 & fire.year <=  1970 ~ '1910-1970',#'81-95',
  # fire.year >= 1936 & fire.year <= 1950 ~ '65-79',
  # fire.year >= 1951 & fire.year <= 1965 ~ '50-64',
  # fire.year >= 1951 & fire.year <= 1960 ~ '55-64',
  # fire.year >= 1971 & fire.year <= 1980 ~ '1971-1980',#'56-80',
  # fire.year >= 1985 & fire.year <= 1990 ~ '1985-1990',
  # fire.year >= 1991 & fire.year <= 2000 ~ '1991-2000',#'31-55', 
  # fire.year >= 1991 & fire.year <= 2000 ~ '15-24',
  fire.year >= 1985 & fire.year <= 2010 ~ '1985-2010',
  # fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2017')) #,
  # fire.year >= 2019 ~ '2019-2020'))#'0-4'))

#Fire Severity Bins
#With re-export type needs to be converted to sev
sev.pixel.data <- sev.pixel.data %>% mutate(sev.bin = case_when(
  fire_sev_2010 == '0' ~ 'No Fire',
  fire_sev_2010 == '1' ~ 'Unchanged',
  fire_sev_2010 == '2' ~ 'Low',
  fire_sev_2010 == '3' ~ 'Mid',
  fire_sev_2010 == '4' ~ 'High',
  fire_sev_2010 == '255' ~ 'Masked')) # end function
sev.pixel.data %>% summary()
#Fire year bins for Fire Severity Data
sev.pixel.data$fire.year.bin = with(sev.pixel.data, factor(fire.year.bin, levels = c('2011-2017', '1985-2010')))#c('0-4','5-30','31-55','56-80',

#Make the years bin lables in the correct order
sev.pixel.data$sev.bin = with(sev.pixel.data, factor(sev.bin, levels = c('No Fire','Unchanged', 'Low','Mid', 'High')))#c('No Fire','Masked', 'Unchanged or Low','Mid or High')))

#Recode the veg type data
sev.pixel.data$veg_name <- recode(.x=sev.pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
                              '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
                              '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

sev.pixel.data %>% summary()

#Select strat categories for fire treatments
un.strat <- sev.pixel.data %>% filter(sev.bin == 'Unchanged' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n() /35) %>% filter(n >= 5) %>% pull(stratlayer) 
lo.strat <- sev.pixel.data %>% filter(sev.bin == 'Low' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n() /35) %>% filter(n >= 5) %>% pull(stratlayer)
mid.strat <- sev.pixel.data %>% filter(sev.bin == 'Mid' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n() /35) %>% filter(n >= 5) %>% pull(stratlayer)
hi.strat <- sev.pixel.data %>% filter(sev.bin == 'High' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n() /35) %>% filter(n >= 5) %>% pull(stratlayer)


#
#Tree Cover versus Elevation versus Latitude
p1 <- ggplot() +
  #Data Summary
  geom_bin2d(data = sev.pixel.data %>% #sev.bin != 'Unchanged' & 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, treatment, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2013, 2014)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = Tree_Cover, group = Tree_Cover), binwidth = c(500, 0.25)) + 
  theme_bw() +
  scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ sev.bin) +
  ylab('Latitude')
p1

# pixel.data %>% summary()
p2<- ggplot() +
  #Data Summary
  geom_bin2d(data = sev.pixel.data %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, treatment, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2017, 2018)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = dTree, group = dTree), binwidth = c(500, 0.25)) + 
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-10, 5), midpoint = 0, na.value = 'transparent') +  #  
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ sev.bin) +
  ylab('Latitude')
p2

#ADS die-off
p3 <- ggplot() +
  geom_bin2d(data = sev.pixel.data %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, treatment, sev.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015], elevation = elevation[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = tpa_max, group = tpa_max), binwidth = c(500, 0.25)) + 
  scale_fill_gradient(name = "Die-off \n(trees per hectare)", low = "white", high = "red", na.value = 'transparent') +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ sev.bin) +
  ylab('Latitude') + xlab('Elevation (m)')
p3             

p4<- ggplot() +
  #Data Summary
  geom_bin2d(data = sev.pixel.data %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2018, 2019)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], count = sum(elevation[vi.year == 2015]), n = n(), SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation), binwidth = c(500, 0.25)) + 
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  # scale_fill_gradient2(name = "Die-off (% Tree Cover)", limits = c(-50, 20), midpoint = 0, low = "red", mid = "white", high = "blue", na.value = 'transparent') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(. ~ sev.bin) +
  ylab('Latitude') + xlab('Elevation (m)')
p4

f1 <- ggarrange(p1, p2, p3, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f1
#Save the data
ggsave(filename = 'Fig2c_sev_fire_dieoff_tree_cover_fireyear_geographic_distribution.png', height=28, width= 24, units = 'cm', dpi=900)

#Figure of Dead Trees per acre separated by fire years with time series
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # & 
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), # %>%
            # filter(if_else(sev.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mean, color = treatment, linetype = treatment), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                 sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                 sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                 sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, sev.bin, treatment) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), #%>%
              # filter(if_else(sev.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date, fill = treatment), alpha = 0.3) +
  #Do the Formating
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ sev.bin) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p5

#Create the 
p6 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = sev.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()), #%>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = Tree_Cover.mean, color = treatment, linetype = treatment), 
            size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                 sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                 sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                 sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin, treatment) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), count = n()), # %>%  
                # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(count)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(count)),
                            x = date, fill = treatment), alpha = 0.3) +
  #Do the Formating
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #ylim(20, 45) + #facet_grid(. ~ sev.bin) + #ylim(20, 50) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p6

f2 <- ggarrange(p5, p6, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f2
#Save the data
ggsave(filename = 'Fig3c_dieoff_tree_cover_severity_time_series.png', height=12, width= 18, units = 'cm', dpi=900)

#Create a Precip time series figure
p7 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()), # %>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = ppt.mean, color = treatment, linetype = treatment), 
            size = 1) +
  #Precip 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                 sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                 sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                 sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin, treatment) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()), #%>%
                # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date, fill = treatment), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #
  ylab(expression('Precip (mm yr'^-1*')')) + xlab('Year') 
p7

#Create a AET time series figure
p8 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()), # %>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = AET.mean, color = treatment, linetype = treatment), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                 sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                 sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                 sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin, treatment) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()), #%>%  
                # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date, fill = treatment), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + ylim(250, 600) + 
  #facet_grid(. ~ sev.bin) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year') 
p8

#Create the Water Stress Panel
p10 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = sev.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # & 
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(PrET.mean = mean(PrET), PrET.n = n(), count = n()), #%>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = PrET.mean, color = treatment, linetype = treatment), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # & 
                #Match the controls to the disturbed based on the stratified sampling bins
                filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                 sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                 sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                 sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin, treatment) %>%
                summarize(PrET.mean = mean(PrET),
                          PrET.sd = sd(PrET), PrET.n = n(), count = n()), #%>%  
                # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=PrET.mean - 1.96*(PrET.sd / sqrt(PrET.n)),
                            ymax=PrET.mean + 1.96*(PrET.sd / sqrt(PrET.n)),
                            x = date, fill = treatment), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.15, 0.35), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ sev.bin) +
  ylab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) + xlab('Year')
p10

f3 <- ggarrange(p7, p8, p10, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f3
#Save the data
ggsave(filename = 'Fig4c_sev_water_fluxes_time_series.png', height=16, width= 18, units = 'cm', dpi=900)

#Figure 5c: Tree Cover Die-off
p11 <- ggplot() +
  #Data Summary
  stat_summary(data = sev.pixel.data %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = dTree, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.data %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # & 
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>% 
                 # filter(elevation <= 3000) %>% 
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin, treatment) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                            Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = sev.bin, y = dTree, color = treatment), 
                fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.9, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Stand Age (10-year Bins)') + ylab('Die-off (dTree %)')
p11

#RdTree Plot
p12 <- ggplot() +
  #Data Summary
  stat_summary(data = sev.pixel.data %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), 
                         RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = RdTree * 100, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.data %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # filter(elevation <= 3000) %>% 
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin, treatment) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = sev.bin, y = RdTree * 100, color = treatment), 
                fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Stand Age (10-year Bins)') + ylab('Relative dTree (%)')
p12

#ADS die-off
p13 <- ggplot() +
  stat_summary(data = sev.pixel.data %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = tpa_max, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.data %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # filter(elevation <= 3000) %>% 
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin, treatment) %>%
                  summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
                mapping = aes(x = sev.bin, y = tpa_max, color = treatment), 
                fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Mortality (trees/ha)') 
p13

# ggsave(filename = 'Fig10_ADS_mortality_stand_age_wildfire_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)

#Pre-Die-off Tree Cover
p14 <- ggplot() +
  stat_summary(data = sev.pixel.data %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(sev.bin = sev.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2014, 2015)])),
             mapping = aes(x = sev.bin, y = Tree_Cover, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.data %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # filter(elevation <= 3000) %>% 
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin, treatment) %>%
                  summarize(sev.bin = sev.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2014, 2015)])),
                mapping = aes(x = sev.bin, y = Tree_Cover, color = treatment), 
                fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Tree Cover (%)')
p14

#Water Stress
p15 <- ggplot() +
  stat_summary(data = sev.pixel.data %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                   # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), sev.bin = sev.bin[vi.year == 2010], 
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
             mapping = aes(x = sev.bin, y = Water_Stress, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.data %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # filter(elevation <= 3000) %>% 
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin, treatment) %>%
                  summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), sev.bin = sev.bin[vi.year == 2010], 
                            Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
                mapping = aes(x = sev.bin, y = Water_Stress, color = treatment), 
                fun.data = mean_se, geom = "errorbar", size = 1, position = 'dodge') + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Fire Severity') + ylab(expression('Four-year Pr-ET (mm 4yr'^-1*')'))
p15

#Combine the Panels
f5 <- ggarrange(p11, p12, p13, p14, p15,  ncol = 1, nrow = 5, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)', 'e)'))
f5

ggsave(filename = 'Fig5c_sev_fire_bar_chart_comparison.png', height=24, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills
p18 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire perimeter and fier year by pixel match 
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub', linetype = treatment), size = 1) +
  #Shrub Cover 95% CI
  geom_errorbar(data = sev.pixel.data %>% 
                  filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                   sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                   sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                   sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, treatment, sev.bin) %>%
                  summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                            Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
                mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                              ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                              x = stand.age, color = "Shrub",  linetype = treatment), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree',  linetype = treatment), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = sev.pixel.data %>% 
                  filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                   sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                   sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                   sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, treatment, sev.bin) %>%
                  summarize(Tree_Cover.mean = mean(Tree_Cover),
                            Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
                mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                              ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                              x = stand.age, color = "Tree",  linetype = treatment), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb',  linetype = treatment), size = 1) + 
  #Herb Cover 95% CI
  geom_errorbar(data = sev.pixel.data %>% 
                  filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                   sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                   sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                   sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, treatment, sev.bin) %>%
                  summarize(Herb_Cover.mean = mean(Herb_Cover),
                            Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
                mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                              ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                              x = stand.age, color = "Herb",  linetype = treatment), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare',  linetype = treatment), size = 1) + 
  #Bare Cover 95% CI
  geom_errorbar(data = sev.pixel.data %>%
                  filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                   sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                   sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                   sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, treatment, sev.bin) %>%
                  summarize(Bare_Cover.mean = mean(Bare_Cover),
                            Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
                mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                              ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                              x = stand.age, color = "Bare",  linetype = treatment), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + facet_grid(. ~ sev.bin) +
  guides(fill = "none") +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire')
p18

#Save the data
ggsave(filename = 'Fig8c_sev_stand_age_veg_cover.png', height=18, width= 20, units = 'cm', dpi=900)

p19 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) &sev.bin != 'Unchanged' &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Tree_Cover.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = sev.pixel.data %>%
                  filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                   sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                   sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                   sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, sev.bin) %>%
                  summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control']),
                            Tree_Cover.sd = sd(Tree_Cover[treatment == 'Disturb'])^2 + sd(Tree_Cover[treatment == 'Control'])^2, 
                            Tree_Cover.n = n()),
                mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / Tree_Cover.n),
                              ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / Tree_Cover.n),
                              x = stand.age)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') + facet_grid(. ~ sev.bin) +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('Tree Reduction (%)')) + xlab('Years Since Fire')
p19

#Save the data
ggsave(filename = 'Fig9c_sev_stand_age_tree_cover.png', height=18, width= 20, units = 'cm', dpi=900)

#AET change with wildfire (FRAP)
p20 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, sev.bin) %>%
              summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = AET.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = sev.pixel.data %>%
                  filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                   sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                   sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                   sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, sev.bin) %>%
                  summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control']),
                            AET.sd = sd(AET[treatment == 'Disturb'])^2 + sd(AET[treatment == 'Control'])^2, 
                            AET.n = n()),
                mapping = aes(ymin=AET.mean - 1.96*(AET.sd / AET.n),
                              ymax=AET.mean + 1.96*(AET.sd / AET.n),
                              x = stand.age)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') + facet_grid(. ~ sev.bin) +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('AET Reduction (mm yr'^-1*')')) + xlab('Years Since Fire')
p20

#Save the data
ggsave(filename = 'Fig10c_sev_stand_age_AET.png', height=18, width= 20, units = 'cm', dpi=900)

#Pr-ET change with wildfire (FRAP)
p21 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                               sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                               sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                               sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, sev.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Shrub_Cover.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = sev.pixel.data %>%
                  filter(stand.age >= -10 & stand.age <= 10 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                   sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                   sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                   sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, sev.bin) %>%
                  summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control']),
                            Shrub_Cover.sd = sd(Shrub_Cover[treatment == 'Disturb'])^2 + sd(Shrub_Cover[treatment == 'Control'])^2, 
                            Shrub_Cover.n = n()),
                mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / Shrub_Cover.n),
                              ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / Shrub_Cover.n),
                              x = stand.age)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') + facet_grid(. ~ sev.bin) +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('Shrub Change (%)')) + xlab('Years Since Fire')
p21

#Save the data
ggsave(filename = 'Fig11c_sev_stand_age_shrub.png', height=18, width= 20, units = 'cm', dpi=900)

sev.pixel.data %>% summary()

#Stand age versus die-off
p22 <- ggplot(data = sev.pixel.data %>% filter(fire.year <= 2010 & fire.year >= 1921 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                                 sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                                 sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                                 sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                dplyr::group_by(system.index, treatment, sev.bin) %>% 
                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
                          tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE),
                          Water_Stress = Water_Stress[vi.year == 2015], stand.age = stand.age[vi.year == 2015]),
              mapping = aes(x = stand.age, y = tpa_max)) + 
  facet_wrap(.~ sev.bin) + theme_bw() +
  geom_point(mapping = aes(color = treatment), size = 1) + 
  geom_smooth(mapping = aes(color = treatment), method = 'lm') +
  stat_cor(mapping = aes(color = treatment)) +
  xlab('Years Since Fire') + ylab(expression('Die-off (trees ha'^-1*')'))
p22

#Save the data
ggsave(filename = 'Fig7c_sev_stand_age_dieoff.png', height=24, width= 24, units = 'cm', dpi=900)

sev.pixel.data %>% summary()
sev.pixel.data %>% 
  filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1985 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
  #Match the controls to the disturbed based on the stratified sampling bins
  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                   sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                   sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                   sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
  dplyr::group_by(system.index, sev.bin, treatment) %>%
  summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), 
            sev.bin = sev.bin[vi.year == 2010], 
            clm_precip_sum = clm_precip_sum[vi.year == 2010],
            clm_temp_mean = clm_temp_mean[vi.year == 2010],
            Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
            Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013,2014)]),
pre.tree = Tree_Cover[vi.year == fire.year - 1])

#Testing out the control matches...
ggplot(data = sev.pixel.data %>% 
         filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & sev.bin != 'Unchanged' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
         #Match the controls to the disturbed based on the stratified sampling bins
         filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                          sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                          sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                          sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
         dplyr::group_by(system.index, sev.bin, treatment) %>%
         summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), 
                   sev.bin = sev.bin[vi.year == 2010], 
                   clm_precip_sum = clm_precip_sum[vi.year == 2010],
                   clm_temp_mean = clm_temp_mean[vi.year == 2010],
                   Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
                   Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013,2014)]),
                   pre.tree = Tree_Cover[vi.year == fire.year - 2])) + 
                  geom_density(mapping = aes(x = clm_temp_mean, color = treatment)) +
                  facet_wrap(. ~ sev.bin)