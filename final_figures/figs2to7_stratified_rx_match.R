#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: January 23, 2022
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the data
# pixel.data <- read.csv(file.path(dir_in, "Stratified_sample_stand_age_2012_no_fire_history_mask_20210629_30m_v2.csv"), header = TRUE, na.strings = "NaN") #v2 is for all of Sierra and Socal
# pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_stand_age_no_fire_history_mask_01242022_30m.csv"), header = TRUE, na.strings = "NaN")
# pixel.data <- read.csv(file.path(dir_in, "fraprx_ecoregion_stratified_sample_100pts_30m_ts8_20220713.csv"), header = TRUE, na.strings = "NaN")
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_400pt_ts8_300m_20230112.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
rx.data$treatment <- 'Disturb'
# list.files(fire_in)
# list.files(fire_in)
rx.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Rx_4km_buffer_400pt_ts16_300m_20230123.csv"), header = TRUE, na.strings = "NaN")

#Add Fire Columns
# rx.control.data$fire_type_2010 <- -9999
# rx.control.data$fire_year_2010 <- -9999
rx.control.data$fire_count_2010 <- -9999
rx.control.data$fire_type_2019 <- -9999
rx.control.data$fire_year_2019 <- -9999
rx.control.data$fire_year_2019 <- -9999
rx.control.data$fire_count_2019 <- -9999
rx.control.data$fire_type_2020 <- -9999
rx.control.data$fire_year_2020 <- -9999
rx.control.data$fire_count_2020 <- -9999

#Add buffer dummy columns for fire.data
# rx.data$buffer_type_2010 <- -9999
# rx.data$buffer_type_2019 <- -9999
# rx.data$buffer_year_2010 <- -9999
# rx.data$buffer_year_2019 <- -9999

#Add Label to Control data
rx.control.data$treatment <- 'Control' #Try making this 1-km versus, 2-km

# summary(fire.data)
# summary(control.data)
#Combine the data together
rx.pixel.data <- rbind(rx.data, rx.control.data)
# pixel.data <- rbind(combine.data, control.data.2km)
summary(rx.pixel.data)

`%notin%` <- Negate(`%in%`)

#Convert data to long format
rx.pixel.data <- rx.pixel.data %>% #dplyr::select(-c('latitude', 'longitude')) %>% 
               pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

rx.pixel.data$year <- as.numeric(rx.pixel.data$year) + 1984 

#Convert missing TPA data to NAs
rx.pixel.data[rx.pixel.data$tpa_max < 0,]$tpa_max <- NA

#Convert fire data -9999 to NAs
rx.pixel.data[rx.pixel.data$fire_type_2010 == -9999,]$fire_type_2010 <- NA
rx.pixel.data[rx.pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
rx.pixel.data[rx.pixel.data$fire_count_2010 == -9999,]$fire_count_2010 <- NA
rx.pixel.data[rx.pixel.data$fire_type_2019 == -9999,]$fire_type_2019 <- NA
rx.pixel.data[rx.pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
rx.pixel.data[rx.pixel.data$fire_count_2019 == -9999,]$fire_count_2019 <- NA
rx.pixel.data[rx.pixel.data$fire_type_2020 == -9999,]$fire_type_2020 <- NA
rx.pixel.data[rx.pixel.data$fire_year_2020 == -9999,]$fire_year_2020 <- NA
rx.pixel.data[rx.pixel.data$fire_count_2020 == -9999,]$fire_count_2020 <- NA

#Convert to trees per hectare
rx.pixel.data$tpa_max <- rx.pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
rx.pixel.data$date <- as.Date(as.character(rx.pixel.data$year), format = '%Y')
# rx.pixel.data$vi.year <- format(rx.pixel.data$date , '%Y')
rx.pixel.data$vi.year <- rx.pixel.data$year
#Use the FRAP fire perimeter year
rx.pixel.data$fire.year <- rx.pixel.data$fire_year_2010
rx.pixel.data$stand.age <- as.numeric(rx.pixel.data$year) - as.numeric(rx.pixel.data$fire.year) 

#Update Cover data to 100% scale
rx.pixel.data$Tree_Cover <- rx.pixel.data$Tree_Cover / 100
rx.pixel.data$Shrub_Cover <- rx.pixel.data$Shrub_Cover / 100
rx.pixel.data$Herb_Cover <- rx.pixel.data$Herb_Cover / 100
rx.pixel.data$Bare_Cover <- rx.pixel.data$Bare_Cover / 100

#Convert the SPI48 scale back to decimal
rx.pixel.data$SPI48 <- rx.pixel.data$SPI48 / 100

#Try to fix soil moisture by dividing by 10
rx.pixel.data$Soil_Moisture <- rx.pixel.data$Soil_Moisture / 10

#Rename ppt and Water Stress
rx.pixel.data$Water_Stress <- rx.pixel.data$Water_Stress
rx.pixel.data$ppt <- rx.pixel.data$ppt
rx.pixel.data$AET <- rx.pixel.data$AET
rx.pixel.data$GPP <- rx.pixel.data$GPP
rx.pixel.data$elevation <- rx.pixel.data$elevation
rx.pixel.data$PrET <- rx.pixel.data$ppt - rx.pixel.data$AET
# 
rx.pixel.data <- rx.pixel.data %>% mutate(fire.year.bin = case_when(
  # bin >= 1 ~ '1900',
  # bin == 2 ~ '1909-1910',
  # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
  treatment == 'Control' | fire.year < 1980 ~ 'No Fire',
  # fire.year >= 1910 & fire.year <=  1970 ~ '1910-1970',#'81-95',
  # # fire.year >= 1936 & fire.year <= 1950 ~ '65-79',
  # # fire.year >= 1951 & fire.year <= 1965 ~ '50-64',
  # # fire.year >= 1951 & fire.year <= 1960 ~ '55-64',
  # fire.year >= 1971 & fire.year <= 1980 ~ '1971-1980',#'56-80',
  # fire.year >= 1981 & fire.year <= 1990 ~ '1981-1990',
  # fire.year >= 1991 & fire.year <= 2000 ~ '1991-2000',#'31-55', 
  # fire.year >= 1991 & fire.year <= 2000 ~ '15-24',
  fire.year >= 1980 & fire.year <= 2010 ~ '1980-2010',
  # fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2018',
  fire.year >= 2019 ~ '2019-2020'))#'0-4'))

rx.pixel.data <- rx.pixel.data %>% mutate(fire.count.bin = case_when(
  # bin >= 1 ~ '1900',
  # bin == 2 ~ '1909-1910',
  # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
  is.na(fire_count_2010) ~ 'No Fire',
  fire_count_2010 == 1 ~ '1 Fire',
  fire_count_2010 == 2 ~ '2 Fires',
  fire_count_2010 == 3 ~ '3 Fires',
  fire_count_2010 >= 4 ~ '4+ Fires'))#'0-4'))

# summary(rx.pixel.data)

# rx.pixel.data$stand.age.bin = with(rx.pixel.data, factor(stand.age.bin, levels = c('2019-2020', '2011-2018', '2001-2010', '1986-2000', '1970-1985', '1910-1969', 'No Fire')))#c('0-4','5-30','31-55','56-80',
#'81-95')))

rx.pixel.data$fire.year.bin = with(rx.pixel.data, factor(fire.year.bin, levels = c('2019-2020', '2011-2018', '1980-2010', 'No Fire')))#c('0-4','5-30','31-55','56-80',

#Recode the veg type data
rx.pixel.data$veg_name <- recode(.x=rx.pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
                                  '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
                                  '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

#Figure 1
#Tree Cover versus Elevation versus Latitude
p1 <- ggplot() +
  #Data Summary
  geom_bin2d(data = rx.pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
               # filter(elevation <= 3000) %>%
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2013, 2014)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = Tree_Cover, group = Tree_Cover), binwidth = c(250, 0.1)) +  
  theme_bw() +
  scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ fire.year.bin) +
  ylab('Latitude')
p1

# pixel.data %>% summary()
p2 <- ggplot() +
  #Data Summary
  geom_bin2d(data = rx.pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
               # filter(elevation <= 3000) %>%
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2017, 2018)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = dTree, group = dTree), binwidth = c(250, 0.1)) + 
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-10, 5), midpoint = 0, na.value = 'transparent') +  #  
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ fire.year.bin) +
  ylab('Latitude')
p2

#ADS die-off
p3 <- ggplot() +
  geom_bin2d(data = rx.pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
               # filter(elevation <= 3000) %>%
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015], elevation = elevation[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation, fill = tpa_max, group = tpa_max), binwidth = c(250, 0.1)) +  
  scale_fill_gradient(name = "Die-off \n(trees per hectare)", low = "white", high = "red", na.value = 'transparent') +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), legend.position = "right",
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(. ~ fire.year.bin) +
  ylab('Latitude')
p3             

p4<- ggplot() +
  #Data Summary
  geom_bin2d(data = rx.pixel.data %>% 
               filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
               # filter(elevation <= 3000) %>%
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2018, 2019)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], count = sum(elevation[vi.year == 2015]), n = n(), SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(y = latitude, x = elevation), binwidth = c(250, 0.1)) + 
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  # scale_fill_gradient2(name = "Die-off (% Tree Cover)", limits = c(-50, 20), midpoint = 0, low = "red", mid = "white", high = "blue", na.value = 'transparent') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ fire.year.bin) +
  ylab('Latitude') + xlab('Elevation (m)')
p4

f1 <- ggarrange(p1, p2, p4, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f1
#Save the data
ggsave(filename = 'Fig2b_rx_fire_dieoff_tree_cover_fireyear_geographic_distribution.png', height=24, width= 14, units = 'cm', dpi=900)

# control.data %>% summary()

#Figure 2
#Figure of Dead Trees per acre separated by fire years with time series
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & stratlayer %in% strat.list  & stratlayer %in% strat.list
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), # %>%
            # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & stratlayer %in% strat.list  & stratlayer %in% strat.list
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, fire.year.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), #%>%
              # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
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
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p5

#Create the 
p6 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = rx.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(fire.year.bin == '1980-2010', Tree_Cover.n >= 550, Tree_Cover.n >= 0)),
            mapping = aes(x = date, y = Tree_Cover.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()) %>%  
                filter(if_else(fire.year.bin == '1980-2010', Tree_Cover.n >= 550, Tree_Cover.n >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
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
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + 
  ylim(35, 58) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p6

f2 <- ggarrange(p5, p6, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f2
#Save the data
ggsave(filename = 'Fig3b_frap_fire_dieoff_tree_cover_stand_age_time_series_.png', height=12, width= 10, units = 'cm', dpi=900)

#Figure 4b: Precip, ET, Soil moisture, Water Stress time series figure
p7 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()) %>%  
              filter(if_else(fire.year.bin == '1980-2010', count >= 550, count >= 0)),
            mapping = aes(x = date, y = ppt.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()) %>%  
                filter(if_else(fire.year.bin == '1980-2010', count >= 550, count >= 0)),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
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
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression('Precip (mm yr'^-1*')')) + xlab('Year') 
p7

#Create a water stress time series figure
p8 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()) %>%  
              filter(if_else(fire.year.bin == '1980-2010', count >= 550, count >= 0)),
            mapping = aes(x = date, y = AET.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()) %>%  
                filter(if_else(fire.year.bin == '1980-2010', count >= 550, count >= 0)),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
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
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + ylim(300, 750) + 
  #facet_grid(. ~ fire.year.bin) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year') 
p8

#Create the Soil Moisture Panel
p9 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = rx.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture), Soil_Moisture.n = n(), count = n()) %>%  
              filter(if_else(fire.year.bin == '1980-2010', count >= 550, count >= 0)),
            mapping = aes(x = date, y = Soil_Moisture.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1) + 
  #Soil Moisture 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(Soil_Moisture.mean = mean(Soil_Moisture),
                          Soil_Moisture.sd = sd(Soil_Moisture), Soil_Moisture.n = n(), count = n()) %>%  
                filter(if_else(fire.year.bin == '1980-2010', count >= 550, count >= 0)),
              mapping = aes(ymin=Soil_Moisture.mean - 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            ymax=Soil_Moisture.mean + 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  guides(color = guide_legend(), linetype = guide_legend()) +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression('Soil Moisture (mm)')) + xlab('Year')
p9

#Create the Water Stress Panel
p10 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = rx.pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(PrET.mean = mean(PrET), PrET.n = n(), count = n()) %>%  
              filter(if_else(fire.year.bin == '1980-2010', count >= 550, count >= 0)),
            mapping = aes(x = date, y = PrET.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                filter(vi.year >= 2010) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(PrET.mean = mean(PrET),
                          PrET.sd = sd(PrET), PrET.n = n(), count = n()) %>%  
                filter(if_else(fire.year.bin == '1980-2010', count >= 550, count >= 0)),
              mapping = aes(ymin=PrET.mean - 1.96*(PrET.sd / sqrt(PrET.n)),
                            ymax=PrET.mean + 1.96*(PrET.sd / sqrt(PrET.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
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
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) + xlab('Year')
p10

f3 <- ggarrange(p7, p8, p10, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f3
#Save the data
ggsave(filename = 'Fig4b_water_stress_stand_age_frap_perimeter_10pt_sample_300m_time_series.png', height=16, width= 10, units = 'cm', dpi=900)

#Figure 5b: Bar Chats, this could be for statistics
p11 <- ggplot() +
  #Calculate the Mean
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>% 
                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                           Water_Stress = Water_Stress[vi.year == 2015]),
               mapping = aes(x = fire.year.bin, y = dTree), 
               fun = mean, geom = "bar", fill = 'grey') + 
  #Calculate the Standard Error
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>% 
                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                           Water_Stress = Water_Stress[vi.year == 2015]),
               mapping = aes(x = fire.year.bin, y = dTree), 
               fun.data = mean_se, geom = "errorbar", size = 1) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.8, 0.75), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Stand Age (10-year Bins)') + ylab('dTree (%)')
p11

#RdTree Plot
p12 <- ggplot() +
  #Data Summary
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>% 
                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), Water_Stress = Water_Stress[vi.year == 2015]),
               mapping = aes(x = fire.year.bin, y = RdTree * 100), 
               fun = mean, geom = "bar", fill = 'grey') + 
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>% 
                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), Water_Stress = Water_Stress[vi.year == 2015]),
               mapping = aes(x = fire.year.bin, y = RdTree * 100), 
               fun.data = mean_se, geom = "errorbar", size = 1) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Stand Age (10-year Bins)') + ylab('Relative dTree (%)')
p12

#ADS die-off
p13 <- ggplot() +
  #Create bars and error bars
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
               mapping = aes(x = fire.year.bin, y = tpa_max), 
               fun = mean, geom = "bar", fill = 'grey') + 
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
               mapping = aes(x = fire.year.bin, y = tpa_max), 
               fun.data = mean_se, geom = "errorbar", size = 1) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Mortality (trees/ha)') 
p13

# ggsave(filename = 'Fig10_ADS_mortality_stand_age_wildfire_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)

#Pre-Die-off Tree Cover
p14 <- ggplot() +
  #Create the Error Bars
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>%
                 summarize(fire.year.bin = fire.year.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013, 2014)])),
               mapping = aes(x = fire.year.bin, y = Tree_Cover), 
               fun = mean, geom = "bar", fill = 'grey') + 
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>%
                 summarize(fire.year.bin = fire.year.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013, 2014)])),
               mapping = aes(x = fire.year.bin, y = Tree_Cover), 
               fun.data = mean_se, geom = "errorbar", size = 1) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Tree Cover (%)')
p14

#Water Stress
p15 <- ggplot() +
  #Create the Error Bars
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], 
                           Water_Stress = Water_Stress[vi.year == 2015], 
                           PrET_4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
               mapping = aes(x = fire.year.bin, y = PrET_4yr), 
               fun = mean, geom = "bar", fill = 'grey') + 
  stat_summary(data = rx.pixel.data %>% 
                 filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, fire.year.bin) %>%
                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], 
                           Water_Stress = Water_Stress[vi.year == 2015], PrET_4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
               mapping = aes(x = fire.year.bin, y = PrET_4yr), 
               fun.data = mean_se, geom = "errorbar", size = 1) + 
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Fire Year') + ylab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) 
p15

#Combine the Panels
f4 <- ggarrange(p11, p12, p13, p14, p15,  ncol = 1, nrow = 5, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)', 'e)'))
f4

ggsave(filename = 'Fig5b_rx_fire_bar_chart_comparison.png', height=24, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

#Figure 6b
#Water Stress Versus dTree to check the mechanism...
#figure out how to filter low count numbers (look at manuscript 1)
p16 <- ggplot(data = rx.pixel.data %>% filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & fire_type_2010 == 2 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
                # filter(elevation <= 3000) %>%
                dplyr::group_by(system.index) %>% 
                summarize(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                          RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
                          Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
                          fire.year.bin = fire.year.bin[vi.year == 2010])) +
  geom_bin2d(binwidth = c(100, 1), mapping = aes(x = Water_Stress, y = dTree, group = ..count..)) +
  scale_fill_gradient2(limits = c(0,130), breaks = c(0,30,60,90,120), midpoint = 62.5, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') + #
  # geom_point(mapping = aes(x = Water_Stress, y = dTree), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = Water_Stress, y = dTree), color = 'black', linetype = 'dashed', size = 2) + facet_wrap (. ~ fire.year.bin) +
  stat_cor( mapping = aes(x = Water_Stress, y = dTree), color = 'black') + #xlim(-600, 0) + #
  theme_bw() +xlab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) + ylab('Change in Tree Cover (%)')
p16
ggsave(filename = 'Fig6b_water_stress_dTree_300m.png', height=16, width= 18, units = 'cm', dpi=900)

#Figure 7b
#Tree Cover Die-off
# p17 <- ggplot(data = rx.pixel.data %>% filter((fire.year.bin == '1980-2010' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & Tree_Cover > 0)) %>% # &
#                 filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                 filter(elevation <= 3000) %>%
#                 dplyr::group_by(system.index) %>% 
#                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
#                           RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2015)]), 
#                           Water_Stress = Water_Stress[vi.year == 2015], stand.age = stand.age[vi.year == 2015])) +
#   geom_point(mapping = aes(x = stand.age, y = RdTree * 100), color = 'grey', size = 1.5) +
#   geom_smooth(method = 'lm', mapping = aes(x = stand.age, y = RdTree * 100), linetype = 'dashed', color = 'black', size = 2) +
#   stat_cor( mapping = aes(x = stand.age, y = RdTree * 100)) +
#   # geom_errorbar(data = pixel.data %>% dplyr::filter((fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | is.na(fire.year)) %>% 
#   #                 dplyr::group_by(system.index) %>% 
#   #                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), fire.year.grp = fire.year.grp[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
#   #               mapping = aes(x = fire.year.grp, y = dTree), stat = 'summary') + 
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   xlab('Stand Age') + ylab('dTree (%)')
# p17
# 
# ggsave(filename = 'Fig7b_stand_age_dTree_300m.png', height=16, width= 18, units = 'cm', dpi=900)

# summary(pixel.data)
#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills
# 
# summary(pixel.data)
#TEsting for issues
#Count number of pixels by stand age
p1a <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 85, color = 'red') +
  #Create a shrub cover line
  geom_line(data = rx.pixel.data %>%
              filter((treatment == 'Disturb' & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971)) %>%
              group_by(stand.age, treatment) %>%
              # dplyr::select(FIRE_NUM) %>%
              # unique() %>%
              summarize(count = n()), mapping = aes(x = stand.age, y = count, linetype = treatment), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab(expression('# Pixels')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1a

#Get the mean elevation by stand age
p1b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 85, color = 'red') +
  #Create a shrub cover line
  geom_line(data = rx.pixel.data %>%
              filter((treatment == 'Disturb' & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971)) %>%
              group_by(stand.age, treatment) %>%
              summarize(elevation.mean = mean(elevation), elevation.sd = sd(elevation), elevation.n = n()), mapping = aes(x = stand.age, y = elevation.mean, linetype = treatment), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab(expression('Elevation (m)')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1b
# summary(pixel.data)

summary(rx.pixel.data)
# pixel.data %>% filter(!is.na(Shrub_Cover) & vi.year <= 2010 & fire.year <= 2010 & !is.na(fire.year)) %>% 
#   group_by(stand.age) %>% dplyr::select(fire.year) %>% unique() %>% summarize(count = n())
#Count the number of fires by stand age
#There is something weird about the Fire Name column
# p1c <- ggplot() +
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#   geom_vline(xintercept = -10, color = 'red') + 
#   geom_vline(xintercept = 85, color = 'red') +
#   #Create a shrub cover line
#   geom_line(data = pixel.data %>%
#               filter(vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971) %>%
#               group_by(stand.age, treatment) %>%
#               dplyr::select(FIRE_NUM) %>% 
#               unique() %>%
#               summarize(fire.year.count = n()), mapping = aes(x = stand.age, y = fire.year.count, linetype = treatment), size = 1) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   ylab(expression('# Fire Perimeters')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
# p1c

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#What is the most common fire year in each stand age?
p1d <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 85, color = 'red') +
  #Create a shrub cover line
  geom_line(data = rx.pixel.data %>%
              filter((treatment == 'Disturb' & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971)) %>%
              group_by(stand.age, treatment) %>%
              summarize(fire_year.mode = find_mode(fire.year)), mapping = aes(x = stand.age, y = fire_year.mode, linetype = treatment), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + ylim(1900, 2010) +
  ylab(expression('Modal Fire Year')) #+ xlab('Years Since Fire') 
p1d

#What is the most common Vi Year in each stand age
p1e <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 85, color = 'red') +
  #Create a shrub cover line
  geom_line(data = rx.pixel.data %>%
              filter((treatment == 'Disturb' & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971)) %>%
              group_by(stand.age, treatment) %>%
              summarize(vi.year.mode = find_mode(vi.year)), mapping = aes(x = stand.age, y = vi.year.mode, linetype = treatment), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + ylim(1985, 2015) +
  ylab(expression('Modal VI Year')) + xlab('Years Since Fire') 
p1e

f1a <- ggarrange(p1a, p1b, p1d, p1e, ncol = 1, nrow = 4, common.legend = TRUE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f1a
#Save the data
ggsave(filename = 'Fig87_data_check_10pt_frap_perimeter_chronosequence.png', height=22, width= 16, units = 'cm', dpi=900)

# Fire Recovery Curve figures
p35 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_point(data = rx.pixel.data %>%
               dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1980) #&
                             # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))
               ) %>%
               group_by(date, fire.year) %>%
               summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()), #%>%  
             # filter(if_else(fire.year.bin == '1981-2010', Tree_Cover.n >= 600, Tree_Cover.n >= 0)) %>%
             # group_by(date, fire.year.bin) %>%
             # summarize(Tree_Cover.diff = Tree_Cover.mean[treatment == 'Buffer'] - Tree_Cover.mean[treatment == 'Wildfire']), #%>%
             # group_by(date, fire.year.bin) %>%
             # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
             mapping = aes(x = date, y = Tree_Cover.mean), 
             size = 0.1) + 
#Do the Formating
# scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # scale_shape(name = 'Treatment') +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_wrap(.~ fire.year) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + #ylim(25, 55) +
  ylab(expression('Tree (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p35

#Save the data
ggsave(filename = 'Fig88_fire year_tree_cover_frap_perimeter.png', height=18, width= 20, units = 'cm', dpi=900)

#Fire out which strat layers have data for at least on fire year group and No Fires
# group.test <- rx.pixel.data %>%
#   dplyr::filter(((!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1971) | (!is.na(Tree_Cover) & is.na(fire.year))) #&
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))
#   ) %>%
#   group_by(stratlayer, fire.year.bin, .drop = FALSE) %>%
#   summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()) %>% 
#   filter(fire.year.bin != '1900-1919' & fire.year.bin != '2011-2018' & fire.year.bin != '2019-2020') %>%
#   group_by(stratlayer) %>%
#   filter(any(count >= 175 & fire.year.bin == 'No Fire') & any(count >= 175 & fire.year.bin =='1971-1980') & 
#          any(count >= 175 & fire.year.bin =='1981-1990') & any(count >= 175 & fire.year.bin =='1991-2000') & 
#          any(count >= 175 & fire.year.bin =='2001-2010'))
# 
# #Select the stratlayer that have at least No Fire and One Fire Year Group
# rx.strat.list <- group.test$stratlayer %>% unique()


# Stratified sample layers
p36 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_point(data = rx.pixel.data %>%
               dplyr::filter(((!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1980) | (!is.na(Tree_Cover) & is.na(fire.year))) #&
                             # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))
               ) %>%
               group_by(date, stratlayer, fire.year.bin) %>%
               summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()),
             # filter(if_else(fire.year.bin == '1981-2010', Tree_Cover.n >= 600, Tree_Cover.n >= 0)) %>%
             # group_by(date, fire.year.bin) %>%
             # summarize(Tree_Cover.diff = Tree_Cover.mean[treatment == 'Buffer'] - Tree_Cover.mean[treatment == 'Wildfire']), #%>%
             # group_by(date, fire.year.bin) %>%
             # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
             mapping = aes(x = date, y = Tree_Cover.mean, color = fire.year.bin), 
             size = 0.1) + 
  #Do the Formating
  # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # scale_shape(name = 'Treatment') +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_wrap(.~ stratlayer) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + #ylim(25, 55) +
  ylab(expression('Tree (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p36

#Save the data
ggsave(filename = 'Fig89_fire year_tree_cover_frap_perimeter.png', height=36, width= 36, units = 'cm', dpi=900)


#Elevation limits
# elev.lower <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$elevation %>% quantile(0.0)
# elev.lower
# elev.upper <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$elevation %>% quantile(0.95)
# elev.upper
# ppt.upper <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$clm_precip_sum_mean %>% quantile(1)
# ppt.upper
# ppt.lower <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$clm_precip_sum_mean %>% quantile(0.05)
# ppt.lower
# temp.lower <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$clm_temp_mean_mean %>% quantile(0.0)
# temp.lower
#Figure of mean Cover changes by stand age
#Tyring out adding elevation lower bound...
p2 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = rx.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 30 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                       # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire perimeter and fier year by pixel match 
              group_by(stand.age, treatment ) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub'), size = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = rx.pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 30 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                         # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, fill = "Shrub"), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = rx.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 30 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                       # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree'), size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = rx.pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 30 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                         # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, fill = "Tree"), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = rx.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 30 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                       # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb'), size = 1) + 
  #Herb Cover 95% CI
  geom_ribbon(data = rx.pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 30 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                         # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment) %>%
                summarize(Herb_Cover.mean = mean(Herb_Cover),
                          Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
              mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            x = stand.age, fill = "Herb"), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = rx.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 30 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                       # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare'), size = 1) + 
  #Bare Cover 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter(stand.age >= -10 & stand.age <= 30 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                         # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment) %>%
                summarize(Bare_Cover.mean = mean(Bare_Cover),
                          Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
              mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
  x = stand.age, fill = "Bare"), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire')
p2

#Save the data
ggsave(filename = 'Fig90_veg_cover_stand_age_10pt_300m_frap_perimeter.png', height=18, width= 20, units = 'cm', dpi=900)

#Checking why there is a dip around 2002
p9 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              filter(!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1980 & stand.age > 2 & Tree_Cover > 0) %>%
              filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                       # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, fire.year.bin, treatment) %>%
              summarize(count = n()), mapping = aes(x = date, y = count, color = fire.year.bin, linetype = fire.year.bin),
            size = 1
  ) +
  #Do the Formating
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_wrap(. ~ treatment) +
  ylab('Count') + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p9

p10 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              filter(!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1980 & stand.age > 2) %>%
              filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, fire.year.bin, treatment) %>%
              summarize(stand.age.mean = mean(stand.age)), mapping = aes(x = date, y = stand.age.mean, color = fire.year.bin, linetype = fire.year.bin),
            size = 1
  ) +
  #Do the Formating
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x =element_text(size = 10), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_wrap(. ~ treatment) +
  ylab('Stand Age') + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p10

f4 <- ggarrange(p9, p10, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f4
# 
ggsave(filename = 'Fig91_data_check_time_series_perimeter_10pt_300m.png', height=16, width= 16, units = 'cm', dpi=900)

# summary(pixel.data)
#Figure of Dead Trees per acre separated by fire years with time series
p3 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              filter((!is.na(tpa_max) & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980 & stand.age > 2 ) | (!is.na(tpa_max) & treatment == 'Control' & is.na(fire.year))) %>% # & 
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), # %>%
              # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mean), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter((!is.na(tpa_max) & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980 & stand.age > 2) | (!is.na(tpa_max) & treatment == 'Control' & is.na(fire.year))) %>% # &
                         # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, fire.year.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), #%>%
                # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date), alpha = 0.3) +
  #Do the Formating
  # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # scale_linetype(name = 'Treatment') +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ fire.year.bin) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p3
# 
# test <- rx.pixel.data %>%
#   filter((!is.na(Tree_Cover) & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980 & stand.age > 2) | (!is.na(Tree_Cover) & treatment == 'Control' & is.na(fire.year))) %>% # &
#   # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
#   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
#   group_by(date, fire.year.bin) %>%
#   summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()) %>%  
#   filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,
#                    fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0))

#Create the 
p4 <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_line(data = rx.pixel.data %>%
              filter((!is.na(Tree_Cover) & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980 & stand.age > 2) | (!is.na(Tree_Cover) & treatment == 'Control' & is.na(fire.year))) %>% # &
              group_by(date, fire.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()) %>%  
              filter(case_when(fire.year.bin == '1980-2010' ~ count >= 1300, fire.year.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(x = date, y = Tree_Cover.mean), 
              size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter((!is.na(Tree_Cover) & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980 & stand.age > 2) | ( !is.na(Tree_Cover) & treatment == 'Control' & is.na(fire.year))) %>% # &
                group_by(date, fire.year.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), count = n()) %>%
                filter(case_when(fire.year.bin == '1980-2010' ~ count >= 1300, fire.year.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(count)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(count)),
                            x = date), alpha = 0.3) +
  #Do the Formating
  # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # scale_linetype(name = 'Treatment') +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ fire.year.bin) + #ylim(20, 50) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') 
p4

f2 <- ggarrange(p3, p4, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f2
#Save the data
ggsave(filename = 'Fig92_dieoff_tree_cover_stand_age_time_series_frap_perimeter_10pt_sample_300m.png', height=12, width= 14, units = 'cm', dpi=900)

#Create a Precip time series figure
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (treatment == 'Control' & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                       # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()) %>%  
              filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,                                  fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = ppt.mean), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (treatment == 'Control' & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()) %>%  
                filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,                                  fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date), alpha = 0.3) +
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
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ fire.year.bin) +
  ylab(expression('Precip (mm yr'^-1*')')) + xlab('Year') 
p5

#Create a water stress time series figure
p6 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (treatment == 'Control' & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()) %>%  
              filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,                                  fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = AET.mean), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (treatment == 'Control' & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()) %>%  
                filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,                                  
                                 fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date), alpha = 0.3) +
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
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + ylim(400, 800) + 
  facet_grid(. ~ fire.year.bin) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year') 
p6

#Create the Soil Moisture Panel
p7 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = rx.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (treatment == 'Control' & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture), Soil_Moisture.n = n(), count = n()) %>%  
              filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,                                  
                               fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(x = date, y = Soil_Moisture.mean), 
            size = 1) + 
  #Soil Moisture 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (treatment == 'Control' & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(Soil_Moisture.mean = mean(Soil_Moisture),
                          Soil_Moisture.sd = sd(Soil_Moisture), Soil_Moisture.n = n(), count = n()) %>%  
                filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,                                  
                                 fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=Soil_Moisture.mean - 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            ymax=Soil_Moisture.mean + 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  guides(color = guide_legend(), linetype = guide_legend()) +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ fire.year.bin) +
  ylab(expression('Soil Moisture (mm)')) + xlab('Year')
p7

#Create the Water Stress Panel
p8 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = rx.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (treatment == 'Control' & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # & 
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, fire.year.bin) %>%
              summarize(Water_Stress.mean = mean(Water_Stress), Water_Stress.n = n(), count = n()) %>%  
              filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,                                  
                               fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = Water_Stress.mean), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = rx.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (treatment == 'Control' & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # & 
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, fire.year.bin) %>%
                summarize(Water_Stress.mean = mean(Water_Stress),
                          Water_Stress.sd = sd(Water_Stress), Water_Stress.n = n(), count = n()) %>%  
                filter(case_when(fire.year.bin == '2001-2010' ~ count >= 120, fire.year.bin == '1991-2000' ~ count >= 50, fire.year.bin == '1981-1990' ~ count >= 50,                                  
                                 fire.year.bin == '1971-1980' ~ count >= 0, fire.year.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=Water_Stress.mean - 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            ymax=Water_Stress.mean + 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            x = date), alpha = 0.3) +
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
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ fire.year.bin) +
  ylab(expression('Water Stress (mm)')) + xlab('Year')
p8

f3 <- ggarrange(p5, p6, p7, p8, ncol = 1, nrow = 4, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f3
#Save the data
ggsave(filename = 'Fig93_water_stress_stand_age_frap_perimeter_10pt_sample_300m_time_series.png', height=22, width= 16, units = 'cm', dpi=900)

#Add another fire year grouping
# pixel.data <- pixel.data %>% mutate(fire.year.grp = case_when(
#   # bin >= 1 ~ '1900',
#   # bin == 2 ~ '1909-1910',
#   # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
#   is.na(fire.year) ~ 'No Fire',
#   fire.year <=  1920 ~ '1900-1920',#'81-95',
#   fire.year >= 1971 & fire.year <= 1965 ~ '1971-1965',
#   # fire.year >= 1931 & fire.year <= 1940 ~ '1931-1940',
#   # fire.year >= 1941 & fire.year <= 1950 ~ '1941-1950',
#   # fire.year >= 1951 & fire.year <= 1980 ~ '1951-1980',#'56-80',
#   # fire.year >= 1961 & fire.year <= 1970 ~ '1961-1970',
#   # fire.year >= 1971 & fire.year <= 1980 ~ '1971-1980',
#   fire.year >= 1966 & fire.year <= 2010 ~ '1966-2010',
#   # fire.year >= 1991 & fire.year <= 2000 ~ '1991-2000',
#   # fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
#   fire.year >= 2011 & fire.year <= 2020 ~ '2011-2020'))#,
# #fire.year >= 2019 ~ '2019-2020'))#'0-4'))
# 
# pixel.data$fire.year.grp = with(pixel.data, factor(fire.year.grp, levels = c('2011-2020', '1966-2010', '1971-1965', '1900-1920', 'No Fire')))#c('0-4','5-30','31-55','56-80',

# pixel.data %>% filter(FIRE_NAME != "") %>% group_by(FIRE_NAME, treatment)
# pixel.data %>% 
#   dplyr::filter(fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & 
#                   elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
#                   if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
#                   dplyr::group_by(system.index, treatment) %>% 
#        summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#             fire.year.grp = fire.year.grp[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]) %>%
#                   summary()

#Tree Cover Die-off
p14 <- ggplot() +
       #Data Summary
       geom_point(data = rx.pixel.data %>% 
                    filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                             # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                             # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                    dplyr::group_by(system.index, fire.year.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
       geom_errorbar(data = rx.pixel.data %>% 
                       filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # & 
                                     # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                                     # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                       dplyr::group_by(system.index, fire.year.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.8, 0.75), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
       xlab('Stand Age (10-year Bins)') + ylab('dTree (%)')
p14

#RdTree Plot
p15 <- ggplot() +
  #Data Summary
  geom_point(data = rx.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = fire.year.bin, y = RdTree * 100), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
  geom_errorbar(data = rx.pixel.data %>% 
                  filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, fire.year.bin) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = fire.year.bin, y = RdTree * 100), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Stand Age (10-year Bins)') + ylab('Relative dTree (%)')
p15

#ADS die-off
p16 <- ggplot() +
  geom_point(data = rx.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
             mapping = aes(x = fire.year.bin, y = tpa_max), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
  geom_errorbar(data = rx.pixel.data %>% 
                  filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, fire.year.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
             mapping = aes(x = fire.year.bin, y = tpa_max), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Mortality (trees/ha)') 
p16

# ggsave(filename = 'Fig10_ADS_mortality_stand_age_wildfire_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)

#Pre-Die-off Tree Cover
p17 <- ggplot() +
  geom_point(data = rx.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin) %>%
               summarize(fire.year.bin = fire.year.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2014, 2015)])),
             mapping = aes(x = fire.year.bin, y = Tree_Cover), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
  geom_errorbar(data = rx.pixel.data %>% 
                  filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, fire.year.bin) %>%
                  summarize(fire.year.bin = fire.year.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2014, 2015)])),
                mapping = aes(x = fire.year.bin, y = Tree_Cover), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Tree Cover (%)')
p17

#Water Stress
p18 <- ggplot() +
  geom_point(data = rx.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = fire.year.bin, y = Water_Stress), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
  geom_errorbar(data = rx.pixel.data %>% 
                  filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, fire.year.bin) %>%
                  summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = fire.year.bin, y = Water_Stress), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
                axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
                legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
                legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Years Since Fire') + ylab('Water Stress (mm)') 
p18

#Pr-ET 4yr
# p19 <- ggplot() +
#   geom_point(data = pixel.data %>% 
#                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2) | (treatment == 'Control' & is.na(fire.year))) %>% # &
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, treatment) %>%
#                summarize(PrET.4yr = sum(PrET[vi.year %in% c(2012, 2013, 2014, 2015)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
#              mapping = aes(x = fire.year.bin, y = PrET.4yr, color = treatment), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
#   geom_errorbar(data = pixel.data %>% 
#                   dplyr::filter(fire.year <= 2010 & fire.year >= 1971 & stand.age > 2) %>% # & 
#                   # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                   dplyr::group_by(system.index, treatment) %>%
#                   summarize(PrET.4yr = sum(PrET[vi.year %in% c(2012, 2013, 2014, 2015)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
#                 mapping = aes(x = fire.year.bin, y = PrET.4yr, color = treatment), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
#   scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   xlab('Years Since Fire') + ylab('Pr-ET four-year (mm/4yr)')
# p19

#Combine the Panels
f5 <- ggarrange(p14, p15, p16, p17, p18,  ncol = 1, nrow = 5, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)', 'e)'))
f5

ggsave(filename = 'Fig93_stand_age_bins_dieoff_treecover_water_stress_10pt_300m.png', height=24, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

# pixel.data %>% group_by(treatment, system.index, fire.year.grp) %>% dplyr::select(elevation, clm_precip_sum_mean, clm_temp_mean_mean) 

#Data Distribution
p20 <- ggplot() +
geom_histogram(data = rx.pixel.data %>% dplyr::filter((fire.year <= 2010 & fire.year >= 1971 & stand.age > 2) | (is.na(fire.year))) %>% #&
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
             dplyr::group_by(system.index, treatment) %>%
             summarize(elevation = elevation[vi.year == 2010] , clm_precip = clm_precip_sum[vi.year == 2010], fire.year.bin = fire.year.bin[vi.year == 2010], clm_temp = clm_temp_mean[vi.year == 2015]),
           mapping = aes(y = elevation, fill = treatment), position = position_dodge()) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.15, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_wrap(. ~ fire.year.bin) +
  xlab('Fire Years') + ylab('Elevation (m)')
p20

p21 <- ggplot() +
  geom_histogram(data = rx.pixel.data %>% dplyr::filter((fire.year <= 2010 & fire.year >= 1971 & stand.age > 2) | (is.na(fire.year))) %>% #&
                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, treatment) %>%
                 summarize(elevation = elevation[vi.year == 2010] , clm_precip = clm_precip_sum[vi.year == 2010], fire.year.bin = fire.year.bin[vi.year == 2010], clm_temp = clm_temp_mean[vi.year == 2015]),
               mapping = aes(y = clm_precip, fill = treatment), position = position_dodge()) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.15, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_wrap(. ~ fire.year.bin) +
  xlab('Fire Years') + ylab('Precip Climatology (mm/yr)')
p21

p22 <- ggplot() +
  geom_histogram(data = rx.pixel.data %>% dplyr::filter((fire.year <= 2010 & fire.year >= 1971 & stand.age > 2) | (is.na(fire.year))) %>% #&
                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, treatment) %>%
                 summarize(elevation = elevation[vi.year == 2010] , clm_precip = clm_precip_sum[vi.year == 2010], fire.year.bin = fire.year.bin[vi.year == 2010], clm_temp = clm_temp_mean[vi.year == 2015]),
               mapping = aes(y = clm_temp, fill = treatment), position = position_dodge()) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.15, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_wrap(. ~ fire.year.bin) +
  xlab('Fire Years') + ylab('Temp Climatology (C)')
p22

p23 <- ggplot() +
  geom_histogram(data = rx.pixel.data %>% 
                   dplyr::filter((fire.year <= 2010 & fire.year >= 1971 & stand.age > 2) | (is.na(fire.year))) %>% #&
                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, treatment) %>%
                 summarize(latitude = latitude[vi.year == 2010] , clm_precip = clm_precip_sum[vi.year == 2010], fire.year.bin = fire.year.bin[vi.year == 2010], clm_temp = clm_temp_mean[vi.year == 2015]),
               mapping = aes(y = latitude, fill = treatment), position = position_dodge()) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.15, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_wrap(. ~ fire.year.bin) +
  xlab('Count') + ylab('Latitude')
p23

f6 <- ggarrange(p20, p21, p22, p23, ncol = 1, nrow = 4, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f6

ggsave(filename = 'Fig94_fire_sample_characteristics_300m.png', height=28, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

summary(rx.pixel.data)
#Figure of Dead Trees per acre separated by fire years with time series
#Create a manual color scale
cols.1 <- c("2001-2010"="black","1991-2000"="gray", "1981-1990" = "white", "1971-1980" = "dark gray")
lines.1 <- c("2001-2010"="solid","1991-2000"="dashed", "1981-1990" = "dotted", "1971-1980" = "dotdash")
#ADS Difference
p24 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = rx.pixel.data %>%
              dplyr::filter((!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | ( !is.na(tpa_max) &is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
                              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%    
              group_by(date, fire.year.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()) %>% 
              # ungroup() %>%
              group_by(date) %>%
              summarize(tpa_max.2010 = tpa_max.mean[fire.year.bin == '2001-2010'] - tpa_max.mean[fire.year.bin == 'No Fire']), #%>%
              # group_by(date, fire.year.bin) %>%
              # summarize(tpa_max.diff.mean = mean(tpa_max.diff)),
            # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.2010, color = '2001-2010', linetype = '2001-2010'), 
            size = 1
  ) +
  #Do the subtraction versus 1991-2000  
  geom_line(data = rx.pixel.data %>%
            dplyr::filter((!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | ( !is.na(tpa_max) &is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
            # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%    
            group_by(date, fire.year.bin) %>%
            summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()) %>% 
            # ungroup() %>%
            group_by(date) %>%
            summarize(tpa_max.2000 = tpa_max.mean[fire.year.bin == '1991-2000'] - tpa_max.mean[fire.year.bin == 'No Fire']), #%>%
          # group_by(date, fire.year.bin) %>%
          # summarize(tpa_max.diff.mean = mean(tpa_max.diff)),
          # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
          mapping = aes(x = date, y = tpa_max.2000, color = '1991-2000', linetype = '1991-2000'), 
          size = 1
  ) +
  #Do subtraction versus 1981-1990
  geom_line(data = rx.pixel.data %>%
              dplyr::filter((!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | ( !is.na(tpa_max) &is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%    
              group_by(date, fire.year.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()) %>% 
              # ungroup() %>%
              group_by(date) %>%
              summarize(tpa_max.1990 = tpa_max.mean[fire.year.bin == '1981-1990'] - tpa_max.mean[fire.year.bin == 'No Fire']), #%>%
            # group_by(date, fire.year.bin) %>%
            # summarize(tpa_max.diff.mean = mean(tpa_max.diff)),
            # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.1990, color = '1981-1990', linetype = '1981-1990'), 
            size = 1
  ) +
  #Do subtraction versus 1971-1980
  geom_line(data = rx.pixel.data %>%
              dplyr::filter((!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | ( !is.na(tpa_max) &is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%    
              group_by(date, fire.year.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()) %>% 
              # ungroup() %>%
              group_by(date) %>%
              summarize(tpa_max.1980 = tpa_max.mean[fire.year.bin == '1971-1980'] - tpa_max.mean[fire.year.bin == 'No Fire']), #%>%
            # group_by(date, fire.year.bin) %>%
            # summarize(tpa_max.diff.mean = mean(tpa_max.diff)),
            # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.1980, color = '1971-1980', linetype = '1971-1980'), 
            size = 1
  ) +
  #Do the Formating
  # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  scale_colour_manual(name="Fire Years",values=cols.1, aesthetics = 'color') +
  scale_linetype_manual(name = 'Fire Years', values = lines.1) +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression(atop('ADS Die-off (Fire - Control)', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p24

# rx.pixel.data %>%
#   dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (!is.na(Tree_Cover) & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
#   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
#   group_by(date, fire.year.bin) %>%
#   summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
#   filter(if_else(fire.year.bin == '2001-2010', Tree_Cover.n >= 1700, Tree_Cover.n >= 0)) %>%
#   group_by(date) %>%
#   summarize(Tree_Cover.2010 = Tree_Cover.mean[fire.year.bin == '2001-2010'] - Tree_Cover.mean[fire.year.bin == 'No Fire'])

#Create the
p25 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  #2001-2010 Tree Cover Difference
  geom_line(data = rx.pixel.data %>%
              dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (!is.na(Tree_Cover) & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
              group_by(date, fire.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(fire.year.bin == '2001-2010', Tree_Cover.n >= 120, Tree_Cover.n >= 0)) %>%
              group_by(date) %>%
              summarize(Tree_Cover.2010 = Tree_Cover.mean[fire.year.bin == '2001-2010'] - Tree_Cover.mean[fire.year.bin == 'No Fire']), #%>%
              # group_by(date, fire.year.bin) %>%
              # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
            mapping = aes(x = date, y = Tree_Cover.2010, color = '2001-2010', linetype = '2001-2010'), 
            size = 1) + 
  #1991-2000 tree cover difference
  geom_line(data = rx.pixel.data %>%
              dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (!is.na(Tree_Cover) & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
              # if_else(fire.year.bin == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
              group_by(date, fire.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(fire.year.bin == '1991-2000', Tree_Cover.n >= 50, Tree_Cover.n >= 0)) %>%
              group_by(date) %>%
              summarize(Tree_Cover.2000 = Tree_Cover.mean[fire.year.bin == '1991-2000'] - Tree_Cover.mean[fire.year.bin == 'No Fire']), #%>%
            # group_by(date, fire.year.bin) %>%
            # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
            mapping = aes(x = date, y = Tree_Cover.2000, color = '1991-2000', linetype = '1991-2000'), 
            size = 1) + 
  #1981-1990 tree cover difference
  geom_line(data = rx.pixel.data %>%
              dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (!is.na(Tree_Cover) & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
              # if_else(fire.year.bin == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
              group_by(date, fire.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(fire.year.bin == '1981-1990', Tree_Cover.n >= 50, Tree_Cover.n >= 0)) %>%
              group_by(date) %>%
              summarize(Tree_Cover.1990 = Tree_Cover.mean[fire.year.bin == '1981-1990'] - Tree_Cover.mean[fire.year.bin == 'No Fire']), #%>%
            # group_by(date, fire.year.bin) %>%
            # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
            mapping = aes(x = date, y = Tree_Cover.1990, color = '1981-1990', linetype = '1981-1990'), 
            size = 1) + 
  #1971-1980 tree cover difference
  geom_line(data = rx.pixel.data %>%
              dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1971 & stand.age > 2 & stratlayer %in% rx.strat.list) | (!is.na(Tree_Cover) & is.na(fire.year) & stratlayer %in% rx.strat.list)) %>% # &
              # if_else(fire.year.bin == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
              group_by(date, fire.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              # filter(if_else(fire.year.bin == '1981-1990', Tree_Cover.n >= 490, Tree_Cover.n >= 0)) %>%
              group_by(date) %>%
              summarize(Tree_Cover.1980 = Tree_Cover.mean[fire.year.bin == '1971-1980'] - Tree_Cover.mean[fire.year.bin == 'No Fire']), #%>%
            # group_by(date, fire.year.bin) %>%
            # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
            mapping = aes(x = date, y = Tree_Cover.1980, color = '1971-1980', linetype = '1971-1980'), 
            size = 1) +
  #Do the Formating
  # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  scale_colour_manual(name="Fire Years",values=cols.1, aesthetics = 'color') +
  scale_linetype_manual(name = 'Fire Years', values = lines.1) +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + #ylim(25, 55) +
  ylab(expression('dTree (Wildfire - Control)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p25

#Adding the Water_Stress difference could also be helpful

f7 <- ggarrange(p24, p25, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f7
#Save the data
ggsave(filename = 'Fig95_dieoff_tree_cover_stand_age_time_series_frap_perimeter_10pt_sample_300m.png', height=12, width= 14, units = 'cm', dpi=900)

# #Tree Cover versus Elevation versus Latitude
# p26 <- ggplot() +
#   #Data Summary
#   geom_bin2d(data = rx.pixel.data %>% 
#                filter((Tree_Cover >= 0 & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980) | (Tree_Cover >= 0 & is.na(fire.year))) %>% # &
#                filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, fire.year.bin) %>% 
#                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#                          Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2013, 2014)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
#                          latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
#              mapping = aes(x = latitude, y = elevation, fill = Tree_Cover, group = Tree_Cover), binwidth = c(0.1, 250)) +  
#   # geom_errorbar(data = pixel.data %>% 
#   #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & stratlayer %in% strat.list) | (is.na(fire.year) & stratlayer %in% strat.list)) %>% # & 
#   #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#   #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#   #                 dplyr::group_by(system.index, fire.year.bin) %>% 
#   #                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#   #                           Water_Stress = Water_Stress[vi.year == 2015]),
#   #               mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + 
#   theme_bw() +
#   scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
#   # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ fire.year.bin) +
#   ylab('Elevation (m)')
# p26
# 
# # pixel.data %>% summary()
# p29<- ggplot() +
#   #Data Summary
#   geom_bin2d(data = rx.pixel.data %>% 
#                filter((!is.na(Tree_Cover) & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980) | (!is.na(Tree_Cover) & is.na(fire.year))) %>% # &
#                filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, fire.year.bin) %>% 
#                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
#                          Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2017, 2018)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
#                          latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
#              mapping = aes(x = latitude, y = elevation, fill = dTree, group = dTree), binwidth = c(0.1, 250)) + 
#   # geom_errorbar(data = pixel.data %>% 
#   #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & stratlayer %in% strat.list) | (is.na(fire.year) & stratlayer %in% strat.list)) %>% # & 
#   #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#   #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#   #                 dplyr::group_by(system.index, fire.year.bin) %>% 
#   #                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#   #                           Water_Stress = Water_Stress[vi.year == 2015]),
#   #               mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + 
#   theme_bw() +
#   # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-10, 5), midpoint = 0, na.value = 'transparent') +  #  
#   theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ fire.year.bin) +
#   ylab('Elevation (m)')
# p29
# 
# #ADS die-off
# p30 <- ggplot() +
#   geom_bin2d(data = rx.pixel.data %>% 
#                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980 & !is.na(tpa_max)) | (is.na(fire.year) & !is.na(tpa_max))) %>% # &
#                filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, fire.year.bin) %>%
#                summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015], elevation = elevation[vi.year == 2015],
#                          latitude = latitude[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
#              mapping = aes(x = latitude, y = elevation, fill = tpa_max, group = tpa_max), binwidth = c(0.1, 250)) +  
#   # geom_errorbar(data = pixel.data %>% 
#   #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & !is.na(tpa_max)) | (is.na(fire.year) & !is.na(tpa_max))) %>% # &
#   #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#   #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#   #                 dplyr::group_by(system.index, fire.year.bin) %>%
#   #                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
#   #               mapping = aes(x = fire.year.bin, y = tpa_max), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
#   # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   scale_fill_gradient(name = "Die-off \n(trees per hectare)", low = "white", high = "red", na.value = 'transparent') +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), legend.position = "right",
#         axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(. ~ fire.year.bin) +
#   ylab('Elevation (m)')
# p30             
# 
# p31<- ggplot() +
#   #Data Summary
#   geom_bin2d(data = rx.pixel.data %>% 
#                filter((Tree_Cover >= 0 & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1980) | (Tree_Cover >= 0 & is.na(fire.year))) %>% # &
#                filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, fire.year.bin) %>% 
#                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#                          Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2018, 2019)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
#                          latitude = latitude[vi.year == 2015], count = sum(elevation[vi.year == 2015]), n = n(), SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
#              mapping = aes(x = latitude, y = elevation), binwidth = c(0.1, 250)) + 
#   # geom_errorbar(data = pixel.data %>% 
#   #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & stratlayer %in% strat.list) | (is.na(fire.year) & stratlayer %in% strat.list)) %>% # & 
#   #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#   #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#   #                 dplyr::group_by(system.index, fire.year.bin) %>% 
#   #                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#   #                           Water_Stress = Water_Stress[vi.year == 2015]),
#   #               mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + 
#   theme_bw() +
#   # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   # scale_fill_gradient2(name = "Die-off (% Tree Cover)", limits = c(-50, 20), midpoint = 0, low = "red", mid = "white", high = "blue", na.value = 'transparent') +
#   theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ fire.year.bin) +
#   xlab('Latitude') + ylab('Elevation (m)')
# p31
# 
# f8 <- ggarrange(p26, p29, p31, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
# f8
# #Save the data
# ggsave(filename = 'Fig108_rx_fire_dieoff_tree_cover_fireyear_geographic_distribution.png', height=18, width= 24, units = 'cm', dpi=900)

p29a<- ggplot() +
  #Data Summary
  geom_bin2d(data = rx.pixel.data %>% 
               filter((!is.na(Tree_Cover) & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971) | (!is.na(Tree_Cover) & is.na(fire.year))) %>% # &
               filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% #Filter by the tree types
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, fire.year.bin, veg_name) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2017, 2018)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = latitude, y = elevation, fill = dTree, group = dTree), binwidth = c(0.25, 500)) + 
  # geom_errorbar(data = pixel.data %>% 
  #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & stratlayer %in% strat.list) | (is.na(fire.year) & stratlayer %in% strat.list)) %>% # & 
  #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
  #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
  #                 dplyr::group_by(system.index, fire.year.bin) %>% 
  #                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
  #                           Water_Stress = Water_Stress[vi.year == 2015]),
  #               mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + 
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-10, 5), midpoint = 0, na.value = 'transparent') +  #  
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(veg_name ~ fire.year.bin) +
  ylab('Elevation (m)')
p29a
