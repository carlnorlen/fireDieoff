#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: June 20, 2023
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo', 'purrr')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('zoo'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(zoo)
#Set the working directory

#Home data directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')
dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"

#Lab data directory
# setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/landsat')
# dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"

#Add the Wildfire data
frap.fire.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_wildfire_500pt_fire_year_5tree_ts8_300m_20230327.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
frap.fire.data$treatment <- 'Disturb'

#Add the Wildfire buffer data
frap.control.data <- read.csv(file.path(dir_in, "control_south_sierra_FRAP_2km_buffer_500pt_fire_year_5tree_ts16_300m_20230327.csv"), header = TRUE, na.strings = "NaN")

#Add Fire Columns
frap.control.data$fire_count_2010 <- -9999
frap.control.data$fire_type_2019 <- -9999
frap.control.data$fire_year_2019 <- -9999
frap.control.data$fire_year_2019 <- -9999
frap.control.data$fire_count_2019 <- -9999
frap.control.data$fire_type_2020 <- -9999
frap.control.data$fire_year_2020 <- -9999
frap.control.data$fire_count_2020 <- -9999

#Add the treatment Column
frap.control.data$treatment <- 'Control' 

#Combine the data together
frap.pixel.data <- rbind(frap.fire.data, frap.control.data)

#Add the Rx fire data
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_500pt_fire_year_5tree_ts8_300m_20230327.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
rx.data$treatment <- 'Disturb'

#Add teh Rx fire buffer data
rx.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Rx_2km_buffer_500pt_fire_year_5tree_ts16_300m_20230327.csv"), header = TRUE, na.strings = "NaN")

#Add Fire Columns
rx.control.data$fire_count_2010 <- -9999
rx.control.data$fire_type_2019 <- -9999
rx.control.data$fire_year_2019 <- -9999
rx.control.data$fire_year_2019 <- -9999
rx.control.data$fire_count_2019 <- -9999
rx.control.data$fire_type_2020 <- -9999
rx.control.data$fire_year_2020 <- -9999
rx.control.data$fire_count_2020 <- -9999

#Add the treatment column
rx.control.data$treatment <- 'Control' #Try making this 1-km versus, 2-km

#Combine the data together
rx.pixel.data <- rbind(rx.data, rx.control.data)

#Combine all the data together
pixel.data <- rbind(frap.pixel.data, rx.pixel.data)

`%notin%` <- Negate(`%in%`)
# summary(pixel.data)
#Convert fire data -9999 to NAs
# pixel.data[pixel.data$fire_type_2010 == -9999,]$fire_type_2010 <- NA
# pixel.data[pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
pixel.data[pixel.data$fire_type_2019 == -9999,]$fire_type_2019 <- NA
pixel.data[pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
pixel.data[pixel.data$fire_type_2020 == -9999,]$fire_type_2020 <- NA
pixel.data[pixel.data$fire_year_2020 == -9999,]$fire_year_2020 <- NA

#Use the FRAP fire perimeter year (use fire year 2010)
pixel.data$fire.year <- pixel.data$fire_year_2010

#Add the Fire types
pixel.data <- pixel.data %>% mutate(fire.type.bin = case_when(
  fire_type_2010 == 1 ~ 'Wildfire',
  fire_type_2010 == 2 ~ 'Rxfire'
))
# 
# summary(pixel.data)
#Make treatment a factor
pixel.data$treatment = with(pixel.data, factor(treatment, levels = c('Control', 'Disturb')))#

#Recode the veg type data
# pixel.data$veg_name <- recode(.x=pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
#                               '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
#                               '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

#Select strat categories for fire treatments
#Select strat categories for fire treatments
frap.disturb <- pixel.data %>% filter(fire.type.bin == 'Wildfire' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n()) 
rx.disturb <- pixel.data %>% filter(fire.type.bin == 'Rxfire' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n()) 
# print(frap.strat)

frap.control <- pixel.data %>% filter(fire.type.bin == 'Wildfire' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n())
rx.control <- pixel.data %>% filter(fire.type.bin == 'Rxfire' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n()) 

#Get final stratlayers and numbers to sample from each
frap.strat <- inner_join(frap.disturb, frap.control, by = 'stratlayer') %>% 
  group_by(stratlayer) %>% summarize(n = min(n.x,n.y)) 

rx.strat <- inner_join(rx.disturb, rx.control, by = 'stratlayer') %>% #Inner Join the disturb and control data sets
  group_by(stratlayer) %>% summarize(n = min(n.x,n.y)) #Take the minimum of the number of pixels as the sample number

# rx.strat %>% pull(n)

#Set the random number seed
set.seed(4561)
# colnames(pixel.data %>% dplyr::select(-'AET':'tpa_max'))
#Sample the prescribed fire control pixels
rx.sample <- pixel.data %>%
  filter(treatment == 'Control' & fire.type.bin == 'Rxfire' & stratlayer %in% (rx.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (rx.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work, .y = n
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the Wildfire Control control pixels
frap.sample <- pixel.data %>%
  filter(treatment == 'Control' & fire.type.bin == 'Wildfire' & stratlayer %in% (frap.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (frap.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the moderate severity control pixels

#Make sure the stratlayer bins match with the sampled control bins
#Make sure the stratlayer bins match with the sampled control bins
#Make sure the stratlayer disturb bins match with the sampled control bins
rx.disturb <- pixel.data %>%
  filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire' & stratlayer %in% (rx.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (rx.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work, .y = n
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the Wildfire Disturb pixels
frap.disturb <- pixel.data %>%
  filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire' & stratlayer %in% (frap.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (frap.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data                                                                                                                 fire.type.bin == 'Wildfire' ~ stratlayer %in% (frap.strat %>% pull(stratlayer))))

#Combine the sampled data back together
pixel.sample <- rbind(frap.disturb, rx.disturb, rx.sample, frap.sample)

#Convert data to long format
#This should be moved later
pixel.sample <- pixel.sample %>% 
  pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

#Convert the year outputs to actual years
pixel.sample$year <- as.numeric(pixel.sample$year) + 1984 

#Convert missing TPA data to NAs
pixel.sample[pixel.sample$tpa_max < 0,]$tpa_max <- NA

#Convert to trees per hectare
pixel.sample$tpa_max <- pixel.sample$tpa_max * 2.47105

#Make the dates into date time format for R
pixel.sample$date <- as.Date(as.character(pixel.sample$year), format = '%Y')
pixel.sample$vi.year <- pixel.sample$year
pixel.sample$stand.age <- as.numeric(pixel.sample$year) - as.numeric(pixel.sample$fire.year) 

#Update Cover data to 100% scale
pixel.sample$Tree_Cover.2 <- pixel.sample$Tree_Cover / 100
pixel.sample$Shrub_Cover.2 <- pixel.sample$Shrub_Cover / 100
pixel.sample$Herb_Cover.2 <- pixel.sample$Herb_Cover / 100
pixel.sample$Bare_Cover.2 <- pixel.sample$Bare_Cover / 100
# pixel.sample$Tree_Cover.2 <- pixel.sample$Tree_Cover

#Rename Montana Tree Cover
pixel.sample$Tree_Cover <- pixel.sample$TRE
pixel.sample$Shrub_Cover <- pixel.sample$SHR
pixel.sample$Herb_Cover <- pixel.sample$AFG + pixel.sample$PFG
pixel.sample$Bare_Cover <- pixel.sample$BGR 

#Convert the SPI48 scale back to decimal
pixel.sample$SPI48 <- pixel.sample$SPI48 / 100

#Try to fix soil moisture by dividing by 10
pixel.sample$Soil_Moisture <- pixel.sample$Soil_Moisture / 10

#Calculate Pr-ET
pixel.sample$PrET <- pixel.sample$ppt - pixel.sample$AET

#Separate the data
pixel.sample <- pixel.sample %>% mutate(std.year.bin = case_when(
  # fire.year < 1980 ~ '< 1980',
  fire.year >= 1984 & fire.year <= 1990 ~ '1984-1990',
  fire.year >= 1991 & fire.year <= 1995 ~ '1991-1995',
  fire.year >= 1996 & fire.year <= 2000 ~ '1996-2000',
  fire.year >= 2001 & fire.year <= 2005 ~ '2001-2005',
  fire.year >= 2006 & fire.year <= 2010 ~ '2006-2010'))

pixel.sample$std.year.bin = with(pixel.sample, factor(std.year.bin, levels = c('2006-2010', '2001-2005','1996-2000', '1991-1995','1984-1990')))#
# summary(pixel.sample)
#Tree Cover versus Elevation versus Latitude
#Tree Cover versus Elevation versus Latitude

#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills

# summary(pixel.sample)
p1 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(stand.age, treatment, fire.type.bin,std.year.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub', linetype = treatment), linewidth = 1) +
  #Shrub Cover 95% CI
  geom_errorbar(data = pixel.sample %>% 
                filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, color = "Shrub",  linetype = treatment), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree',  linetype = treatment), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.sample %>% 
                filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, color = "Tree",  linetype = treatment), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb',  linetype = treatment), linewidth = 1) + 
  #Herb Cover 95% CI
  geom_errorbar(data = pixel.sample %>% 
                filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
                summarize(Herb_Cover.mean = mean(Herb_Cover),
                          Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
              mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            x = stand.age, color = "Herb",  linetype = treatment), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare',  linetype = treatment), linewidth = 1) + 
  #Bare Cover 95% CI
  geom_errorbar(data = pixel.sample %>%
                filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, fire.type.bin, std.year.bin) %>%
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
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') + facet_grid(std.year.bin ~ fire.type.bin) +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire')
p1

#Save the data
ggsave(filename = 'FigS3_frap_stand_age_veg_cover.png', height=18, width= 20, units = 'cm', dpi=900)

#Subtract the pre-fire values for AET, tree and shrub cover
pixel.sample <- pixel.sample %>%
         group_by(system.index, fire.type.bin) %>% 
         mutate(dAET = AET - mean(AET[stand.age %in% c(-1, -2)]),
                dTree_Cover = Tree_Cover - mean(Tree_Cover[stand.age %in% c(-1, -2)]),
                dShrub_Cover = Shrub_Cover - mean(Shrub_Cover[stand.age %in% c(-1, -2)])) %>%
         ungroup()
  # group_by(stand.age, fire.type.bin) %>%
  # summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control']))
summary(pixel.sample)

#Create fire recovery curves
p2a <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
              group_by(stand.age, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(dTree_Cover[treatment == 'Disturb']) - mean(dTree_Cover[treatment == 'Control'])),
            mapping = aes(x = stand.age, y = Tree_Cover.mean, color = fire.type.bin), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.sample %>%
                  filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(Tree_Cover.mean = mean(dTree_Cover[treatment == 'Disturb']) - mean(dTree_Cover[treatment == 'Control']),
                            Tree_Cover.sd = sd(dTree_Cover[treatment == 'Disturb'])^2 + sd(dTree_Cover[treatment == 'Control'])^2, 
                            Tree_Cover.n = n()),
                mapping = aes(ymin=Tree_Cover.mean - 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
                              ymax=Tree_Cover.mean + 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
                              x = stand.age, color = fire.type.bin)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.06, 0.4), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  # scale_fill_manual(values = fills) + 
  # guides(fill = "none") +
  ylab(expression('Tree Change (%)')) + xlab('Years Since Fire')
p2a

#Pr-ET change with wildfire (FRAP)
p2b <- ggplot() + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(stand.age, fire.type.bin) %>%
              summarize(Shrub_Cover.mean = mean(dShrub_Cover[treatment == 'Disturb']) - mean(dShrub_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = fire.type.bin), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.sample %>%
                  filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(Shrub_Cover.mean = mean(dShrub_Cover[treatment == 'Disturb']) - mean(dShrub_Cover[treatment == 'Control']),
                            Shrub_Cover.sd = sd(dShrub_Cover[treatment == 'Disturb'])^2 + sd(dShrub_Cover[treatment == 'Control'])^2, 
                            Shrub_Cover.n = n()),
                mapping = aes(ymin=Shrub_Cover.mean - 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
                              ymax=Shrub_Cover.mean + 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
                              x = stand.age, color = fire.type.bin)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  ylab(expression('Shrub Change (%)')) + xlab('Years Since Fire')
p2b

#AET change with wildfire (FRAP)
p2c <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#Create a Tree Cover line
geom_line(data = pixel.sample %>%
            filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
            group_by(stand.age, fire.type.bin) %>%
            summarize(AET.mean = mean(dAET[treatment == 'Disturb']) - mean(dAET[treatment == 'Control'])), 
          mapping = aes(x = stand.age, y = AET.mean, color = fire.type.bin), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = pixel.sample %>%
                  filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year >= 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(AET.mean = mean(dAET[treatment == 'Disturb']) - mean(dAET[treatment == 'Control']),
                            AET.sd = sd(dAET[treatment == 'Disturb'])^2 + sd(dAET[treatment == 'Control'])^2, 
                            AET.n = n()),
                mapping = aes(ymin=AET.mean - 1.96*(sqrt(AET.sd / AET.n)),
                              ymax=AET.mean + 1.96*(sqrt(AET.sd / AET.n)),
                              x = stand.age, color = fire.type.bin)) +
theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  #scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  ylab(expression('AET Change (mm yr'^-1*')')) + xlab('Years Since Fire')
p2c

f1 <- ggarrange(p2a,p2b,p2c, nrow = 3, ncol = 1, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v")
f1
#Save the data
ggsave(filename = 'Fig6_frap_stand_age_tree_shrub_ET.png', height=15, width= 20, units = 'cm', dpi=900)
