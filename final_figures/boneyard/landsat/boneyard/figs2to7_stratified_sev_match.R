#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: March 25, 2023
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'MatchIt', 'purrr')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# library(purrr)
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')
# setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/landsat')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
# dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"

#Add the data
sev.data <- read.csv(file.path(dir_in, "fire_south_sierra_USFS_sevfire_500pt_200mm_5tree_ts8_300m_20230322.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
sev.data$treatment <- 'Disturb'
# summary(sev.data)
# list.files(fire_in)
# list.files(fire_in)
raw.sev.control.data <- read.csv(file.path(dir_in, "control_south_sierra_sev_2km_buffer_500pt_200mm_5tree_ts16_300m_20230322.csv"), header = TRUE, na.strings = "NaN")
# unchanged.control.data <- read.csv(file.path(dir_in, "control_south_sierra_unchanged_sev_2km_buffer_200pt_100mm_2C_5tree_ts16_300m_20230227_V2.csv"), header = TRUE, na.strings = "NaN")
# low.control.data <- read.csv(file.path(dir_in, "control_south_sierra_low_sev_2km_buffer_200pt_100mm_2C_5tree_ts16_300m_20230227_V2.csv"), header = TRUE, na.strings = "NaN")
# med.control.data <- read.csv(file.path(dir_in, "control_south_sierra_med_sev_2km_buffer_200pt_100mm_2C_5tree_ts16_300m_20230227_V2.csv"), header = TRUE, na.strings = "NaN")
# high.control.data <- read.csv(file.path(dir_in, "control_south_sierra_high_sev_2km_buffer_200pt_100mm_2C_5tree_ts16_300m_20230227_V2.csv"), header = TRUE, na.strings = "NaN")

#Duplicate and add the fire severity columns
unchanged.control.data <- raw.sev.control.data
unchanged.control.data$fire_sev_2010 <- 1
low.control.data <- raw.sev.control.data
low.control.data$fire_sev_2010 <- 2
med.control.data <- raw.sev.control.data
med.control.data$fire_sev_2010 <- 3
high.control.data <- raw.sev.control.data
high.control.data$fire_sev_2010 <- 4
# unchanged.control.data
# raw.sev.control.data
# sev.data
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

#Add Control treatment column
sev.control.data$treatment <- 'Control' #Try making this 1-km versus, 2-km

#Combine the data together
sev.pixel.data <- rbind(sev.data, sev.control.data)
# pixel.data <- rbind(combine.data, control.data.2km)
# summary(sev.pixel.data)

`%notin%` <- Negate(`%in%`)

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

#Use the FRAP fire perimeter year
sev.pixel.data$fire.year <- sev.pixel.data$fire_year_2010

#Do categorical treatments
sev.pixel.data <- sev.pixel.data %>% mutate(treat = case_when(treatment == 'Disturb' ~ 1, treatment == 'Control' ~ 0))

#Create Fire Year Bins
#Separate the data
sev.pixel.sample <- sev.pixel.sample %>% mutate(fire.year.bin = case_when(
  # fire.year < 1980 ~ '< 1980',
  fire.year >= 1985 & fire.year <= 1990 ~ '1985-1990',
  fire.year >= 1991 & fire.year <= 1995 ~ '1991-1995',
  fire.year >= 1996 & fire.year <= 2000 ~ '1996-2000',
  fire.year >= 2001 & fire.year <= 2005 ~ '2001-2005',
  fire.year >= 2006 & fire.year <= 2010 ~ '2006-2010'))

#Fire year bins for Fire Severity Data
sev.pixel.data$fire.year.bin = with(sev.pixel.data, factor(fire.year.bin, levels = c('2006-2010', '2001-2005','1996-2000', '1991-1995','1985-1990')))

#Fire Severity Bins
#With re-export type needs to be converted to sev
sev.pixel.data <- sev.pixel.data %>% mutate(sev.bin = case_when(
  fire_sev_2010 == '0' ~ 'No Fire',
  fire_sev_2010 == '1' ~ 'Unchanged',
  fire_sev_2010 == '2' ~ 'Low',
  fire_sev_2010 == '3' ~ 'Mid',
  fire_sev_2010 == '4' ~ 'High',
  fire_sev_2010 == '255' ~ 'Masked')) # end function
# sev.pixel.data %>% summary()


#Make the years bin lables in the correct order
sev.pixel.data$sev.bin = with(sev.pixel.data, factor(sev.bin, levels = c('No Fire','Unchanged', 'Low','Mid', 'High')))#c('No Fire','Masked', 'Unchanged or Low','Mid or High')))

#Recode the veg type data
# sev.pixel.data$veg_name <- recode(.x=sev.pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
#                               '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
#                               '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

# sev.pixel.data %>% summary()

#Select strat categories for fire treatments
un.disturb <- sev.pixel.data %>% filter(sev.bin == 'Unchanged' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n())
lo.disturb <- sev.pixel.data %>% filter(sev.bin == 'Low' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n())
mid.disturb <- sev.pixel.data %>% filter(sev.bin == 'Mid' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n())
hi.disturb <- sev.pixel.data %>% filter(sev.bin == 'High' & treatment == 'Disturb') %>% group_by(stratlayer) %>% summarize(n = n())

un.control <- sev.pixel.data %>% filter(sev.bin == 'Unchanged' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n())  
un.test <- un.control <- sev.pixel.data %>% filter(sev.bin == 'Unchanged' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n())
lo.control <- sev.pixel.data %>% filter(sev.bin == 'Low' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n())
mid.control <- sev.pixel.data %>% filter(sev.bin == 'Mid' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n())
hi.control <- sev.pixel.data %>% filter(sev.bin == 'High' & treatment == 'Control') %>% group_by(stratlayer) %>% summarize(n = n())

un.strat <- inner_join(un.disturb, un.control, by = 'stratlayer') %>%
            group_by(stratlayer) %>% summarize(n = min(n.x,n.y)) 
lo.strat <- inner_join(lo.disturb, lo.control, by = 'stratlayer') %>%
            group_by(stratlayer) %>% summarize(n = min(n.x,n.y)) 
mid.strat <- inner_join(mid.disturb, mid.control, by = 'stratlayer') %>%
             group_by(stratlayer) %>% summarize(n = min(n.x,n.y)) 
hi.strat <- inner_join(hi.disturb, hi.control, by = 'stratlayer') %>%
              group_by(stratlayer) %>% summarize(n = min(n.x,n.y)) 

#Set the random number seed
set.seed(4561)

#Sample the unchanged control pixels
un.sample <- sev.pixel.data %>%
                    filter(treatment == 'Control' & sev.bin == 'Unchanged' & stratlayer %in% (un.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
                    group_by(stratlayer) %>% #Group by Stratification layer
                    nest() %>% #Nest the data
                    ungroup() %>% #Un group the data
                    mutate(n = (un.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
                    mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work.
                    dplyr::select(-data) %>% #Get rid of the data column
                    unnest(samp) #unnest the data

#Sample the low severity control pixels
lo.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'Low' & stratlayer %in% (lo.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (lo.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the moderate severity control pixels
mid.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'Mid' & stratlayer %in% (mid.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (mid.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#High Severity Samples
hi.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'High' & stratlayer %in% (hi.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (hi.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Make sure the stratlayer bins match with the sampled control bins
#Sample the unchanged control pixels
un.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Unchanged' & stratlayer %in% (un.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (un.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work.
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the low severity control pixels
lo.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Low' & stratlayer %in% (lo.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (lo.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the moderate severity control pixels
mid.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Mid' & stratlayer %in% (mid.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (mid.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#High Severity Samples
hi.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'High' & stratlayer %in% (hi.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (hi.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Combine the sampled data back together
sev.pixel.sample <- rbind(un.disturb, lo.disturb, mid.disturb, hi.disturb, un.sample, lo.sample, mid.sample, hi.sample)

sev.pixel.sample <- sev.pixel.sample %>% 
  pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

#Convert the year outputs to actual years
sev.pixel.sample$year <- as.numeric(sev.pixel.sample$year) + 1984 

#Convert missing TPA data to NAs
sev.pixel.sample[sev.pixel.sample$tpa_max < 0,]$tpa_max <- NA

#Convert to trees per hectare
sev.pixel.sample$tpa_max <- sev.pixel.sample$tpa_max * 2.47105

#Make the dates into date time format for R
sev.pixel.sample$date <- as.Date(as.character(sev.pixel.sample$year), format = '%Y')

#Add VI Year
sev.pixel.sample$vi.year <- sev.pixel.sample$year

#Caluclate Stand AGe
sev.pixel.sample$stand.age <- as.numeric(sev.pixel.sample$year) - as.numeric(sev.pixel.sample$fire.year) 

#Update Cover data to 100% scale
sev.pixel.sample$Tree_Cover.2 <- sev.pixel.sample$Tree_Cover / 100
sev.pixel.sample$Shrub_Cover.2 <- sev.pixel.sample$Shrub_Cover / 100
sev.pixel.sample$Herb_Cover.2 <- sev.pixel.sample$Herb_Cover / 100
sev.pixel.sample$Bare_Cover.2 <- sev.pixel.sample$Bare_Cover / 100

#Add Montana Veg Cover
sev.pixel.sample$Tree_Cover <- sev.pixel.sample$TRE
sev.pixel.sample$Shrub_Cover <- sev.pixel.sample$SHR
sev.pixel.sample$Herb_Cover <- sev.pixel.sample$AFG + sev.pixel.sample$PFG
sev.pixel.sample$Bare_Cover <- sev.pixel.sample$BGR 

#Convert the SPI48 scale back to decimal
sev.pixel.sample$SPI48 <- sev.pixel.sample$SPI48 / 100

#Try to fix soil moisture by dividing by 10
sev.pixel.sample$Soil_Moisture <- sev.pixel.sample$Soil_Moisture / 10

#Rename ppt and Water Stress
sev.pixel.sample$Water_Stress <- sev.pixel.sample$Water_Stress
sev.pixel.sample$ppt <- sev.pixel.sample$ppt
sev.pixel.sample$AET <- sev.pixel.sample$AET
sev.pixel.sample$GPP <- sev.pixel.sample$GPP
sev.pixel.sample$elevation <- sev.pixel.sample$elevation
sev.pixel.sample$PrET <- sev.pixel.sample$ppt - sev.pixel.sample$AET

summary(sev.pixel.sample)

#Testing out the control matches...
pa <- ggplot(data = sev.pixel.sample %>% 
         filter(fire.year <= 2010 & fire.year > 1986 & sev.bin != 'No Fire' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
         #Match the controls to the disturbed based on the stratified sampling bins
         # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
         #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
         #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
         #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
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
pa

pb <- ggplot(data = sev.pixel.sample %>% 
               filter(fire.year <= 2010 & fire.year > 1986 & sev.bin != 'No Fire' & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
               #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
               #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
               #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), 
                         sev.bin = sev.bin[vi.year == 2010], 
                         clm_precip_sum = clm_precip_sum[vi.year == 2010],
                         clm_temp_mean = clm_temp_mean[vi.year == 2010],
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
                         Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013,2014)]),
                         pre.tree = Tree_Cover[vi.year == fire.year - 2])) + 
  geom_density(mapping = aes(x = clm_precip_sum, color = treatment)) +
  facet_wrap(. ~ sev.bin)
pb

pc <- ggplot(data = sev.pixel.sample %>% 
               filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
               #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
               #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
               #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), 
                         sev.bin = sev.bin[vi.year == 2010], 
                         clm_precip_sum = clm_precip_sum[vi.year == 2010],
                         clm_temp_mean = clm_temp_mean[vi.year == 2010],
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
                         Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013,2014)]),
                         pre.tree = Tree_Cover[vi.year == fire.year - 2])
             #%>% filter(!is.na(pre.tree))
             ) + 
  geom_density(mapping = aes(x = pre.tree, color = treatment)) +
  facet_wrap(. ~ sev.bin)
pc

pd <- ggplot(data = sev.pixel.sample %>% 
               filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
               #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
               #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
               #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), 
                         sev.bin = sev.bin[vi.year == 2010], 
                         elevation = elevation[vi.year == 2010],
                         clm_temp_mean = clm_temp_mean[vi.year == 2010],
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
                         Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013,2014)]),
                         pre.tree = Tree_Cover[vi.year == fire.year - 2])
             #%>% filter(!is.na(pre.tree))
) + 
  geom_density(mapping = aes(x = elevation, color = treatment)) +
  facet_wrap(. ~ sev.bin)
pd

pe <- ggplot(data = sev.pixel.sample %>% 
               filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
               #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
               #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
               #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE), 
                         sev.bin = sev.bin[vi.year == 2010], 
                         latitude = latitude[vi.year == 2010],
                         clm_temp_mean = clm_temp_mean[vi.year == 2010],
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
                         Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013,2014)]),
                         pre.tree = Tree_Cover[vi.year == fire.year - 2])
             #%>% filter(!is.na(pre.tree))
) + 
  geom_density(mapping = aes(x = latitude, color = treatment)) +
  facet_wrap(. ~ sev.bin)
pe

#
#Tree Cover versus Elevation versus Latitude
p1 <- ggplot() +
  #Data Summary
  geom_bin2d(data = sev.pixel.sample %>% #sev.bin != 'Unchanged' & 
               filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
               #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
               #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
               #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, treatment, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2016, 2017)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), 
                         Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2011, 2012)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = Water_Stress, y = Tree_Cover, fill = dTree, group = dTree), binwidth = c(500, 10)) + 
  theme_bw() +
  scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-25, 25), midpoint = 0, na.value = 'transparent') + 
  # scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ sev.bin) +
  xlab('Water Stress') + ylab('Tree Cover (%)')
p1

p2 <- ggplot() +
  #Data Summary
  geom_bin2d(data = sev.pixel.sample %>% #sev.bin != 'Unchanged' & 
               filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
               #Match the controls to the disturbed based on the stratified sampling bins
               # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
               #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
               #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
               #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
               dplyr::group_by(system.index, treatment, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2016, 2017)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
                         tpa_max = max(tpa_max[vi.year %in% c(2015, 2016, 2017)], na.rm = TRUE),
                         Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2011, 2012)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = Water_Stress, y = Tree_Cover, fill = tpa_max, group = tpa_max), binwidth = c(500, 10)) + 
  theme_bw() +
  scale_fill_gradient(name = "Die-off \n(trees per hectare)", low = "white", high = "red", na.value = 'transparent') +
  # scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(treatment ~ sev.bin) +
  xlab('Water Stress') + ylab('Tree Cover (%)')
p2

f1 <- ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f1
#Save the data
ggsave(filename = 'Fig2c_sev_fire_dieoff_tree_cover_fireyear_geographic_distribution.png', height=20, width= 24, units = 'cm', dpi=900)

#Figure of Dead Trees per acre separated by fire years with time series
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # & 
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
  geom_ribbon(data = sev.pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
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
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
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
  geom_line(data = sev.pixel.sample %>%
              filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()), #%>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = Tree_Cover.mean, color = treatment, linetype = treatment), 
            size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
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
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
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
  geom_line(data = sev.pixel.sample %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat & clm_precip_sum <= 1500)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()), # %>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = ppt.mean, color = treatment, linetype = treatment), 
            size = 1) +
  #Precip 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                #                  sev.bin == 'High' ~ stratlayer %in% hi.strat & clm_precip_sum <= 1500)) %>%
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
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
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
  geom_line(data = sev.pixel.sample %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat & clm_precip_sum <= 1500)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()), # %>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = AET.mean, color = treatment, linetype = treatment), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                #                  sev.bin == 'High' ~ stratlayer %in% hi.strat & clm_precip_sum <= 1500)) %>%
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
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + ylim(200, 550) + 
  #facet_grid(. ~ sev.bin) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year') 
p8

#Create the Water Stress Panel
p10 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = sev.pixel.sample %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # & 
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat & clm_precip_sum <= 1500)) %>%
              filter(vi.year >= 2010) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin, treatment) %>%
              summarize(PrET.mean = mean(PrET), PrET.n = n(), count = n()), #%>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = PrET.mean, color = treatment, linetype = treatment), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # & 
                #Match the controls to the disturbed based on the stratified sampling bins
                # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                #                  sev.bin == 'High' ~ stratlayer %in% hi.strat & clm_precip_sum <= 1500)) %>%
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
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.15, 0.35), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ sev.bin) +
  ylab(expression('Pr-ET (mm yr'^-1*')')) + xlab('Year')
p10

f3 <- ggarrange(p7, p8, p10, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f3
#Save the data
ggsave(filename = 'Fig4c_sev_water_fluxes_time_series.png', height=16, width= 18, units = 'cm', dpi=900)

#Figure 5c: Tree Cover Die-off
p11 <- ggplot() +
  #Data Summary
  stat_summary(data = sev.pixel.sample %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = dTree, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.sample %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # & 
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>% 
                 # filter(elevation <= 3000) %>% 
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin, treatment) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
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
  stat_summary(data = sev.pixel.sample %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                         RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]), Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = RdTree * 100, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.sample %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # filter(elevation <= 3000) %>% 
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin, treatment) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]), Water_Stress = Water_Stress[vi.year == 2015]),
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
  stat_summary(data = sev.pixel.sample %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = tpa_max, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.sample %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
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
  stat_summary(data = sev.pixel.sample %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(sev.bin = sev.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011, 2012)])),
             mapping = aes(x = sev.bin, y = Tree_Cover, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.sample %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # filter(elevation <= 3000) %>% 
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin, treatment) %>%
                  summarize(sev.bin = sev.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011, 2012)])),
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
  stat_summary(data = sev.pixel.sample %>% 
               filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                   # filter(elevation <= 3000) %>%
                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin, treatment) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), sev.bin = sev.bin[vi.year == 2010], 
                         Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
             mapping = aes(x = sev.bin, y = Water_Stress, fill = treatment), 
             fun = mean, geom = "bar", position = 'dodge', alpha = 0.7) + 
  stat_summary(data = sev.pixel.sample %>% 
                  filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | is.na(fire_year_2019))) %>% # &
                 #Match the controls to the disturbed based on the stratified sampling bins
                 # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                 #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                 #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                 #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                 # # filter(elevation <= 3000) %>% 
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
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire perimeter and fier year by pixel match 
              group_by(stand.age, treatment, sev.bin, fire.year.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub', linetype = treatment), size = 1) +
  #Shrub Cover 95% CI
  geom_errorbar(data = sev.pixel.sample %>% 
                  filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                  #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                  #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                  #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, treatment, sev.bin, fire.year.bin) %>%
                  summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                            Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
                mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                              ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                              x = stand.age, color = "Shrub",  linetype = treatment), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin, fire.year.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree',  linetype = treatment), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = sev.pixel.sample %>% 
                  filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                  #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                  #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                  #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, treatment, sev.bin, fire.year.bin) %>%
                  summarize(Tree_Cover.mean = mean(Tree_Cover),
                            Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
                mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                              ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                              x = stand.age, color = "Tree",  linetype = treatment), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin, fire.year.bin) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb',  linetype = treatment), size = 1) + 
  #Herb Cover 95% CI
  geom_errorbar(data = sev.pixel.sample %>% 
                  filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                  #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                  #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                  #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, treatment, sev.bin, fire.year.bin) %>%
                  summarize(Herb_Cover.mean = mean(Herb_Cover),
                            Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
                mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                              ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                              x = stand.age, color = "Herb",  linetype = treatment), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin, fire.year.bin) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare',  linetype = treatment), size = 1) + 
  #Bare Cover 95% CI
  geom_errorbar(data = sev.pixel.sample %>%
                  filter(stand.age >= -5 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                  #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                  #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                  #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                  # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                  group_by(stand.age, treatment, sev.bin, fire.year.bin) %>%
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
  scale_fill_manual(values = fills) + facet_grid(fire.year.bin ~ sev.bin) +
  guides(fill = "none") +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire')
p18

#Save the data
ggsave(filename = 'Fig8c_sev_stand_age_veg_cover.png', height=18, width= 20, units = 'cm', dpi=900)

p19 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Tree_Cover.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = sev.pixel.sample %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                  #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                  #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                  #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
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
ggsave(filename = 'Fig9c_sev_stand_age_tree_cover.png', height=8, width= 20, units = 'cm', dpi=900)

#AET change with wildfire (FRAP)
p20 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, sev.bin) %>%
              summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = AET.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = sev.pixel.sample %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
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
ggsave(filename = 'Fig10c_sev_stand_age_AET.png', height=8, width= 20, units = 'cm', dpi=900)

#Pr-ET change with wildfire (FRAP)
p21 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              #Match the controls to the disturbed based on the stratified sampling bins
              # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
              #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
              #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
              #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, sev.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Shrub_Cover.mean), size = 1) + 
  #Tree Cover 95% CI
  geom_errorbar(data = sev.pixel.sample %>%
                  filter(stand.age >= -5 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  #Match the controls to the disturbed based on the stratified sampling bins
                  # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                  #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                  #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                  #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
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
ggsave(filename = 'Fig11c_sev_stand_age_shrub.png', height=8, width= 20, units = 'cm', dpi=900)

# sev.pixel.sample %>% summary()
# summary(sev.pixel.sample)

#Stand age versus die-off
p22 <- ggplot(data = sev.pixel.sample %>% filter(fire.year <= 2010 & fire.year > 1986 & !is.na(tpa_max) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                #Match the controls to the disturbed based on the stratified sampling bins
                # filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                #                  sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                #                  sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                #                  sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                dplyr::group_by(system.index, treatment, sev.bin) %>% 
                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
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
