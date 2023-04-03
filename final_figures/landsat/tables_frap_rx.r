#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: April 3, 2023
#Purpose: Create merge split raster files

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/chrono
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/chrono
#Run the script: R < stand_age.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggplot2', 'magrittr', 'stats', 'patchwork','ggpmisc', 'raster', 'RStoolbox', 'quantreg','segmented', 'RColorBrewer',
	   'gt', 'gtsummary', 'webshot', 'stargazer', 'kableExtra', 'broom', 'svglite','sjPlot','purrr', 'sjmisc', 'magick', 'magrittr', 'knitr', 'xtable', 'purrr')
# install.packages('quantreg',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\CECS"
dir <- "D:\\Large_Files\\CECS"
# memory.limit(32000)

#Set the working directory
# setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')
setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/landsat')

#The data directory
# dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the Wildfire data
frap.fire.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_wildfire_500pt_fire_year_5tree_ts8_300m_20230403.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
frap.fire.data$treatment <- 'Disturb'

#Add the Wildfire buffer data
frap.control.data <- read.csv(file.path(dir_in, "control_south_sierra_FRAP_2km_buffer_500pt_fire_year_5tree_ts16_300m_20230403.csv"), header = TRUE, na.strings = "NaN")

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
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_500pt_fire_year_5tree_ts8_300m_20230403.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
rx.data$treatment <- 'Disturb'

#Add teh Rx fire buffer data
rx.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Rx_2km_buffer_500pt_fire_year_5tree_ts16_300m_20230403.csv"), header = TRUE, na.strings = "NaN")

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

#Add the fire year bins
pixel.data <- pixel.data %>% mutate(fire.year.bin = case_when(
  treatment == 'Control' | fire.year < 1980 ~ 'Control',
  fire.year >= 1980 & fire.year <= 2010 ~ 'Disturb',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2018',
  fire.year >= 2019 ~ '2019-2020'))#'0-4'))

pixel.data <- pixel.data %>% mutate(fire.type.bin = case_when(
  fire_type_2010 == 1 ~ 'Wildfire',
  fire_type_2010 == 2 ~ 'Rxfire'
))

summary(pixel.data)

pixel.data$fire.year.bin = with(pixel.data, factor(fire.year.bin, levels = c('2019-2020', '2011-2018', 'Control', 'Disturb')))#

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
  inner_join(rx.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (rx.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work, .y = n
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the Wildfire Control control pixels
frap.sample <- pixel.data %>%
  filter(treatment == 'Control' & fire.type.bin == 'Wildfire' & stratlayer %in% (frap.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(frap.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (frap.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the moderate severity control pixels

#Make sure the stratlayer bins match with the sampled control bins
#Make sure the stratlayer disturb bins match with the sampled control bins
rx.disturb.sample <- pixel.data %>%
  filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire' & stratlayer %in% (rx.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(rx.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (rx.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work, .y = n
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the Wildfire Disturb pixels
#Weird bug the alignment between frap.strat and the pixel.data seems to be off by one so the stratlayer and n-values don't match
frap.disturb.sample <- pixel.data %>%
  filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire' & stratlayer %in% (frap.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(frap.strat) %>% #Add the sample sizes for the stratlayers in the disturbed data
  ungroup() %>% #Un group the data
  # mutate(n = (frap.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data                                                                                                                 fire.type.bin == 'Wildfire' ~ stratlayer %in% (frap.strat %>% pull(stratlayer))))

# print(frap.disturb.sample[39, ])
# frap.strat %>% pull(n)
# frap.strat %>% pull(stratlayer)
# frap.disturb.sample %>% pull(stratlayer)
# print(frap.disturb %>% dplyr::filter(stratlayer == 19900600))
# print(frap.control %>% dplyr::filter(stratlayer == 19900600))
# print(frap.strat %>% dplyr::filter(stratlayer == 19900600))
#Combine the sampled data back together
pixel.sample <- rbind(frap.disturb.sample, rx.disturb.sample, rx.sample, frap.sample)

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
#Update Cover data to 100% scale
pixel.sample$Tree_Cover.2 <- pixel.sample$Tree_Cover / 100
pixel.sample$Shrub_Cover.2 <- pixel.sample$Shrub_Cover / 100
pixel.sample$Herb_Cover.2 <- pixel.sample$Herb_Cover / 100
pixel.sample$Bare_Cover.2 <- pixel.sample$Bare_Cover / 100

#Add Montana Veg Cover
pixel.sample$Tree_Cover <- pixel.sample$TRE
pixel.sample$Shrub_Cover <- pixel.sample$SHR
pixel.sample$Herb_Cover <- pixel.sample$AFG + pixel.sample$PFG
pixel.sample$Bare_Cover <- pixel.sample$BGR 

# #Rename Montana Tree Cover
# pixel.sample$Tree_Cover <- pixel.sample$TRE

#Convert the SPI48 scale back to decimal
pixel.sample$SPI48 <- pixel.sample$SPI48 / 100

#Try to fix soil moisture by dividing by 10
pixel.sample$Soil_Moisture <- pixel.sample$Soil_Moisture / 10

#Calculate Pr-ET
pixel.sample$PrET <- pixel.sample$ppt - pixel.sample$AET

#Filter the data into subsets for modeling
pixel.filter <- pixel.sample %>% filter(fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
  # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
  #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
  dplyr::group_by(system.index) %>% 
  summarize(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011, 2012)]),
            RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]),
            Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011,2012)]),
            PrET_4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
            Water_Stress = Water_Stress[vi.year == 2015],
            ADS = mean(tpa_max[vi.year %in% c(2015, 2016, 2017)]), 
            dNDMI = mean(NDMI[vi.year %in% c(2016, 2017)]) - mean(NDMI[vi.year %in% c(2009, 2010, 2011)]),
            fire.year.bin = fire.year.bin[vi.year == 2010],
            treatment = treatment[vi.year == 2010],
            fire.type.bin = fire.type.bin[vi.year == 2010])

age.dNDMI.rq <- rq(dNDMI_2015_mean ~ stand_age_mean, data = stand.age.sample, tau = q10)
print(age.dNDMI.rq %>% tidy())
tb1 <- age.dNDMI.rq %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 1: Quantile Regression, Die-off(dNDMI) ~ f(Stand Age)") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 5, file = "T1_dNDMI_stand_age_quantile_regression_results.png", zoom = 4.0)  


age.ADS.rq <- rq(ADS_2017_mean ~ stand_age_mean, data = stand.age.sample, tau = q10)
print(age.ADS.rq %>% tidy())
tb2 <- age.ADS.rq %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 2: Quantile Regression, Die-off(ADS) ~ f(Stand Age)") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 5, file = "T2_ADS_stand_age_quantile_regression_results.png", zoom = 4.0)  

stand.age.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age)

summary(stand.age.lm)
#Treatment Grid Cells
aov.dNDMI.stand.age.treatment <- aov(dNDMI_2015_mean ~ agegroup, data = stand.age.sample)
summary(aov.dNDMI.stand.age.treatment)

aov.PET4yr.stand.age.treatment <- aov(PET_4yr_2015_mean ~ agegroup, data = stand.age.sample)
summary(aov.PET4yr.stand.age.treatment)

aov.biomass.stand.age.treatment <- aov(biomass_2012_mean ~ agegroup, data = stand.age.sample)
summary(aov.biomass.stand.age.treatment)

tukey.dNDMI.stand.age.treatment <- TukeyHSD(aov.dNDMI.stand.age.treatment)
print(tukey.dNDMI.stand.age.treatment)

tukey.PET4yr.stand.age.treatment <- TukeyHSD(aov.PET4yr.stand.age.treatment)
print(tukey.PET4yr.stand.age.treatment)

tukey.biomass.stand.age.treatment <- TukeyHSD(aov.biomass.stand.age.treatment)
print(tukey.biomass.stand.age.treatment)

# anovas.treament <- anova(aov.dNDMI.stand.age.treatment, aov.PET4yr.stand.age.treatment, aov.biomass.stand.age.treatment)
# print(anovas.treatment) 

#Control Grid Cells
aov.dNDMI.stand.age.control <- aov(dNDMI_2015_mean ~ agegroup, data = stand.age.control)
summary(aov.dNDMI.stand.age.control)

aov.PET4yr.stand.age.control <- aov(PET_4yr_2015_mean ~ agegroup, data = stand.age.control)
summary(aov.PET4yr.stand.age.control)

aov.biomass.stand.age.control <- aov(biomass_2012_mean ~ agegroup, data = stand.age.control)
summary(aov.biomass.stand.age.control)

tukey.dNDMI.stand.age.control <- TukeyHSD(aov.dNDMI.stand.age.control)
print(tukey.dNDMI.stand.age.control)

tukey.PET4yr.stand.age.control <- TukeyHSD(aov.PET4yr.stand.age.control)
print(tukey.PET4yr.stand.age.control)

tukey.biomass.stand.age.control <- TukeyHSD(aov.biomass.stand.age.control)
print(tukey.biomass.stand.age.control)

tb1 <- tukey.dNDMI.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 1: Not Exposed, Tukey HSD, dNDMI ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 5, file = "Table1_Not_Exposed_Mortality_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb2 <- tukey.dNDMI.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 2: Exposed, Tukey HSD, dNDMI ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 5, file = "Table2_Exposed_Mortality_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb3 <- tukey.PET4yr.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 3: Not Exposed, Tukey HSD, four-year Pr-ET ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb3, width = 5, file = "Table3_Not_Exposed_Water_Deficit_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb4 <- tukey.PET4yr.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 4: Exposed, Tukey HSD, four-year Pr-ET ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb4, width = 5, file = "Table4_Exposed_Water_Deficit_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb5 <- tukey.biomass.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 5: Not Exposed, Tukey HSD, Biomass ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb5, width = 5, file = "Table5_Not_Exposed_Biomass_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb6 <- tukey.biomass.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 6: Exposed, Tukey HSD, Biomass ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb6, width = 5, file = "Table6_Exposed_Biomass_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

#Summary Statistics
summary.control <- stand.age.control %>% 
  group_by(agegroup) %>% 
  summarize(dNDMI_mean = mean(dNDMI_2015_mean),
            dNDMI_sd = sd(dNDMI_2015_mean),
			biomass_mean = mean(biomass_2012_mean),
			biomass_sd = sd(biomass_2012_mean))

print(summary.control)

summary.treatment <- stand.age.sample %>% 
  group_by(agegroup) %>% 
  summarize(dNDMI_mean = mean(dNDMI_2015_mean),
            dNDMI_sd = sd(dNDMI_2015_mean),
			biomass_mean = mean(biomass_2012_mean),
			biomass_sd = sd(biomass_2012_mean))

print(summary.treatment)

# stand.age.lm2 <- lm(dNDMI_2015_mean ~ stand_age_mean, data = stand.age)

# summary(stand.age.lm2)

# stand.age.socal.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age.socal)

# summary(stand.age.socal.lm)

# stand.age.norcal.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age.norcal)

# summary(stand.age.norcal.lm)

nrow(stand.age)
# summary(stand.age)
# nrow(stand.age.norcal)
# nrow(stand.age.socal)