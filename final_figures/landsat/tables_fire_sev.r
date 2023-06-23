#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: June 23, 2023
#Purpose: Create merge split raster files

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/chrono
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/chrono
#Run the script: R < stand_age.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggplot2', 'magrittr', 'stats', 'patchwork','ggpmisc', 'raster', 'RStoolbox', 'quantreg','segmented', 'RColorBrewer',
	   'gt', 'gtsummary', 'webshot', 'stargazer', 'kableExtra', 'broom', 'svglite','sjPlot','purrr', 'sjmisc', 'magick', 'magrittr', 'knitr', 'xtable')
# install.packages('quantreg',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\CECS"
# dir <- "D:\\Large_Files\\CECS"
# memory.limit(32000)

setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')
# setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/landsat')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
# dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"

#Add the data
sev.data <- read.csv(file.path(dir_in, "fire_south_sierra_USFS_sevfire_500pt_fire_year_5tree_ts8_300m_20230403.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
sev.data$treatment <- 'Disturb'
# summary(sev.data)
# list.files(fire_in)
# list.files(fire_in)
raw.sev.control.data <- read.csv(file.path(dir_in, "control_south_sierra_sev_2km_buffer_500pt_fire_year_5tree_ts16_300m_20230403.csv"), header = TRUE, na.strings = "NaN")
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
# sev.pixel.data[sev.pixel.data$fire_sev_2010 == -9999,]$fire_sev_2010 <- NA
# sev.pixel.data[sev.pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
# sev.pixel.data[sev.pixel.data$fire_ID_2010 == -9999,]$fire_ID_2010 <- NA
# sev.pixel.data[sev.pixel.data$fire_count_2010 == -9999,]$fire_count_2010 <- NA
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
  inner_join(un.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (un.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work.
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the low severity control pixels
lo.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'Low' & stratlayer %in% (lo.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(lo.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (lo.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the moderate severity control pixels
mid.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'Mid' & stratlayer %in% (mid.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(mid.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (mid.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#High Severity Samples
hi.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'High' & stratlayer %in% (hi.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(hi.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (hi.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Make sure the stratlayer bins match with the sampled control bins
#Sample the unchanged control pixels
un.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Unchanged' & stratlayer %in% (un.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(un.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (un.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work.
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the low severity control pixels
lo.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Low' & stratlayer %in% (lo.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(lo.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (lo.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the moderate severity control pixels
mid.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Mid' & stratlayer %in% (mid.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(mid.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (mid.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#High Severity Samples
hi.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'High' & stratlayer %in% (hi.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  inner_join(hi.strat) %>%
  ungroup() %>% #Un group the data
  # mutate(n = (hi.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
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

#Do Fire year bins
sev.pixel.sample <- sev.pixel.sample %>% mutate(fire.year.bin = case_when(
  # fire.year < 1980 ~ '< 1980',
  fire.year >= 1985 & fire.year <= 1990 ~ '1985-1990',
  fire.year >= 1991 & fire.year <= 1995 ~ '1991-1995',
  fire.year >= 1996 & fire.year <= 2000 ~ '1996-2000',
  fire.year >= 2001 & fire.year <= 2005 ~ '2001-2005',
  fire.year >= 2006 & fire.year <= 2010 ~ '2006-2010'))

#Fire year bins for Fire Severity Data
sev.pixel.sample$fire.year.bin = with(sev.pixel.sample, factor(fire.year.bin, levels = c('2006-2010', '2001-2005','1996-2000', '1991-1995','1985-1990')))

sev.pixel.filter <- sev.pixel.sample %>% filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
  dplyr::group_by(system.index, treatment, sev.bin) %>% 
  reframe(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011, 2012)]),
            RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]),
            Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011,2012)]),
            ET = mean(AET[vi.year %in% c(2011,2012)]),
            ADS = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)]),
            Water_Stress = Water_Stress[vi.year == 2015],
            PrET_4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), 
            sev.bin = sev.bin[vi.year == 2010],
            treatment = treatment[vi.year == 2010])

#Results for Table summarizing Figures 2 and 3
aov.dTree.treatment.sev <- aov(dTree ~ treatment * sev.bin, data = sev.pixel.filter)
summary(aov.dTree.treatment.sev)

aov.ADS.treatment.sev <- aov(ADS ~ treatment * sev.bin, data = sev.pixel.filter)
summary(aov.ADS.treatment.sev)

aov.PrET4yr.treatment.sev <- aov(PrET_4yr ~ treatment * sev.bin, data = sev.pixel.filter)
summary(aov.PrET4yr.treatment.sev)

aov.tree.treatment.sev <- aov(Tree_Cover ~ treatment * sev.bin, data = sev.pixel.filter)
summary(aov.tree.treatment.sev)

aov.ET.treatment.sev <- aov(ET ~ treatment * sev.bin, data = sev.pixel.filter)
summary(aov.tree.treatment.sev)

tukey.dTree.treatment.sev <- TukeyHSD(aov.dTree.treatment.sev)
print(tukey.dTree.treatment.sev)

tukey.ADS.treatment.sev <- TukeyHSD(aov.ADS.treatment.sev)
print(tukey.ADS.treatment.sev)

tukey.PrET4yr.treatment.sev <- TukeyHSD(aov.PrET4yr.treatment.sev)
print(tukey.PrET4yr.treatment.sev)

tukey.tree.treatment.sev <- TukeyHSD(aov.tree.treatment.sev)
print(tukey.tree.treatment.sev)

tukey.ET.treatment.sev <- TukeyHSD(aov.ET.treatment.sev)
print(tukey.ET.treatment.sev)

#Create the tables!
# tb5 <- tukey.dTree.treatment.sev %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 5: Tukey HSD, Die-off (dTree) ~ treatment * fire severity", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
# as_image(x = tb5, width = 5, file = "Table5_dTree_dieoff_fire_sev_Tukey_HSD.png", zoom = 4.0) 
# 
# tb6 <- tukey.ADS.treatment.sev %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 6: Tukey HSD, Die-off (ADS) ~ treatment * fire severity", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
# as_image(x = tb6, width = 5, file = "Table6_ADS_dieoff_fire_sev_Tukey_HSD.png", zoom = 4.0) 
# 
# tb7 <- tukey.PrET4yr.treatment.sev %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 7: four-year Pr-ET ~ treatment * fire severity", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
# as_image(x = tb7, width = 5, file = "Table7_PrET_4yr_fire_sev_Tukey_HSD_.png", zoom = 4.0) 
# 
# tb8 <- tukey.tree.treatment.sev %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 8: Pre-Drought Tree Cover ~ treatment * fire severity", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
# as_image(x = tb8, width = 5, file = "Table8_Tree_Cover_fire_sev_Tukey_HSD_.png", zoom = 4.0) 
# 
# tb9 <- tukey.ET.treatment.sev %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 9: Pre-Drought ET ~ treatment * fire severity", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
# as_image(x = tb9, width = 5, file = "Table9_ET_fire_sev_Tukey_HSD_.png", zoom = 4.0) 

#Work on combining the different data sets into a table
#Tukey HSD posthoc tests
#Combine all the t-test results in a list
tHSD <- list(tukey.ADS.treatment.sev, #tukey.PrET4yr.treatment.sev, 
             tukey.tree.treatment.sev, tukey.ET.treatment.sev)

#Combine the t-test results in a data frame
df.tHSD <- as.data.frame(purrr::map_df(tHSD, tidy))
tHSD.filter <- df.tHSD %>% filter(contrast %in% c('Disturb:Unchanged-Control:Unchanged', 'Disturb:Low-Control:Low', 
                                   'Disturb:Mid-Control:Mid', 'Disturb:High-Control:High'))
#Add a variable label column
# tHSD.filter$variable
tHSD.filter$variable = c('Die-off (trees ha<sup>-1</sup>)','Die-off (trees ha<sup>-1</sup>)','Die-off (trees ha<sup>-1</sup>)','Die-off (trees ha<sup>-1</sup>)',
                   # 'Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)',
                   'Pre-Drought Tree Cover (%)','Pre-Drought Tree Cover (%)','Pre-Drought Tree Cover (%)','Pre-Drought Tree Cover (%)',
                   'Pre-Drought ET (mm yr<sup>-1</sup>)','Pre-Drought ET (mm yr<sup>-1</sup>)','Pre-Drought ET (mm yr<sup>-1</sup>)','Pre-Drought ET (mm yr<sup>-1</sup>)')

tHSD.filter$fire.severity = c('Unchanged', 'Low', 'Moderate', 'High',
                              #'Unchanged', 'Low', 'Moderate', 'High',
                              'Unchanged', 'Low', 'Moderate', 'High',
                              'Unchanged', 'Low', 'Moderate', 'High')

#Add a drought sequence column
# tHSD$sequence <- c('Both Droughts', '2nd Drougth Only', 'Both Droughts', '2nd Drougth Only',
#                    'Both Droughts', '2nd Drougth Only','Both Droughts', '2nd Drougth Only')

#Add mean values for Estimate 1
tHSD.filter$estimate.1 <- c(#ADS
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$ADS, na.rm = T),
  #Pr-ET 4yr
  # mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$PrET_4yr, na.rm = T),
  # mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$PrET_4yr, na.rm = T),
  # mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$PrET_4yr, na.rm = T),
  # mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$PrET_4yr, na.rm = T),
  #Tree Cover
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$Tree_Cover, na.rm = T),
  #ET
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$ET, na.rm = T)
)

#Add mean values for Estimate 2
tHSD.filter$estimate.2 <- c(#ADS
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$ADS, na.rm = T),
  #Pr-ET 4yr
  # mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$PrET_4yr, na.rm = T),
  # mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$PrET_4yr, na.rm = T),
  # mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$PrET_4yr, na.rm = T),
  # mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$PrET_4yr, na.rm = T),
  #Tree Cover
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$Tree_Cover, na.rm = T),
  #ET
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$ET, na.rm = T)
)

#Calculate proportion differences from Tukey HSD tests
tHSD.filter$diff.pct <- tHSD.filter$estimate / tHSD.filter$estimate.2 * 100

tHSD.filter$low.pct <- tHSD.filter$conf.low / tHSD.filter$estimate.2 * 100

tHSD.filter$high.pct <- tHSD.filter$conf.high / tHSD.filter$estimate.2 * 100

#Select and sort the tukey HSD columns and 
tHSD.filter.sup <- tHSD.filter %>% dplyr::select(variable, fire.severity, estimate.1, estimate.2, estimate, conf.low, conf.high, 
                                                 diff.pct, high.pct, low.pct, adj.p.value)

#Name the columns of the data frame
colnames(tHSD.filter.sup) <- c('Variable', 'Fire Severity', 'Disturb Estimate', 'Control Estimate','Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')
ncol(tHSD.filter.sup)
#ANOVA and Tukey HSD comparing by time period and drought sequence, same as Table S2 plus % changes
tb2 <- kbl(tHSD.filter.sup, format = 'html', caption = "Table 2: Tukey HSD Comparisons between Fire Severity Groups", digits = c(0,0,1,1,1,1,1,1,1,1,3), escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 10, file = "STable10_tHSD_test_results_with_pct.png", zoom = 5.0) 