#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: June 23, 2023
#Purpose: Create merge split raster files

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/chrono
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/chrono
#Run the script: R < stand_age.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggplot2', 'magrittr', 'stats', 'patchwork','ggpmisc', 'raster', 'RStoolbox', 'quantreg','segmented', 'RColorBrewer',
	   'gt', 'gtsummary', 'webshot', 'stargazer', 'kableExtra', 'broom', 'svglite','sjPlot','purrr', 'sjmisc', 'magick', 'magrittr', 'knitr', 'xtable', 'purrr')
# install.packages('quantreg',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\CECS"
# dir <- "D:\\Large_Files\\CECS"
# memory.limit(32000)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')
# setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/landsat')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"
# dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"
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
  dplyr::group_by(system.index, treatment, fire.type.bin) %>% 
  reframe(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011, 2012)]),
            RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])) / mean(Tree_Cover[vi.year %in% c(2011, 2012)]),
            Tree_Cover = mean(Tree_Cover[vi.year %in% c(2011,2012)]),
            ET = mean(AET[vi.year %in% c(2011,2012)]),
            PrET_4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
            Water_Stress = Water_Stress[vi.year == 2015],
            ADS = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)]), 
            dNDMI = mean(NDMI[vi.year %in% c(2016, 2017)]) - mean(NDMI[vi.year %in% c(2009, 2010, 2011)])#,
            # fire.year.bin = fire.year.bin[vi.year == 2010],
            # treatment = treatment[vi.year == 2010],
            # fire.type.bin = fire.type.bin[vi.year == 2010])
            )

# age.dNDMI.rq <- rq(dNDMI_2015_mean ~ stand_age_mean, data = stand.age.sample, tau = q10)
# print(age.dNDMI.rq %>% tidy())
# tb1 <- age.dNDMI.rq %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 1: Quantile Regression, Die-off(dNDMI) ~ f(Stand Age)") %>% kable_classic_2(font_size = 14, full_width = F)
# as_image(x = tb1, width = 5, file = "T1_dNDMI_stand_age_quantile_regression_results.png", zoom = 4.0)  


# age.ADS.rq <- rq(ADS_2017_mean ~ stand_age_mean, data = stand.age.sample, tau = q10)
# print(age.ADS.rq %>% tidy())
# tb2 <- age.ADS.rq %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 2: Quantile Regression, Die-off(ADS) ~ f(Stand Age)") %>% kable_classic_2(font_size = 14, full_width = F)
# as_image(x = tb2, width = 5, file = "T2_ADS_stand_age_quantile_regression_results.png", zoom = 4.0)  

# stand.age.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age)
# 
# summary(stand.age.lm)
#Treatment Grid Cells
aov.dTree.rxfrap <- aov(dTree ~ treatment * fire.type.bin, data = pixel.filter)
summary(aov.dTree.rxfrap)

aov.ADS.rxfrap <- aov(ADS ~ treatment * fire.type.bin, data = pixel.filter)
summary(aov.ADS.rxfrap)

aov.PrET4yr.rxfrap <- aov(PrET_4yr ~ treatment * fire.type.bin, data = pixel.filter)
summary(aov.PrET4yr.rxfrap)

aov.tree.rxfrap <- aov(Tree_Cover ~ treatment * fire.type.bin, data = pixel.filter)
summary(aov.tree.rxfrap)

aov.ET.rxfrap <- aov(ET ~ treatment * fire.type.bin, data = pixel.filter)
summary(aov.tree.rxfrap)

tukey.dTree.rxfrap <- TukeyHSD(aov.dTree.rxfrap)
print(tukey.dTree.rxfrap)

tukey.ADS.rxfrap <- TukeyHSD(aov.ADS.rxfrap)
print(tukey.ADS.rxfrap)

tukey.PrET4yr.rxfrap <- TukeyHSD(aov.PrET4yr.rxfrap)
print(tukey.PrET4yr.rxfrap)

tukey.tree.rxfrap <- TukeyHSD(aov.tree.rxfrap)
print(tukey.tree.rxfrap)

tukey.ET.rxfrap <- TukeyHSD(aov.ET.rxfrap)
print(tukey.ET.rxfrap)

rxfrap.tHSD <- list(tukey.ADS.rxfrap, #tukey.PrET4yr.rxfrap.sev, 
             tukey.tree.rxfrap, tukey.ET.rxfrap)

#Combine the t-test results in a data frame
df.rxfrap.tHSD <- as.data.frame(purrr::map_df(rxfrap.tHSD, tidy))
rxfrap.tHSD.filter <- df.rxfrap.tHSD %>% filter(contrast %in% c('Disturb:Rxfire-Control:Rxfire', 'Disturb:Wildfire-Control:Wildfire'))
                                                  # 'Disturb:Mid-Control:Mid', 'Disturb:High-Control:High'))
#Add a variable label column
# tHSD.filter$variable
rxfrap.tHSD.filter$variable = c('Die-off (trees ha<sup>-1</sup>)','Die-off (trees ha<sup>-1</sup>)',
                         # 'Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)',
                         'Pre-Drought Tree Cover (%)','Pre-Drought Tree Cover (%)',
                         'Pre-Drought ET (mm yr<sup>-1</sup>)','Pre-Drought ET (mm yr<sup>-1</sup>)')

rxfrap.tHSD.filter$fire.type = c('Prescribed Fire', 'Wild Fire',
                              #'Unchanged', 'Low', 'Moderate', 'High',
                          'Prescribed Fire', 'Wild Fire',
                          'Prescribed Fire', 'Wild Fire')

#Add a drought sequence column
# tHSD$sequence <- c('Both Droughts', '2nd Drougth Only', 'Both Droughts', '2nd Drougth Only',
#                    'Both Droughts', '2nd Drougth Only','Both Droughts', '2nd Drougth Only')

#Add mean values for Estimate 1
rxfrap.tHSD.filter$estimate.1 <- c(#ADS
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire'))$ADS, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire'))$ADS, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$ADS, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$ADS, na.rm = T),
  #Pr-ET 4yr
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$PrET_4yr, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$PrET_4yr, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$PrET_4yr, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$PrET_4yr, na.rm = T),
  #Tree Cover
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire'))$Tree_Cover, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire'))$Tree_Cover, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$Tree_Cover, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$Tree_Cover, na.rm = T),
  #ET
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire'))$ET, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire'))$ET, na.rm = T)
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$ET, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$ET, na.rm = T)
)

#Add mean values for Estimate 2
rxfrap.tHSD.filter$estimate.2 <- c(#ADS
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Rxfire'))$ADS, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Wildfire'))$ADS, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$ADS, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$ADS, na.rm = T),
  #Pr-ET 4yr
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$PrET_4yr, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$PrET_4yr, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$PrET_4yr, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$PrET_4yr, na.rm = T),
  #Tree Cover
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Rxfire'))$Tree_Cover, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Wildfire'))$Tree_Cover, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$Tree_Cover, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$Tree_Cover, na.rm = T),
  #ET
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Rxfire'))$ET, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Wildfire'))$ET, na.rm = T)#,
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$ET, na.rm = T),
  # mean((pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$ET, na.rm = T)
)

#Calculate proportion differences from Tukey HSD tests
rxfrap.tHSD.filter$diff.pct <- rxfrap.tHSD.filter$estimate / rxfrap.tHSD.filter$estimate.2 * 100

rxfrap.tHSD.filter$low.pct <- rxfrap.tHSD.filter$conf.low / rxfrap.tHSD.filter$estimate.2 * 100

rxfrap.tHSD.filter$high.pct <- rxfrap.tHSD.filter$conf.high / rxfrap.tHSD.filter$estimate.2 * 100

#Select and sort the tukey HSD columns and 
rxfrap.tHSD.filter.tab <- rxfrap.tHSD.filter %>% dplyr::select(variable, fire.type, 
                                                               diff.pct, high.pct, low.pct, adj.p.value)

#Name the columns of the data frame
colnames(rxfrap.tHSD.filter.tab) <- c('Variable', 'Fire Severity', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')
# ncol(rxfrap.tHSD.filter.tab)
#ANOVA and Tukey HSD comparing by time period and drought sequence, same as Table S2 plus % changes
tb1 <- kbl(rxfrap.tHSD.filter.tab, format = 'html', caption = "Table 1: Tukey HSD Comparisons between Fire Type Groups", digits = c(0,0,1,1,1,3), escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 10, file = "Table1_fire_type_tHSD_test_results_with_pct.png", zoom = 5.0) 

#Select and sort the tukey HSD columns and 
rxfrap.tHSD.filter.sup <- rxfrap.tHSD.filter %>% dplyr::select(variable, fire.type, estimate.1, estimate.2, estimate, conf.low, conf.high, 
                                                 diff.pct, high.pct, low.pct, adj.p.value)

#Name the columns of the data frame
colnames(rxfrap.tHSD.filter.sup) <- c('Variable', 'Fire Severity', 'Disturb Estimate', 'Control Estimate','Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')
ncol(rxfrap.tHSD.filter.sup)
#ANOVA and Tukey HSD comparing by time period and drought sequence, same as Table S2 plus % changes
tb2 <- kbl(rxfrap.tHSD.filter.sup, format = 'html', caption = "Table 1: Tukey HSD Comparisons between Fire Type Groups", digits = c(0,0,1,1,1,1,1,1,1,1,3), escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 10, file = "TableS1_fire_type_tHSD_test_results_with_pct.png", zoom = 5.0) 

