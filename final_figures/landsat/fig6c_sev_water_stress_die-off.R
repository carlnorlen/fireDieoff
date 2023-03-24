#Author: Carl Norlen
#Date Created: January 23, 2023
#Date Updated: February 14, 2023
#Purpose: Create Pr-ET four-year versus dTree figures

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo', 'segmented')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('zoo'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(segmented)
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the Wildfire data
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

#Add the data
sev.data <- read.csv(file.path(dir_in, "fire_south_sierra_USFS_sevfire_500pt_200mm_5tree_ts8_300m_20230322.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
sev.data$treatment <- 'Disturb'
# summary(sev.data)
# list.files(fire_in)
# list.files(fire_in)
raw.sev.control.data <- read.csv(file.path(dir_in, "control_south_sierra_sev_2km_buffer_500pt_200mm_5tree_ts16_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

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
sev.pixel.data <- sev.pixel.data %>% mutate(fire.year.bin = case_when(
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
# sev.pixel.data %>% summary()
#Fire year bins for Fire Severity Data
sev.pixel.data$fire.year.bin = with(sev.pixel.data, factor(fire.year.bin, levels = c('2011-2017', '1985-2010')))#c('0-4','5-30','31-55','56-80',

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
sev.pixel.sample$Herb_Cover <- sev.pixel.sample$AFG + pixel.sample$PFG
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

#Filter the data into subsets for modeling
sev.pixel.filter <- sev.pixel.sample %>% filter(fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
                   #  filter(case_when(sev.bin == 'Unchanged' ~ stratlayer %in% un.strat,
                   # sev.bin == 'Low' ~ stratlayer %in% lo.strat,
                   # sev.bin == 'Mid' ~ stratlayer %in% mid.strat,
                   # sev.bin == 'High' ~ stratlayer %in% hi.strat)) %>%
                dplyr::group_by(system.index) %>% 
                summarize(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
                    RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
                    Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), 
                    sev.bin = sev.bin[vi.year == 2010],
                    treatment = treatment[vi.year == 2010])

sev.hi.control <- sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == "High")
sev.hi.disturb <- sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == "High")
sev.mid.control <- sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == "Mid")
sev.mid.disturb <- sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == "Mid")
sev.lo.control <- sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == "Low")
sev.lo.disturb <- sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == "Low")  
  

#Models for Wild Fire
sev.hi.control.lm <- lm(data = sev.hi.control, dTree~ Water_Stress) 
sev.hi.disturb.lm <- lm(data = sev.hi.disturb, dTree ~ Water_Stress) 

#Models for Mid Severity fire
sev.mid.control.lm <- lm(data = sev.mid.control, dTree~ Water_Stress) 
sev.mid.disturb.lm <- lm(data = sev.mid.disturb, dTree ~ Water_Stress) 

#Models for Rx Fire
sev.lo.control.lm <- lm(data = sev.lo.control, dTree~ Water_Stress) 
sev.lo.disturb.lm <- lm(data = sev.lo.disturb, dTree ~ Water_Stress) 

#Calculate the sgemented models
sev.hi.control.seg <- segmented(sev.hi.control.lm)
sev.hi.disturb.seg <- segmented(sev.hi.disturb.lm)
sev.mid.control.seg <- segmented(sev.mid.control.lm)
sev.mid.disturb.seg <- segmented(sev.mid.disturb.lm)
sev.lo.control.seg <- segmented(sev.lo.control.lm)
sev.lo.disturb.seg <- segmented(sev.lo.disturb.lm)

#Add predicted dNDMI values
sev.hi.control$dTree.predict = predict(sev.hi.control.seg)
sev.hi.disturb$dTree.predict = predict(sev.hi.disturb.seg)
sev.mid.control$dTree.predict = predict(sev.mid.control.seg)
sev.mid.disturb$dTree.predict = predict(sev.mid.disturb.seg)
sev.lo.control$dTree.predict = predict(sev.lo.control.seg)
sev.lo.disturb$dTree.predict = predict(sev.lo.disturb.seg)

#Add the segmented fits and Standard Errors
#Fits
sev.hi.control$dTree.fit = broken.line(sev.hi.control.seg)$fit
sev.hi.disturb$dTree.fit = broken.line(sev.hi.disturb.seg )$fit
sev.mid.control$dTree.fit = broken.line(sev.mid.control.seg)$fit
sev.mid.disturb$dTree.fit = broken.line(sev.mid.disturb.seg )$fit
sev.lo.control$dTree.fit = broken.line(sev.lo.control.seg)$fit
sev.lo.disturb$dTree.fit = broken.line(sev.lo.disturb.seg)$fit

#SE fit
sev.hi.control$dTree.se.fit = broken.line(sev.hi.control.seg)$se.fit
sev.hi.disturb$dTree.se.fit = broken.line(sev.hi.disturb.seg)$se.fit
sev.mid.control$dTree.se.fit = broken.line(sev.mid.control.seg)$se.fit
sev.mid.disturb$dTree.se.fit = broken.line(sev.mid.disturb.seg)$se.fit
sev.lo.control$dTree.se.fit = broken.line(sev.lo.control.seg)$se.fit
sev.lo.disturb$dTree.se.fit = broken.line(sev.lo.disturb.seg)$se.fit

#Recombine the data frames with the model fitted dNDMI as a column
sev.all.models <- rbind(sev.hi.control, sev.hi.disturb, sev.mid.control, sev.mid.disturb, sev.lo.control, sev.lo.disturb)

#R-Squared values for the four models
r2.a  <- format(summary(sev.hi.control.seg)$r.squared, digits = 2) #I could switch this back to segmented
r2.b <- format(summary(sev.hi.disturb.seg)$r.squared, digits = 2)
r2.c  <- format(summary(sev.mid.control.seg)$r.squared, digits = 2) #I could switch this back to segmented
r2.d <- format(summary(sev.mid.disturb.seg)$r.squared, digits = 2)
r2.e <- format(summary(sev.lo.control.seg)$r.squared, digits = 2)
r2.f <- format(summary(sev.lo.disturb.seg)$r.squared, digits = 2) #I could switch this back to segmented

#Create a data.frame of R.squared values
r2.text <- data.frame(
  label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 =r2.a)))), 
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.b)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.c)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.d)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.e)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.f))))
  ),
  treatment = c('Control', 'Disturb', 'Control', 'Disturb', 'Control', 'Disturb'),
  sev.bin = c('High', 'High', 'Mid', 'Mid', 'Low', 'Low'),
  x = c(3500, 3500, 3500, 3500, 3500, 3500),
  y = c(-20, -20, -20, -20, -20, -20)
)

#Create the figure
p1 <- ggplot(data = sev.all.models) +
  #Create the density layer
  geom_bin2d(binwidth = c(200, 2), mapping = aes(x = Water_Stress, y = dTree, group = ..count..)) +
  #Piecewise linear regression fit line
  geom_line(mapping = aes(x=Water_Stress, y=dTree.fit), size=2, color = 'black', linetype = 'dotdash') +
  #Piecewise fit uncertainty
  geom_ribbon(mapping = aes(x = Water_Stress, y = dTree.fit, ymax = dTree.fit + 1.96*dTree.se.fit, ymin = dTree.fit - 1.96*dTree.se.fit), alpha = 0.4) +  
  guides(alpha = "none") +
  scale_fill_gradient2(limits = c(0,360), breaks = c(5,90,180,270), midpoint = 180, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  scale_alpha(range = c(1, 1), limits = c(5, 360), na.value = 0.4) +
  # stat_cor( mapping = aes(x = Water_Stress, y = dTree), color = 'black') + facet_grid(fire.type.bin ~ treatment) +
  labs(fill = "Grid Cells") +
  #Add the R^2 values
  geom_text(data = r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  #Add the R^2 text
  # geom_text(data = letter.text, mapping = aes(x = x, y = y, label = label), size = 5, fontface = "bold") +
  theme_bw() +
  facet_grid(factor(sev.bin, levels = c('Low', 'Mid', 'High')) ~ treatment) +
  xlab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) + ylab('Die-off (dTree %)')
p1

p2 <- p1 + theme(
  legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
  legend.justification = c(1, 0),
  legend.position = c(0.5, 0.4),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.direction = "vertical") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3,
                               title.position = "top", 
                               title.hjust = 0.5, 
                               ticks.colour = "black"))

p2

ggsave(filename = 'Fig6c_sev_water_stress_dTree_300m.png', height=24, width= 16, units = 'cm', dpi=900)
