#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: July 26, 2024
#Purpose: Create figures for publication

#Add the needed R packages
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'gstat',
      'sf', 'ncdf4', 'gtools', 'tigris', 'patchwork', 'ggpubr', 'ggnewscale', 'segmented',
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo', 'purrr', 'webshot2', 'stargazer', 'kableExtra',
       'broom', 'svglite','sjPlot','purrr', 'sjmisc', 'magick', 'magrittr', 'knitr', 'xtable', 'tidymodels', 'vip')

# install.packages('devtools',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)


#Set the working directory
#Home data directory
setwd('C://Users/can02/mystuff/fireDieoff/figures')
dir_in <- "D:\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"

#Lab data directory
# setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures')
# dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"
# fire_in <- "D:\\Large_Files\\Fire_Dieoff"

#Add the Wildfire data
frap.fire.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_wildfire_300pt_5_fire_year_10tree_ts4_30m_20231204.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
frap.fire.data$treatment <- 'Disturb'

#Add the Wildfire buffer data
frap.control.data <- read.csv(file.path(dir_in, "control_south_sierra_FRAP_2km_buffer_300pt_5_fire_year_10tree_ts4_30m_20231204.csv"), header = TRUE, na.strings = "NaN")

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
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_300pt_5_fire_year_10tree_ts4_30m_20231204.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
rx.data$treatment <- 'Disturb'

#Add teh Rx fire buffer data
rx.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Rx_2km_buffer_300pt_5_fire_year_10tree_ts4_30m_20231204.csv"), header = TRUE, na.strings = "NaN")

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

#Convert fire data -9999 to NAs
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
pixel.data$veg_name <- recode(.x=pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
                              '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer',
                              '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

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

#Sample the Wildfire control pixels
frap.sample <- pixel.data %>%
  filter(treatment == 'Control' & fire.type.bin == 'Wildfire' & stratlayer %in% (frap.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (frap.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-c(data, n)) %>% #Get rid of the data column
  unnest(samp) #unnest the data

#Sample the prescribed fire control pixels
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
#Vegetation Cover from Wang et al, 2022
pixel.sample$Tree_Cover.2 <- pixel.sample$Tree_Cover / 100
pixel.sample$Shrub_Cover.2 <- pixel.sample$Shrub_Cover / 100
pixel.sample$Herb_Cover.2 <- pixel.sample$Herb_Cover / 100
pixel.sample$Bare_Cover.2 <- pixel.sample$Bare_Cover / 100
pixel.sample$Tree_Cover.2 <- pixel.sample$Tree_Cover

#Rename Montana Tree Cover
#Vegetation Cover from Allred et al, 2021
pixel.sample$Tree_Cover <- pixel.sample$TRE
pixel.sample$Shrub_Cover <- pixel.sample$SHR
pixel.sample$Herb_Cover <- pixel.sample$AFG + pixel.sample$PFG
pixel.sample$Bare_Cover <- pixel.sample$BGR

#Convert the SPI48 scale back to decimal
pixel.sample$SPI48 <- pixel.sample$SPI48 / 100

#Try to fix soil moisture by dividing by 10
# pixel.sample$Soil_Moisture <- pixel.sample$Soil_Moisture / 10

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

#Subtract the pre-fire values for AET, tree and shrub cover
pixel.sample <- pixel.sample %>%
         group_by(system.index, fire.type.bin) %>% 
         mutate(dAET = AET - mean(AET[stand.age %in% c(-1, -2)]),
                dTree_Cover = Tree_Cover - mean(Tree_Cover[stand.age %in% c(-1, -2)]),
                dShrub_Cover = Shrub_Cover - mean(Shrub_Cover[stand.age %in% c(-1, -2)])) %>%
         ungroup()

summary(pixel.sample)

#Do some calculations for the results section of the manuscript
pixel.summary <- pixel.sample %>% 
  filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
  group_by(stand.age, fire.type.bin) %>% 
  reframe(Tree_Cover.mean = mean(dTree_Cover[treatment == 'Disturb']) - mean(dTree_Cover[treatment == 'Control']),
          Tree_Cover.mean.control = mean(Tree_Cover[treatment == 'Control']),
          Tree_Cover.mean.disturb = mean(Tree_Cover[treatment == 'Disturb']),
          Tree_Cover.sd = sd(dTree_Cover[treatment == 'Disturb'])^2 + sd(dTree_Cover[treatment == 'Control'])^2, 
          Tree_Cover.sd.pct = Tree_Cover.sd / Tree_Cover.mean.control * 100,
          Tree_Cover.n = n(),
          Shrub_Cover.mean = mean(dShrub_Cover[treatment == 'Disturb']) - mean(dShrub_Cover[treatment == 'Control']),
          Shrub_Cover.mean.control = mean(Shrub_Cover[treatment == 'Control']),
          # Shrub_Cover.mean.pct = Shrub_Cover.mean / Shrub_Cover.mean.control * 100,
          Shrub_Cover.sd = sd(dShrub_Cover[treatment == 'Disturb'])^2 + sd(dShrub_Cover[treatment == 'Control'])^2, 
          Shrub_Cover.sd.pct = Shrub_Cover.sd / Shrub_Cover.mean.control * 100,
          Shrub_Cover.n = n(),
          AET.mean = mean(dAET[treatment == 'Disturb']) - mean(dAET[treatment == 'Control']),
          AET.mean.control = mean(AET[treatment == 'Control']),
          AET.sd = sd(dAET[treatment == 'Disturb'])^2 + sd(dAET[treatment == 'Control'])^2, 
          AET.sd.pct = AET.sd / AET.mean.control * 100,
          AET.n = n()) %>% 
  #Remove the pre-fire means
  mutate(#Corrected means
         dTree_Cover.mean = Tree_Cover.mean - mean(Tree_Cover.mean[stand.age %in% c(-1, -2)]),
         dShrub_Cover.mean = Shrub_Cover.mean - mean(Shrub_Cover.mean[stand.age %in% c(-1, -2)]),
         dAET.mean = AET.mean - mean(AET.mean[stand.age %in% c(-1, -2)])) %>%
         #Percentage Means
  mutate(dTree_Cover.mean.pct = dTree_Cover.mean / Tree_Cover.mean.control * 100,
         dShrub_Cover.mean.pct = dShrub_Cover.mean / Shrub_Cover.mean.control * 100,
         dAET.mean.pct = dAET.mean / AET.mean.control * 100) %>%
  #Add the upper and lower 95% confidence intervals
  mutate(tree.ci.95.lower = dTree_Cover.mean - 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
         tree.ci.95.upper = dTree_Cover.mean + 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
         tree.ci.95.lower.pct = dTree_Cover.mean.pct - 1.96*(sqrt(Tree_Cover.sd.pct / Tree_Cover.n)),
         tree.ci.95.upper.pct = dTree_Cover.mean.pct + 1.96*(sqrt(Tree_Cover.sd.pct / Tree_Cover.n)),
         shrub.ci.95.lower = dShrub_Cover.mean - 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
         shrub.ci.95.upper = dShrub_Cover.mean + 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
         shrub.ci.95.lower.pct = dShrub_Cover.mean.pct - 1.96*(sqrt(Shrub_Cover.sd.pct / Shrub_Cover.n)),
         shrub.ci.95.upper.pct = dShrub_Cover.mean.pct + 1.96*(sqrt(Shrub_Cover.sd.pct / Shrub_Cover.n)),
         et.ci.95.lower = dAET.mean - 1.96*(sqrt(AET.sd / AET.n)),
         et.ci.95.upper = dAET.mean + 1.96*(sqrt(AET.sd / AET.n)),
         et.ci.95.lower.pct = dAET.mean.pct - 1.96*(sqrt(AET.sd.pct / AET.n)),
         et.ci.95.upper.pct = dAET.mean.pct + 1.96*(sqrt(AET.sd.pct / AET.n)))

#Select the columns I want for the data
results.data <- pixel.summary %>% dplyr::select(fire.type.bin, stand.age, tree.ci.95.lower, tree.ci.95.upper, shrub.ci.95.lower, shrub.ci.95.upper, et.ci.95.lower, et.ci.95.upper)
results.data.pct <- pixel.summary %>% dplyr::select(fire.type.bin, stand.age, tree.ci.95.lower.pct, tree.ci.95.upper.pct, shrub.ci.95.lower.pct, shrub.ci.95.upper.pct, et.ci.95.lower.pct, et.ci.95.upper.pct)

# results.data
# results.data.pct

#Filter the data into subsets for modeling
pixel.filter <- pixel.sample %>% filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
  #Group into grid cell bins for each treatment and fire type
  dplyr::group_by(system.index, treatment, fire.type.bin) %>%
  #Calculate summaries for each grid cell
  reframe(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2010, 2011)]),
          RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2010, 2011)])) / mean(Tree_Cover[vi.year %in% c(2010, 2011)]),
          Tree_Cover = mean(Tree_Cover[vi.year %in% c(2010, 2011)]),
          Shrub_Cover = mean(Shrub_Cover[vi.year %in% c(2010, 2011)]),
          ET = mean(AET[vi.year %in% c(2010, 2011)]),
          PrET_4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]),
          # Water_Stress = Water_Stress[vi.year == 2015],
          ADS = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)]), 
          dNDMI = mean(NDMI[vi.year %in% c(2016, 2017)]) - mean(NDMI[vi.year %in% c(2009, 2010, 2011)]),
          latitude = first(latitude),
          longitude = first(longitude),
          elevation = first(elevation),
          stand.age = first(stand.age[vi.year == 2010])
  )

#Create fire recovery curves for Figure 2
p2a <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>% #stand.age <= 20 & 
              filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
              group_by(stand.age, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control'])) %>%
              group_by(fire.type.bin) %>%
              mutate(dTree_Cover.mean = Tree_Cover.mean - mean(Tree_Cover.mean[stand.age %in% c(-1, -2)])),
            mapping = aes(x = stand.age, y = dTree_Cover.mean, color = fire.type.bin), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.sample %>% #stand.age <= 20 & 
                  filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control']),
                            Tree_Cover.sd = sd(Tree_Cover[treatment == 'Disturb'])^2 + sd(Tree_Cover[treatment == 'Control'])^2, 
                            Tree_Cover.n = n()) %>%
                group_by(fire.type.bin) %>%
                mutate(dTree_Cover.mean = Tree_Cover.mean - mean(Tree_Cover.mean[stand.age %in% c(-1, -2)])),
                mapping = aes(ymin= dTree_Cover.mean - 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
                              ymax= dTree_Cover.mean + 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
                              x = stand.age, fill = fire.type.bin), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = c(0.065, 0.4), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  ylab(expression(atop('Tree Cover', 'Change (%)'))) + xlab('Years Since Fire')
p2a

#Pr-ET change with wildfire (FRAP)
p2b <- ggplot() + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>% #stand.age <= 20 
              filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(stand.age, fire.type.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control'])) %>%
              group_by(fire.type.bin) %>%
              mutate(dShrub_Cover.mean = Shrub_Cover.mean - mean(Shrub_Cover.mean[stand.age %in% c(-1, -2)])),
            mapping = aes(x = stand.age, y = dShrub_Cover.mean , color = fire.type.bin), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.sample %>% #stand.age <= 20 
                  filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control']),
                            Shrub_Cover.sd = sd(Shrub_Cover[treatment == 'Disturb'])^2 + sd(Shrub_Cover[treatment == 'Control'])^2, 
                            Shrub_Cover.n = n()) %>%
                group_by(fire.type.bin) %>%
                mutate(dShrub_Cover.mean = Shrub_Cover.mean - mean(Shrub_Cover.mean[stand.age %in% c(-1, -2)])),
                mapping = aes(ymin= dShrub_Cover.mean - 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
                              ymax= dShrub_Cover.mean + 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
                              x = stand.age, fill = fire.type.bin), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  ylab(expression(atop('Shrub Cover', 'Change (%)'))) + xlab('Years Since Fire')
p2b

#AET change with wildfire (FRAP)
p2c <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#Create a Tree Cover line
geom_line(data = pixel.sample %>%
            filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
            group_by(stand.age, fire.type.bin) %>%
            summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control'])) %>%
            group_by(fire.type.bin) %>%
            mutate(dAET.mean = AET.mean - mean(AET.mean[stand.age %in% c(-1, -2)])), 
          mapping = aes(x = stand.age, y = dAET.mean, color = fire.type.bin), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.sample %>%
                  filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  group_by(stand.age, fire.type.bin) %>%
                  summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control']),
                            AET.sd = sd(AET[treatment == 'Disturb'])^2 + sd(AET[treatment == 'Control'])^2, 
                            AET.n = n()) %>%
                group_by(fire.type.bin) %>%
                mutate(dAET.mean = AET.mean - mean(AET.mean[stand.age %in% c(-1, -2)])),
                mapping = aes(ymin= dAET.mean - 1.96*(sqrt(AET.sd / AET.n)),
                              ymax= dAET.mean + 1.96*(sqrt(AET.sd / AET.n)),
                              x = stand.age, fill = fire.type.bin), alpha = 0.3) +
theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 10),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  #scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  ylab(expression(atop('ET Change', '(mm yr'^-1*')'))) + xlab('Years Since Fire')
p2c

f1 <- ggarrange(p2a,p2b,p2c, nrow = 3, ncol = 1, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a', 'b', 'c'))
f1

#Save the data
ggsave(filename = '../figures/Fig2_frap_stand_age_tree_shrub_ET.png', height=15, width= 20, units = 'cm', dpi=900)

#Create Figure 4: Dead Trees per acre separated by fire years with time series
p1a <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(date, treatment, fire.type.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), 
            mapping = aes(x = date, y = tpa_max.mean, color = fire.type.bin, linetype = treatment), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                
                group_by(date, treatment, fire.type.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), 
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date, fill = fire.type.bin, alpha = treatment)) +
  #Do the Formating
  scale_linetype(name = 'Treatment', labels = c('Unburned', 'Burned')) +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = c(0.15, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(), panel.spacing = unit(20, "pt"),
        legend.title = element_text(size = 10), legend.text = element_text(size = 8),
        strip.text = element_text(size = 12)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + 
  ylab(expression(atop('Dieback Severity', '(trees ha'^-1*')'))) + xlab('Year') 
p1a

#Create the 
p1b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & stratlayer %in% strat.list & stratlayer %in% strat.list
              group_by(date, treatment, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()), 
            # filter(if_else(treatment == '1980-2010', Tree_Cover.n >= 2500, Tree_Cover.n >= 0)),
            mapping = aes(x = date, y = Tree_Cover.mean, color = fire.type.bin, linetype = treatment), 
            size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                group_by(date, treatment, fire.type.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),  
              # filter(if_else(treatment == '1980-2010', Tree_Cover.n >= 2500, Tree_Cover.n >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = date, fill = fire.type.bin, alpha = treatment)) +
  #Do the Formatting
  scale_linetype(name = 'Treatment') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(), panel.spacing = unit(20, "pt"),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + #facet_grid(. ~ treatment) + 
  ylim(18, 55) +
  ylab(expression(atop('Tree Cover', '(%)'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1b

p1c <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.sample %>%
              filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              group_by(date, treatment, fire.type.bin) %>%
              summarize(PrET.mean = mean(PrET), PrET.n = n(), count = n()),
            mapping = aes(x = date, y = PrET.mean, color = fire.type.bin, linetype = treatment),
            size = 1) +
  #Water Stress 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                group_by(date, treatment, fire.type.bin) %>%
                summarize(PrET.mean = mean(PrET),
                          PrET.sd = sd(PrET), PrET.n = n(), count = n()),
              mapping = aes(ymin=PrET.mean - 1.96*(PrET.sd / sqrt(PrET.n)),
                            ymax=PrET.mean + 1.96*(PrET.sd / sqrt(PrET.n)),
                            x = date, fill = fire.type.bin, alpha = treatment)) +
  #Do the Formatting
  scale_linetype(name = 'Treatment') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA),  axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + #facet_grid(. ~ treatment) +
  ylab(expression(atop('Pr-ET', '(mm yr'^-1*')'))) + xlab('Year')
p1c

f2 <- ggarrange(p1a, p1b, p1c, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(1, 0.9, 1.1), align = "v", labels = c('a', 'b', 'c'))
f2

#Save the figure
ggsave(filename = '../figures/Fig4_frap_rx_dieoff_tree_cover_stand_age_time_series.png', height=12, width= 12, units = 'cm', dpi=900)

#Figure S1: Precip, Water Stress time series figure
p3a <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.sample %>%
              filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, treatment, fire.type.bin) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()),
            mapping = aes(x = date, y = ppt.mean, color = fire.type.bin, linetype = treatment),
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, treatment, fire.type.bin) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date, fill = fire.type.bin, alpha = treatment)) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = c(0.2, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression('Precip (mm yr'^-1*')')) + xlab('Year')
p3a

p3b <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, treatment, fire.type.bin) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()),
            mapping = aes(x = date, y = AET.mean, color = fire.type.bin, linetype = treatment),
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, treatment, fire.type.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date, fill = fire.type.bin, alpha = treatment)) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 10), panel.spacing = unit(20, "pt"),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        # strip.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + ylim(325, 650) +
  #facet_grid(. ~ fire.year.bin) +
  ylab(expression('ET (mm yr'^-1*')')) + xlab('Year')
p3b

f3 <- ggarrange(p3a, p3b, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a', 'b'))
f3

# #Save the data
ggsave(filename = 'FigS1_frap_rx_water_fluxes_time_series.png', height=12, width= 18, units = 'cm', dpi=900)

#Create Figure 5: Bar Chart with statistical comparisons
p7a <- ggbarplot(pixel.filter,
                y = "ADS", position = position_dodge(), facet.by = "fire.type.bin", fill = "fire.type.bin", x = 'treatment',
                add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, 
                xlab = NULL, #order = c("1999-2002", "2012-2015")
                ) +
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.text.x = element_text(size = 12, face = 'bold')) +
  # labs(tag = 'a') +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.77,
    method = "tukey_hsd", label = "p.format"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Dieback Severity','(trees ha'^-1*')')))
p7a

p7b <- ggbarplot(pixel.filter,
                 y = "dTree", position = position_dodge(), facet.by = "fire.type.bin", fill = "fire.type.bin", x = 'treatment',
                 add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, 
                 xlab = NULL, #order = c("1999-2002", "2012-2015")
) +
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.52,
    method = "tukey_hsd", label = "p.format"
  ) +l;

  scale_y_reverse(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Dieback Severity', '('*Delta*'Tree %)')))
p7b

p7c <- ggbarplot(pixel.filter,
                 y = "Tree_Cover", position = position_dodge(), facet.by = "fire.type.bin", fill = "fire.type.bin", x = 'treatment',
                 add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, 
                 xlab = NULL, #order = c("1999-2002", "2012-2015")
) +
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.44,
    method = "tukey_hsd", label = "p.format"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Pre-Drought','Tree Cover (%)')))
p7c

p7d <- ggbarplot(pixel.filter,
                 y = "ET", position = position_dodge(), facet.by = "fire.type.bin", fill = "fire.type.bin", x = 'treatment',
                 add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, 
                 xlab = NULL, #order = c("1999-2002", "2012-2015")
) +
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.34,
    method = "tukey_hsd", label = "p.format"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Pre-Drought','ET (mm yr'^-1*')'))) + xlab('Treatment') +
scale_x_discrete(labels = c("Unburned", "Burned")) #+

p7d

p7e <- ggbarplot(pixel.filter,
                 y = "PrET_4yr", position = position_dodge(), facet.by = "fire.type.bin", fill = "fire.type.bin", x = 'treatment',
                 add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, 
                 xlab = NULL, #order = c("1999-2002", "2012-2015")
) +
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.54,
    method = "tukey_hsd", label = "p.format"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Pr-ET', '(mm 4yr'^-1*')'))) + xlab('Treatment') +
  scale_x_discrete(labels = c("Unburned", "Burned")) #+

p7e

f7 <- (p7a / p7b / p7c / p7d / p7e) + plot_annotation(tag_levels = 'a')
f7

ggsave(filename = 'Fig5_frap_rx_comparison_barchart.png', height=20, width= 12, units = 'cm', dpi=900)

#Create Table 1 for the manuscript and Table S1 for the supplement
#Calculate the sample sizes for the treatment and controls
pixel.filter %>% group_by(treatment, fire.type.bin) %>%
  summarize(count = n())

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

#Combine the tukey tests together
rxfrap.tHSD <- list(tukey.ADS.rxfrap, tukey.dTree.rxfrap,
                    tukey.tree.rxfrap, tukey.ET.rxfrap, tukey.PrET4yr.rxfrap)

#Combine the t-test results in a data frame
df.rxfrap.tHSD <- as.data.frame(purrr::map_df(rxfrap.tHSD, tidy))
rxfrap.tHSD.filter <- df.rxfrap.tHSD %>% filter(contrast %in% c('Disturb:Rxfire-Control:Rxfire', 'Disturb:Wildfire-Control:Wildfire'))

#Add a variable label column
rxfrap.tHSD.filter$variable = c('Dieback (trees ha<sup>-1</sup>)','Dieback (trees ha<sup>-1</sup>)',
                                'Dieback (% Tree Cover)', 'Dieback (% Tree Cover)',
                                'Pre-Drought Tree Cover (%)','Pre-Drought Tree Cover (%)',
                                'Pre-Drought ET (mm yr<sup>-1</sup>)','Pre-Drought ET (mm yr<sup>-1</sup>)',
                                'Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)')

rxfrap.tHSD.filter$fire.type = c('Prescribed Fire', 'Wild Fire',
                                 'Prescribed Fire', 'Wild Fire',
                                 'Prescribed Fire', 'Wild Fire',
                                 'Prescribed Fire', 'Wild Fire',
                                 'Prescribed Fire', 'Wild Fire')

#Add mean values for Estimate 1
rxfrap.tHSD.filter$estimate.1 <- c(
  #Dieback (ADS)
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire'))$ADS, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire'))$ADS, na.rm = T),
  #Dieback (dTree)
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire'))$dTree, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire'))$dTree, na.rm = T),
  #Tree Cover
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire'))$Tree_Cover, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire'))$Tree_Cover, na.rm = T),
  #ET
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire'))$ET, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire'))$ET, na.rm = T),
  #Pr-ET 4yr
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Rxfire'))$PrET_4yr, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Disturb' & fire.type.bin == 'Wildfire'))$PrET_4yr, na.rm = T)
)

#Add mean values for Estimate 2
rxfrap.tHSD.filter$estimate.2 <- c(
  #Dieback (ADS)
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Rxfire'))$ADS, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Wildfire'))$ADS, na.rm = T),
  #Dieback (dTree)
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Rxfire'))$dTree, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Wildfire'))$dTree, na.rm = T),
  #Tree Cover
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Rxfire'))$Tree_Cover, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Wildfire'))$Tree_Cover, na.rm = T),
  #ET
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Rxfire'))$ET, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Wildfire'))$ET, na.rm = T),
  #Pr-ET 4yr
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Rxfire'))$PrET_4yr, na.rm = T),
  mean((pixel.filter %>% filter(treatment == 'Control' & fire.type.bin == 'Wildfire'))$PrET_4yr, na.rm = T)
)

#Calculate proportion differences from Tukey HSD tests
rxfrap.tHSD.filter$diff.pct <- rxfrap.tHSD.filter$estimate / rxfrap.tHSD.filter$estimate.2 * 100

rxfrap.tHSD.filter$low.pct <- rxfrap.tHSD.filter$conf.low / rxfrap.tHSD.filter$estimate.2 * 100

rxfrap.tHSD.filter$high.pct <- rxfrap.tHSD.filter$conf.high / rxfrap.tHSD.filter$estimate.2 * 100

#Select and sort the tukey HSD columns and 
rxfrap.tHSD.filter.sup <- rxfrap.tHSD.filter %>% dplyr::select(variable, fire.type, estimate.1, estimate.2, estimate, conf.low, conf.high, 
                                                               diff.pct, high.pct, low.pct, adj.p.value)

#Name the columns of the data frame
colnames(rxfrap.tHSD.filter.sup) <- c('Variable', 'Fire Severity', 'Disturb Estimate', 'Control Estimate','Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')
ncol(rxfrap.tHSD.filter.sup)
#ANOVA and Tukey HSD comparing by time period and drought sequence, same as Table S2 plus % changes
print(rxfrap.tHSD.filter.sup)
tb2 <- kbl(rxfrap.tHSD.filter.sup, format = 'html', caption = "Tukey HSD Comparisons between Fire Type Groups", digits = c(0,0,1,1,1,1,1,1,1,1,3), escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
tb2
#The as_image command isn't working any more
#Saved with the viewer window
as_image(x = tb2, width = 14, file = "TableS1_fire_type_tHSD_test_results_with_pct_v2.png", zoom = 6.0)

#Figure S3: Vegetation recovery
p4a <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & !is.na(fire.type.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(stand.age, treatment, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = fire.type.bin,  linetype = treatment), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.sample %>% 
                filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & !is.na(fire.type.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                group_by(stand.age, treatment, fire.type.bin,) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, fill = fire.type.bin,  alpha = treatment)) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10), legend.position = c(0.35, 0.3), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_linetype(name = 'Treatment') +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  # guides(color = guide_legend(), linetype = 'none', fill = guide_legend(), alpha = 'none') +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  facet_grid(. ~ fire.type.bin) +
  ylab(expression('Tree Cover (%)')) + xlab('Years Since Fire')
p4a

p4b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, fire.type.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = pixel.sample %>% 
                filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                group_by(stand.age, treatment, fire.type.bin) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, fill = fire.type.bin, alpha = treatment)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.15, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_linetype(name = 'Treatment') +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  # guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  guides(color = 'none', linetype = 'none', fill = 'none', alpha = 'none') +
  facet_grid(. ~ fire.type.bin) +
  # guides(fill = "none") +
  ylab(expression('Shrub Cover (%)')) + xlab('Years Since Fire')
p4b  

p4c <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, fire.type.bin) %>%
              summarize(AET.mean = mean(AET)), mapping = aes(x = stand.age, y = AET.mean, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = pixel.sample %>% 
                filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                group_by(stand.age, treatment, fire.type.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = stand.age, fill = fire.type.bin, alpha = treatment)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.15, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_linetype(name = 'Treatment') +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  # guides(color = 'none', linetype = 'gu, fill = 'none', alpha = 'none') +
  guides(color = 'none', linetype = 'none', fill = 'none', alpha = 'none') +
  facet_grid(. ~ fire.type.bin) +
  # guides(fill = "none") +
  ylab(expression('ET (mm yr'^-1*')')) + xlab('Years Since Fire')
p4c  

f5 <- ggarrange(p4a, p4b, p4c, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(1.0, 0.9, 1), align = "v", labels = c('a', 'b', 'c'))
f5

#Save the data
ggsave(filename = 'FigS3_firetype_stand_age_treatment_veg_cover.png', height=18, width= 18, units = 'cm', dpi=900)

#Create Fig S4, the data check figure
p5a <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()), mapping = aes(x = stand.age, y = Tree_Cover.n, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.15, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_linetype(name = 'Treatment') +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  facet_grid(. ~ fire.type.bin) +
  ylab(expression('# Pixels')) 
p5a

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

p5b <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, fire.type.bin) %>%
              summarize(fire_year.mode = mean(fire.year)), mapping = aes(x = stand.age, y = fire_year.mode, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  ylim(1985, 2015) +
  scale_linetype(name = 'Treatment') +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = 'none', linetype = 'none', fill = 'none', alpha = 'none') +
  facet_grid(. ~ fire.type.bin) +
  ylab(expression('Mean Fire Year')) #+ xlab('Years Since Fire') 
p5b

p5c <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, fire.type.bin) %>%
              summarize(vi.year.mode = mean(vi.year)), mapping = aes(x = stand.age, y = vi.year.mode, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  ylim(1985, 2015) +
  scale_linetype(name = 'Treatment') +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  guides(color = 'none', linetype = 'none', fill = 'none', alpha = 'none') +
  facet_grid(. ~ fire.type.bin) +
  ylab(expression('Mean VI Year')) + xlab('Years Since Fire') 
p5c

f6 <- ggarrange(p5a, p5b, p5c, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(1, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f6

#Save the data
ggsave(filename = 'FigS4_data_check_fire_type.png', height=18, width= 18, units = 'cm', dpi=900)

#Elevation Separation Data
pixel.elev.data <- pixel.filter %>% 
  #filter(!is.na(ADS)) %>%
  # left_join(y = data %>% dplyr::select(c(latitude, longitude, system.index)), by = join_by(system.index == system.index)) %>%
  # dplyr::mutate(socal = as.integer(USFS_zone == 262), sierra = as.integer(USFS_zone == 261)) %>% #Make new columns that have 0,1 for Sierra and socal to calculate proportions later
  dplyr::mutate(elev.bin = cut(elevation, breaks = seq(0, 4000, by = 500))#,
                #lat.bin = cut(latitude, breaks = seq(34.8, 39.0, by = 0.2))),
  ) %>%
  dplyr::group_by(elev.bin, fire.type.bin, treatment) %>%
  dplyr::summarize(ADS.mean = mean(ADS, na.rm = TRUE), count = n(), ADS.sd = sd(ADS, na.rm = TRUE), 
                   dTree.mean = mean(dTree), dTree.sd = sd(dTree), Tree.mean = mean(Tree_Cover), Tree.sd = sd(Tree_Cover),
                   Shrub.mean = mean(Shrub_Cover), Shrub.sd = sd(Shrub_Cover), PrET_4yr.mean = mean(PrET_4yr), PrET_4yr.sd = sd(PrET_4yr), 
                   ET.mean = mean(ET), ET.sd = sd(ET), elevation.mean = mean(elevation)) %>%
  ungroup()

#calculate the number o samples 
pixel.ADS.count <- pixel.filter %>% 
filter(!is.na(ADS)) %>%
  dplyr::mutate(elev.bin = cut(elevation, breaks = seq(0, 4000, by = 500))#,
                #lat.bin = cut(latitude, breaks = seq(34.8, 39.0, by = 0.2))),
  ) %>%
  dplyr::group_by(elev.bin, fire.type.bin, treatment) %>%
  dplyr::summarize(ADS.count = n()) %>%
  ungroup()

#Combined pixel elevation data
pixel.elev.merge <- merge(pixel.elev.data, pixel.ADS.count %>% dplyr::select(fire.type.bin, treatment, elev.bin, ADS.count), by = c("treatment", "fire.type.bin", "elev.bin"))

#Create Figure S12: Comparing dieback and dieback predictors by elevation
#Dieback Distribution Chart
p6a <- ggplot(data = pixel.elev.merge) +  
  facet_grid(. ~ fire.type.bin) +
  scale_y_reverse() +
  geom_line(mapping = aes(y = dTree.mean, x = elevation.mean, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  geom_errorbar(mapping = aes(y = dTree.mean, x = elevation.mean, ymax = dTree.mean + 1.96*(dTree.sd / sqrt(count)), ymin = dTree.mean - 1.96*(dTree.sd / sqrt(count)), color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = c(0.9, 0.7)) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1, guide = 'none') +
  scale_linetype(name = 'Treatment') +
  xlab(expression('Elevation')) + ylab('Dieback (Tree Cover %)')
p6a

p6b <- ggplot(data = pixel.elev.merge %>% filter(count >= 5)) +
  #geom_bin_2d(binwidth = c(5, 200), mapping = aes(group = dTree.mean)) +
  #scale_fill_gradient(high = 'yellow', low = '#de2d26', name = expression(atop('Observed', 'Dieback (%)'))) + # (trees ha'^-1*')'))) +
  facet_grid(. ~ fire.type.bin) +
  # scale_y_reverse() +
  geom_line(mapping = aes(y = ADS.mean, x = elevation.mean, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  geom_errorbar(mapping = aes(y = ADS.mean, x = elevation.mean, ymax = ADS.mean + 1.96*(ADS.sd / sqrt(count)), ymin = ADS.mean - 1.96*(ADS.sd / sqrt(count)), color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = 'none') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  xlab(expression('Elevation')) + ylab(expression('Dieback (tree ha'^-1*')'))
p6b

p6c <- ggplot(data = pixel.elev.merge %>% filter(count >= 5)) +
  #geom_bin_2d(binwidth = c(5, 200), mapping = aes(group = dTree.mean)) +
  #scale_fill_gradient(high = 'yellow', low = '#de2d26', name = expression(atop('Observed', 'Dieback (%)'))) + # (trees ha'^-1*')'))) +
  facet_grid(. ~ fire.type.bin) +
  # scale_y_reverse() +
  geom_line(mapping = aes(y = Tree.mean, x = elevation.mean, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  geom_errorbar(mapping = aes(y = Tree.mean, x = elevation.mean, ymax = Tree.mean + 1.96*(Tree.sd / sqrt(count)), ymin = Tree.mean - 1.96*(Tree.sd / sqrt(count)), color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = 'none') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  xlab(expression('Elevation')) + ylab('Tree Cover (%)')
p6c

p6d <- ggplot(data = pixel.elev.data %>% filter(count >= 5)) +
  #geom_bin_2d(binwidth = c(5, 200), mapping = aes(group = dTree.mean)) +
  #scale_fill_gradient(high = 'yellow', low = '#de2d26', name = expression(atop('Observed', 'Dieback (%)'))) + # (trees ha'^-1*')'))) +
  facet_grid(. ~ fire.type.bin) +
  # scale_y_reverse() +
  geom_line(mapping = aes(y = Shrub.mean, x = elevation.mean, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  geom_errorbar(mapping = aes(y = Shrub.mean, x = elevation.mean, ymax = Shrub.mean + 1.96*(Shrub.sd / sqrt(count)), ymin = Shrub.mean - 1.96*(Shrub.sd / sqrt(count)), color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = 'none') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  xlab(expression('Elevation')) + ylab('Shrub Cover (%)')
p6d

p6e <- ggplot(data = pixel.elev.data %>% filter(count >= 5)) +
  #geom_bin_2d(binwidth = c(5, 200), mapping = aes(group = dTree.mean)) +
  #scale_fill_gradient(high = 'yellow', low = '#de2d26', name = expression(atop('Observed', 'Dieback (%)'))) + # (trees ha'^-1*')'))) +
  facet_grid(. ~ fire.type.bin) +
  # scale_y_reverse() +
  geom_line(mapping = aes(y = ET.mean, x = elevation.mean, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  geom_errorbar(mapping = aes(y = ET.mean, x = elevation.mean, ymax = ET.mean + 1.96*(ET.sd / sqrt(count)), ymin = ET.mean - 1.96*(ET.sd / sqrt(count)), color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = 'none') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  xlab(expression('Elevation')) + ylab(expression('ET (mm yr'^-1*')'))
p6e

p6f <- ggplot(data = pixel.elev.data %>% filter(count >= 5)) +
  #geom_bin_2d(binwidth = c(5, 200), mapping = aes(group = dTree.mean)) +
  #scale_fill_gradient(high = 'yellow', low = '#de2d26', name = expression(atop('Observed', 'Dieback (%)'))) + # (trees ha'^-1*')'))) +
  facet_grid(. ~ fire.type.bin) +
  # scale_y_reverse() +
  geom_line(mapping = aes(y = PrET_4yr.mean, x = elevation.mean, color = fire.type.bin, linetype = treatment), linewidth = 1) +
  geom_errorbar(mapping = aes(y = PrET_4yr.mean, x = elevation.mean, ymax = PrET_4yr.mean + 1.96*(PrET_4yr.sd / sqrt(count)), ymin = PrET_4yr.mean - 1.96*(PrET_4yr.sd / sqrt(count)), color = fire.type.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_fill_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  scale_color_brewer(type = 'qual', palette = 'Set2', name = 'Fire Type', direction = 1) +
  xlab(expression('Elevation')) + ylab(expression('Pr-ET (mm 4yr'^-1*')'))
p6f

f7 <- ggarrange(p6a,p6b,p6c,p6d,p6e,p6f, nrow = 6, ncol = 1, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a', 'b', 'c', 'd', 'e', 'f'))
f7

ggsave(filename = 'FigS12_forest_type_comparison_by_elevation_bin.png', height=32, width= 16, units = 'cm', dpi=900)

#Create Figure R3 (or S14): Stand Age Die-off comparison for response to reviewers
p7a <- ggplot(data = pixel.filter %>% filter(treatment == 'Disturb')) +
  #Create the density layer
  geom_bin2d(binwidth = c(1, 10), mapping = aes(x = stand.age, y = ADS)) + #, group = after_stat(count), alpha = after_stat(count))) +
  scale_fill_gradient2(limits = c(0,400), breaks = c(0,100,200,300), midpoint = 200, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  # scale_alpha(range = c(1, 1), limits = c(5, 1600), na.value = 0.4) +labs(fill = "Grid Cells") +
  guides(alpha = 'none') +
  geom_smooth(method = 'lm', mapping = aes(x = stand.age, y = ADS), color = 'black', size = 2, linetype = 'dashed') +
  stat_cor(mapping = aes(x = stand.age, y = ADS, label = paste(..rr.label..))) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  # xlim(0, 30) +
  facet_wrap(. ~ fire.type.bin) +
  xlab('Years Since Fire') + ylab(expression('Dieback (trees ha'^-1*')'))
p7a

p7b <- ggplot(data = pixel.filter %>% filter(treatment == 'Disturb')) +
  #Create the density layer
  geom_bin2d(binwidth = c(1, 5), mapping = aes(x = stand.age, y = dTree)) + #, group = after_stat(count), alpha = after_stat(count))) +
  scale_fill_gradient2(limits = c(0,500), breaks = c(0,100,200,300,400), midpoint = 250, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  # scale_alpha(range = c(1, 1), limits = c(5, 1600), na.value = 0.4) +labs(fill = "Grid Cells") +
  guides(alpha = 'none') +
  geom_smooth(method = 'lm', mapping = aes(x = stand.age, y = dTree), color = 'black', size = 2, linetype = 'dashed') +
  stat_cor(mapping = aes(x = stand.age, y = dTree, label = paste(..rr.label..))) +
  theme_bw() +
  # xlim(0, 30) +
  scale_y_reverse() +
  facet_wrap(. ~ fire.type.bin) +
  ylim(50, -105) +
  xlab('Years Since Fire') + ylab('Dieback (Tree Cover %)')
p7b

f8 <- ggarrange(p7a, p7b, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a', 'b'))
f8

ggsave(filename = 'FigS14_frap_rx_stand_age_dieoff_dieoff_comparison.png', height=16, width= 16, units = 'cm', dpi=900)