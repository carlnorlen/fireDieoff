#Author: Carl Norlen
#Date Created: January 24, 2022
#Date Updated: March 28, 2023
#Purpose: Create figures for chapter 2 manuscript

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo', 'purrr')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('zoo'),repo='https://cran.r-project.org/')
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
#Add the Wildfire data
frap.fire.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_wildfire_500pt_200mm_5tree_ts8_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
frap.fire.data$treatment <- 'Disturb'

#Add the Wildfire buffer data
frap.control.data <- read.csv(file.path(dir_in, "control_south_sierra_FRAP_2km_buffer_500pt_200mm_5tree_ts16_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

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
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_500pt_200mm_5tree_ts8_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
rx.data$treatment <- 'Disturb'

#Add teh Rx fire buffer data
rx.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Rx_2km_buffer_500pt_200mm_5tree_ts16_300m_20230322.csv"), header = TRUE, na.strings = "NaN")

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


#Figure of Dead Trees per acre separated by fire years with time series
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), 
            mapping = aes(x = date, y = tpa_max.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 

                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), 
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
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
  geom_line(data = pixel.sample %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & stratlayer %in% strat.list & stratlayer %in% strat.list
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()), 
              # filter(if_else(fire.year.bin == '1980-2010', Tree_Cover.n >= 2500, Tree_Cover.n >= 0)),
            mapping = aes(x = date, y = Tree_Cover.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),  
                # filter(if_else(fire.year.bin == '1980-2010', Tree_Cover.n >= 2500, Tree_Cover.n >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + 
  #ylim(30, 55) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p6

p10 <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.sample %>%
              filter(fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(PrET.mean = mean(PrET), PrET.n = n(), count = n()),
            mapping = aes(x = date, y = PrET.mean, color = fire.year.bin, linetype = fire.year.bin),
            size = 1) +
  #Water Stress 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(PrET.mean = mean(PrET),
                          PrET.sd = sd(PrET), PrET.n = n(), count = n()),
              mapping = aes(ymin=PrET.mean - 1.96*(PrET.sd / sqrt(PrET.n)),
                            ymax=PrET.mean + 1.96*(PrET.sd / sqrt(PrET.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA),  axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression('Pr-ET (mm yr'^-1*')')) + xlab('Year')
p10

f2 <- ggarrange(p5, p6, p10, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a', 'b', 'c'))
f2
#Save the data
ggsave(filename = 'Fig2_frap_rx_dieoff_tree_cover_stand_age_time_series.png', height=18, width= 18, units = 'cm', dpi=900)

#Figure 4: Precip, ET, Soil moisture, Water Stress time series figure
p7 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.sample %>%
              filter(fire.year <= 2010 & fire.year >= 1980 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()),
            mapping = aes(x = date, y = ppt.mean, color = fire.year.bin, linetype = fire.year.bin),
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(fire.year <= 2010 & fire.year >= 1980 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.2, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression('Precip (mm yr'^-1*')')) + xlab('Year')
p7

#Create an AET time series figure
p8 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.sample %>%
              filter(fire.year <= 2010 & fire.year >= 1980 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
              #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()),
            mapping = aes(x = date, y = AET.mean, color = fire.year.bin, linetype = fire.year.bin),
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.sample %>%
                filter(fire.year <= 2010 & fire.year >= 1980 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(case_when(fire.type.bin == 'Wildfire' ~ stratlayer %in% frap.strat,
                #                  fire.type.bin == 'Rxfire' ~ stratlayer %in% rx.strat)) %>%
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  #Pick the plot theme
  theme_bw() + 
  #Do the faceting
  facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + ylim(300, 700) +
  #facet_grid(. ~ fire.year.bin) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year')
p8

f3 <- ggarrange(p7, p8, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a', 'b'))
f3

# #Save the data
ggsave(filename = 'FigS1_frap_rx_water_fluxes_time_series.png', height=12, width= 18, units = 'cm', dpi=900)
