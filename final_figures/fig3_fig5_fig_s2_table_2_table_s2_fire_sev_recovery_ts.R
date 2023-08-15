#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: August 9, 2023
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 
       'sf', 'RStoolbox', 'gtools', 'tigris', 'patchwork',
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo', 'purrr', 'webshot', 'stargazer', 'kableExtra',
       'broom', 'svglite','sjPlot','purrr', 'sjmisc', 'magick', 'magrittr', 'knitr', 'xtable', 'scales')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('scales'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# library(scales)
#Home Computer directories
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures')
dir_in <- "D:\\Fire_Dieoff"


#Lab computer directories
# setwd('C:\\Users\\Carl\\mystuff\\fireDieoff\\final_figures')
# dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"

#Add the data
sev.data <- read.csv(file.path(dir_in, "fire_south_sierra_USFS_sevfire_500pt_fire_year_5tree_ts8_300m_20230403.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
sev.data$treatment <- 'Disturb'
# summary(sev.data)
# list.files(fire_in)
# list.files(fire_in)
raw.sev.control.data <- read.csv(file.path(dir_in, "control_south_sierra_sev_2km_buffer_500pt_fire_year_5tree_ts16_300m_20230403.csv"), header = TRUE, na.strings = "NaN")

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
# summary(sev.pixel.data)
#Convert fire data -9999 to NAs
# sev.pixel.data$fire_sev_2010
# sev.pixel.data[sev.pixel.data$fire_sev_2010 == -9999,]$fire_sev_2010 <- NA
# sev.pixel.data[sev.pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
# sev.pixel.data[sev.pixel.data$fire_ID_2010 == -9999,]$fire_ID_2010 <- NA
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
sev.pixel.sample$fire.year.bin = with(sev.pixel.sample, factor(fire.year.bin, levels = c('2006-2010', '2001-2005','1996-2000', '1991-1995','1985-1990')))

#Subtract the pre-fire values for AET, tree and shrub cover
sev.pixel.sample <- sev.pixel.sample %>%
  group_by(system.index, sev.bin) %>% 
  mutate(dAET = AET - mean(AET[stand.age %in% c(-1, -2)]),
         dTree_Cover = Tree_Cover - mean(Tree_Cover[stand.age %in% c(-1, -2)]),
         dShrub_Cover = Shrub_Cover - mean(Shrub_Cover[stand.age %in% c(-1, -2)])) %>%
  ungroup()

sev.pixel.summary <- sev.pixel.sample %>% 
  filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
  group_by(stand.age, sev.bin) %>% 
  reframe(Tree_Cover.mean = mean(dTree_Cover[treatment == 'Disturb']) - mean(dTree_Cover[treatment == 'Control']),
          Tree_Cover.sd = sd(dTree_Cover[treatment == 'Disturb'])^2 + sd(dTree_Cover[treatment == 'Control'])^2, 
          Tree_Cover.n = n(),
          Shrub_Cover.mean = mean(dShrub_Cover[treatment == 'Disturb']) - mean(dShrub_Cover[treatment == 'Control']),
          Shrub_Cover.sd = sd(dShrub_Cover[treatment == 'Disturb'])^2 + sd(dShrub_Cover[treatment == 'Control'])^2, 
          Shrub_Cover.n = n(),
          AET.mean = mean(dAET[treatment == 'Disturb']) - mean(dAET[treatment == 'Control']),
          AET.sd = sd(dAET[treatment == 'Disturb'])^2 + sd(dAET[treatment == 'Control'])^2, 
          AET.n = n()) %>% 
  #Add the upper and lower 95% confidence intervals
  mutate(tree.ci.95.lower = Tree_Cover.mean - 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
         tree.ci.95.upper = Tree_Cover.mean + 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
         shrub.ci.95.lower = Shrub_Cover.mean - 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
         shrub.ci.95.upper = Shrub_Cover.mean + 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
         et.ci.95.lower = AET.mean - 1.96*(sqrt(AET.sd / AET.n)),
         et.ci.95.upper = AET.mean + 1.96*(sqrt(AET.sd / AET.n)))

#Select the columns I want for the data
sev.results.data <- sev.pixel.summary %>% dplyr::select(sev.bin, stand.age, tree.ci.95.lower, tree.ci.95.upper, shrub.ci.95.lower, shrub.ci.95.upper, et.ci.95.lower, et.ci.95.upper)

#Create Figure 3
#Create a unique palette
mypalette <- brewer_pal('seq', "YlOrRd")(5)[2:5]
# mypalette
#Create fire recover curves
p2a <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(stand.age, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(dTree_Cover[treatment == 'Disturb']) - mean(dTree_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Tree_Cover.mean, color = sev.bin), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                  filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, sev.bin) %>%
                  summarize(Tree_Cover.mean = mean(dTree_Cover[treatment == 'Disturb']) - mean(dTree_Cover[treatment == 'Control']),
                            Tree_Cover.sd = sd(dTree_Cover[treatment == 'Disturb'])^2 + sd(dTree_Cover[treatment == 'Control'])^2, 
                            Tree_Cover.n = n()),
                mapping = aes(ymin=Tree_Cover.mean - 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
                              ymax=Tree_Cover.mean + 1.96*(sqrt(Tree_Cover.sd / Tree_Cover.n)),
                              x = stand.age, fill = sev.bin), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.06, 0.4), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  ylab(expression('Tree Cover Change (%)')) + xlab('Years Since Fire')
p2a

#Pr-ET change with wildfire (FRAP)
p2b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(stand.age, sev.bin) %>%
              summarize(Shrub_Cover.mean = mean(dShrub_Cover[treatment == 'Disturb']) - mean(dShrub_Cover[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = sev.bin), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                  filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, sev.bin) %>%
                  summarize(Shrub_Cover.mean = mean(dShrub_Cover[treatment == 'Disturb']) - mean(dShrub_Cover[treatment == 'Control']),
                            Shrub_Cover.sd = sd(dShrub_Cover[treatment == 'Disturb'])^2 + sd(dShrub_Cover[treatment == 'Control'])^2, 
                            Shrub_Cover.n = n()),
                mapping = aes(ymin=Shrub_Cover.mean - 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
                              ymax=Shrub_Cover.mean + 1.96*(sqrt(Shrub_Cover.sd / Shrub_Cover.n)),
                              x = stand.age, fill = sev.bin), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  ylab(expression('Shrub Cover Change (%)')) + xlab('Years Since Fire')
p2b

#AET change with wildfire (FRAP)
p2c <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, sev.bin) %>%
              summarize(AET.mean = mean(dAET[treatment == 'Disturb']) - mean(dAET[treatment == 'Control'])), 
            mapping = aes(x = stand.age, y = AET.mean, color = sev.bin), size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                  filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
                  group_by(stand.age, sev.bin) %>%
                  summarize(AET.mean = mean(dAET[treatment == 'Disturb']) - mean(dAET[treatment == 'Control']),
                            AET.sd = sd(dAET[treatment == 'Disturb'])^2 + sd(dAET[treatment == 'Control'])^2, 
                            AET.n = n()),
                mapping = aes(ymin=AET.mean - 1.96*(sqrt(AET.sd / AET.n)),
                              ymax=AET.mean + 1.96*(sqrt(AET.sd / AET.n)),
                              x = stand.age, fill = sev.bin), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  #Add the Color and fill scales
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  guides(color = guide_legend(), linetype = 'none', fill = 'none') +
  ylab(expression('ET Change (mm yr'^-1*')')) + xlab('Years Since Fire')
p2c

f2 <- ggarrange(p2a,p2b,p2c, nrow = 3, ncol = 1, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v")
f2

#Save the data
ggsave(filename = 'Fig3_sev_fire_stand_age_tree_shrub_ET.png', height=15, width= 20, units = 'cm', dpi=900)

#Figure of Dead Trees per acre separated by fire years with time series
p1a <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # & 
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), # %>%
            # filter(if_else(sev.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mean, color = sev.bin, linetype = treatment), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                filter(vi.year >= 2010) %>%
                group_by(date, sev.bin, treatment) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), #%>%
              # filter(if_else(sev.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date, fill = sev.bin, alpha = treatment)) +
  #Do the Formating
  scale_linetype(name = 'Treatment', labels = c('Unburned', 'Burned')) +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = c(0.07, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(), panel.spacing = unit(20, 'pt'),
        legend.title = element_text(size = 10), legend.text = element_text(size = 8),
        strip.text.x = element_text(size = 12)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + facet_grid(. ~ sev.bin) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1a

#Create the 
p1b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = sev.pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()), #%>%  
            # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = Tree_Cover.mean, color = sev.bin, linetype = treatment), 
            size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                filter(vi.year >= 2010) %>%
                group_by(date, sev.bin, treatment) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), count = n()), # %>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(count)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(count)),
                            x = date, fill = sev.bin, alpha = treatment)) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(), panel.spacing = unit(20, 'pt'),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + #ylim(20, 45) + #facet_grid(. ~ sev.bin) + #ylim(20, 50) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1b

p1c <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()), # %>%  
            # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = AET.mean, color = sev.bin, linetype = treatment), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                filter(vi.year >= 2010) %>%
                group_by(date, sev.bin, treatment) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()), #%>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date, fill = sev.bin, alpha = treatment)) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 10), panel.spacing = unit(20, 'pt'),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + ylim(200, 550) + 
  #facet_grid(. ~ sev.bin) +
  ylab(expression('ET (mm yr'^-1*')')) + xlab('Year') 
p1c

f2 <- ggarrange(p1a, p1b, p1c, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(1, 0.9, 1.1), align = "v", labels = c('a', 'b', 'c'))
f2

#Save the data
ggsave(filename = 'Fig6_dieoff_tree_cover_severity_time_series.png', height=12, width= 22, units = 'cm', dpi=900)

#Create a Precip time series figure
p2a <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()), # %>%  
            # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = ppt.mean, color = sev.bin, linetype = treatment), 
            size = 1) +
  #Precip 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                filter(vi.year >= 2010) %>%
                group_by(date, sev.bin, treatment) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()), #%>%
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date, fill = sev.bin, alpha = treatment)) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.09, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + #
  ylab(expression('Precip (mm yr'^-1*')')) + xlab('Year') 
p2a

#Create a AET time series figure
p2b <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_line(data = sev.pixel.sample %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>%  
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(PrET.mean = mean(PrET), PrET.n = n(), count = n()), 
            mapping = aes(x = date, y = PrET.mean, color = sev.bin, linetype = treatment), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>%  
                filter(vi.year >= 2010) %>%
                group_by(date, sev.bin, treatment) %>%
                summarize(PrET.mean = mean(PrET),
                          PrET.sd = sd(PrET), PrET.n = n(), count = n()), #%>%  
              mapping = aes(ymin=PrET.mean - 1.96*(PrET.sd / sqrt(PrET.n)),
                            ymax=PrET.mean + 1.96*(PrET.sd / sqrt(PrET.n)),
                            x = date, fill = sev.bin, alpha = treatment)) +
  #Do the Formating
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-01-01'),as.Date('2020-01-01')) + #facet_grid(. ~ sev.bin) +
  ylab(expression('Pr-ET (mm yr'^-1*')')) + xlab('Year')
p2b

#Create the Water Stress Panel
f3 <- ggarrange(p2a, p2b, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a', 'b'))
f3

#Save the data
ggsave(filename = 'FigS2_sev_water_fluxes_time_series.png', height=12, width= 18, units = 'cm', dpi=900)

#Table 2 and Table S2
sev.pixel.filter <- sev.pixel.sample %>% filter(fire.year <= 2010 & fire.year > 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
  dplyr::group_by(system.index, treatment, sev.bin) %>% 
  reframe(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2010, 2011)]),
          RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2012,2013)])) / mean(Tree_Cover[vi.year %in% c(2010, 2011)]),
          Tree_Cover = mean(Tree_Cover[vi.year %in% c(2010, 2011)]),
          ET = mean(AET[vi.year %in% c(2010, 2011)]),
          ADS = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)]),
          # Water_Stress = Water_Stress[vi.year == 2015],
          PrET_4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), 
          sev.bin = sev.bin[vi.year == 2010],
          treatment = treatment[vi.year == 2010])

#Create Bar Chart as a Potential Alternative to Table 1
p7a <- ggbarplot(sev.pixel.filter,
                 y = "ADS", position = position_dodge(), fill = "sev.bin", x = 'treatment', #x = 'sev.bin',
                 add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, #palette = c("gray25", "gray75"),
                 xlab = NULL, #order = c("1999-2002", "2012-2015")
) +
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  facet_wrap(. ~ sev.bin, nrow = 1, ncol = 4) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 11), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.text.x = element_text(size = 12, face = 'bold')) +
  # labs(tag = 'a') +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.82,
    method = "tukey_hsd", label = "p.format", #group.by = 'treatment',
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Die-off Severity','(trees ha'^-1*')')))
p7a

p7b <- ggbarplot(sev.pixel.filter,
                 y = "dTree", position = position_dodge(), fill = "sev.bin", x = 'treatment',
                 add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, 
                 xlab = NULL, #order = c("1999-2002", "2012-2015")
) +
  theme_bw() + guides(color = 'none') +
  facet_wrap(. ~ sev.bin, nrow = 1, ncol = 4) +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.64, group.by = "sev.bin",
    method = "tukey_hsd", label = "p.format"
  ) +
  scale_y_reverse(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Die-off Severity', '('*Delta*'Tree %)')))
p7b

p7c <- ggbarplot(sev.pixel.filter,
                 y = "Tree_Cover", position = position_dodge(), fill = "sev.bin", x = 'treatment',
                 add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, 
                 xlab = NULL, #order = c("1999-2002", "2012-2015")
) +
  theme_bw() + guides(color = 'none') +
  facet_wrap(. ~ sev.bin, nrow = 1, ncol = 4) +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.57, #p.adjust.method = "bonferroni",
    method = "tukey_hsd", label = "p.format"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Pre-Drought','Tree Cover (%)')))
p7c

p7d <- ggbarplot(sev.pixel.filter,
                 y = "ET", position = position_dodge(), fill = "sev.bin", x = 'treatment',
                 add = "mean_ci" , error.plot = "errorbar", alpha = 0.8, width = 0.5, 
                 xlab = NULL, #order = c("1999-2002", "2012-2015")
) +
  theme_bw() + guides(color = 'none') +
  facet_wrap(. ~ sev.bin, nrow = 1, ncol = 4) +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + #labs(tag = 'b)') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), #plot.tag.position = c(0.53, 0.96), #c(0.52, 0.96)
        plot.tag = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_pwc(
    tip.length = 0, bracket.nudge.y = -0.45,
    method = "tukey_hsd", label = "p.format"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  ylab(expression(atop('Pre-Drought','ET (mm yr'^-1*')'))) + xlab('Treatment') +
  scale_x_discrete(labels = c("Unburned", "Burned")) #+

p7d

f7 <- (p7a / p7b / p7c / p7d) + plot_annotation(tag_levels = 'a')
f7

#Save PNG file
ggsave(filename = 'Fig7_sev_comparison_barchart.png', height=16, width= 20, units = 'cm', dpi=900)
#Save SVG file
ggsave(filename = 'Fig7_sev_comparison_barchart.svg', height=16, width= 20, units = 'cm', dpi=900)

#Calculate the sample sizes for the treatment and controls
sev.pixel.filter %>% group_by(treatment) %>%
  summarize(count = n())

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

#Work on combining the different data sets into a table
#Tukey HSD posthoc tests
#Combine all the t-test results in a list
tHSD <- list(tukey.ADS.treatment.sev, tukey.dTree.treatment.sev, 
             tukey.tree.treatment.sev, tukey.ET.treatment.sev, tukey.PrET4yr.treatment.sev)

#Combine the t-test results in a data frame
df.tHSD <- as.data.frame(purrr::map_df(tHSD, tidy))
tHSD.filter <- df.tHSD %>% filter(contrast %in% c('Disturb:Unchanged-Control:Unchanged', 'Disturb:Low-Control:Low', 
                                                  'Disturb:Mid-Control:Mid', 'Disturb:High-Control:High'))
#Add a variable label column
# tHSD.filter$variable
tHSD.filter$variable = c('Die-off (trees ha<sup>-1</sup>)','Die-off (trees ha<sup>-1</sup>)','Die-off (trees ha<sup>-1</sup>)','Die-off (trees ha<sup>-1</sup>)',
                         'Die-off (% Tree Cover)','Die-off (% Tree Cover)','Die-off (% Tree Cover)','Die-off (% Tree Cover)',
                         'Pre-Drought Tree Cover (%)','Pre-Drought Tree Cover (%)','Pre-Drought Tree Cover (%)','Pre-Drought Tree Cover (%)',
                         'Pre-Drought ET (mm yr<sup>-1</sup>)','Pre-Drought ET (mm yr<sup>-1</sup>)','Pre-Drought ET (mm yr<sup>-1</sup>)','Pre-Drought ET (mm yr<sup>-1</sup>)',
                         'Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)')

tHSD.filter$fire.severity = c('Unchanged', 'Low', 'Moderate', 'High',
                              'Unchanged', 'Low', 'Moderate', 'High',
                              'Unchanged', 'Low', 'Moderate', 'High',
                              'Unchanged', 'Low', 'Moderate', 'High',
                              'Unchanged', 'Low', 'Moderate', 'High')

#Add mean values for Estimate 1
tHSD.filter$estimate.1 <- c(
  # Die-off (ADS)
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$ADS, na.rm = T),
  # Die-off (dtree)
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$dTree, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$dTree, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$dTree, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$dTree, na.rm = T),
  #Tree Cover
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$Tree_Cover, na.rm = T),
  #ET
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$ET, na.rm = T),
  # Pr-ET 4yr
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Unchanged'))$PrET_4yr, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Low'))$PrET_4yr, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'Mid'))$PrET_4yr, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == 'High'))$PrET_4yr, na.rm = T)
)

#Add mean values for Estimate 2
tHSD.filter$estimate.2 <- c(
  # Die-off (ADS)
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$ADS, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$ADS, na.rm = T),
  # Die-off (dtree)
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$dTree, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$dTree, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$dTree, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$dTree, na.rm = T),
  #Tree Cover
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$Tree_Cover, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$Tree_Cover, na.rm = T),
  #ET
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$ET, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$ET, na.rm = T),
  #Pr-ET 4yr
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Unchanged'))$PrET_4yr, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Low'))$PrET_4yr, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'Mid'))$PrET_4yr, na.rm = T),
  mean((sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == 'High'))$PrET_4yr, na.rm = T)
)

#Calculate proportion differences from Tukey HSD tests
tHSD.filter$diff.pct <- tHSD.filter$estimate / tHSD.filter$estimate.2 * 100

tHSD.filter$low.pct <- tHSD.filter$conf.low / tHSD.filter$estimate.2 * 100

tHSD.filter$high.pct <- tHSD.filter$conf.high / tHSD.filter$estimate.2 * 100

#Select and sort the tukey HSD columns and 
tHSD.filter.tab <- tHSD.filter %>% dplyr::select(variable, fire.severity, diff.pct, high.pct, low.pct, adj.p.value) 

#Name the columns of the data frame
colnames(tHSD.filter.tab) <- c('Variable', 'Fire Severity', 'Difference (%)', 'Low 95% CI', 'High 95% CI', 'p-value')

#ANOVA and Tukey HSD comparing by time period and drought sequence, same as Table S2 plus % changes
tb1 <- kbl(tHSD.filter.tab, format = 'html', caption = "Tukey HSD Comparisons between Fire Severity Groups", digits = c(0,0,1,1,1,3), escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 10, file = "Table2_fire_severity_tHSD_test_results_with_pct.png", zoom = 5.0) 

#Select and sort the tukey HSD columns and 
tHSD.filter.sup <- tHSD.filter %>% dplyr::select(variable, fire.severity, estimate.1, estimate.2, estimate, conf.low, conf.high, 
                                                 diff.pct, high.pct, low.pct, adj.p.value)

#Name the columns of the data frame
colnames(tHSD.filter.sup) <- c('Variable', 'Fire Severity', 'Disturb Estimate', 'Control Estimate','Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')
# ncol(tHSD.filter.sup)
#ANOVA and Tukey HSD comparing by time period and drought sequence, same as Table S2 plus % changes
tb2 <- kbl(tHSD.filter.sup, format = 'html', caption = "Tukey HSD Comparisons between Fire Severity Groups", digits = c(0,0,1,1,1,1,1,1,1,1,3), escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 10, file = "TableS2_fire_severity_tHSD_test_results_with_pct.png", zoom = 5.0) 

#Create Figure S4
p3a <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(dTree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = sev.bin,  linetype = treatment), linewidth = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>% 
                  filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & !is.na(sev.bin) & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                  group_by(stand.age, treatment, sev.bin,) %>%
                  summarize(Tree_Cover.mean = mean(dTree_Cover),
                            Tree_Cover.sd = sd(dTree_Cover), Tree_Cover.n = n()),
                mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                              ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                              x = stand.age, fill = sev.bin,  alpha = treatment)) +
theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10), legend.position = c(0.15, 0.4), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  # guides(color = guide_legend(), linetype = 'none', fill = guide_legend(), alpha = 'none') +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  facet_grid(. ~ sev.bin) +
  ylab(expression('Tree Cover (%)')) + xlab('Years Since Fire')
p3a

p3b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Shrub_Cover.mean = mean(dShrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = sev.bin, linetype = treatment), linewidth = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>% 
                  filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                  group_by(stand.age, treatment, sev.bin) %>%
                  summarize(Shrub_Cover.mean = mean(dShrub_Cover),
                            Shrub_Cover.sd = sd(dShrub_Cover), Shrub_Cover.n = n()),
                mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                              ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                              x = stand.age, fill = sev.bin, alpha = treatment)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.15, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  # guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  guides(color = 'none', linetype = 'none', fill = 'none', alpha = 'none') +
  facet_grid(. ~ sev.bin) +
  # guides(fill = "none") +
  ylab(expression('Shrub Cover (%)')) + xlab('Years Since Fire')
p3b  

p3c <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(AET.mean = mean(dAET)), mapping = aes(x = stand.age, y = AET.mean, color = sev.bin, linetype = treatment), linewidth = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>% 
                filter(stand.age >= -2 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
                group_by(stand.age, treatment, sev.bin) %>%
                summarize(AET.mean = mean(dAET),
                          AET.sd = sd(dAET), AET.n = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = stand.age, fill = sev.bin, alpha = treatment)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.15, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  # guides(color = 'none', linetype = 'gu, fill = 'none', alpha = 'none') +
  guides(color = 'none', linetype = 'none', fill = 'none', alpha = 'none') +
  facet_grid(. ~ sev.bin) +
  # guides(fill = "none") +
  ylab(expression('ET (mm yr'^-1*')')) + xlab('Years Since Fire')
p3c  

f4 <- ggarrange(p3a, p3b, p3c, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(1.0, 0.9, 1), align = "v", labels = c('a', 'b', 'c'))
f4

#Save the data
ggsave(filename = 'FigS5_sev_stand_age_treatment_veg_cover.png', height=18, width= 20, units = 'cm', dpi=900)

#Create Fig S5, the data check figure
p4a <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()), mapping = aes(x = stand.age, y = Tree_Cover.n, color = sev.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.15, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  facet_grid(. ~ sev.bin) +
  ylab(expression('# Pixels')) 
p4a

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

p4b <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(fire_year.mode = find_mode(fire.year)), mapping = aes(x = stand.age, y = fire_year.mode, color = sev.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  ylim(1985, 2015) +
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  guides(color = 'none', linetype = 'none', fill = 'none', alpha = 'none') +
  facet_grid(. ~ sev.bin) +
  ylab(expression('Modal Fire Year')) #+ xlab('Years Since Fire') 
p4b

p4c <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.sample %>%
              filter(stand.age >= -2 & stand.age <= 20 & vi.year <= 2012 & fire.year > 1986 & fire.year <= 2010 &  (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(vi.year.mode = find_mode(vi.year)), mapping = aes(x = stand.age, y = vi.year.mode, color = sev.bin, linetype = treatment), linewidth = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  ylim(1985, 2015) +
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  guides(color = 'none', linetype = 'none', fill = 'none', alpha = 'none') +
  facet_grid(. ~ sev.bin) +
  ylab(expression('Modal VI Year')) + xlab('Years Since Fire') 
p4c

f5 <- ggarrange(p4a, p4b, p4c, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(1, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f5

#Save the data
ggsave(filename = 'FigS6_data_check_fire_sev.png', height=18, width= 20, units = 'cm', dpi=900)

#Figure S8: ADS vs. dTree
p6 <- ggplot(data = sev.pixel.filter) +
  #Create the density layer
  geom_bin2d(binwidth = c(2.5, 10), mapping = aes(x = dTree, y = ADS, group = after_stat(count), alpha = after_stat(count))) +
  scale_fill_gradient2(limits = c(0,2000), breaks = c(5,500,1000,1500), midpoint = 1000, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  scale_alpha(range = c(1, 1), limits = c(5, 2000), na.value = 0.4) +labs(fill = "Grid Cells") +
  guides(alpha = 'none') +
  geom_smooth(method = 'lm', mapping = aes(x = dTree, y = ADS), color = 'black', size = 2, linetype = 'dashed') +
  stat_cor(mapping = aes(x = dTree, y = ADS, label = paste(..rr.label..))) +
  theme_bw() +
  xlab('Die-off (% Tree Cover)') + ylab(expression('Die-off (trees ha'^-1*')'))
p6

ggsave(filename = 'FigS8_frap_rx_dieoff_comparison.png', height=16, width= 16, units = 'cm', dpi=900)
