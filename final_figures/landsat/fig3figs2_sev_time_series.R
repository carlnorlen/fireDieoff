#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: June 22, 2023
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'mgcViz','zoo', 'purrr')
# p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
#        'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
#        'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo', 'purrr', 'mgcViz', 'relaimpo', 'dplyr')
# install.packages('mgcViz',repo='https://cran.r-project.org/')
# library(dplyr)
# library(zoo)
# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Home computer
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')
dir_in <- "D:\\Fire_Dieoff"

#Lab computer
# setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/landsat')
# dir_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Fire_Dieoff"

#Add the data
sev.data <- read.csv(file.path(dir_in, "fire_south_sierra_USFS_sevfire_500pt_200mm_5tree_ts8_300m_20230322.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
sev.data$treatment <- 'Disturb'
# sev.data$treatment.cat <- 0

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
# sev.control.data$treatment.cat <- 0

#Combine the data together
sev.pixel.data <- rbind(sev.data, sev.control.data)
# pixel.data <- rbind(combine.data, control.data.2km)
# summary(sev.pixel.data)

`%notin%` <- Negate(`%in%`)

#Convert fire data -9999 to NAs
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
sev.pixel.data <- sev.pixel.data %>% dplyr::mutate(treat = case_when(treatment == 'Disturb' ~ 1, treatment == 'Control' ~ 0))

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
# tidyr::unnest()
#Sample the unchanged control pixels
un.sample <- sev.pixel.data %>%
                    filter(treatment == 'Control' & sev.bin == 'Unchanged' & stratlayer %in% (un.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
                    group_by(stratlayer) %>% #Group by Stratification layer
                    tidyr::nest() %>% #Nest the data
                    ungroup() %>% #Un group the data
                    mutate(n = (un.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
                    mutate(samp = purrr::map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work.
                    dplyr::select(-data) %>% #Get rid of the data column
                    tidyr::unnest(samp) #unnest the data

#Sample the low severity control pixels
lo.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'Low' & stratlayer %in% (lo.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  tidyr::nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (lo.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = purrr::map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  tidyr::unnest(samp) #unnest the data

#Sample the moderate severity control pixels
mid.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'Mid' & stratlayer %in% (mid.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  tidyr::nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (mid.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = purrr::map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  tidyr::unnest(samp) #unnest the data

#High Severity Samples
hi.sample <- sev.pixel.data %>%
  filter(treatment == 'Control' & sev.bin == 'High' & stratlayer %in% (hi.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  tidyr::nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (hi.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = purrr::map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  tidyr::unnest(samp) #unnest the data

#Make sure the stratlayer bins match with the sampled control bins
#Sample the unchanged control pixels
un.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Unchanged' & stratlayer %in% (un.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  tidyr::nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (un.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = purrr::map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample, but slice sample doesn't work.
  dplyr::select(-data) %>% #Get rid of the data column
  tidyr::unnest(samp) #unnest the data

#Sample the low severity control pixels
lo.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Low' & stratlayer %in% (lo.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  tidyr::nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (lo.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = purrr::map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  tidyr::unnest(samp) #unnest the data

#Sample the moderate severity control pixels
mid.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'Mid' & stratlayer %in% (mid.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  tidyr::nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (mid.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = purrr::map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  tidyr::unnest(samp) #unnest the data

#High Severity Samples
hi.disturb <- sev.pixel.data %>%
  filter(treatment == 'Disturb' & sev.bin == 'High' & stratlayer %in% (hi.strat %>% pull(stratlayer))) %>% #Get just the unchanged control stratification layers
  group_by(stratlayer) %>% #Group by Stratification layer
  tidyr::nest() %>% #Nest the data
  ungroup() %>% #Un group the data
  mutate(n = (hi.strat %>% pull(n))) %>% #Add the sample sizes for the stratlayers in the disturbed data
  mutate(samp = purrr::map2(data, n, sample_n)) %>% #Do the random sample, sample_n is depricated for slice_sample
  dplyr::select(-data) %>% #Get rid of the data column
  tidyr::unnest(samp) #unnest the data

#Combine the sampled data back together
sev.pixel.sample <- rbind(un.disturb, lo.disturb, mid.disturb, hi.disturb, un.sample, lo.sample, mid.sample, hi.sample)

sev.pixel.sample <- sev.pixel.sample %>% 
  tidyr::pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

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


#Create the palette
mypalette <- brewer_pal('seq', "YlOrRd")(5)[2:5]

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
  scale_linetype(name = 'Treatment') +
  scale_color_manual(values = mypalette, name = 'Fire Severity') +
  scale_fill_manual(values = mypalette, name = 'Fire Severity') +
  scale_alpha_discrete(range = c(0.3, 0.3)) +
  guides(color = 'none', linetype = guide_legend(), fill = 'none', alpha = 'none') +
  #Pick the plot theme
  theme_bw() + 
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.07, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ sev.bin) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1a

#Create the 
p1b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = sev.pixel.sample %>%
              filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()), #%>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = Tree_Cover.mean, color = sev.bin, linetype = treatment), 
            size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
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
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #ylim(20, 45) + #facet_grid(. ~ sev.bin) + #ylim(20, 50) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1b

p1c <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_line(data = sev.pixel.sample %>%
              filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>%  
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(PrET.mean = mean(PrET), PrET.n = n(), count = n()), 
            mapping = aes(x = date, y = PrET.mean, color = sev.bin, linetype = treatment), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>%  
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
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ sev.bin) +
  ylab(expression('Pr-ET (mm yr'^-1*')')) + xlab('Year')
p1c

f2 <- ggarrange(p1a, p1b, p1c, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a', 'b', 'c'))
f2

#Save the data
ggsave(filename = 'Fig3_dieoff_tree_cover_severity_time_series.png', height=18, width= 18, units = 'cm', dpi=900)

#Create a Precip time series figure
p7 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.sample %>%
              filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()), # %>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = ppt.mean, color = treatment, linetype = treatment), 
            size = 1) +
  #Precip 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                filter(vi.year >= 2010) %>%
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
        axis.title.x = element_blank(), legend.position = c(0.09, 0.6), legend.background = element_rect(colour = NA, fill = NA),
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
              filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
              filter(vi.year >= 2010) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()), # %>%  
              # filter(case_when(sev.bin == 'Unchanged or Low' ~ count >= 2500, sev.bin == 'Mid or High' ~ count >= 2700, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = AET.mean, color = treatment, linetype = treatment), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = sev.pixel.sample %>%
                filter(fire.year <= 2010 & fire.year > 1986 & !is.na(sev.bin) & (fire_year_2019 <=2010 | treatment == 'Control')) %>% # &
                filter(vi.year >= 2010) %>%
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
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_grid(. ~ sev.bin) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + ylim(200, 550) + 
  #facet_grid(. ~ sev.bin) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year') 
p8

#Create the Water Stress Panel
f3 <- ggarrange(p7, p8, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a', 'b'))
f3

#Save the data
ggsave(filename = 'FigS2_sev_water_fluxes_time_series.png', height=16, width= 18, units = 'cm', dpi=900)

summary(sev.pixel.sample)
#Do stand age versus die-off tests
p4a <- ggplot(data = sev.pixel.sample %>% filter(fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                dplyr::group_by(system.index, treatment, sev.bin) %>% 
                reframe(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                        Tree = mean(Tree_Cover[vi.year %in% c(2011,2012)]),
                        tpa_max = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)], na.rm = TRUE),
                        Water_Stress = Water_Stress[vi.year == 2015], 
                        stand.age = stand.age[vi.year == 2015]) %>%
                filter(stand.age <= 25), # %>% 
                # group_by(system.index, sev.bin) %>%
                # reframe(tpa_max.mean = mean(tpa_max[treatment == 'Disturb']) - mean(tpa_max[treatment == 'Control']),
                #         Tree.mean = mean(Tree[treatment == 'Disturb']) - mean(Tree[treatment == 'Control'])),
              mapping = aes(x = Tree, y = tpa_max, color = treatment)) + 
  facet_grid(.~ sev.bin) + 
  theme_bw() +
  geom_point() +
  # geom_point(mapping = aes(color = sev.bin), size = 1) + 
  # geom_line(mapping = aes(color = sev.bin), linewidth = 1) + 
  geom_smooth(mapping = aes(color = treatment), method = 'lm') +
  stat_cor(mapping = aes(color = treatment)) +
  xlab('Tree Cover (%)') + ylab(expression('Die-off (trees ha'^-1*')'))
p4a

p4b <- ggplot(data = sev.pixel.sample %>% filter(fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                dplyr::group_by(system.index, treatment, sev.bin) %>% 
                reframe(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                        ET = mean(AET[vi.year %in% c(2011,2012)]),
                        tpa_max = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)], na.rm = TRUE),
                        Water_Stress = Water_Stress[vi.year == 2015], 
                        stand.age = stand.age[vi.year == 2015]), # %>% 
              # group_by(system.index, sev.bin) %>%
              # reframe(tpa_max.mean = mean(tpa_max[treatment == 'Disturb']) - mean(tpa_max[treatment == 'Control']),
              #         Tree.mean = mean(Tree[treatment == 'Disturb']) - mean(Tree[treatment == 'Control'])),
              mapping = aes(x = ET, y = tpa_max, color = treatment)) + 
  facet_grid(.~ sev.bin) + 
  theme_bw() +
  geom_point() +
  # geom_point(mapping = aes(color = sev.bin), size = 1) + 
  # geom_line(mapping = aes(color = sev.bin), linewidth = 1) + 
  geom_smooth(mapping = aes(color = treatment), method = 'lm') +
  stat_cor(mapping = aes(color = treatment)) +
  xlab(expression('ET (mm yr'^-1*')')) + ylab(expression('Die-off (trees ha'^-1*')'))
p4b

p4c <- ggplot(data = sev.pixel.sample %>% filter(fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                dplyr::group_by(system.index, treatment, sev.bin) %>% 
                reframe(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                        ET = mean(AET[vi.year %in% c(2011,2012)]),
                        tpa_max = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)], na.rm = TRUE),
                        Water_Stress = PrET[vi.year %in% c(2012,2013,2014,2015)], 
                        stand.age = stand.age[vi.year == 2015]), # %>% 
              # group_by(system.index, sev.bin) %>%
              # reframe(tpa_max.mean = mean(tpa_max[treatment == 'Disturb']) - mean(tpa_max[treatment == 'Control']),
              #         Tree.mean = mean(Tree[treatment == 'Disturb']) - mean(Tree[treatment == 'Control'])),
              mapping = aes(x = Water_Stress, y = tpa_max, color = treatment)) + 
  facet_grid(.~ sev.bin) + 
  theme_bw() +
  geom_point() +
  # geom_point(mapping = aes(color = sev.bin), size = 1) + 
  # geom_line(mapping = aes(color = sev.bin), linewidth = 1) + 
  geom_smooth(mapping = aes(color = treatment), method = 'lm') +
  stat_cor(mapping = aes(color = treatment)) +
  xlab(expression('Pr-ET (mm 4yr'^-1*')')) + ylab(expression('Die-off (trees ha'^-1*')'))
p4c

p4d <- ggplot2::ggplot(data = sev.pixel.sample %>% filter(fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                dplyr::group_by(system.index, treatment, sev.bin) %>% 
                reframe(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])), 
                        ET = mean(AET[vi.year %in% c(2011,2012)]),
                        tpa_max = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)], na.rm = TRUE),
                        Water_Stress = PrET[vi.year %in% c(2012,2013,2014,2015)], 
                        stand.age = stand.age[vi.year == 2015]), # %>% 
               # filter(sev.bin %in% c('Mid', 'High')),
              # reframe(tpa_max.mean = mean(tpa_max[treatment == 'Disturb']) - mean(tpa_max[treatment == 'Control']),
              #         Tree.mean = mean(Tree[treatment == 'Disturb']) - mean(Tree[treatment == 'Control'])),
              mapping = aes(x = stand.age, y = tpa_max, color = treatment)) + 
  facet_grid(.~ sev.bin) + 
  theme_bw() +
  geom_point() +
  # geom_point(mapping = aes(color = sev.bin), size = 1) + 
  # geom_line(mapping = aes(color = sev.bin), linewidth = 1) + 
  geom_smooth(mapping = aes(color = treatment), method = 'lm') +
  stat_cor(mapping = aes(color = treatment)) +
  xlab('Years Since Fire') + ylab(expression('Die-off (trees ha'^-1*')'))
p4d
# glimpse(sev.pixel.sample)
model.data <- sev.pixel.sample %>% filter(fire.year <= 2010 & fire.year >= 1986 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
  dplyr::group_by(system.index, treatment, sev.bin) %>% 
  reframe(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2011,2012)])),
          Tree = mean(Tree_Cover[vi.year %in% c(2011,2012)]), 
          ET = mean(AET[vi.year %in% c(2011,2012)]),
          tpa_max = sum(tpa_max[vi.year %in% c(2015, 2016, 2017, 2018)], na.rm = TRUE),
          Water_Stress = PrET[vi.year %in% c(2012,2013,2014,2015)], 
          stand.age = stand.age[vi.year == 2015],
          fire_sev = fire_sev_2010[vi.year == 2015],
          treat = treat[vi.year == 2015],
          ppt_climate = clm_precip_sum[vi.year == 2015],
          temp_climate = clm_temp_mean[vi.year == 2015],
          elevation = elevation[vi.year == 2015])
glimpse(model.data)
sev.lm <- lm(data = model.data, formula = tpa_max ~ ET + Tree + Water_Stress + stand.age * fire_sev + treat)
summary(sev.lm)
dieoff.relimp <- relaimpo::calc.relimp(sev.lm, rela = TRUE, type = "lmg") 
dieoff.relimp
# dtree.lm <- lm(data = model.data, formula = dTree ~ ET + Tree + Water_Stress + stand.age)
# summary(dtree.lm)

### Fig SX: GAM 10 yr ###
dieoff_gam <- model.data %>% 
        # filter(sev.bin %in% c('Mid', 'High')) %>%
        mgcv::gam(tpa_max ~ 
        s(Tree,bs='cs') + 
        s(ET,bs='cs') + 
        s(Water_Stress,bs='cs') + 
        s(ppt_climate,bs='cs', k = 3) +
        stand.age + 
        fire_sev + treat,
      data = ., method='REML')
summary(dieoff_gam)
#Visualize the game relationships
dieoff_gam_perc_plot <- mgcViz::getViz(dieoff_gam)
dieoff_gam_perc_plot
#Get the intercept
dieoff_gam_int = coef(dieoff_gam)[1]
dieoff_gam_int
#Plot out the non-linear relationships?
print(plot(dieoff_gam_perc_plot, allTerms = TRUE), pages = 1)
# plot fancy gam residuals
# sm?
gam1_trees <- ggplot(plot(sm(dieoff_gam_perc_plot, 1))$data$fit) + geom_line(aes(x=x,y=y),size=0.75) +
  geom_ribbon(aes(x=x,ymax=y+se,ymin=y-se),alpha=0.2) +
  #coord_cartesian(ylim=c(-75,25)) +
  theme(axis.title.y=element_blank()) +
  geom_hline(yintercept=0,alpha=0.5,size=0.5) +
  labs(x='Tree Cover (%)',y=expression(Mortality~residual~(trees~har^-1)))
gam1_trees