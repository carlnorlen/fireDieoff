#Author: Carl Norlen
#Date Created: January 24, 2022
#Date Updated: January 24, 2023
#Purpose: Create figures for EEB GSS presentation

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('zoo'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(zoo)
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the Wildfire data
frap.fire.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_wildfire_400pt_ts8_300m_20230112.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
frap.fire.data$treatment <- 'Disturb'

#Add the Wildfire buffer data
frap.control.data <- read.csv(file.path(dir_in, "control_south_sierra_FRAP_4km_buffer_400pt_ts16_300m_20230124.csv"), header = TRUE, na.strings = "NaN")

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
rx.data <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_rxfire_400pt_ts8_300m_20230112.csv"), header = TRUE, na.strings = "NaN")

#Add the treatment column
rx.data$treatment <- 'Disturb'

#Add teh Rx fire buffer data
rx.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Rx_2km_buffer_400pt_ts16_300m_20230124.csv"), header = TRUE, na.strings = "NaN")

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
# pixel.data <- rbind(combine.data, control.data.2km)
summary(rx.pixel.data)

#Combine the wildfire and Rx fire data together
pixel.data <- combine(frap.pixel.data, rx.pixel.data)

summary(pixel.data)

`%notin%` <- Negate(`%in%`)

#Convert data to long format
pixel.data <- pixel.data %>% 
  pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

pixel.data$year <- as.numeric(pixel.data$year) + 1984 

#Convert missing TPA data to NAs
pixel.data[pixel.data$tpa_max < 0,]$tpa_max <- NA

#Convert fire data -9999 to NAs
pixel.data[pixel.data$fire_type_2010 == -9999,]$fire_type_2010 <- NA
pixel.data[pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
pixel.data[pixel.data$fire_type_2019 == -9999,]$fire_type_2019 <- NA
pixel.data[pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
pixel.data[pixel.data$fire_type_2020 == -9999,]$fire_type_2020 <- NA
pixel.data[pixel.data$fire_year_2020 == -9999,]$fire_year_2020 <- NA

#Convert to trees per hectare
pixel.data$tpa_max <- pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
pixel.data$date <- as.Date(as.character(pixel.data$year), format = '%Y')
# pixel.data$vi.year <- format(pixel.data$date , '%Y')
pixel.data$vi.year <- pixel.data$year
#Use the FRAP fire perimeter year (use fire year 2010)
pixel.data$fire.year <- pixel.data$fire_year_2010
pixel.data$stand.age <- as.numeric(pixel.data$year) - as.numeric(pixel.data$fire.year) 

#Update Cover data to 100% scale
pixel.data$Tree_Cover <- pixel.data$Tree_Cover / 100
pixel.data$Shrub_Cover <- pixel.data$Shrub_Cover / 100
pixel.data$Herb_Cover <- pixel.data$Herb_Cover / 100
pixel.data$Bare_Cover <- pixel.data$Bare_Cover / 100

#Convert the SPI48 scale back to decimal
pixel.data$SPI48 <- pixel.data$SPI48 / 100

#Try to fix soil moisture by dividing by 10
pixel.data$Soil_Moisture <- pixel.data$Soil_Moisture / 10

#Calculate Pr-ET
pixel.data$PrET <- pixel.data$ppt - pixel.data$AET

pixel.data %>% summary()

pixel.data <- pixel.data %>% mutate(fire.year.bin = case_when(
  treatment == 'Control' | fire.year < 1980 ~ 'No Fire',
  fire.year >= 1980 & fire.year <= 2010 ~ '1980-2010',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2018',
  fire.year >= 2019 ~ '2019-2020'))#'0-4'))

pixel.data <- pixel.data %>% mutate(fire.type.bin = case_when(
  fire_type_2010 == 1 ~ 'Wildfire',
  fire_type_2010 == 2 ~ 'Rxfire'
))

summary(pixel.data)

pixel.data$fire.year.bin = with(pixel.data, factor(fire.year.bin, levels = c('2019-2020', '2011-2018', '1980-2010',  'No Fire')))#

#Recode the veg type data
pixel.data$veg_name <- recode(.x=pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
                              '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
                              '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

#Figure of Dead Trees per acre separated by fire years with time series
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & stratlayer %in% strat.list  & stratlayer %in% strat.list
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), # %>%
            # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & stratlayer %in% strat.list  & stratlayer %in% strat.list
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), #%>%
              # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formating
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() + facet_grid(. ~ fire.type.bin) +
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
  geom_line(data = pixel.data %>%
              filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & stratlayer %in% strat.list & stratlayer %in% strat.list
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()), 
              # filter(if_else(fire.year.bin == '1980-2010', Tree_Cover.n >= 2500, Tree_Cover.n >= 0)),
            mapping = aes(x = date, y = Tree_Cover.mean, color = fire.year.bin, linetype = fire.year.bin), 
            size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(Tree_Cover > 0 & fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% 
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),  
                # filter(if_else(fire.year.bin == '1980-2010', Tree_Cover.n >= 2500, Tree_Cover.n >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formating
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() + facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + 
  ylim(30, 55) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p6

f2 <- ggarrange(p5, p6, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f2
#Save the data
ggsave(filename = 'Fig3a_frap_rx_dieoff_tree_cover_stand_age_time_series.png', height=12, width= 18, units = 'cm', dpi=900)

#Figure 4: Precip, ET, Soil moisture, Water Stress time series figure
p7 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()),
            mapping = aes(x = date, y = ppt.mean, color = fire.year.bin, linetype = fire.year.bin),
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() + facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
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
  geom_line(data = pixel.data %>%
              filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()),
            mapping = aes(x = date, y = AET.mean, color = fire.year.bin, linetype = fire.year.bin),
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() + facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + ylim(300, 750) +
  #facet_grid(. ~ fire.year.bin) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year')
p8

#Create the Water Stress Panel
p10 <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.data %>%
              filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
              # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, fire.year.bin, fire.type.bin) %>%
              summarize(PrET.mean = mean(PrET), PrET.n = n(), count = n()),
            mapping = aes(x = date, y = PrET.mean, color = fire.year.bin, linetype = fire.year.bin),
            size = 1) +
  #Water Stress 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # &
                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, fire.year.bin, fire.type.bin) %>%
                summarize(PrET.mean = mean(PrET),
                          PrET.sd = sd(PrET), PrET.n = n(), count = n()),
              mapping = aes(ymin=PrET.mean - 1.96*(PrET.sd / sqrt(PrET.n)),
                            ymax=PrET.mean + 1.96*(PrET.sd / sqrt(PrET.n)),
                            x = date, fill = fire.year.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() + facet_grid(. ~ fire.type.bin) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.15, 0.35), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('2010-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression('Pr-ET (mm yr'^-1*')')) + xlab('Year')
p10

f3 <- ggarrange(p7, p8, p10, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f3
# #Save the data
ggsave(filename = 'Fig4a_frap_rx_water_fluxes_time_series.png', height=16, width= 18, units = 'cm', dpi=900)
# 
# #Figure 5: Bar Chats, this could be for statistics
# p11 <- ggplot() +
#   #Calculate the Mean
#   stat_summary(data = pixel.data %>% 
#                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% 
#                # filter(elevation <= 3000) %>%
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, fire.year.bin) %>% 
#                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
#                          Water_Stress = Water_Stress[vi.year == 2015]),
#              mapping = aes(x = fire.year.bin, y = dTree), 
#              fun = mean, geom = "bar", fill = 'grey') + 
#   #Calculate the Standard Error
#   stat_summary(data = pixel.data %>% 
#                   filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                   # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                   # filter(elevation <= 3000) %>%
#                   # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                   dplyr::group_by(system.index, fire.year.bin) %>% 
#                   summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
#                             Water_Stress = Water_Stress[vi.year == 2015]),
#                 mapping = aes(x = fire.year.bin, y = dTree), 
#                 fun.data = mean_se, geom = "errorbar", size = 1) + 
#   theme_bw() +
#   scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), legend.position = c(0.8, 0.75), axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   xlab('Stand Age (10-year Bins)') + ylab('dTree (%)')
# p11
# 
# #RdTree Plot
# p12 <- ggplot() +
#   #Data Summary
#   stat_summary(data = pixel.data %>% 
#                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                # filter(elevation <= 3000) %>%
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, fire.year.bin) %>% 
#                summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
#                          RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), Water_Stress = Water_Stress[vi.year == 2015]),
#              mapping = aes(x = fire.year.bin, y = RdTree * 100), 
#              fun = mean, geom = "bar", fill = 'grey') + 
#   stat_summary(data = pixel.data %>% 
#                   filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                   # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                   # filter(elevation <= 3000) %>%
#                   # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                   dplyr::group_by(system.index, fire.year.bin) %>% 
#                   summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), 
#                             RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), Water_Stress = Water_Stress[vi.year == 2015]),
#                 mapping = aes(x = fire.year.bin, y = RdTree * 100), 
#                fun.data = mean_se, geom = "errorbar", size = 1) + 
#   theme_bw() +
#   scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   xlab('Stand Age (10-year Bins)') + ylab('Relative dTree (%)')
# p12
# 
# #ADS die-off
# p13 <- ggplot() +
#   #Create bars and error bars
#   stat_summary(data = pixel.data %>% 
#                filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% 
#                # filter(elevation <= 3000) %>%
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, fire.year.bin) %>%
#                summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
#              mapping = aes(x = fire.year.bin, y = tpa_max), 
#              fun = mean, geom = "bar", fill = 'grey') + 
#   stat_summary(data = pixel.data %>% 
#                   filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                   # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                   # filter(elevation <= 3000) %>%
#                   # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                   dplyr::group_by(system.index, fire.year.bin) %>%
#                   summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
#                 mapping = aes(x = fire.year.bin, y = tpa_max), 
#                fun.data = mean_se, geom = "errorbar", size = 1) + 
#   theme_bw() +
#   scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   ylab('Mortality (trees/ha)') 
# p13
# 
# # ggsave(filename = 'Fig10_ADS_mortality_stand_age_wildfire_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)
# 
# #Pre-Die-off Tree Cover
# p14 <- ggplot() +
#   #Create the Error Bars
#   stat_summary(data = pixel.data %>% 
#                  filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% 
#                  # filter(elevation <= 3000) %>%
#                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                  dplyr::group_by(system.index, fire.year.bin) %>%
#                  summarize(fire.year.bin = fire.year.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013, 2014)])),
#                mapping = aes(x = fire.year.bin, y = Tree_Cover), 
#                fun = mean, geom = "bar", fill = 'grey') + 
#   stat_summary(data = pixel.data %>% 
#                  filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                  # filter(elevation <= 3000) %>%
#                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                  dplyr::group_by(system.index, fire.year.bin) %>%
#                  summarize(fire.year.bin = fire.year.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2013, 2014)])),
#                mapping = aes(x = fire.year.bin, y = Tree_Cover), 
#                fun.data = mean_se, geom = "errorbar", size = 1) + 
#   theme_bw() +
#   scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   ylab('Tree Cover (%)')
# p14
# 
# #Water Stress
# p15 <- ggplot() +
#   #Create the Error Bars
#   stat_summary(data = pixel.data %>% 
#                  filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
#                  # filter(elevation <= 3000) %>%
#                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                  dplyr::group_by(system.index, fire.year.bin) %>%
#                  summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], 
#                            Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
#                mapping = aes(x = fire.year.bin, y = Water_Stress), 
#                fun = mean, geom = "bar", fill = 'grey') + 
#   stat_summary(data = pixel.data %>% 
#                  filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2) | (is.na(fire.year))) %>% # &
#                  # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%  
#                  # filter(elevation <= 3000) %>%
#                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                  dplyr::group_by(system.index, fire.year.bin) %>%
#                  summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], 
#                            Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)])),
#                mapping = aes(x = fire.year.bin, y = Water_Stress), 
#                fun.data = mean_se, geom = "errorbar", size = 1) + 
#   theme_bw() +
#   scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   xlab('Fire Year') + ylab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) 
# p15
# 
# #Combine the Panels
# f4 <- ggarrange(p11, p12, p13, p14, p15,  ncol = 1, nrow = 5, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)', 'e)'))
# f4
# 
# ggsave(filename = 'Fig5a_wild_fire_bar_chart_comparison.png', height=24, width = 18, units = 'cm', dpi=900)
# # summary(pixel.data)
# 
# #Water Stress Versus dTree to check the mechanism...
# #figure out how to filter low count numbers (look at manuscript 1)
# p16 <- ggplot(data = pixel.data %>% filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & Tree_Cover > 0) | (is.na(fire.year) & Tree_Cover > 0)) %>% # &
#                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>% 
#                 # filter(elevation <= 3000) %>%
#                 dplyr::group_by(system.index) %>% 
#                 summarize(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
#                           RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
#                           Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), 
#                           fire.year.bin = fire.year.bin[vi.year == 2010])) +
#   geom_bin2d(binwidth = c(100, 1), mapping = aes(x = Water_Stress, y = RdTree * 100, group = ..count..)) +
#   scale_fill_gradient2(limits = c(0,130), breaks = c(0,30,60,90,120), midpoint = 62.5, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') + #
#   # geom_point(mapping = aes(x = Water_Stress, y = dTree), size = 1) + 
#   geom_smooth(method = 'lm', mapping = aes(x = Water_Stress, y = RdTree * 100), color = 'black', linetype = 'dashed', size = 2) +
#   facet_wrap (. ~ fire.year.bin) +
#   stat_cor( mapping = aes(x = Water_Stress, y = RdTree * 100), color = 'black') + #xlim(-600, 0) + 
#   theme_bw() +
#   xlab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) + ylab('Die-off (Relative dTree %)')
# p16
# ggsave(filename = 'Fig6a_water_stress_dTree_300m.png', height=16, width= 18, units = 'cm', dpi=900)
# 
# pixel.data %>% summary()
# 
# #Create a manual color scale
# cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
# fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
# fills
# p18 <- ggplot() + 
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#   #Create a shrub cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire perimeter and fier year by pixel match 
#               group_by(stand.age, treatment ) %>%
#               summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub', linetype = treatment), size = 1) +
#   #Shrub Cover 95% CI
#   geom_errorbar(data = pixel.data %>% 
#                 filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%         
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                 group_by(stand.age, treatment) %>%
#                 summarize(Shrub_Cover.mean = mean(Shrub_Cover),
#                           Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
#               mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
#                             ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
#                             x = stand.age, color = "Shrub",  linetype = treatment), alpha = 0.3) +
#   #Create a Tree Cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#               group_by(stand.age, treatment) %>%
#               summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree',  linetype = treatment), size = 1) + 
#   #Tree Cover 95% CI
#   geom_errorbar(data = pixel.data %>% 
#                 filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                 group_by(stand.age, treatment) %>%
#                 summarize(Tree_Cover.mean = mean(Tree_Cover),
#                           Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
#               mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
#                             ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
#                             x = stand.age, color = "Tree",  linetype = treatment), alpha = 0.3) +
#   #Create an Herb cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#               group_by(stand.age, treatment) %>%
#               summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb',  linetype = treatment), size = 1) + 
#   #Herb Cover 95% CI
#   geom_errorbar(data = pixel.data %>% 
#                 filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                 group_by(stand.age, treatment) %>%
#                 summarize(Herb_Cover.mean = mean(Herb_Cover),
#                           Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
#               mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
#                             ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
#                             x = stand.age, color = "Herb",  linetype = treatment), alpha = 0.3) +
#   #Create a Bare cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#               group_by(stand.age, treatment) %>%
#               summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare',  linetype = treatment), size = 1) + 
#   #Bare Cover 95% CI
#   geom_errorbar(data = pixel.data %>%
#                 filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#                 # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                 group_by(stand.age, treatment) %>%
#                 summarize(Bare_Cover.mean = mean(Bare_Cover),
#                           Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
#               mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
#                             ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
#                             x = stand.age, color = "Bare",  linetype = treatment), alpha = 0.3) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
#   scale_fill_manual(values = fills) + 
#   guides(fill = "none") +
#   ylab(expression('Cover (%)')) + xlab('Years Since Fire')
# p18
# 
# #Save the data
# ggsave(filename = 'Fig8a_frap_stand_age_veg_cover.png', height=18, width= 20, units = 'cm', dpi=900)
# 
# p19 <- ggplot() + 
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#   #Create a Tree Cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#               group_by(stand.age) %>%
#               summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control'])), 
#             mapping = aes(x = stand.age, y = Tree_Cover.mean), size = 1) + 
#   #Tree Cover 95% CI
#   geom_errorbar(data = pixel.data %>%
#                   filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#                   # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                   # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                   group_by(stand.age) %>%
#                   summarize(Tree_Cover.mean = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control']),
#                             Tree_Cover.sd = sd(Tree_Cover[treatment == 'Disturb'])^2 + sd(Tree_Cover[treatment == 'Control'])^2, 
#                             Tree_Cover.n = n()),
#                 mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / Tree_Cover.n),
#                               ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / Tree_Cover.n),
#                               x = stand.age)) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
#   scale_fill_manual(values = fills) + 
#   guides(fill = "none") +
#   ylab(expression('Tree Reduction (%)')) + xlab('Years Since Fire')
# p19
# 
# #Save the data
# ggsave(filename = 'Fig9a_frap_stand_age_tree_cover.png', height=18, width= 20, units = 'cm', dpi=900)
# 
# #AET change with wildfire (FRAP)
# p20 <- ggplot() + 
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
# #Create a Tree Cover line
# geom_line(data = pixel.data %>%
#             filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#             # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#             # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#             # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#             group_by(stand.age) %>%
#             summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control'])), 
#           mapping = aes(x = stand.age, y = AET.mean), size = 1) + 
#   #Tree Cover 95% CI
#   geom_errorbar(data = pixel.data %>%
#                   filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#                   # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                   # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                   group_by(stand.age) %>%
#                   summarize(AET.mean = mean(AET[treatment == 'Disturb']) - mean(AET[treatment == 'Control']),
#                             AET.sd = sd(AET[treatment == 'Disturb'])^2 + sd(AET[treatment == 'Control'])^2, 
#                             AET.n = n()),
#                 mapping = aes(ymin=AET.mean - 1.96*(AET.sd / AET.n),
#                               ymax=AET.mean + 1.96*(AET.sd / AET.n),
#                               x = stand.age)) +
# theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
#   scale_fill_manual(values = fills) + 
#   guides(fill = "none") +
#   ylab(expression('AET Reduction (mm yr'^-1*')')) + xlab('Years Since Fire')
# p20
# 
# #Save the data
# ggsave(filename = 'Fig10a_frap_stand_age_AET.png', height=18, width= 20, units = 'cm', dpi=900)
# 
# #Pr-ET change with wildfire (FRAP)
# p21 <- ggplot() + 
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#   #Create a Tree Cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#               # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#               group_by(stand.age) %>%
#               summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control'])), 
#             mapping = aes(x = stand.age, y = Shrub_Cover.mean), size = 1) + 
#   #Tree Cover 95% CI
#   geom_errorbar(data = pixel.data %>%
#                   filter(stand.age >= -10 & stand.age <= 25 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1980 & fire.year <= 2010 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper & stratlayer %in% strat.list
#                   # filter(lf_evt_2001 %in% c(2031, 2173, 2027, 2019, 2032, 2033, 2172, 2053)) %>%
#                   # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
#                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                   group_by(stand.age) %>%
#                   summarize(Shrub_Cover.mean = mean(Shrub_Cover[treatment == 'Disturb']) - mean(Shrub_Cover[treatment == 'Control']),
#                             Shrub_Cover.sd = sd(Shrub_Cover[treatment == 'Disturb'])^2 + sd(Shrub_Cover[treatment == 'Control'])^2, 
#                             Shrub_Cover.n = n()),
#                 mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / Shrub_Cover.n),
#                               ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / Shrub_Cover.n),
#                               x = stand.age)) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
#   scale_fill_manual(values = fills) + 
#   guides(fill = "none") +
#   ylab(expression('Shrub Change (%)')) + xlab('Years Since Fire')
# p21