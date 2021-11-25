#Author: Carl Norlen
#Date Created: November 23, 2021
#Date Updated: November 24, 2021
#Purpose: Explore Landsat NIRv, NDMI, and LMA data at flux tower sites

#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggplot2', 'stats', 'patchwork', 'data.table')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff')

#Data directory 
dir_in <- "D:\\Large_Files\\Fire_Dieoff"

#Add the data
data <- read.csv(file.path(dir_in, "Flux_tower_veg_indices_30m_11212021_v2.csv"), header = TRUE, na.strings = "NaN")

unique(data$Site)

#Rename the flux tower sites
data$site.name <- recode(.x=data$Site, 'US-SCg' ='Grassland US-SCg', 'US-SCf' = 'Oak Pine US-SCf', 'US-SCd' = 'Desert US-SCd', 'US-CZ3' = 'Mixed Conifer US-CZ3', 'US-SCw' = 'Pinyon US-SCw', 'US-CZ1' = 'Oak Pine US-CZ1', 'US-SCs' = 'Coastal Sage US-SCs',
                             'US-CZ4' = 'Subalpine US-CZ4', 'US-CZ2' = 'Ponderosa Pine US-CZ2', 'US-SCc' = 'Chaparral US-SCc')

#Format dates for the data
data$date <- as.Date(data$date)
data$year <- format(data$date, '%Y')
data$month <- format(data$date, '%m')
data$yday <- yday(data$date)

#DOY figures
#LMA
p1 <- ggplot(data = filter(data, LMA_mean >= 0 & LMA_mean < 1), mapping = aes(x=yday, y = LMA_mean, color = year)) + 
  geom_point(size = 0.1) + facet_wrap(~ site.name) + ylab('LMA') + 
  theme_bw()
p1

ggsave(filename = 'Fig1_Landsat_LMA.png', height=12, width=16, units = 'cm', dpi=900)

#NDMI
p2 <- ggplot(data = data, mapping = aes(x=yday, y = NDMI_mean, color = year)) + 
  geom_point(size = 0.1) + facet_wrap(~ site.name) + ylab('NDMI') + 
  theme_bw()
p2

ggsave(filename = 'Fig2_Landsat_NDMI.png', height=12, width=16, units = 'cm', dpi=900)

#NIRv
p3 <- ggplot(data = data, mapping = aes(x=yday, y = NIRv_mean, color = year)) + 
  geom_point(size = 0.1) + facet_wrap(~ site.name) + ylab('NIRv') + 
  theme_bw()
p3

ggsave(filename = 'Fig3_Landsat_NIRv.png', height=12, width=16, units = 'cm', dpi=900)

#NIRv
p4 <- ggplot(data = data, mapping = aes(x=yday, y = NDVI_mean, color = year)) + 
  geom_point(size = 0.1) + facet_wrap(~ site.name) + ylab('NDVI') + 
  theme_bw()
p4

ggsave(filename = 'Fig4_Landsat_NDVI.png', height=12, width=16, units = 'cm', dpi=900)

#Brightness
p5 <- ggplot(data = data, mapping = aes(x=yday, y = brightness_mean, color = year)) + 
  geom_point(size = 0.1) + facet_wrap(~ site.name) + ylab('Brightness') + 
  theme_bw()
p5

ggsave(filename = 'Fig5_Landsat_Brightness.png', height=12, width=16, units = 'cm', dpi=900)

# data %>% filter(month == 8 | month == 9 | month == 10) %>%
#   group_by(year, site.name) #%>% 
#   #mutate(LMA.mean = mean(LMA_mean))

#LMA Time Series
p6 <- ggplot(data = data %>% filter((month == 8 | month == 9 | month == 10) & (LMA_mean < 1 & LMA_mean > -1)) %>%
                                    # & (Site == "US-SCf" | Site == "US-CZ1" | Site == "US-CZ2" | Site == "US-CZ3" |
                                    #    Site == "US-CZ4" | Site == "US-SCw")) %>% 
               group_by(site.name, year) %>% mutate(LMA.mean = mean(LMA_mean))) + #& LMA_mean < 1 & LMA_mean > -1)) + #%>%
            #group_by(year, site.name) %>% mutate(LMA.mean = mean(LMA_mean))) +
            geom_line(mapping = aes(x = date, y = LMA.mean), color = 'black', size = 1) +
            xlab('Year') + ylab(expression('LMA (g m'^-2*')')) + facet_wrap(~ site.name) + theme_bw()
p6

ggsave(filename = 'Fig6_Landsat_LMA_time_series.png', height=12, width=16, units = 'cm', dpi=900)

#NDMI Time Series
p7 <- ggplot(data = data %>% filter(month == 8 | month == 9 | month == 10 )%>% 
               group_by(site.name, year) %>% mutate(NDMI.mean = mean(NDMI_mean))) +
  geom_line(mapping = aes(x = date, y = NDMI.mean), color = 'black', size = 1) +
  xlab('Year') + ylab('NDMI') + facet_wrap(~ site.name) + theme_bw()
p7

ggsave(filename = 'Fig7_Landsat_NDMI_time_series.png', height=12, width=16, units = 'cm', dpi=900)

#NIRv Time Series
p8 <- ggplot(data = data %>% filter(month == 8 | month == 9 | month == 10) %>% 
               group_by(site.name, year) %>% mutate(NIRv.mean = mean(NIRv_mean))) +
  geom_line(mapping = aes(x = date, y = NIRv.mean), color = 'black', size = 1) +
  xlab('Year') + ylab('NIRv') + facet_wrap(~ site.name) + theme_bw()
p8

ggsave(filename = 'Fig8_Landsat_NIRv_time_series.png', height=12, width=16, units = 'cm', dpi=900)