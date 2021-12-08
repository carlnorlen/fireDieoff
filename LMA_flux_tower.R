#Author: Carl Norlen
#Date Created: November 23, 2021
#Date Updated: December 3, 2021
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

#Add a decade bin column to the data
data <- data %>% mutate(decade.bin = case_when(
  year >= 1984 & year <= 2000 ~ '1984-2000',
  year >= 2001 & year <= 2020 ~ '2001-2019'))

data <- data %>% mutate(month.bin = case_when(
  month == '02' | month == '03' | month == '04' ~ 'Feb-Apr',
  month == '05' | month == '06' | month == '07' ~ 'May-Jul',
  month == '10' | month == '09' | month == '10' ~ 'Aug-Oct'))

# data %>%
#   group_by(yday, site.name, decade.bin) %>%
#   filter(!is.na(LMA_mean)) %>%
#   summarize(LMA.mean = mean(LMA_mean))

#DOY figures
#LMA
p1 <- ggplot(data = filter(data, !is.na(LMA_mean) & LMA_mean >= 0 & LMA_mean < 1), mapping = aes(x = yday, y = LMA_mean)) + 
  geom_bin2d() +
  geom_line(data = data %>%
              group_by(yday, site.name) %>%
              filter(!is.na(LMA_mean) & LMA_mean >= 0 & LMA_mean < 1) %>%
              summarize(LMA.mean = mean(LMA_mean)), mapping = aes(x = yday, y = LMA.mean), 
            size = 1, color = 'black'
  ) +
  facet_wrap(~ site.name) + ylab(expression('LMA (g m'^-2*')')) + 
  scale_fill_gradient2(limits = c(0,200), breaks = c(0,50,100,150), midpoint = 100, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  theme_bw()
p1

ggsave(filename = 'Fig1_Landsat_LMA_DOY.png', height=12, width=16, units = 'cm', dpi=900)

#NDMI
p2 <- ggplot(data = data, mapping = aes(x=yday, y = NDMI_mean)) + 
  geom_bin2d() +
  geom_line(data = data %>%
              group_by(yday, site.name) %>%
              filter(!is.na(NDMI_mean)) %>%
              summarize(NDMI.mean = mean(NDMI_mean)), mapping = aes(x = yday, y = NDMI.mean), 
            size = 1, color = 'black'
  ) +
  facet_wrap(~ site.name) + ylab('NDMI') + 
  scale_fill_gradient2(limits = c(0,200), breaks = c(0,50,100,150), midpoint = 100, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  theme_bw()
p2

ggsave(filename = 'Fig2_Landsat_NDMI_DOY.png', height=12, width=16, units = 'cm', dpi=900)

#NIRv
p3 <- ggplot(data = data, mapping = aes(x=yday, y = NIRv_mean)) + 
  geom_bin2d() +
  geom_line(data = data %>%
              group_by(yday, site.name) %>%
              filter(!is.na(NIRv_mean)) %>%
              summarize(NIRv.mean = mean(NIRv_mean)), mapping = aes(x = yday, y = NIRv.mean), 
            size = 1, color = 'black'
  ) +
  facet_wrap(~ site.name) + ylab('NIRv') + 
  scale_fill_gradient2(limits = c(0,200), breaks = c(0,50,100,150), midpoint = 100, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  theme_bw()
p3

ggsave(filename = 'Fig3_Landsat_NIRv_DOY.png', height=12, width=16, units = 'cm', dpi=900)

#NDVI
p4 <- ggplot(data = data, mapping = aes(x=yday, y = NDVI_mean)) + 
  geom_bin2d() +
  geom_line(data = data %>%
              group_by(yday, site.name) %>%
              filter(!is.na(NDVI_mean)) %>%
              summarize(NDVI.mean = mean(NDVI_mean)), mapping = aes(x = yday, y = NDVI.mean), 
            size = 1, color = 'black'
  ) +
  facet_wrap(~ site.name) + ylab('NDVI') + 
  scale_fill_gradient2(limits = c(0,200), breaks = c(0,50,100,150), midpoint = 100, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  theme_bw()
p4

ggsave(filename = 'Fig4_Landsat_NDVI_DOY.png', height=12, width=16, units = 'cm', dpi=900)

#Brightness
p5 <- ggplot(data = data, mapping = aes(x=yday, y = brightness_mean)) + 
  geom_bin2d() +
  geom_line(data = data %>%
              group_by(yday, site.name) %>%
              filter(!is.na(brightness_mean)) %>%
              summarize(brightness.mean = mean(brightness_mean)), mapping = aes(x = yday, y = brightness.mean), 
            size = 1, color = 'black'
  ) +
  facet_wrap(~ site.name) + ylab('Brightness') + 
  scale_fill_gradient2(limits = c(0,200), breaks = c(0,50,100,150), midpoint = 100, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  theme_bw()
p5

ggsave(filename = 'Fig5_Landsat_Brightness.png', height=12, width=16, units = 'cm', dpi=900)

#LMA Time Series
p6 <- ggplot() + 
      geom_line(data = data %>% filter((LMA_mean < 1 & LMA_mean > -1) & Site != "US-SCd" & Site != "US-CZ4" & Site != "US-CZ3" & Site != "US-SCs" & Site != 'US-SCg' & !is.na(month.bin)) %>%
                        group_by(site.name, year, month.bin) %>% 
                        mutate(LMA.mean = mean(LMA_mean)), mapping = aes(x = date, y = LMA.mean, color = month.bin), size = 1) +
            xlab('Year') + ylab(expression('LMA (g m'^-2*')')) + facet_wrap(~ site.name) + theme_bw()
p6

ggsave(filename = 'Fig6_Landsat_LMA_time_series.png', height=12, width=16, units = 'cm', dpi=900)

#NDMI Time Series
p7 <- ggplot() +
  geom_line(data = data %>% filter(!is.na(NDMI_mean) & Site != "US-SCd" & Site != "US-CZ4" & Site != "US-CZ3"& Site != "US-SCs" & Site != 'US-SCg'  & !is.na(month.bin)) %>%
              group_by(site.name, year, month.bin) %>% 
              mutate(NDMI.mean = mean(NDMI_mean)), mapping = aes(x = date, y = NDMI.mean, color = month.bin), size = 1) +
  xlab('Year') + ylab('NDMI') + facet_wrap(~ site.name) + theme_bw()
p7

ggsave(filename = 'Fig7_Landsat_NDMI_time_series.png', height=12, width=16, units = 'cm', dpi=900)

#NIRv Time Series
p8 <- ggplot() +
  geom_line(data = data %>% filter(!is.na(NIRv_mean) & Site != "US-SCd" & Site != "US-CZ4" & Site != "US-CZ3" & Site != "US-SCs" & Site != 'US-SCg' & !is.na(month.bin)) %>%
              group_by(site.name, year, month.bin) %>% 
              mutate(NIRv.mean = mean(NIRv_mean)), mapping = aes(x = date, y = NIRv.mean, color = month.bin), size = 1) +
  xlab('Year') + ylab('NIRv') + facet_wrap(~ site.name) + theme_bw()
p8

ggsave(filename = 'Fig8_Landsat_NIRv_time_series.png', height=12, width=16, units = 'cm', dpi=900)
