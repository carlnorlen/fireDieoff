#Author: Carl Norlen
#Date Created: November 23, 2021
#Date Updated: January 25, 2022
#Purpose: Explore Landsat NIRv, NDMI, and LMA data at flux tower sites

#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggplot2', 'stats', 'patchwork', 'data.table')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/LMA')

#Data directory 
dir_in <- "D:\\Fire_Dieoff"

#Add the data
data <- read.csv(file.path(dir_in, "Flux_tower_veg_indices_30m_11212021_v2.csv"), header = TRUE, na.strings = "NaN")

annual.data <- read.csv(file.path(dir_in, "Flux_tower_annual_indices_30m_06132022.csv"), header = TRUE, na.strings = "NaN")

#Rename the flux tower sites
data$site.name <- recode(.x=data$Site, 'US-SCg' ='Grassland US-SCg', 'US-SCf' = 'Oak Pine US-SCf', 'US-SCd' = 'Desert US-SCd', 'US-CZ3' = 'Mixed Conifer US-CZ3', 'US-SCw' = 'Pinyon US-SCw', 'US-CZ1' = 'Oak Pine US-CZ1', 'US-SCs' = 'Coastal Sage US-SCs',
                             'US-CZ4' = 'Subalpine US-CZ4', 'US-CZ2' = 'Ponderosa Pine US-CZ2', 'US-SCc' = 'Chaparral US-SCc')

annual.data$site.name <- recode(.x=annual.data$Site, 'US-SCg' ='Grassland US-SCg', 'US-SCf' = 'Oak Pine US-SCf', 'US-SCd' = 'Desert US-SCd', 'US-CZ3' = 'Mixed Conifer US-CZ3', 'US-SCw' = 'Pinyon US-SCw', 'US-CZ1' = 'Oak Pine US-CZ1', 'US-SCs' = 'Coastal Sage US-SCs',
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

#Format the annual data
#Format dates for the data
annual.data$date <- as.Date(annual.data$date)
annual.data$year <- format(annual.data$date, '%Y')
annual.data$month <- format(annual.data$date, '%m')
annual.data$yday <- yday(annual.data$date)

# annual.data %>% summary()

#Update Cover data to 100% scale
annual.data$Tree_Cover <- annual.data$Tree_Cover_mean / 100
annual.data$Shrub_Cover <- annual.data$Shrub_Cover_mean / 100
annual.data$Herb_Cover <- annual.data$Herb_Cover_mean / 100
annual.data$Bare_Cover <- annual.data$Bare_Cover_mean / 100

#Convert the SPI48 scale back to decimal
annual.data$SPI48 <- annual.data$SPI48_mean / 100

#Try to fix soil moisture by dividing by 10
annual.data$Soil_Moisture <- annual.data$Soil_Moisture_mean / 10
annual.data$GPP <- annual.data$GPP_mean
annual.data$AET <- annual.data$AET_mean
annual.data$Water_Stress <- annual.data$Water_Stress_mean
annual.data$ppt <- annual.data$ppt_mean
annual.data$tmax <- annual.data$tmax_mean

#Assign no data to TPAmax
annual.data[annual.data$tpa_max_mean == -9999,]$tpa_max_mean <- NA
annual.data$tpa_max <- annual.data$tpa_max_mean

#Add a decade bin column to the data
annual.data <- annual.data %>% mutate(decade.bin = case_when(
  year >= 1984 & year <= 2000 ~ '1984-2000',
  year >= 2001 & year <= 2020 ~ '2001-2019'))

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
      geom_line(data = data %>% filter((LMA_mean < 1 & LMA_mean > -1) & !is.na(month.bin) & month.bin != 'Feb-Apr') %>% # & Site != "US-SCd" & Site != "US-CZ4" & Site != "US-CZ3" & Site != "US-SCs" & Site != 'US-SCg') %>%
                        group_by(site.name, year, month.bin) %>% 
                        mutate(LMA.mean = mean(LMA_mean)), mapping = aes(x = date, y = LMA.mean, color = month.bin), size = 1) +
            xlab('Year') + ylab(expression('LMA (g m'^-2*')')) + facet_wrap(~ site.name) + theme_bw() + theme(strip.text.x = element_text(size = 7)) 
p6

ggsave(filename = 'Fig6_Landsat_LMA_time_series.png', height=12.5, width=16, units = 'cm', dpi=900)

#NDMI Time Series
p7 <- ggplot() +
  geom_line(data = data %>% filter(!is.na(NDMI_mean) & !is.na(month.bin)) %>% # & Site != "US-SCd" & Site != "US-CZ4" & Site != "US-CZ3"& Site != "US-SCs" & Site != 'US-SCg'  ) %>%
              group_by(site.name, year, month.bin) %>% 
              mutate(NDMI.mean = mean(NDMI_mean)), mapping = aes(x = date, y = NDMI.mean, color = month.bin), size = 1) +
  xlab('Year') + ylab('NDMI') + facet_wrap(~ site.name) + theme_bw() + theme(strip.text.x = element_text(size = 7)) 
p7

ggsave(filename = 'Fig7_Landsat_NDMI_time_series.png', height=12, width=16, units = 'cm', dpi=900)

#NIRv Time Series
p8 <- ggplot() +
  geom_line(data = data %>% filter(!is.na(NIRv_mean) & !is.na(month.bin)) %>% # & Site != "US-SCd" & Site != "US-CZ4" & Site != "US-CZ3" & Site != "US-SCs" & Site != 'US-SCg') %>%
              group_by(site.name, year, month.bin) %>% 
              mutate(NIRv.mean = mean(NIRv_mean)), mapping = aes(x = date, y = NIRv.mean, color = month.bin), size = 1) +
  xlab('Year') + ylab('NIRv') + facet_wrap(~ site.name) + theme_bw() + theme(strip.text.x = element_text(size = 7)) 
p8

ggsave(filename = 'Fig8_Landsat_NIRv_time_series.png', height=12, width=16, units = 'cm', dpi=900)

#NIRv Time Series
p9 <- ggplot() +
  geom_line(data = data %>% filter(!is.na(NDVI_mean) & !is.na(month.bin)) %>% # & Site != "US-SCd" & Site != "US-CZ4" & Site != "US-CZ3" & Site != "US-SCs" & Site != 'US-SCg') %>%
              group_by(site.name, year, month.bin) %>% 
              mutate(NDVI.mean = mean(NDVI_mean)), mapping = aes(x = date, y = NDVI.mean, color = month.bin), size = 1) +
  xlab('Year') + ylab('NDVI') + facet_wrap(~ site.name) + theme_bw() + theme(strip.text.x = element_text(size = 7)) 
p9

ggsave(filename = 'Fig8_Landsat_NDVI_time_series.png', height=12, width=16, units = 'cm', dpi=900)

#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")

#Veg Type Time Series
p10 <- ggplot() + 
  geom_line(data = annual.data %>% filter(!is.na(Shrub_Cover)) %>% 
              group_by(site.name, year) %>% 
              mutate(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = date, y = Shrub_Cover.mean, color = "Shrub"), size = 1) +
  geom_line(data = annual.data %>% filter(!is.na(Tree_Cover)) %>% 
              group_by(site.name, year) %>% 
              mutate(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = date, y = Tree_Cover.mean, color = "Tree"), size = 1) +
  geom_line(data = annual.data %>% filter(!is.na(Bare_Cover)) %>% 
              group_by(site.name, year) %>% 
              mutate(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = date, y = Bare_Cover.mean, color = "Bare"), size = 1) +
  geom_line(data = annual.data %>% filter(!is.na(Herb_Cover)) %>% 
              group_by(site.name, year) %>% 
              mutate(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = date, y = Herb_Cover.mean, color = "Herb"), size = 1) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  xlab('Year') + ylab(expression('Cover (%)')) + facet_wrap(~ site.name) + theme_bw() + theme(strip.text.x = element_text(size = 7)) 
p10

ggsave(filename = 'Fig10_Veg_Cover_Flux_Tower_time_series.png', height=12.5, width=16, units = 'cm', dpi=900)

#LMA Time Series
p11 <- ggplot() + 
  geom_line(data = data %>% filter((LMA_mean < 1 & LMA_mean > -1) & !is.na(month.bin) & Site != "US-SCd" & Site != "US-CZ4" & Site != "US-CZ3" & Site != "US-SCs" & Site != 'US-SCg') %>%
              group_by(site.name, year, month.bin) %>% 
              mutate(LMA.mean = mean(LMA_mean)), mapping = aes(x = date, y = LMA.mean, color = month.bin), size = 1) +
  xlab('Year') + ylab(expression('LMA (g m'^-2*')')) + facet_wrap(~ site.name) + theme_bw() + theme(strip.text.x = element_text(size = 7)) 
p11

ggsave(filename = 'Fig11_Landsat_LMA_flux_tower_subset_time_series.png', height=12.5, width=16, units = 'cm', dpi=900)

p12<- ggplot() + 
  geom_line(data = annual.data %>% filter(!is.na(tpa_max)) %>% 
              group_by(site.name, year) %>% 
              mutate(tpa_max.mean = mean(tpa_max)), mapping = aes(x = date, y = tpa_max.mean), size = 1) +
              theme_bw()  + facet_wrap(~ site.name)
p12

ggsave(filename = 'Fig12_flux_tower_ads_dieoff_time_series.png', height=12.5, width=16, units = 'cm', dpi=900)
