#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: August 1, 2022
#Purpose: Create figures South Sierra sample figures.

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/pixel_sample')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the data
# pixel.data <- read.csv(file.path(dir_in, "Stratified_sample_stand_age_2012_no_fire_history_mask_20210629_30m_v2.csv"), header = TRUE, na.strings = "NaN") #v2 is for all of Sierra and Socal
# pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_stand_age_no_fire_history_mask_01242022_30m.csv"), header = TRUE, na.strings = "NaN")
# pixel.data <- read.csv(file.path(dir_in, "fraprx_ecoregion_stratified_sample_100pts_30m_ts8_20220713.csv"), header = TRUE, na.strings = "NaN")
pixel.data <- read.csv(file.path(dir_in, "Sierra_ecoregion_simple_sample_1pct_300m_ts16_20220801.csv"), header = TRUE, na.strings = "NaN")
# list.files(fire_in)
summary(pixel.data)
#Get a  of the data
# summary(pixel.data)
# pixel.data <- pixel.data %>% filter(fire.year >= 1919 & !is.na(stand.age) & !is.na(NDMI))

`%notin%` <- Negate(`%in%`)

#Convert data to long format
pixel.data <- pixel.data %>% #dplyr::select(-c('latitude', 'longitude')) %>% 
               pivot_longer(cols = X10_AET_mean:X9_tpa_max_mode, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

pixel.data$year <- as.numeric(pixel.data$year) + 1984 

#Convert missing TPA data to NAs
pixel.data[pixel.data$tpa_max_mean < 0,]$tpa_max_mean <- NA

#Convert fire data -9999 to NAs
pixel.data[pixel.data$fire_type_2010_mode == -9999,]$fire_type_2010_mode <- NA
pixel.data[pixel.data$fire_year_2010_mode == -9999,]$fire_year_2010_mode <- NA
pixel.data[pixel.data$fire_type_2020_mode == -9999,]$fire_type_2020_mode <- NA
pixel.data[pixel.data$fire_year_2020_mode == -9999,]$fire_year_2020_mode <- NA

#Convert to trees per hectare
pixel.data$tpa_max <- pixel.data$tpa_max_mean * 2.47105

#Make the dates into date time format for R
pixel.data$date <- as.Date(as.character(pixel.data$year), format = '%Y')
# pixel.data$vi.year <- format(pixel.data$date , '%Y')
pixel.data$vi.year <- pixel.data$year
#Use the FRAP fire perimeter year
# pixel.data$fire.year <- pixel.data$perimeter_year
# pixel.data$stand.age <- as.numeric(pixel.data$year) - as.numeric(pixel.data$fire.year) 

#Update Cover data to 100% scale
pixel.data$Tree_Cover <- pixel.data$Tree_Cover_mean / 100
pixel.data$Shrub_Cover <- pixel.data$Shrub_Cover_mean / 100
pixel.data$Herb_Cover <- pixel.data$Herb_Cover_mean / 100
pixel.data$Bare_Cover <- pixel.data$Bare_Cover_mean / 100

#Convert the SPI48 scale back to decimal
pixel.data$SPI48 <- pixel.data$SPI48_mean / 100

#Try to fix soil moisture by dividing by 10
pixel.data$Soil_Moisture <- pixel.data$Soil_Moisture_mean / 10

#Rename ppt and Water Stress
pixel.data$Water_Stress <- pixel.data$Water_Stress_mean
pixel.data$ppt <- pixel.data$ppt_mean
pixel.data$AET <- pixel.data$AET_mean

#Figure of Dead Trees per acre separated by fire years with time series
p3 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(tpa_max) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% # & vi.year >= 2003) %>%
              group_by(date) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), # %>%
              # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mean), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(tpa_max) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% 
                group_by(date) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), #%>%
                # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date), alpha = 0.3) +
  #Do the Formating
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p3

# pixel.data %>%
#   filter(!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1920 & stand.age >= 2 & !is.na(fire.year.bin)) %>% 
#   group_by(date, fire.year.bin) %>%
#   summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
#   filter(if_else(fire.year.bin == '1990-2010', Tree_Cover.n >= 12000 , Tree_Cover.n >= 0))

#Create the 
p4 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.data %>%
              filter(!is.na(Tree_Cover) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode)))  %>% 
              group_by(date) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(x = date, y = Tree_Cover.mean), 
              size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(Tree_Cover) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% 
                group_by(date) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = date), alpha = 0.3) +
  #Do the Formating
scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p4

f2 <- ggarrange(p3, p4, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f2
#Save the data
ggsave(filename = 'Fig21_dieoff_tree_cover_stand_age_time_series_frap_perimeter_50pt_sample_300m.png', height=12, width= 14, units = 'cm', dpi=900)

#Create a Precip time series figure
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(ppt) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode)))  %>% 
              # fire.year %notin% c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003)) %>%
              group_by(date) %>%
              summarize(ppt.mean = mean(ppt), count = n()), 
            mapping = aes(x = date, y = ppt.mean), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(ppt) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% 
                # fire.year %notin% c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003)) %>%
                group_by(date) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Precip (mm yr'^-1*')')) + xlab('Year') 
p5

#Create a water stress time series figure
p6 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(AET) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode)))  %>% 
                       # fire.year %notin% c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003)) %>%
              group_by(date) %>%
              summarize(AET.mean = mean(AET), count = n()), 
            mapping = aes(x = date, y = AET.mean), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(AET) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% 
                         # fire.year %notin% c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003)) %>%
                group_by(date) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year') 
p6

#Create the Soil Moisture Panel
p7 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.data %>%
              filter(!is.na(Soil_Moisture) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode)))  %>% 
              group_by(date) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture), count = n()), 
              mapping = aes(x = date, y = Soil_Moisture.mean), 
            size = 1) + 
  #Soil Moisture 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(Soil_Moisture) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode)))  %>% 
                group_by(date) %>%
                summarize(Soil_Moisture.mean = mean(Soil_Moisture),
                          Soil_Moisture.sd = sd(Soil_Moisture), Soil_Moisture.n = n(), count = n()),
              mapping = aes(ymin=Soil_Moisture.mean - 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            ymax=Soil_Moisture.mean + 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  guides(color = guide_legend(), linetype = guide_legend()) +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Soil Moisture (mm)')) + xlab('Year')
p7

#Create the Water Stress Panel
p8 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.data %>%
              filter(!is.na(Water_Stress) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% 
              group_by(date) %>%
              summarize(Water_Stress.mean = mean(Water_Stress), count = n()), 
            mapping = aes(x = date, y = Water_Stress.mean), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(Water_Stress) & clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% 
                group_by(date) %>%
                summarize(Water_Stress.mean = mean(Water_Stress),
                          Water_Stress.sd = sd(Water_Stress), Water_Stress.n = n(), count = n()),
              mapping = aes(ymin=Water_Stress.mean - 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            ymax=Water_Stress.mean + 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.15, 0.35), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Water Stress (mm)')) + xlab('Year')
p8

f3 <- ggarrange(p5, p6, p7, p8, ncol = 1, nrow = 4, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f3
#Save the data
ggsave(filename = 'Fig22_sierra_sample_water_stress_time_series.png', height=22, width= 16, units = 'cm', dpi=900)

#Creating a fire year dTree plot
p11 <- ggplot(data = pixel.data %>% dplyr::filter(clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% dplyr::group_by(system.index) %>% 
                summarize(dTree = Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015], Water_Stress.4yr = Water_Stress[vi.year == 2015], Tree = Tree_Cover[vi.year == 2015])) +
  geom_point(mapping = aes(x = Water_Stress.4yr, y = dTree), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = Water_Stress.4yr, y = dTree)) +
  stat_cor( mapping = aes(x = Water_Stress.4yr, y = dTree)) +
  theme_bw()
p11
ggsave(filename = 'Fig23_water_stress_stand_age_frap_50pt_sample_300m.png', height=16, width= 18, units = 'cm', dpi=900)

# pixel.data %>% summary()
p12 <- ggplot(data = pixel.data %>% dplyr::filter(clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% dplyr::group_by(system.index) %>% 
                summarize(dTree = Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015], Water_Stress = Water_Stress[vi.year == 2015], SPI48 = SPI48[vi.year == 2015])) +
  geom_point(mapping = aes(x = SPI48, y = dTree), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = SPI48, y = dTree)) +
  stat_cor( mapping = aes(x = SPI48, y = dTree)) +
  theme_bw()
p12

ggsave(filename = 'Fig24_SPI48_stand_age_frap_50pt_sample_300m.png', height=16, width= 18, units = 'cm', dpi=900)

pixel.data$PrET <- pixel.data$ppt - pixel.data$AET
p13 <- ggplot(data = pixel.data %>% dplyr::filter(clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% dplyr::group_by(system.index) %>% 
                summarize(dTree = Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015], Water_Stress = Water_Stress[vi.year == 2015], SPI48 = SPI48[vi.year == 2015], PrET.4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]))) +
  geom_point(mapping = aes(x = PrET.4yr, y = dTree), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = PrET.4yr, y = dTree)) +
  stat_cor(mapping = aes(x = PrET.4yr, y = dTree) ) +
  theme_bw()
p13

ggsave(filename = 'Fig25_PrET4yr_stand_age_frap_50pt_sample_300m.png', height=16, width= 18, units = 'cm', dpi=900)

summary(pixel.data)
#Creating a fire year dTree plot
p14 <- ggplot(data = pixel.data %>% dplyr::filter(clm_precip_sum_mean >= 600 & (fire_year_2010_mode < 1920 | is.na(fire_year_2010_mode))) %>% dplyr::group_by(system.index) %>% 
                summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)], na.rm = TRUE), Water_Stress.4yr = Water_Stress[vi.year == 2015])) +
  geom_point(mapping = aes(x = Water_Stress.4yr, y = tpa_max), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = Water_Stress.4yr, y = tpa_max)) +
  stat_cor( mapping = aes(x = Water_Stress.4yr, y = tpa_max)) +
  theme_bw()
p14
ggsave(filename = 'Fig26_water_stress_ads_dieoff.png', height=16, width= 18, units = 'cm', dpi=900)