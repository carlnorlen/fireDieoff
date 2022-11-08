#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: November 8, 2022
#Purpose: Create figures for EEB GSS presentation

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
# pixel.data <- read.csv(file.path(dir_in, "frapsev_ecoregion_stratified_sample_100pts_30m_ts8_20220713.csv"), header = TRUE, na.strings = "NaN")
sev.data <- read.csv(file.path(dir_in, "fire_south_sierra_USFS_sevfire_100pts_100elev_200precip_5temp_ts8_300m_20221027.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
sev.data$treatment <- 'Disturb'
summary(sev.data)
# list.files(fire_in)
# list.files(fire_in)
control.data <- read.csv(file.path(dir_in, "control_south_sierra_no_FRAP_undist_100pts_100elev_200precip_5temp_ts8_300m_20221025.csv"), header = TRUE, na.strings = "NaN")

#Add Fire Columns
control.data$fire_type_2010 <- -9999
control.data$fire_year_2010 <- -9999
control.data$fire_ID_2010 <- -9999
control.data$fire_count_2010 <- -9999
control.data$fire_type_2019 <- -9999
control.data$fire_year_2019 <- -9999
control.data$fire_ID_2019 <- -9999
control.data$fire_count_2019 <- -9999
control.data$fire_type_2020 <- -9999
control.data$fire_year_2020 <- -9999
control.data$fire_ID_2020 <- -9999
control.data$fire_count_2020 <- -9999

# summary(control.data)
#Get a  of the data
# summary(pixel.data)
# pixel.data <- pixel.data %>% filter(fire.year >= 1919 & !is.na(stand.age) & !is.na(NDMI))
# control.data$fire.year <- control.data$perimeter_year
control.data$treatment <- 'Control' #Try making this 1-km versus, 2-km
# control.data
# fire.data
#Data on 2-km buffers
# control.data.2km <- read.csv(file.path(dir_in, "frapsev_ecoregion_simple_sample_by_2km_wildfire_buffers_10pt_300m_ts8_20220926.csv"), header = TRUE, na.strings = "NaN")
# summary(control.data)
#Get a  of the data
# summary(pixel.data)
# pixel.data <- pixel.data %>% filter(fire.year >= 1919 & !is.na(stand.age) & !is.na(NDMI))
# control.data.2km$fire.year <- control.data.2km$perimeter_year
# control.data.2km$treatment <- '2-km Buffer' #Try making this 1-km versus, 2-km

# summary(fire.data)
# summary(control.data)
#Combine the data together
sev.pixel.data <- rbind(sev.data, control.data)
# pixel.data <- rbind(combine.data, control.data.2km)
summary(sev.pixel.data)

`%notin%` <- Negate(`%in%`)

#Convert data to long format
sev.pixel.data <- sev.pixel.data %>% #dplyr::select(-c('latitude', 'longitude')) %>% 
               pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

sev.pixel.data$year <- as.numeric(sev.pixel.data$year) + 1984 

#Convert missing TPA data to NAs
sev.pixel.data[sev.pixel.data$tpa_max < 0,]$tpa_max <- NA

#Convert fire data -9999 to NAs
sev.pixel.data[sev.pixel.data$fire_type_2010 == -9999,]$fire_type_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_ID_2010 == -9999,]$fire_ID_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_count_2010 == -9999,]$fire_count_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_type_2019 == -9999,]$fire_type_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_ID_2019 == -9999,]$fire_ID_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_count_2019 == -9999,]$fire_count_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_type_2020 == -9999,]$fire_type_2020 <- NA
sev.pixel.data[sev.pixel.data$fire_year_2020 == -9999,]$fire_year_2020 <- NA
sev.pixel.data[sev.pixel.data$fire_ID_2020 == -9999,]$fire_ID_2020 <- NA
sev.pixel.data[sev.pixel.data$fire_count_2020 == -9999,]$fire_count_2020 <- NA

#Convert to trees per hectare
sev.pixel.data$tpa_max <- sev.pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
sev.pixel.data$date <- as.Date(as.character(sev.pixel.data$year), format = '%Y')
# sev.pixel.data$vi.year <- format(sev.pixel.data$date , '%Y')
sev.pixel.data$vi.year <- sev.pixel.data$year
#Use the FRAP fire perimeter year
sev.pixel.data$fire.year <- sev.pixel.data$fire_year_2019
sev.pixel.data$stand.age <- as.numeric(sev.pixel.data$year) - as.numeric(sev.pixel.data$fire.year) 

#Update Cover data to 100% scale
sev.pixel.data$Tree_Cover <- sev.pixel.data$Tree_Cover / 100
sev.pixel.data$Shrub_Cover <- sev.pixel.data$Shrub_Cover / 100
sev.pixel.data$Herb_Cover <- sev.pixel.data$Herb_Cover / 100
sev.pixel.data$Bare_Cover <- sev.pixel.data$Bare_Cover / 100

#Convert the SPI48 scale back to decimal
sev.pixel.data$SPI48 <- sev.pixel.data$SPI48 / 100

#Try to fix soil moisture by dividing by 10
sev.pixel.data$Soil_Moisture <- sev.pixel.data$Soil_Moisture / 10

#Rename ppt and Water Stress
sev.pixel.data$Water_Stress <- sev.pixel.data$Water_Stress
sev.pixel.data$ppt <- sev.pixel.data$ppt
sev.pixel.data$AET <- sev.pixel.data$AET
sev.pixel.data$GPP <- sev.pixel.data$GPP
sev.pixel.data$elevation <- sev.pixel.data$elevation
sev.pixel.data$PrET <- sev.pixel.data$ppt - sev.pixel.data$AET
# 
sev.pixel.data <- sev.pixel.data %>% mutate(fire.year.bin = case_when(
  # bin >= 1 ~ '1900',
  # bin == 2 ~ '1909-1910',
  # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
  is.na(fire.year) ~ 'No Fire',
  # fire.year >= 1910 & fire.year <=  1970 ~ '1910-1970',#'81-95',
  # fire.year >= 1936 & fire.year <= 1950 ~ '65-79',
  # fire.year >= 1951 & fire.year <= 1965 ~ '50-64',
  # fire.year >= 1951 & fire.year <= 1960 ~ '55-64',
  # fire.year >= 1971 & fire.year <= 1980 ~ '1971-1980',#'56-80',
  fire.year >= 1985 & fire.year <= 1990 ~ '1985-1990',
  fire.year >= 1991 & fire.year <= 2000 ~ '1991-2000',#'31-55', 
  # fire.year >= 1991 & fire.year <= 2000 ~ '15-24',
  fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
  # fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2017')) #,
  # fire.year >= 2019 ~ '2019-2020'))#'0-4'))

#Fire Severity Bins
#With re-export type needs to be converted to sev
sev.pixel.data <- sev.pixel.data %>% mutate(sev.bin = case_when(
  fire_type_2019 == '0' | is.na(fire_type_2019) ~ 'No Fire',
  fire_type_2019 == '1' ~ 'Unchanged', 
  fire_type_2019 == '2' ~ 'Low',
  fire_type_2019 == '3' ~ 'Mid',
  fire_type_2019 == '4' ~ 'High',
  fire_type_2019 == '255' ~ 'Masked')) # end function

#Fire year bins for Fire Severity Data
sev.pixel.data$fire.year.bin = with(sev.pixel.data, factor(fire.year.bin, levels = c('2011-2017', '2001-2010', '1991-2000', '1985-1990', 'No Fire')))#c('0-4','5-30','31-55','56-80',

#Make the years bin lables in the correct order
sev.pixel.data$sev.bin = with(sev.pixel.data, factor(sev.bin, levels = c('No Fire','Masked', 'Unchanged', 'Lowest', 'Low','Mid', 'High')))

# summary(pixel.data)
#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills
# 
# summary(pixel.data)
#TEsting for issues
#Count number of pixels by stand age
p1a <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 85, color = 'red') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & vi.year <= 2014 & fire.year <= 2010)) %>%
              group_by(stand.age, treatment) %>%
              # dplyr::select(FIRE_NUM) %>%
              # unique() %>%
              summarize(count = n()), mapping = aes(x = stand.age, y = count, linetype = treatment), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab(expression('# Pixels')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1a

#Get the mean elevation by stand age
p1b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 85, color = 'red') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & vi.year <= 2014 & fire.year <= 2010)) %>%
              group_by(stand.age, treatment) %>%
              summarize(elevation.mean = mean(elevation), elevation.sd = sd(elevation), elevation.n = n()), mapping = aes(x = stand.age, y = elevation.mean, linetype = treatment), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab(expression('Elevation (m)')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1b
# summary(pixel.data)

summary(sev.pixel.data)
# pixel.data %>% filter(!is.na(Shrub_Cover) & vi.year <= 2010 & fire.year <= 2010 & !is.na(fire.year)) %>% 
#   group_by(stand.age) %>% dplyr::select(fire.year) %>% unique() %>% summarize(count = n())
#Count the number of fires by stand age
#There is something weird about the Fire Name column
# p1c <- ggplot() +
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#   geom_vline(xintercept = -10, color = 'red') + 
#   geom_vline(xintercept = 85, color = 'red') +
#   #Create a shrub cover line
#   geom_line(data = pixel.data %>%
#               filter(vi.year <= 2014 & fire.year <= 2010) %>%
#               group_by(stand.age, treatment) %>%
#               dplyr::select(FIRE_NUM) %>% 
#               unique() %>%
#               summarize(fire.year.count = n()), mapping = aes(x = stand.age, y = fire.year.count, linetype = treatment), size = 1) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   ylab(expression('# Fire Perimeters')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
# p1c

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#What is the most common fire year in each stand age?
p1d <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 85, color = 'red') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & vi.year <= 2014 & fire.year <= 2010)) %>%
              group_by(stand.age, treatment) %>%
              summarize(fire_year.mode = find_mode(fire.year)), mapping = aes(x = stand.age, y = fire_year.mode, linetype = treatment), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + ylim(1900, 2010) +
  ylab(expression('Modal Fire Year')) #+ xlab('Years Since Fire') 
p1d

#What is the most common Vi Year in each stand age
p1e <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 85, color = 'red') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & vi.year <= 2014 & fire.year <= 2010)) %>%
              group_by(stand.age, treatment) %>%
              summarize(vi.year.mode = find_mode(vi.year)), mapping = aes(x = stand.age, y = vi.year.mode, linetype = treatment), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + ylim(1985, 2015) +
  ylab(expression('Modal VI Year')) + xlab('Years Since Fire') 
p1e

f1a <- ggarrange(p1a, p1b, p1d, p1e, ncol = 1, nrow = 4, common.legend = TRUE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f1a
#Save the data
ggsave(filename = 'Fig96_data_check_10pt_frap_perimeter_chronosequence.png', height=22, width= 16, units = 'cm', dpi=900)

# Fire Recovery Curve figures
p35 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_point(data = sev.pixel.data %>%
               dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010) #&
                             # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))
               ) %>%
               group_by(date, fire.year, sev.bin) %>%
               summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()), #%>%  
             # filter(if_else(fire.year.bin == '1981-2010', Tree_Cover.n >= 600, Tree_Cover.n >= 0)) %>%
             # group_by(date, fire.year.bin) %>%
             # summarize(Tree_Cover.diff = Tree_Cover.mean[treatment == 'Buffer'] - Tree_Cover.mean[treatment == 'Wildfire']), #%>%
             # group_by(date, fire.year.bin) %>%
             # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
             mapping = aes(x = date, y = Tree_Cover.mean, color = sev.bin), 
             size = 0.1) + 
#Do the Formating
# scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # scale_shape(name = 'Treatment') +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_wrap(.~ fire.year) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + #ylim(25, 55) +
  ylab(expression('Tree (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p35

#Save the data
ggsave(filename = 'Fig97_fire year_tree_cover_frap_perimeter.png', height=18, width= 20, units = 'cm', dpi=900)

#Fire out which strat layers have data for at least on fire year group and No Fires
# group.test <- sev.pixel.data %>%
#   dplyr::filter(((!is.na(Tree_Cover) & fire.year <= 2010) | (!is.na(Tree_Cover) & is.na(fire.year))) #&
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))
#   ) %>%
#   group_by(stratlayer, sev.bin, .drop = FALSE) %>%
#   summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()) #%>% 
#   # filter(fire.year.bin != '2011-2017') #%>%
#   # group_by(stratlayer) %>%
#   # filter(any(count >= 175 & fire.year.bin == 'No Fire') & 
#   #        any(count >= 175 & fire.year.bin =='1985-1990') & any(count >= 175 & fire.year.bin =='1991-2000') & 
#   #        any(count >= 175 & fire.year.bin =='2001-2010'))
# group.test
# #Select the stratlayer that have at least No Fire and One Fire Year Group
# sev.strat.list <- group.test$stratlayer %>% unique()
# 
# 
# # Stratified sample layers
# p36 <- ggplot() + 
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
#   geom_point(data = sev.pixel.data %>%
#                dplyr::filter(((!is.na(Tree_Cover) & fire.year <= 2010 ) | (!is.na(Tree_Cover) & is.na(fire.year) & stratlayer %in% strat.list)) #&
#                              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))
#                ) %>%
#                group_by(date, stratlayer, fire.year.bin) %>%
#                summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()),
#              # filter(if_else(fire.year.bin == '1981-2010', Tree_Cover.n >= 600, Tree_Cover.n >= 0)) %>%
#              # group_by(date, fire.year.bin) %>%
#              # summarize(Tree_Cover.diff = Tree_Cover.mean[treatment == 'Buffer'] - Tree_Cover.mean[treatment == 'Wildfire']), #%>%
#              # group_by(date, fire.year.bin) %>%
#              # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
#              mapping = aes(x = date, y = Tree_Cover.mean, color = fire.year.bin), 
#              size = 0.1) + 
#   #Do the Formating
#   # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
#   # scale_shape(name = 'Treatment') +
#   # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
#   guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
#   theme_dark() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
#             fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + facet_wrap(.~ stratlayer) +
#   xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + #ylim(25, 55) +
#   ylab(expression('Tree (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
# p36
# 
# #Save the data
# ggsave(filename = 'Fig89_fire year_tree_cover_frap_perimeter.png', height=36, width= 36, units = 'cm', dpi=900)


#Elevation limits
# elev.lower <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$elevation %>% quantile(0.0)
# elev.lower
# elev.upper <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$elevation %>% quantile(0.95)
# elev.upper
# ppt.upper <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$clm_precip_sum_mean %>% quantile(1)
# ppt.upper
# ppt.lower <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$clm_precip_sum_mean %>% quantile(0.05)
# ppt.lower
# temp.lower <- (pixel.data %>% filter(fire.year >= 1971 & fire.year <= 1950))$clm_temp_mean_mean %>% quantile(0.0)
# temp.lower
#Figure of mean Cover changes by stand age
#Tyring out adding elevation lower bound...
p2 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(sev.bin)) %>% #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                       # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire perimeter and fier year by pixel match 
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub'), size = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = sev.pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(sev.bin)) %>% #& #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                         # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, sev.bin) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, fill = "Shrub"), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(sev.bin)) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                       # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree'), size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(sev.bin)) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                         # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, sev.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, fill = "Tree"), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(sev.bin)) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                       # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb'), size = 1) + 
  #Herb Cover 95% CI
  geom_ribbon(data = sev.pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(sev.bin)) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                         # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, sev.bin) %>%
                summarize(Herb_Cover.mean = mean(Herb_Cover),
                          Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
              mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            x = stand.age, fill = "Herb"), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = sev.pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(sev.bin)) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                       # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
              group_by(stand.age, treatment, sev.bin) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare'), size = 1) + 
  #Bare Cover 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter(stand.age >= -10 & stand.age <= 12 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(sev.bin)) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
                         # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower & #Filter to make the later fires for similar to the earlier fires
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
                group_by(stand.age, treatment, sev.bin) %>%
                summarize(Bare_Cover.mean = mean(Bare_Cover),
                          Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
              mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
  x = stand.age, fill = "Bare"), alpha = 0.3) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") + facet_grid(.~sev.bin) +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire')
p2

#Save the data
ggsave(filename = 'Fig98_veg_cover_stand_age_10pt_120m_frap_perimeter.png', height=18, width= 20, units = 'cm', dpi=900)

#Create a manual color scale
# cols.1 <- c("Shrub"="green","Tree"="forest green", "Woody" = "brown")
# fills.1 <- c("Shrub"="green","Tree"="forest green", "Woody" ="brown")
# fills.1

# veg.diff <- sev.pixel.data %>%
#   dplyr::filter(((!is.na(Tree_Cover) & fire.year <= 2010 ) | (!is.na(Tree_Cover) & is.na(fire.year) )) #&
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))
#   ) %>%
#   group_by(stratlayer, vi.year) %>%
#   summarize(Tree_Cover.diff = mean(Tree_Cover[treatment == 'Disturb']) - mean(Tree_Cover[treatment == 'Control']), count = n())
# veg.diff
#Do just the tree cover line, try to remove the control bins
# p2a <- ggplot() +
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#   #Create a shrub cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 ) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#                        # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#                        # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#               group_by(stand.age, treatment) %>%
#               summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub', linetype = treatment), size = 1) +
#   #Shrub Cover 95% CI
#   geom_ribbon(data = pixel.data %>%
#                 filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#                          # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#                          # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                 group_by(stand.age, treatment) %>%
#                 summarize(Shrub_Cover.mean = mean(Shrub_Cover),
#                           Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
#               mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
#                             ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
#                             x = stand.age, fill = "Shrub", linetype = treatment), alpha = 0.3) +
#   #Create a Tree Cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#               group_by(stand.age, treatment) %>%
#               summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree', linetype = treatment), size = 1) +
#   #Tree Cover 95% CI
#   geom_ribbon(data = pixel.data %>%
#                 filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#                 group_by(stand.age, treatment) %>%
#                 summarize(Tree_Cover.mean = mean(Tree_Cover),
#                           Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
#               mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
#                             ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
#                             x = stand.age, fill = "Tree", linetype = treatment), alpha = 0.3) +
#   #Create a Woody Cover line
#   # geom_line(data = pixel.data %>%
#   #             filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#   #             # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#   #             # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#   #             group_by(stand.age, treatment) %>%
#   #             summarize(Woody_Cover.mean = mean(Tree_Cover + Shrub_Cover)), mapping = aes(x = stand.age, y = Woody_Cover.mean, color = 'Woody', linetype = treatment), size = 1) +
#   # #Tree Cover 95% CI
#   # geom_ribbon(data = pixel.data %>%
#   #               filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#   #               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#   #               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#   #               group_by(stand.age, treatment) %>%
#   #               summarize(Woody_Cover.mean = mean(Tree_Cover + Shrub_Cover),
#   #                         Woody_Cover.sd = sd(Tree_Cover + Shrub_Cover), Woody_Cover.n = n()),
#   #             mapping = aes(ymin=Woody_Cover.mean - 1.96*(Woody_Cover.sd / sqrt(Woody_Cover.n)),
#   #                           ymax=Woody_Cover.mean + 1.96*(Woody_Cover.sd / sqrt(Woody_Cover.n)),
#   #                           x = stand.age, fill = "Woody", linetype = treatment), alpha = 0.3) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   scale_colour_manual(name="Vegetation Type",values=cols.1, aesthetics = 'color') +
#   scale_fill_manual(values = fills.1) +
#   guides(fill = "none") + #ylim(20,85) +
#   ylab(expression('Cover (%)')) + xlab('Years Since Fire')
# p2a
# 
# #Save the data
# ggsave(filename = 'Fig60a_veg_cover_stand_age_10pt_300m_frap_perimeter.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Do split by fire year bins
# p2b <- ggplot() + 
#   # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
#   #Create a shrub cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire
#               group_by(stand.age, treatment, fire.year.bin) %>%
#               summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub'), size = 1) +
#   #Shrub Cover 95% CI
#   geom_ribbon(data = pixel.data %>% 
#                 filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire             group_by(stand.age, treatment, fire.year.bin) %>%
#                 group_by(stand.age, treatment, fire.year.bin) %>%
#                 summarize(Shrub_Cover.mean = mean(Shrub_Cover),
#                           Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
#               mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
#                             ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
#                             x = stand.age, fill = "Shrub"), alpha = 0.3) +
#   #Create a Tree Cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire             group_by(stand.age, treatment, fire.year.bin) %>%
#               group_by(stand.age, treatment, fire.year.bin) %>%
#               summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree'), size = 1) + 
#   #Tree Cover 95% CI
#   geom_ribbon(data = pixel.data %>% 
#                 filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire             group_by(stand.age, treatment, fire.year.bin) %>%
#                 group_by(stand.age, treatment, fire.year.bin) %>%
#                  summarize(Tree_Cover.mean = mean(Tree_Cover),
#                           Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
#               mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
#                             ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
#                             x = stand.age, fill = "Tree"), alpha = 0.3) +
#   #Create an Herb cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire              group_by(stand.age, treatment, fire.year.bin) %>%
#               group_by(stand.age, treatment, fire.year.bin) %>%
#               summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb'), size = 1) + 
#   #Herb Cover 95% CI
#   geom_ribbon(data = pixel.data %>% 
#                 filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire             group_by(stand.age, treatment, fire.year.bin) %>%
#                 group_by(stand.age, treatment, fire.year.bin) %>%
#                 summarize(Herb_Cover.mean = mean(Herb_Cover),
#                           Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
#               mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
#                             ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
#                             x = stand.age, fill = "Herb"), alpha = 0.3) +
#   #Create a Bare cover line
#   geom_line(data = pixel.data %>%
#               filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#               # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire             group_by(stand.age, treatment, fire.year.bin) %>%
#               group_by(stand.age, treatment, fire.year.bin) %>%
#               summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare'), size = 1) + 
#   #Bare Cover 95% CI
#   geom_ribbon(data = pixel.data %>%
#                 filter(stand.age >= -10 & stand.age <= 85 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010) %>% # & #& elevation >= elev.lower & clm_temp_mean_mean >= temp.lower & clm_precip_sum_mean <= ppt.upper
#                 # elevation <= elev.upper &  clm_precip_sum_mean >= ppt.lower & #Filter to make the later fires for similar to the earlier fires
#                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% #Only include places where the fire               group_by(stand.age, treatment, fire.year.bin) %>%
#                 group_by(stand.age, treatment, fire.year.bin) %>%
#                  summarize(Bare_Cover.mean = mean(Bare_Cover),
#                           Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
#               mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
#                             ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
#                             x = stand.age, fill = "Bare"), alpha = 0.3) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
#   scale_fill_manual(values = fills) + 
#   guides(fill = "none") +
#   ylab(expression('Cover (%)')) + xlab('Years Since Fire') + facet_grid(treatment ~ fire.year.bin)
# p2b
# 
# #Save the data
# ggsave(filename = 'Fig60b_veg_cover_stand_age_10pt_300m_frap_perimeter.png', height=18, width= 20, units = 'cm', dpi=900)

#Checking why there is a dip around 2002
p9 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              filter(!is.na(Tree_Cover) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) %>% # &
                       # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(count = n()), mapping = aes(x = date, y = count, color = sev.bin, linetype = sev.bin),
            size = 1
  ) +
  #Do the Formating
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_wrap(. ~ treatment) +
  ylab('Count') + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p9

p10 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              filter(!is.na(Tree_Cover) & fire.year <= 2010 & stand.age > 2  & !is.na(sev.bin)) %>% # &
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
              group_by(date, sev.bin, treatment) %>%
              summarize(stand.age.mean = mean(stand.age)), mapping = aes(x = date, y = stand.age.mean, color = sev.bin, linetype = sev.bin),
            size = 1
  ) +
  #Do the Formating
  scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Fire Years') +
  scale_linetype(name = 'Fire Years') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x =element_text(size = 10), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_wrap(. ~ treatment) +
  ylab('Stand Age') + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p10

f4 <- ggarrange(p9, p10, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f4
# 
ggsave(filename = 'Fig99_data_check_time_series_perimeter_10pt_300m.png', height=16, width= 16, units = 'cm', dpi=900)

# summary(pixel.data)
#Figure of Dead Trees per acre separated by fire years with time series
p3 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              filter((!is.na(tpa_max) & treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2  & !is.na(sev.bin)) | (!is.na(tpa_max) & treatment == 'Control' & is.na(fire.year)  & !is.na(sev.bin))) %>% # & 
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), # %>%
              # filter(if_else(sev.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mean), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter((!is.na(tpa_max) & treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2  & !is.na(sev.bin)) | (!is.na(tpa_max) & treatment == 'Control' & is.na(fire.year)  & !is.na(sev.bin))) %>% # &
                         # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower &
                         # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                group_by(date, sev.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), #%>%
                # filter(if_else(sev.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date), alpha = 0.3) +
  #Do the Formating
  # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # scale_linetype(name = 'Treatment') +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  # guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ sev.bin) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p3

#Create the 
p4 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2  & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year)  & !is.na(sev.bin))) %>% # &
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()) %>%  
              filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                               sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(x = date, y = Tree_Cover.mean), 
              size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # &
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), count = n()) %>%  
                filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                                 sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(count)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(count)),
                            x = date), alpha = 0.3) +
  #Do the Formating
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ sev.bin) + #ylim(20, 50) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p4

f2 <- ggarrange(p3, p4, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f2
#Save the data
ggsave(filename = 'Fig100_dieoff_tree_cover_stand_age_time_series_frap_perimeter_10pt_sample_300m.png', height=12, width= 14, units = 'cm', dpi=900)

#Create a Precip time series figure
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # &
                       # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                       # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin) %>%
              summarize(ppt.mean = mean(ppt), ppt.n = n(), count = n()) %>%  
              filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                               sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = ppt.mean), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # &
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()) %>%  
                filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                                 sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ sev.bin) +
  ylab(expression('Precip (mm yr'^-1*')')) + xlab('Year') 
p5

#Create a water stress time series figure
p6 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # &
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin) %>%
              summarize(AET.mean = mean(AET), AET.n = n(), count = n()) %>%  
              filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                               sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = AET.mean), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # &
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()) %>%  
                filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                                 sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + ylim(250, 600) + 
  facet_grid(. ~ sev.bin) +
  ylab(expression('AET (mm yr'^-1*')')) + xlab('Year') 
p6

#Create the Soil Moisture Panel
p7 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # &
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture), Soil_Moisture.n = n(), count = n()) %>%  
              filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                               sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(x = date, y = Soil_Moisture.mean), 
            size = 1) + 
  #Soil Moisture 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # &
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin) %>%
                summarize(Soil_Moisture.mean = mean(Soil_Moisture),
                          Soil_Moisture.sd = sd(Soil_Moisture), Soil_Moisture.n = n(), count = n()) %>%  
                filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                                 sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=Soil_Moisture.mean - 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            ymax=Soil_Moisture.mean + 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  guides(color = guide_legend(), linetype = guide_legend()) +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ sev.bin) +
  ylab(expression('Soil Moisture (mm)')) + xlab('Year')
p7

#Create the Water Stress Panel
p8 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = sev.pixel.data %>%
              filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # & 
              # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
              group_by(date, sev.bin) %>%
              summarize(Water_Stress.mean = mean(Water_Stress), Water_Stress.n = n(), count = n()) %>%  
              filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                               sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
            mapping = aes(x = date, y = Water_Stress.mean), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = sev.pixel.data %>%
                filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (treatment == 'Control' & is.na(fire.year) & !is.na(sev.bin))) %>% # & 
                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
                group_by(date, sev.bin) %>%
                summarize(Water_Stress.mean = mean(Water_Stress),
                          Water_Stress.sd = sd(Water_Stress), Water_Stress.n = n(), count = n()) %>%  
                filter(case_when(sev.bin == 'Unchanged' ~ count >= 500, sev.bin == 'Low' ~ count >= 1300, sev.bin == 'Mid' ~ count >= 1000,
                                 sev.bin == 'High' ~ count >= 1250, sev.bin == 'No Fire' ~ count >= 0)),
              mapping = aes(ymin=Water_Stress.mean - 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            ymax=Water_Stress.mean + 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            x = date), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  scale_linetype(name = 'Treatment') +
  scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Treatment') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.15, 0.35), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + facet_grid(. ~ sev.bin) +
  ylab(expression('Water Stress (mm)')) + xlab('Year')
p8

f3 <- ggarrange(p5, p6, p7, p8, ncol = 1, nrow = 4, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f3
#Save the data
ggsave(filename = 'Fig101_water_stress_stand_age_frap_perimeter_10pt_sample_300m_time_series.png', height=22, width= 16, units = 'cm', dpi=900)

#Add another fire year grouping
# pixel.data <- pixel.data %>% mutate(fire.year.grp = case_when(
#   # bin >= 1 ~ '1900',
#   # bin == 2 ~ '1909-1910',
#   # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
#   is.na(fire.year) ~ 'No Fire',
#   fire.year <=  1920 ~ '1900-1920',#'81-95',
#   fire.year >= 1971 & fire.year <= 1965 ~ '1971-1965',
#   # fire.year >= 1931 & fire.year <= 1940 ~ '1931-1940',
#   # fire.year >= 1941 & fire.year <= 1950 ~ '1941-1950',
#   # fire.year >= 1951 & fire.year <= 1980 ~ '1951-1980',#'56-80',
#   # fire.year >= 1961 & fire.year <= 1970 ~ '1961-1970',
#   # fire.year >= 1971 & fire.year <= 1980 ~ '1971-1980',
#   fire.year >= 1966 & fire.year <= 2010 ~ '1966-2010',
#   # fire.year >= 1991 & fire.year <= 2000 ~ '1991-2000',
#   # fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
#   fire.year >= 2011 & fire.year <= 2020 ~ '2011-2020'))#,
# #fire.year >= 2019 ~ '2019-2020'))#'0-4'))
# 
# pixel.data$fire.year.grp = with(pixel.data, factor(fire.year.grp, levels = c('2011-2020', '1966-2010', '1971-1965', '1900-1920', 'No Fire')))#c('0-4','5-30','31-55','56-80',

# pixel.data %>% filter(FIRE_NAME != "") %>% group_by(FIRE_NAME, treatment)
# pixel.data %>% 
#   dplyr::filter(fire.year <= 2010 & stand.age > 2 & 
#                   elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & 
#                   if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>% 
#                   dplyr::group_by(system.index, treatment) %>% 
#        summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
#             fire.year.grp = fire.year.grp[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]) %>%
#                   summary()

#Tree Cover Die-off
p14 <- ggplot() +
       #Data Summary
       geom_point(data = sev.pixel.data %>% 
                    filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
                             # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                             # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                    dplyr::group_by(system.index, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = dTree), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
       geom_errorbar(data = sev.pixel.data %>% 
                       filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # & 
                                     # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                                     # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                       dplyr::group_by(system.index, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.8, 0.75), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
       xlab('Stand Age (10-year Bins)') + ylab('dTree (%)')
p14

#RdTree Plot
p15 <- ggplot() +
  #Data Summary
  geom_point(data = sev.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = RdTree * 100), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
  geom_errorbar(data = sev.pixel.data %>% 
                  filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin) %>% 
                  summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = sev.bin, y = RdTree * 100), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Stand Age (10-year Bins)') + ylab('Relative dTree (%)')
p15

#ADS die-off
p16 <- ggplot() +
  geom_point(data = sev.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = tpa_max), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
  geom_errorbar(data = sev.pixel.data %>% 
                  filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = tpa_max), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Mortality (trees/ha)') 
p16

# ggsave(filename = 'Fig10_ADS_mortality_stand_age_wildfire_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)

#Pre-Die-off Tree Cover
p17 <- ggplot() +
  geom_point(data = sev.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin) %>%
               summarize(sev.bin = sev.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2014, 2015)])),
             mapping = aes(x = sev.bin, y = Tree_Cover), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
  geom_errorbar(data = sev.pixel.data %>% 
                  filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin) %>%
                  summarize(sev.bin = sev.bin[vi.year == 2010], Tree_Cover = mean(Tree_Cover[vi.year %in% c(2014, 2015)])),
                mapping = aes(x = sev.bin, y = Tree_Cover), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab('Tree Cover (%)')
p17

#Water Stress
p18 <- ggplot() +
  geom_point(data = sev.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), sev.bin = sev.bin[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
             mapping = aes(x = sev.bin, y = Water_Stress), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
  geom_errorbar(data = sev.pixel.data %>% 
                  filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% # &
                  # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
                  # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                  dplyr::group_by(system.index, sev.bin) %>%
                  summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), sev.bin = sev.bin[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
                mapping = aes(x = sev.bin, y = Water_Stress), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
                axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
                legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
                legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Years Since Fire') + ylab('Water Stress (mm)') 
p18

#Pr-ET 4yr
# p19 <- ggplot() +
#   geom_point(data = pixel.data %>% 
#                filter((treatment == 'Disturb' & fire.year <= 2010 & stand.age > 2) | (treatment == 'Control' & is.na(fire.year))) %>% # &
#                # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                dplyr::group_by(system.index, treatment) %>%
#                summarize(PrET.4yr = sum(PrET[vi.year %in% c(2012, 2013, 2014, 2015)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
#              mapping = aes(x = fire.year.bin, y = PrET.4yr, color = treatment), stat = 'summary', size = 2, position = position_dodge(width = 1)) + 
#   geom_errorbar(data = pixel.data %>% 
#                   dplyr::filter(fire.year <= 2010 & stand.age > 2) %>% # & 
#                   # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
#                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
#                   dplyr::group_by(system.index, treatment) %>%
#                   summarize(PrET.4yr = sum(PrET[vi.year %in% c(2012, 2013, 2014, 2015)], na.rm = TRUE), fire.year.bin = fire.year.bin[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]),
#                 mapping = aes(x = fire.year.bin, y = PrET.4yr, color = treatment), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
#   scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
#   theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
#         legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
#         legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
#   xlab('Years Since Fire') + ylab('Pr-ET four-year (mm/4yr)')
# p19

#Combine the Panels
f5 <- ggarrange(p14, p15, p16, p17, p18,  ncol = 1, nrow = 5, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)', 'e)'))
f5

ggsave(filename = 'Fig102_stand_age_bins_dieoff_treecover_water_stress_10pt_300m.png', height=24, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

# pixel.data %>% group_by(treatment, system.index, fire.year.grp) %>% dplyr::select(elevation, clm_precip_sum_mean, clm_temp_mean_mean) 

#Data Distribution
p20 <- ggplot() +
geom_histogram(data = sev.pixel.data %>% dplyr::filter((fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% #&
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
             dplyr::group_by(system.index, treatment) %>%
             summarize(elevation = elevation[vi.year == 2010] , clm_precip = clm_precip_sum[vi.year == 2010], sev.bin = sev.bin[vi.year == 2010], clm_temp = clm_temp_mean[vi.year == 2015]),
           mapping = aes(y = elevation, fill = treatment), position = position_dodge()) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.15, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_wrap(. ~ sev.bin) +
  xlab('Fire Years') + ylab('Elevation (m)')
p20

p21 <- ggplot() +
  geom_histogram(data = sev.pixel.data %>% dplyr::filter((fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% #&
                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, treatment) %>%
                 summarize(elevation = elevation[vi.year == 2010] , clm_precip = clm_precip_sum[vi.year == 2010], sev.bin = sev.bin[vi.year == 2010], clm_temp = clm_temp_mean[vi.year == 2015]),
               mapping = aes(y = clm_precip, fill = treatment), position = position_dodge()) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.15, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_wrap(. ~ sev.bin) +
  xlab('Fire Years') + ylab('Precip Climatology (mm/yr)')
p21

p22 <- ggplot() +
  geom_histogram(data = sev.pixel.data %>% dplyr::filter((fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% #&
                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, treatment) %>%
                 summarize(elevation = elevation[vi.year == 2010] , clm_precip = clm_precip_sum[vi.year == 2010], sev.bin = sev.bin[vi.year == 2010], clm_temp = clm_temp_mean[vi.year == 2015]),
               mapping = aes(y = clm_temp, fill = treatment), position = position_dodge()) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.15, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_wrap(. ~ sev.bin) +
  xlab('Fire Years') + ylab('Temp Climatology (C)')
p22

p23 <- ggplot() +
  geom_histogram(data = sev.pixel.data %>% 
                   dplyr::filter((fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(sev.bin))) %>% #&
                   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
                 dplyr::group_by(system.index, treatment) %>%
                 summarize(latitude = latitude[vi.year == 2010] , clm_precip = clm_precip_sum[vi.year == 2010], sev.bin = sev.bin[vi.year == 2010], clm_temp = clm_temp_mean[vi.year == 2015]),
               mapping = aes(y = latitude, fill = treatment), position = position_dodge()) +
  theme_bw() +
  scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.15, 0.2), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_wrap(. ~ sev.bin) +
  xlab('Count') + ylab('Latitude')
p23

f6 <- ggarrange(p20, p21, p22, p23, ncol = 1, nrow = 4, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f6

ggsave(filename = 'Fig103_fire_sample_characteristics_300m.png', height=28, width = 18, units = 'cm', dpi=900)
# summary(pixel.data)

# summary(sev.pixel.data)
#Figure of Dead Trees per acre separated by fire years with time series
#Create a manual color scale
cols.1 <- c("Unchanged"="black","Low"="gray", "Mid" = "white", "High" = "dark gray")
lines.1 <- c("Unchanged"="solid","Low"="dashed", "Mid" = "dotted", "High" = "dotdash")
#ADS Difference
p24 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = sev.pixel.data %>%
              dplyr::filter((!is.na(tpa_max) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | ( !is.na(tpa_max) &is.na(fire.year) & !is.na(sev.bin))) %>% # &
                              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%    
              group_by(date, sev.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()) %>% 
              # ungroup() %>%
              group_by(date) %>%
              summarize(tpa_max.unch = tpa_max.mean[sev.bin == 'Unchanged'] - tpa_max.mean[sev.bin == 'No Fire']), #%>%
              # group_by(date, sev.bin) %>%
              # summarize(tpa_max.diff.mean = mean(tpa_max.diff)),
            # filter(if_else(sev.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.unch, color = 'Unchanged', linetype = 'Unchanged'), 
            size = 1
  ) +
  #Do the subtraction versus 1991-2000  
  geom_line(data = sev.pixel.data %>%
            dplyr::filter((!is.na(tpa_max) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | ( !is.na(tpa_max) &is.na(fire.year) & !is.na(sev.bin))) %>% # &
            group_by(date, sev.bin) %>%
            summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()) %>% 
            group_by(date) %>%
            summarize(tpa_max.low = tpa_max.mean[sev.bin == 'Low'] - tpa_max.mean[sev.bin == 'No Fire']), #%>%
          mapping = aes(x = date, y = tpa_max.low, color = 'Low', linetype = 'Low'), 
          size = 1
  ) +
  #Do subtraction versus 1981-1990
  geom_line(data = sev.pixel.data %>%
              dplyr::filter((!is.na(tpa_max) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | ( !is.na(tpa_max) &is.na(fire.year) & !is.na(sev.bin))) %>% # &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%    
              group_by(date, sev.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()) %>% 
              # ungroup() %>%
              group_by(date) %>%
              summarize(tpa_max.mid = tpa_max.mean[sev.bin == 'Mid'] - tpa_max.mean[sev.bin == 'No Fire']), #%>%
            # group_by(date, sev.bin) %>%
            # summarize(tpa_max.diff.mean = mean(tpa_max.diff)),
            # filter(if_else(sev.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mid, color = 'Mid', linetype = 'Mid'), 
            size = 1
  ) +
  #Do subtraction versus 1971-1980
  geom_line(data = sev.pixel.data %>%
              dplyr::filter((!is.na(tpa_max) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | ( !is.na(tpa_max) &is.na(fire.year) & !is.na(sev.bin))) %>% # &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%    
              group_by(date, sev.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()) %>% 
              # ungroup() %>%
              group_by(date) %>%
              summarize(tpa_max.1980 = tpa_max.mean[sev.bin == 'High'] - tpa_max.mean[sev.bin == 'No Fire']), #%>%
            # group_by(date, fire.year.bin) %>%
            # summarize(tpa_max.diff.mean = mean(tpa_max.diff)),
            # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.1980, color = 'High', linetype = 'High'), 
            size = 1
  ) +
  #Do the Formating
  # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  scale_colour_manual(name="Fire Severity",values=cols.1, aesthetics = 'color') +
  scale_linetype_manual(name = 'Fire Severity', values = lines.1) +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) +
  ylab(expression(atop('ADS Die-off (Fire - Control)', '(trees ha'^-1*')'))) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p24

# sev.pixel.data %>%
#   dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & stand.age > 2 ) | (!is.na(Tree_Cover) & is.na(fire.year) )) %>% # &
#   # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
#   group_by(date, fire.year.bin) %>%
#   summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
#   filter(if_else(fire.year.bin == '2001-2010', Tree_Cover.n >= 1700, Tree_Cover.n >= 0)) %>%
#   group_by(date) %>%
#   summarize(Tree_Cover.2010 = Tree_Cover.mean[fire.year.bin == '2001-2010'] - Tree_Cover.mean[fire.year.bin == 'No Fire'])

#Create the
p25 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  #2001-2010 Tree Cover Difference
  geom_line(data = sev.pixel.data %>%
              dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (!is.na(Tree_Cover) & is.na(fire.year) & !is.na(sev.bin))) %>% # &
              # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
              group_by(date, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(sev.bin == 'Unchanged', Tree_Cover.n >= 500, Tree_Cover.n >= 0)) %>%
              group_by(date) %>%
              summarize(Tree_Cover.unch = Tree_Cover.mean[sev.bin == 'Unchanged'] - Tree_Cover.mean[sev.bin == 'No Fire']), #%>%
              # group_by(date, sev.bin) %>%
              # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
            mapping = aes(x = date, y = Tree_Cover.unch, color = 'Unchanged', linetype = 'Unchanged'), 
            size = 1) + 
  #1991-2000 tree cover difference
  geom_line(data = sev.pixel.data %>%
              dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (!is.na(Tree_Cover) & is.na(fire.year) & !is.na(sev.bin))) %>% # &
              # if_else(sev.bin == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
              group_by(date, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(sev.bin == 'Low', Tree_Cover.n >= 1300, Tree_Cover.n >= 0)) %>%
              group_by(date) %>%
              summarize(Tree_Cover.low = Tree_Cover.mean[sev.bin == 'Low'] - Tree_Cover.mean[sev.bin == 'No Fire']), #%>%
            # group_by(date, sev.bin) %>%
            # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
            mapping = aes(x = date, y = Tree_Cover.low, color = 'Low', linetype = 'Low'), 
            size = 1) + 
  #1981-1990 tree cover difference
  geom_line(data = sev.pixel.data %>%
              dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (!is.na(Tree_Cover) & is.na(fire.year) & !is.na(sev.bin))) %>% # &
              # if_else(sev.bin == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
              group_by(date, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(sev.bin == 'Mid', Tree_Cover.n >= 1000, Tree_Cover.n >= 0)) %>%
              group_by(date) %>%
              summarize(Tree_Cover.mid = Tree_Cover.mean[sev.bin == 'Mid'] - Tree_Cover.mean[sev.bin == 'No Fire']), #%>%
            # group_by(date, sev.bin) %>%
            # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
            mapping = aes(x = date, y = Tree_Cover.mid, color = 'Mid', linetype = 'Mid'), 
            size = 1) + 
  #1971-1980 tree cover difference
  geom_line(data = sev.pixel.data %>%
              dplyr::filter((!is.na(Tree_Cover) & fire.year <= 2010 & stand.age > 2 & !is.na(sev.bin)) | (!is.na(Tree_Cover) & is.na(fire.year) & !is.na(sev.bin))) %>% # &
              # if_else(sev.bin == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%  
              group_by(date, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(sev.bin == 'High', Tree_Cover.n >= 1250, Tree_Cover.n >= 0)) %>%
              group_by(date) %>%
              summarize(Tree_Cover.high = Tree_Cover.mean[sev.bin == 'High'] - Tree_Cover.mean[sev.bin == 'No Fire']), #%>%
            # group_by(date, sev.bin) %>%
            # summarize(Tree_Cover.diff.mean = mean(Tree_Cover.diff)),
            mapping = aes(x = date, y = Tree_Cover.high, color = 'High', linetype = 'High'), 
            size = 1) +
  #Do the Formating
  # scale_color_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  scale_colour_manual(name="Fire Years",values=cols.1, aesthetics = 'color') +
  scale_linetype_manual(name = 'Fire Years', values = lines.1) +
  # scale_fill_brewer(type = 'div', palette = 'Spectral', name = 'Fire Year') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) + #facet_grid(. ~ fire.year.bin) + #ylim(25, 55) +
  ylab(expression('dTree (Wildfire - Control)')) + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p25

#Adding the Water_Stress difference could also be helpful

f7 <- ggarrange(p24, p25, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f7
#Save the data
ggsave(filename = 'Fig104_dieoff_tree_cover_stand_age_time_series_frap_perimeter_10pt_sample_300m.png', height=12, width= 14, units = 'cm', dpi=900)

#Tree Cover versus Elevation versus Latitude
p26 <- ggplot() +
  #Data Summary
  geom_bin2d(data = sev.pixel.data %>% 
               filter((Tree_Cover >= 0 & treatment == 'Disturb' & fire.year <= 2010 & !is.na(sev.bin)) | (Tree_Cover >= 0 & is.na(fire.year) & !is.na(sev.bin))) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2013, 2014)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = latitude, y = elevation, fill = Tree_Cover, group = Tree_Cover), binwidth = c(0.1, 200)) + 
  # geom_errorbar(data = pixel.data %>% 
  #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & stratlayer %in% strat.list) | (is.na(fire.year) & stratlayer %in% strat.list)) %>% # & 
  #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
  #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
  #                 dplyr::group_by(system.index, fire.year.bin) %>% 
  #                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
  #                           Water_Stress = Water_Stress[vi.year == 2015]),
  #               mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + 
  theme_bw() +
  scale_fill_gradient(name = "Tree Cover (%)", limits = c(0, 100), low = "brown", high = "forest green", na.value = 'transparent') +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ sev.bin) +
  ylab('Elevation (m)')
p26

# pixel.data %>% summary()
p29<- ggplot() +
  #Data Summary
  geom_bin2d(data = sev.pixel.data %>% 
               filter((!is.na(Tree_Cover) & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & !is.na(sev.bin)) | (!is.na(Tree_Cover) & is.na(fire.year) & !is.na(sev.bin))) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2017, 2018)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = latitude, y = elevation, fill = dTree, group = dTree), binwidth = c(0.1, 200)) + 
  # geom_errorbar(data = pixel.data %>% 
  #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & stratlayer %in% strat.list) | (is.na(fire.year) & stratlayer %in% strat.list)) %>% # & 
  #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
  #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
  #                 dplyr::group_by(system.index, fire.year.bin) %>% 
  #                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
  #                           Water_Stress = Water_Stress[vi.year == 2015]),
  #               mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + 
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_gradient2(name = "Die-off \n(% Tree Cover)", low = "firebrick1", mid = "lightgoldenrodyellow", high = "dodgerblue", limits = c(-10, 5), midpoint = 0, na.value = 'transparent') +  #  
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ sev.bin) +
  ylab('Elevation (m)')
p29

#ADS die-off
p30 <- ggplot() +
  geom_bin2d(data = sev.pixel.data %>% 
               filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & !is.na(tpa_max) & !is.na(sev.bin)) | (is.na(fire.year) & !is.na(tpa_max) & !is.na(sev.bin))) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015], elevation = elevation[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015], SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = latitude, y = elevation, fill = tpa_max, group = tpa_max), binwidth = c(0.1, 200)) + 
  # geom_errorbar(data = pixel.data %>% 
  #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & !is.na(tpa_max)) | (is.na(fire.year) & !is.na(tpa_max))) %>% # &
  #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
  #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
  #                 dplyr::group_by(system.index, fire.year.bin) %>%
  #                 summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), SPI48 = SPI48[vi.year == 2015]),
  #               mapping = aes(x = fire.year.bin, y = tpa_max), stat = 'summary', position = position_dodge(width = 1)) + theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  scale_fill_gradient(name = "Die-off \n(trees per hectare)", low = "white", high = "red", na.value = 'transparent') +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), legend.position = "right",
        axis.title.x = element_blank(), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(. ~ sev.bin) +
  ylab('Elevation (m)')
p30             

p31<- ggplot() +
  #Data Summary
  geom_bin2d(data = sev.pixel.data %>% 
               filter((Tree_Cover >= 0 & treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1971 & !is.na(sev.bin)) | (Tree_Cover >= 0 & is.na(fire.year) & !is.na(sev.bin))) %>% # &
               # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
               # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
               dplyr::group_by(system.index, sev.bin) %>% 
               summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
                         Water_Stress = Water_Stress[vi.year == 2015], Tree_Cover = (mean(Tree_Cover[vi.year %in% c(2018, 2019)])), elevation = elevation[vi.year == 2015], clm_precip_sum = clm_precip_sum[vi.year == 2015],
                         latitude = latitude[vi.year == 2015], count = sum(elevation[vi.year == 2015]), n = n(), SPI48 = SPI48[vi.year == 2015]), # filter for drought areas
             mapping = aes(x = latitude, y = elevation), binwidth = c(0.1, 200)) + 
  # geom_errorbar(data = pixel.data %>% 
  #                 filter((treatment == 'Disturb' & fire.year <= 2010 & fire.year >= 1921 & stand.age > 2 & stratlayer %in% strat.list) | (is.na(fire.year) & stratlayer %in% strat.list)) %>% # & 
  #                 # elevation <= elev.upper & clm_precip_sum_mean >= ppt.lower & #elevation >= elev.lower &
  #                 # if_else(treatment == 'Wildfire', fire.year == fire_year_2019_mode, is.na(fire_year_2019_mode))) %>%
  #                 dplyr::group_by(system.index, fire.year.bin) %>% 
  #                 summarize(dTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])), RdTree = (mean(Tree_Cover[vi.year %in% c(2018, 2019)]) - mean(Tree_Cover[vi.year %in% c(2014,2015)])) / mean(Tree_Cover[vi.year %in% c(2014, 2015)]), 
  #                           Water_Stress = Water_Stress[vi.year == 2015]),
  #               mapping = aes(x = fire.year.bin, y = dTree), stat = 'summary', position = position_dodge(width = 1)) + 
  theme_bw() +
  # scale_color_brewer(type = 'div', palette = 'Set1', name = 'Treatment') +
  # scale_fill_gradient2(name = "Die-off (% Tree Cover)", limits = c(-50, 20), midpoint = 0, low = "red", mid = "white", high = "blue", na.value = 'transparent') +
  theme(axis.text.y = element_text(size = 8), legend.position = "right", axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + facet_grid(.~ sev.bin) +
  xlab('Latitude') + ylab('Elevation (m)')
p31

f8 <- ggarrange(p26, p29, p31, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f8
#Save the data
ggsave(filename = 'Fig109_sev_fire_dieoff_tree_cover_fireyear_geographic_distribution.png', height=24, width= 24, units = 'cm', dpi=900)
