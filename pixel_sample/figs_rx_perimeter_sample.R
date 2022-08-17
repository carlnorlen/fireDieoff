#Author: Carl Norlen
#Date Created: May 11, 2022
#Date Updated: August 2, 2022
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
# pixel.data <- read.csv(file.path(dir_in, "fraprx_ecoregion_stratified_sample_100pts_30m_ts8_20220713.csv"), header = TRUE, na.strings = "NaN")
pixel.data <- read.csv(file.path(dir_in, "fraprx_ecoregion_simple_sample_by_rxfire_10pt_300m_ts4_20220809.csv"), header = TRUE, na.strings = "NaN")
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
pixel.data$fire.year <- pixel.data$perimeter_year
pixel.data$stand.age <- as.numeric(pixel.data$year) - as.numeric(pixel.data$fire.year) 

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
pixel.data$GPP <- pixel.data$GPP_mean
pixel.data$PrET <- pixel.data$ppt - pixel.data$AET
pixel.data$elevation <- pixel.data$elevation_mean

pixel.data <- pixel.data %>% mutate(stand.age.bin = case_when(
  # bin >= 1 ~ '1900',
  # bin == 2 ~ '1909-1910',
  # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
  is.na(fire.year) ~ 'No Fire',
  fire.year >= 1910 & fire.year <=  1969 ~ '1910-1969',#'81-95',
  # fire.year >= 1936 & fire.year <= 1950 ~ '65-79',
  # fire.year >= 1951 & fire.year <= 1965 ~ '50-64',
  # fire.year >= 1951 & fire.year <= 1960 ~ '55-64',
  fire.year >= 1970 & fire.year <= 1985 ~ '1970-1985',#'56-80',
  # fire.year >= 1971 & fire.year <= 1980 ~ '35-44',
  fire.year >= 1986 & fire.year <= 2000 ~ '1986-2000',#'31-55', 
  # fire.year >= 1991 & fire.year <= 2000 ~ '15-24',
  fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
  # fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2018',
  fire.year >= 2019 ~ '2019-2020'))#'0-4'))
summary(pixel.data)
pixel.data <- pixel.data %>% mutate(fire.year.bin = case_when(
  # bin >= 1 ~ '1900',
  # bin == 2 ~ '1909-1910',
  # bin >= 1911 & bin <= 1920 ~ '95-104', #Calculated relative to 2015
  is.na(fire.year) ~ 'No Fire',
  fire.year <  1950 ~ '1900-1949',#'81-95',
  # fire.year >= 1936 & fire.year <= 1950 ~ '65-79',
  # fire.year >= 1951 & fire.year <= 1965 ~ '50-64',
  # fire.year >= 1951 & fire.year <= 1960 ~ '55-64',
  fire.year >= 1950 & fire.year <= 1969 ~ '1950-1969',#'56-80',
  # fire.year >= 1971 & fire.year <= 1980 ~ '35-44',
  ##fire.year >= 1999 & fire.year <= 2001 ~ '1999-2001',#'31-55', 
  # fire.year >= 1991 & fire.year <= 2000 ~ '15-24',
  fire.year >= 1970 & fire.year <= 1989 ~ '1970-1989',
  fire.year >= 1990 & fire.year <= 2010 ~ '1990-2010',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2018',
  fire.year >= 2019 ~ '2019-2020'))#'0-4'))

summary(pixel.data)

pixel.data$stand.age.bin = with(pixel.data, factor(stand.age.bin, levels = c('2019-2020', '2011-2018', '2001-2010', '1986-2000', '1970-1985', '1910-1969', 'No Fire')))#c('0-4','5-30','31-55','56-80',
                                                                             #'81-95')))

pixel.data$fire.year.bin = with(pixel.data, factor(fire.year.bin, levels = c('2019-2020', '2011-2018', '1990-2010', '1970-1989', '1950-1969', '1900-1949', 'No Fire')))#c('0-4','5-30','31-55','56-80',
#'81-95')))

summary(pixel.data)
#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills
# 
# summary(pixel.data)
#TEsting for issues
p1a <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 37, color = 'red') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(!is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()), mapping = aes(x = stand.age, y = Tree_Cover.n), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab(expression('# Pixels')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1a

p1b <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 37, color = 'red') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(!is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(elevation.mean = mean(elevation), elevation.sd = sd(elevation), elevation.n = n()), mapping = aes(x = stand.age, y = elevation.mean), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab(expression('Elevation (m)')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1b
summary(pixel.data)

pixel.data %>% filter(!is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(fire.year)) %>% 
  group_by(stand.age) %>% dplyr::select(fire.year) %>% unique() %>% summarize(count = n())
p1c <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 37, color = 'red') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(!is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              dplyr::select(TREATMENT_NAME) %>% 
              unique() %>%
              summarize(fire.year.count = n()), mapping = aes(x = stand.age, y = fire.year.count), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylab(expression('# Fire Perimeters')) #+ xlab('Years Since Fire') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p1c

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

p1d <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 37, color = 'red') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(!is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(fire_year.mode = find_mode(fire.year)), mapping = aes(x = stand.age, y = fire_year.mode), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + ylim(1900, 2010) +
  ylab(expression('Modal Fire Year')) #+ xlab('Years Since Fire') 
p1d

p1e <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = -10, color = 'red') + 
  geom_vline(xintercept = 37, color = 'red') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(!is.na(Shrub_Cover) & vi.year <= 2014 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(vi.year.mode = find_mode(vi.year)), mapping = aes(x = stand.age, y = vi.year.mode), size = 1) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + ylim(1985, 2015) +
  ylab(expression('Modal VI Year')) + xlab('Years Since Fire') 
p1e

f1a <- ggarrange(p1a, p1b, p1c, p1d, p1e, ncol = 1, nrow = 5, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)', 'e)'))
f1a
#Save the data
ggsave(filename = 'Fig12_data_check_10pt_rx_frap_perimeter_chronosequence.png', height=22, width= 16, units = 'cm', dpi=900)

#Figure of mean Cover changes by stand age
p2 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 37 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1971 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub'), size = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 37 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire.year >= 1971 & fire.year <= 2010 & !is.na(fire.year)) %>%
                group_by(stand.age) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, fill = "Shrub"), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 37 & !is.na(Tree_Cover) & vi.year <= 2014 & fire.year >= 1971 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree'), size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 37 & !is.na(Tree_Cover) & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971 & !is.na(fire.year)) %>%
                group_by(stand.age) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, fill = "Tree"), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 37 & !is.na(Herb_Cover) & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb'), size = 1) + 
  #Herb Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 37 & !is.na(Herb_Cover) & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971 & !is.na(fire.year)) %>%
                group_by(stand.age) %>%
                summarize(Herb_Cover.mean = mean(Herb_Cover),
                          Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
              mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            x = stand.age, fill = "Herb"), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 37 & !is.na(Bare_Cover) & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971 & !is.na(fire.year)) %>% 
              group_by(stand.age) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare'), size = 1) + 
  #Bare Cover 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age >= -10 & stand.age <= 37 & !is.na(Bare_Cover) & vi.year <= 2014 & fire.year <= 2010 & fire.year >= 1971 & !is.na(fire.year)) %>%
                group_by(stand.age) %>%
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
  guides(fill = "none") +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire') 
p2

#Save the data
ggsave(filename = 'Fig13_veg_cover_stand_age_10pt_rx_frap_perimeter.png', height=12.5, width= 20, units = 'cm', dpi=900)

summary(pixel.data)
#Figure of Dead Trees per acre separated by fire years with time series
p3 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% # & vi.year >= 2003) %>%
              group_by(date, stand.age.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), tpa_max.n = n()), # %>%
              # filter(if_else(fire.year.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)), 
            mapping = aes(x = date, y = tpa_max.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(tpa_max) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
                group_by(date, stand.age.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()), #%>%
                # filter(if_else(stand.age.bin == '1985-2010', tpa_max.n >= 6000, tpa_max.n >= 0)),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
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


#Create the 
p4 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + #geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_line(data = pixel.data %>%
              filter(!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
              group_by(date, stand.age.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), Tree_Cover.n = n()) %>%  
              filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', Tree_Cover.n >= 600 , Tree_Cover.n >= 0)),
              mapping = aes(x = date, y = Tree_Cover.mean, color = stand.age.bin, linetype = stand.age.bin), 
              size = 1) + 
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
                group_by(date, stand.age.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()) %>%  
                filter(if_else(stand.age.bin == '2001-2010' | stand.age.bin == '1986-2000', Tree_Cover.n >= 600, Tree_Cover.n >= 0)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
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
ggsave(filename = 'Fig14_dieoff_tree_cover_stand_age_time_series_frap_perimeter_10pt_sample.png', height=12, width= 14, units = 'cm', dpi=900)

#Create a Precip time series figure
p5 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(ppt) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
              # fire.year %notin% c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(ppt.mean = mean(ppt), count = n()) %>%  
              filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', count >= 600, count >= 0)), 
            mapping = aes(x = date, y = ppt.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(ppt) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
                # fire.year %notin% c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003)) %>%
                group_by(date, stand.age.bin) %>%
                summarize(ppt.mean = mean(ppt),
                          ppt.sd = sd(ppt), ppt.n = n(), count = n()) %>%  
                filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', count >= 600, count >= 0)),
              mapping = aes(ymin=ppt.mean - 1.96*(ppt.sd / sqrt(ppt.n)),
                            ymax=ppt.mean + 1.96*(ppt.sd / sqrt(ppt.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
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
              filter(!is.na(AET) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
                       # fire.year %notin% c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003)) %>%
              group_by(date, stand.age.bin) %>%
              summarize(AET.mean = mean(AET), count = n()) %>%  
              filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', count >= 600, count >= 0)), 
            mapping = aes(x = date, y = AET.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1) +
  #AET 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(AET) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
                         # fire.year %notin% c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003)) %>%
                group_by(date, stand.age.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n(), count = n()) %>%  
                filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', count >= 600, count >= 0)),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
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
              filter(!is.na(Soil_Moisture) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 2 & !is.na(stand.age.bin)) %>% 
              group_by(date, stand.age.bin) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture), count = n()) %>%  
              filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', count >= 600, count >= 0)), 
              mapping = aes(x = date, y = Soil_Moisture.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1) + 
  #Soil Moisture 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(Soil_Moisture) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
                group_by(date, stand.age.bin) %>%
                summarize(Soil_Moisture.mean = mean(Soil_Moisture),
                          Soil_Moisture.sd = sd(Soil_Moisture), Soil_Moisture.n = n(), count = n()) %>%  
                filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', count >= 600, count >= 0)),
              mapping = aes(ymin=Soil_Moisture.mean - 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            ymax=Soil_Moisture.mean + 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
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
              filter(!is.na(Water_Stress) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
              group_by(date, stand.age.bin) %>%
              summarize(Water_Stress.mean = mean(Water_Stress), count = n()) %>%  
              filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', count >= 600, count >= 0)), 
            mapping = aes(x = date, y = Water_Stress.mean, color = stand.age.bin, linetype = stand.age.bin), 
            size = 1) + 
  #Water Stress 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(!is.na(Water_Stress) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
                group_by(date, stand.age.bin) %>%
                summarize(Water_Stress.mean = mean(Water_Stress),
                          Water_Stress.sd = sd(Water_Stress), Water_Stress.n = n(), count = n()) %>%  
                filter(if_else(stand.age.bin == '2001-2010'| stand.age.bin == '1986-2000', count >= 600, count >= 0)),
              mapping = aes(ymin=Water_Stress.mean - 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            ymax=Water_Stress.mean + 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            x = date, fill = stand.age.bin), alpha = 0.3) +
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
ggsave(filename = 'Fig15_water_stress_stand_age_rx_frap_perimeter_10pt_sample_time_series.png', height=22, width= 16, units = 'cm', dpi=900)

# test <- pixel.data %>%
#   filter(stand.age >= 0 & fire.year >= 1910 & fire.year <= 2010 & !is.na(tpa_max) & fire_type_last == 1) %>%
#   group_by(date, stand.age.bin) %>%
#   summarize(count = n())

#Checking why there is a dip around 2002
p9 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1970 & stand.age > 3 & !is.na(stand.age.bin)) %>% 
              group_by(date, stand.age.bin) %>%
              summarize(count = n()), mapping = aes(x = date, y = count, color = stand.age.bin, linetype = stand.age.bin), 
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
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab('Count') + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p9 

p10 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(Tree_Cover) & fire.year <= 2010 & fire.year >= 1970 &stand.age > 3 & !is.na(stand.age.bin)) %>% 
              group_by(date, stand.age.bin) %>%
              summarize(stand.age.mean = mean(stand.age)), mapping = aes(x = date, y = stand.age.mean, color = stand.age.bin, linetype = stand.age.bin), 
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
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab('Stand Age') + xlab('Year') #+ facet_wrap(. ~ fire_type_last, labeller = as_labeller(c('1' = 'Wild', '2' = 'Prescribed')))
p10 

f4 <- ggarrange(p9, p10, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f4

ggsave(filename = 'Fig16_data_check_time_series_rx_frap_perimeter_10pt_sample.png', height=16, width= 16, units = 'cm', dpi=900)

# pixel.data %>% dplyr::filter(fire.year <= 2010 & fire.year >= 1970) %>% dplyr::group_by(system.index) %>% 
#   summarize(dTree = Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015], Water_Stress = Water_Stress[vi.year == 2015], stand.age.bin = stand.age.bin[vi.year == 2010])

#Creating a fire year dTree plot
p11 <- ggplot(data = pixel.data %>% dplyr::filter(fire.year <= 2010 & fire.year >= 1970 & stand.age > 3) %>% dplyr::group_by(system.index) %>% 
                summarize(dTree = Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015], Water_Stress = Water_Stress[vi.year == 2015], stand.age.bin = stand.age.bin[vi.year == 2010])) +
  geom_bin2d(binwidth = c(5, 1), mapping = aes(x = Water_Stress, y = dTree, group = ..count..)) +
  scale_fill_gradient2(limits = c(0,50), breaks = c(0,10, 20, 30, 40, 50), midpoint = 25, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') + # 
  # geom_point(mapping = aes(x = Water_Stress, y = dTree), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = Water_Stress, y = dTree), color = 'black', size = 2, linetype = 'dashed') +
  stat_cor( mapping = aes(x = Water_Stress, y = dTree)) +
  theme_bw() + facet_wrap (. ~ stand.age.bin, ncol = 2) +
  theme_bw() +xlab('Water Stress (mm)') + ylab('Change in Tree Cover (%)')
p11

ggsave(filename = 'Fig17_water_stress_stand_age_rx_frap_10pt_sample_300m.png', height=16, width= 18, units = 'cm', dpi=900)

# pixel.data %>% summary()
p12 <- ggplot(data = pixel.data %>% dplyr::filter(fire.year <= 2010 & fire.year >= 1970 & stand.age > 3) %>% dplyr::group_by(system.index) %>% summarize(dTree = Tree_Cover[vi.year == 2017] - Tree_Cover[vi.year == 2015], 
                                                                                                                                         Water_Stress = Water_Stress[vi.year == 2015], SPI48 = SPI48[vi.year == 2015], stand.age.bin = stand.age.bin[vi.year == 2010])) +
  geom_point(mapping = aes(x = SPI48, y = dTree, color = stand.age.bin), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = SPI48, y = dTree, color = stand.age.bin , linetype = stand.age.bin)) +
  stat_cor( mapping = aes(x = SPI48, y = dTree, color = stand.age.bin)) +
  theme_bw()
p12

ggsave(filename = 'Fig18_SPI48_stand_age_rx_frap_10pt_sample.png', height=16, width= 18, units = 'cm', dpi=900)

p13 <- ggplot(data = pixel.data %>% dplyr::filter(fire.year <= 2010 & fire.year >= 1970 & stand.age > 3) %>% dplyr::group_by(system.index) %>% summarize(dTree = Tree_Cover[vi.year == 2017] - Tree_Cover[vi.year == 2015], Water_Stress = Water_Stress[vi.year == 2015], SPI48 = SPI48[vi.year == 2015], PrET.4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), stand.age.bin = stand.age.bin[vi.year == 2010])) +
  geom_bin2d(binwidth = c(50, 1), mapping = aes(x = PrET.4yr, y = dTree, group = ..count..)) +
  scale_fill_gradient2(limits = c(0,20), breaks = c(0,5, 10, 15, 20), midpoint = 10, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') + # 
  # geom_point(mapping = aes(x = PrET.4yr, y = dTree, color = stand.age.bin), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = PrET.4yr, y = dTree), color = 'black', size = 2, linetype = 'dashed') +
  stat_cor(mapping = aes(x = PrET.4yr, y = dTree) ) + facet_wrap (. ~ stand.age.bin, ncol = 2) +
  theme_bw() + xlab('Four-year Pr-ET (mm/4yr)') + ylab('Change in Tree Cover (%)')
p13

ggsave(filename = 'Fig19_PrET4yr_stand_age_rx_frap_10pt_sample.png', height=16, width= 18, units = 'cm', dpi=900)

# pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age >= 0 & fire.year <= 2010 & fire.year >= 1910) %>% 
#   dplyr::group_by(system.index) %>% 
#   summarize(dTree = Tree_Cover[vi.year == 2016] - Tree_Cover[vi.year == 2012], stand.age = stand.age[vi.year == 2010], SPI48 = SPI48[vi.year == 2015]) %>%
#   group_by(stand.age) %>%
#   summarize(dTree = mean(dTree), dTree.n = n(), dTree.sd = sd(dTree))

pixel.data$stand.age.grp <- findInterval(pixel.data$stand.age, c(0, 10, 20, 30, 40))

p14 <- ggplot() +
       #The full data
       # geom_point(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age > 3 & fire.year <= 2010 & fire.year >= 1970) %>%
       #              dplyr::group_by(system.index) %>%
       # summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), stand.age = stand.age[vi.year == 2010], SPI48 = SPI48[vi.year == 2015]),
       # mapping = aes(x = stand.age, y = dTree), size = 0.5, alpha = 0.3, color = 'gray') +
       # #The summarized data
       # geom_point(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age > 3 & fire.year <= 2010 & fire.year >= 1970) %>% 
       #         dplyr::group_by(system.index) %>% 
       #         summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), stand.age = stand.age[vi.year == 2010], SPI48 = SPI48[vi.year == 2015]) %>%
       #         group_by(stand.age) %>%
       #         summarize(dTree = mean(dTree), dTree.n = n(), dTree.sd = sd(dTree)),
       #       mapping = aes(x = stand.age, y = dTree), size = 2) + 
  geom_point(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age > 3 & fire.year <= 2010 & fire.year >= 1970) %>% 
               dplyr::group_by(system.index) %>% 
               summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), stand.age.grp = stand.age.grp[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]) %>%
               group_by(stand.age.grp) %>%
               summarize(dTree = mean(dTree), dTree.n = n(), dTree.sd = sd(dTree)),
             mapping = aes(x = stand.age.grp * 10, y = dTree), size = 2) + 
  geom_errorbar(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age > 3 & fire.year <= 2010 & fire.year >= 1970) %>% 
                  dplyr::group_by(system.index) %>% 
                  summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), stand.age.grp = stand.age.grp[vi.year == 2010], Water_Stress = Water_Stress[vi.year == 2015]) %>%
                  group_by(stand.age.grp) %>%
                  summarize(dTree = mean(dTree), dTree.n = n(), dTree.sd = sd(dTree)),
                mapping = aes(x = stand.age.grp * 5, ymin = dTree - 1.96*(dTree.sd /sqrt(dTree.n)), ymax = dTree + 1.96*(dTree.sd /sqrt(dTree.n)), width = 3)) + 
  ylim(-6, 2) + xlab('Stand Age (10-year Bins)') + ylab('Change in Tree Cover (%)') +
       # geom_smooth(method = 'lm', mapping = aes(x = stand.age, y = dTree), linetype = 'dotdash', size = 2) + 
       # stat_cor(mapping = aes(x = stand.age, y = dTree)) +
       theme_bw() #+ facet_grid(. ~ lf_evt_2001)
p14

ggsave(filename = 'Fig20_dTree_stand_age_rx_frap_10pt_sample.png', height=16, width= 18, units = 'cm', dpi=900)

p15 <- ggplot() +
  # geom_point(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age >= 0 & fire.year <= 2010 & fire.year >= 1970) %>% 
  #              dplyr::group_by(system.index) %>%
  #              summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)], na.rm = TRUE), stand.age = stand.age[vi.year == 2010], SPI48 = SPI48[vi.year == 2015]),
  #            mapping = aes(x = stand.age, y = tpa_max), size = 0.5, alpha = 0.3, color = 'gray') +
  # geom_point(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age >= 0 & fire.year <= 2010 & fire.year >= 1970) %>% 
  #              dplyr::group_by(system.index) %>%
  #              summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)], na.rm = TRUE), stand.age = stand.age[vi.year == 2010], SPI48 = SPI48[vi.year == 2015]) %>%
  #              group_by(stand.age) %>%
  #              summarize(tpa_max = mean(tpa_max), tpa_max.n = n(), tpa_max.sd = sd(tpa_max)),
  #            mapping = aes(x = stand.age, y = tpa_max), size = 2) +
  geom_point(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age > 3 & fire.year <= 2010 & fire.year >= 1970) %>% 
               dplyr::group_by(system.index) %>%
               summarize(tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2019)], na.rm = TRUE), stand.age.grp = stand.age.grp[vi.year == 2010], SPI48 = SPI48[vi.year == 2015]) %>%
               group_by(stand.age.grp) %>%
               summarize(tpa_max = mean(tpa_max), tpa_max.n = n(), tpa_max.sd = sd(tpa_max)),
             mapping = aes(x = stand.age.grp * 10, y = tpa_max), size = 2) + 
  xlab('Stand Age (10-year Bins)') + ylab('Mortality (trees/ha)') + 
  # geom_smooth(method = 'lm', mapping = aes(x = stand.age, y = dTree), linetype = 'dotdash', size = 2) +
  # stat_cor(mapping = aes(x = stand.age, y = dTree)) +
  theme_bw() #+ facet_grid(. ~ lf_evt_2001)
p15
ggsave(filename = 'Fig21_dTree_stand_age_rx_frap_10pt_300m.png', height=16, width= 18, units = 'cm', dpi=900)


p16 <- ggplot(data = pixel.data %>% dplyr::filter(!is.na(stand.age) & stand.age > 3 & fire.year <= 2010 & fire.year >= 1970) %>%
               dplyr::group_by(system.index, stand.age.bin) %>%
               summarize(dTree = (Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015]), stand.age = stand.age[vi.year == 2010], Tree = Tree_Cover[vi.year == 2015])) +
       geom_point(mapping = aes(x = Tree, y = dTree), size = 0.5, alpha = 0.3, color = 'black') +
  xlab('Tree Cover (%)') + ylab('dTree Cover (%)') +
  theme_bw() + facet_grid(. ~ stand.age.bin)
p16
ggsave(filename = 'Fig22_dTree_Tree_rx_frap_10pt_sample.png', height=16, width= 18, units = 'cm', dpi=900)

p17 <- ggplot(data = pixel.data %>% dplyr::filter(fire.year <= 2010 & fire.year >= 1970 & stand.age > 3) %>% dplyr::group_by(system.index) %>% summarize(dTree = Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015], Water_Stress = Water_Stress[vi.year == 2015], SPI48 = SPI48[vi.year == 2015], PrET.4yr = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), stand.age.bin = stand.age.bin[vi.year == 2010])) +
  geom_point(mapping = aes(x = PrET.4yr, y =  Water_Stress), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = PrET.4yr, y = Water_Stress)) +
  stat_cor(mapping = aes(x = PrET.4yr, y =  Water_Stress) ) +
  theme_bw() + facet_wrap(. ~ stand.age.bin)
p17

ggsave(filename = 'Fig23_Water_Stress_PrET_correlation_10pt_sample.png', height=16, width= 18, units = 'cm', dpi=900)

#GPP and AET
p18 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 37 & !is.na(GPP) & vi.year <= 2014 & fire.year >= 1970 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(GPP.mean = mean(GPP)), mapping = aes(x = stand.age, y = GPP.mean), size = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 37 & !is.na(GPP) & vi.year <= 2014 & fire.year >= 1970 & fire.year <= 2010 & !is.na(fire.year)) %>%
                group_by(stand.age) %>%
                summarize(GPP.mean = mean(GPP),
                          GPP.sd = sd(GPP), GPP.n = n()),
              mapping = aes(ymin=GPP.mean - 1.96*(GPP.sd / sqrt(GPP.n)),
                            ymax=GPP.mean + 1.96*(GPP.sd / sqrt(GPP.n)),
                            x = stand.age), alpha = 0.3) +
  #Create a Tree Cover line
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('GPP (g/m^2/yr)')) + xlab('Years Since Fire') 
p18

p19 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 37 & !is.na(AET) & vi.year <= 2014 & fire.year >= 1970 & fire.year <= 2010 & !is.na(fire.year)) %>%
              group_by(stand.age) %>%
              summarize(AET.mean = mean(AET)), mapping = aes(x = stand.age, y = AET.mean), size = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 37 & !is.na(AET) & vi.year <= 2014 & fire.year >= 1970 & fire.year <= 2010 & !is.na(fire.year)) %>%
                group_by(stand.age) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = stand.age), alpha = 0.3) +
  #Create a Tree Cover line
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.35, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  ylab(expression('AET (mm/yr)')) + xlab('Years Since Fire') 
p19

f5 <- ggarrange(p18, p19, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
f5

#Save the data
ggsave(filename = 'Fig26_veg_cover_stand_age_10pt_300m_frap_perimeter.png', height=18, width= 20, units = 'cm', dpi=900)
