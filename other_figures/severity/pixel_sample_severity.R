#Author: Carl Norlen
#Date Created: December 6, 2021
#Date Updated: August 19, 2022
#Purpose: Explore pixel sampling data.

# cd /C/Users/Carl/mystuff/fireDieoff/severity
# cd /C/Users/can02/fireDieoff/severity
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggplot2', 'ggpubr', 'viridis', 'tidyr', 'dplyr', 'gtools', 'tigris', 'patchwork')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/severity')

#The data directory
dir_in <- "D:\\Large_Files\\CECS\\Stand_Age"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"

#Add the data
pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_fire_year_fire_severity_01242022_30m.csv"), header = TRUE, na.strings = "NaN")

#Get a summary of the data
summary(pixel.data)
#Convert missing TPA data to NAs
pixel.data[pixel.data$tpa_max == -9999,]$tpa_max <- NA

#Convert to trees per hectare
pixel.data$tpa_max <- pixel.data$tpa_max * 2.47105

#Make the dates into date time format for R
pixel.data$date <- as.Date(pixel.data$date)
pixel.data$vi.year <- format(pixel.data$date , '%Y')
pixel.data$vi.year <- pixel.data$vi.year %>% as.numeric
pixel.data$fire.year <- pixel.data$fire_year
# head(pixel.data)
pixel.data$stand.age <- as.numeric(pixel.data$vi.year) - as.numeric(pixel.data$fire.year) 

#Update Cover data to 100% scale
pixel.data$Tree_Cover <- pixel.data$Tree_Cover / 100
pixel.data$Shrub_Cover <- pixel.data$Shrub_Cover / 100
pixel.data$Herb_Cover <- pixel.data$Herb_Cover / 100
pixel.data$Bare_Cover <- pixel.data$Bare_Cover / 100

#Try to fix scaling by dividing Soil Moisture by 10
pixel.data$Soil_Moisture <- pixel.data$Soil_Moisture / 10
# summary(pixel.data %>% filter(bin == 13))

#Calculate the Quintiles of precip climate normals
precip.q <- as.data.frame(unname(quantile(pixel.data$clm_precip_sum, prob = seq(0,1, 1/5))))
# precip.q
colnames(precip.q) <- 'Precip'
precip.q$'Quartile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
precip.q

# precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric()

#Plot a histogram with precip quartiles.
ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = clm_precip_sum)) +  
  geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') + 
  geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.4) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (precip.q %>% filter(Quartile == 0.8) %>% dplyr::select(Precip) %>% as.numeric()), color = 'black')

ggsave(filename = 'Fig1_Precip_Quartiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)


#Bin data by precip climatology
pixel.data <- pixel.data %>% mutate(precip.control = case_when(
  clm_precip_sum >= precip.q %>% filter(Quartile == 0.8) %>% dplyr::select(Precip) %>% as.numeric() ~ '> 80 %',
  clm_precip_sum >= precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric() & 
    clm_precip_sum < precip.q %>% filter(Quartile == 0.8) %>% dplyr::select(Precip) %>% as.numeric() ~ '60 to 80 %',
  clm_precip_sum >= precip.q %>% filter(Quartile == 0.4) %>% dplyr::select(Precip) %>% as.numeric() & 
    clm_precip_sum < precip.q %>% filter(Quartile == 0.6) %>% dplyr::select(Precip) %>% as.numeric() ~ '40 to 60 %',
  clm_precip_sum >= precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric()  & 
    clm_precip_sum < precip.q %>% filter(Quartile == 0.4) %>% dplyr::select(Precip) %>% as.numeric() ~ '20 to 40 %',
  clm_precip_sum < precip.q %>% filter(Quartile == 0.2) %>% dplyr::select(Precip) %>% as.numeric() ~ '0 to 20%'))


#Calculate the Quintiles of temperature climate normals
temp.q <- as.data.frame(unname(quantile(pixel.data$clm_temp_mean, prob = seq(0,1, 1/5))))
# precip.q
colnames(temp.q) <- 'Temp'
temp.q$'Quartile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# temp.q

#Histogram Plot of temperature quantiles
ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = clm_temp_mean)) +
  geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.2) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') + 
  geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.4) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.6) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (temp.q %>% filter(Quartile == 0.8) %>% dplyr::select(Temp) %>% as.numeric()), color = 'black')

ggsave(filename = 'Fig2_Temp_Quartiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Create temperature bins for analysis with quantiles
pixel.data <- pixel.data %>% mutate(temp.control = case_when(
  clm_temp_mean >= temp.q %>% filter(Quartile == 0.8) %>% dplyr::select(Temp) %>% as.numeric() ~ '> 80 %',
  clm_temp_mean >= temp.q %>% filter(Quartile == 0.6) %>% dplyr::select(Temp) %>% as.numeric() & 
    clm_temp_mean < temp.q %>% filter(Quartile == 0.8) %>% dplyr::select(Temp) %>% as.numeric() ~ '60 to 80 %',
  clm_temp_mean >= temp.q %>% filter(Quartile == 0.4) %>% dplyr::select(Temp) %>% as.numeric() & 
    clm_temp_mean < temp.q %>% filter(Quartile == 0.6) %>% dplyr::select(Temp) %>% as.numeric() ~ '40 to 60 %',
  clm_temp_mean >= temp.q %>% filter(Quartile == 0.2) %>% dplyr::select(Temp) %>% as.numeric()  & 
    clm_temp_mean < temp.q %>% filter(Quartile == 0.4) %>% dplyr::select(Temp) %>% as.numeric() ~ '20 to 40 %',
  clm_temp_mean < temp.q %>% filter(Quartile == 0.2) %>% dplyr::select(Temp) %>% as.numeric() ~ '0 to 20%'))

#Calculate the Quintiles of elevation
elev.q <- as.data.frame(unname(quantile(pixel.data$elevation, prob = seq(0,1, 1/5))))
# precip.q
colnames(elev.q) <- 'elevation'
elev.q$'Quartile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# temp.q
elev.q
#Histogram Plot of temperature quantiles
ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = elevation)) +
  geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.2) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') + 
  geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.4) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.6) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (elev.q %>% filter(Quartile == 0.8) %>% dplyr::select(elevation) %>% as.numeric()), color = 'black')

ggsave(filename = 'Fig3_Elevation_Quintiles_historgram.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Bin data by elevation. Bins are quintiles.
pixel.data <- pixel.data %>% mutate(elevation.control = case_when(
  elevation >= elev.q %>% filter(Quartile == 0.8) %>% dplyr::select(elevation) %>% as.numeric() ~ '> 80 %',
  elevation >= elev.q %>% filter(Quartile == 0.6) %>% dplyr::select(elevation) %>% as.numeric() & 
    elevation < elev.q %>% filter(Quartile == 0.8) %>% dplyr::select(elevation) %>% as.numeric() ~ '60 to 80 %',
  elevation >= elev.q %>% filter(Quartile == 0.4) %>% dplyr::select(elevation) %>% as.numeric() & 
    elevation < elev.q %>% filter(Quartile == 0.6) %>% dplyr::select(elevation) %>% as.numeric() ~ '40 to 60 %',
  elevation >= elev.q %>% filter(Quartile == 0.2) %>% dplyr::select(elevation) %>% as.numeric()  & 
    elevation < elev.q %>% filter(Quartile == 0.4) %>% dplyr::select(elevation) %>% as.numeric() ~ '20 to 40 %',
  elevation < elev.q %>% filter(Quartile == 0.2) %>% dplyr::select(elevation) %>% as.numeric() ~ '0 to 20%'))
# pixel.data
#Bin data by elevation
# pixel.data <- pixel.data %>% mutate(year.control = case_when(fire.year > 2000 ~ '2001-2020',
# 															 fire.year > 1980 & fire.year <= 2000 ~ '1981-2000',
# 															 fire.year > 1960 & fire.year <= 1980 ~ '1961-1980',
# 															 fire.year > 1940 & fire.year <= 1960 ~ '1941-1960',
# 															 fire.year > 1920 & fire.year <= 1940 ~ '1921-1940',
# 															 fire.year >= 1900 & fire.year <= 1920 ~ '1900-1920')) # end function
# summary(pixel.data)

#New bins based on stand age
pixel.data <- pixel.data %>% mutate(age.bin = case_when(
  stand.age >= -32 & stand.age < -20 ~ '-32 to -21',
  stand.age >= -20 & stand.age < -10 ~ '-20 to -11',
  stand.age >= -10 & stand.age < 0 ~ '-1 to -10',
  stand.age >= 0 & stand.age <= 10 ~ '0 to 10',
  stand.age > 10 & stand.age <= 20 ~ '11 to 20',
  stand.age > 20 & stand.age <= 30 ~ '21 to 30',
  stand.age > 30 & stand.age <= 40 ~ '31 to 34'))

#Create a GAM to predict NDMI by stand.age
# ndmi.gam <- gam(data = filter(pixel.data, vi.year <= 2012 & stand.age >= 0), #fire.year >= 1919 & 
#                 formula = NDMI ~ s(stand.age, bs = "cs", k = 5) + s(fire_sev_last, bs = "cs", k = 3))
# summary(ndmi.gam)
# summary(ndmi.gam)
#Add a new column
# pixel.data$NDMI.predict <- NA
# pixel.data$NDMI.predict[pixel.data$stand.age <= 0] <- filter(pixel.data, stand.age <= 0)$NDMI
# pixel.data$NDMI.predict[pixel.data$stand.age > 0] <- predict(newdata = filter(pixel.data, stand.age > 0), object = ndmi.gam) 


# pixel.data
#Stand Age years
# p6 <- ggplot(data = pixel.data) + geom_histogram(mapping = aes( x = date)) + facet_wrap(~age.bin)
# p6
# 
# ggsave(filename = 'Fig6_Stand_age_bins.png', height=12.5, width= 16, units = 'cm', dpi=900)

#Create new name for data bins
#Bin names will need to be updated
pixel.data <- pixel.data %>% mutate(year.bin = case_when(
  fire_year_last >= 1981 & fire_year_last <= 1990 ~ '1984-1990', 
  fire_year_last >= 1991 & fire_year_last <= 2000 ~ '1991-1995',
  fire_year_last >= 1996 & fire_year_last <= 2000 ~ '1996-2000',
  fire_year_last >= 2001 & fire_year_last <= 2005 ~ '2001-2005',
  fire_year_last >= 2006 & fire_year_last <= 2010 ~ '2006-2010',
  fire_year_last >= 2011 & fire_year_last <= 2015 ~ '2011-2015', 
  fire_year_last >= 2016 & fire_year_last <= 2020 ~'2016-2017')) # end function
# summary(pixel.data)
# 
# pixel.data %>% filter(fire_sev_last == 1)
# bin >= 1981 & bin <= 1995 ~ '20-34', 
# # bin >= 1991 & bin <= 2000 ~ '15-24',
# bin >= 1996 & bin <= 2010 ~ '5-19', 
# bin >= 2011 & bin <= 2020 ~'0-4'))
#Bin names will need to be updated
pixel.data <- pixel.data %>% mutate(stand.age.bin = case_when(
  fire_year_last >= 1984 & fire_year_last <= 1990 ~ '1984-1990', #Calculated Relative to 2015, 
  fire_year_last >= 1991 & fire_year_last <= 2000 ~ '1991-2000',
  fire_year_last >= 2001 & fire_year_last <= 2010 ~ '2001-2010',
  fire_year_last >= 2011 & fire_year_last <= 2020 ~ '2011-2017')) # end function


#Fire Severity Bins
pixel.data <- pixel.data %>% mutate(sev.bin = case_when(
                                    fire_sev_last == '0' ~ 'No Fire',
                                    fire_sev_last == '1' ~ 'Unchanged', 
                                    fire_sev_last == '2' ~ 'Low',
                                    fire_sev_last == '3' ~ 'Mid',
                                    fire_sev_last == '4' ~ 'High',
                                    fire_sev_last == '255' ~ 'Masked')) # end function
pixel.data
#Make the bin lables in the correct order
pixel.data$elevation.control = with(pixel.data, factor(elevation.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))
pixel.data$temp.control = with(pixel.data, factor(temp.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))
pixel.data$precip.control = with(pixel.data, factor(precip.control, levels = c('0 to 20%', '20 to 40 %', '40 to 60 %', '60 to 80 %', '> 80 %')))

#Make the years bin lables in the correct order
pixel.data$sev.bin = with(pixel.data, factor(sev.bin, levels = c('Masked', 'Unchanged', 'Lowest', 'Low','Mid', 'High')))

#Burn Severity Bin
pixel.data$age.bin = with(pixel.data, factor(age.bin, levels = c('-32 to -21', '-20 to -11', '-10 to -1','0 to 10', '11 to 20', '21 to 30', '31 to 34')))

#Fire Year Bins
# pixel.data$year.bin = with(pixel.data, factor(year.bin, levels = c('2011-2017','2001-2010','1991-2000','1984-1990')))

#Stand Age Bins
pixel.data$stand.age.bin = with(pixel.data, factor(stand.age.bin, levels = c('2011-2017','2001-2010','1991-2000','1984-1990'))) #c('0-4', '5-14', '15-24', '25-31')))

#Checking why there is a dip around 2002
p6 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(Tree_Cover) & fire_sev_last != 255 & stand.age > 2 & fire.year <= 2010) %>%
              group_by(date, sev.bin, stand.age.bin) %>%
              summarize(count = n()), mapping = aes(x = date, y = count, color = sev.bin, linetype = sev.bin), 
            size = 1
  ) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'OrRd', name = 'Fire Severity') +
  scale_linetype(name = 'Fire Severity') +
  scale_fill_brewer(palette = 'OrRd') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = 'none', legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab('Count (Tree Cover)') + xlab('Year') + facet_grid(. ~ stand.age.bin)
p6 

p7 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(!is.na(tpa_max) & fire_sev_last != 255 & stand.age > 2 & fire.year <= 2010) %>%
              group_by(date, sev.bin, stand.age.bin) %>%
              summarize(count = n()), mapping = aes(x = date, y = count, color = sev.bin, linetype = sev.bin), 
            size = 1
  ) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'OrRd', name = 'Fire Severity') +
  scale_linetype(name = 'Fire Severity') +
  scale_fill_brewer(palette = 'OrRd') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x =element_text(size = 10), legend.position = c(0.1, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab('Count (tpa max)') + xlab('Year') + facet_grid(. ~ stand.age.bin)
p7 

fa <- ggarrange(p6, p7, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
fa

ggsave(filename = 'Fig7_data_check_time_series_fire_severity.png', height=16, width= 16, units = 'cm', dpi=900)

pixel.data %>%
  filter(fire.year <= 2010 & !is.na(tpa_max) & fire_sev_last != 255 & stand.age >= 2) %>%
  group_by(date, sev.bin, stand.age.bin) %>%
  summarize(tpa_max.mean = mean(tpa_max), count = n()) %>% count()

#Figure of Dead Trees per acre separated by fire years with time series
p8 <- ggplot() + 
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(fire.year <= 2010 & !is.na(tpa_max) & fire_sev_last != 255 & stand.age > 2) %>%
              group_by(date, sev.bin, stand.age.bin) %>%
              summarize(tpa_max.mean = mean(tpa_max), count = n()) %>%
              filter(count >= 175), mapping = aes(x = as.Date(date), y = tpa_max.mean, color = sev.bin, linetype = sev.bin), 
            size = 1
  ) +
  #Dead Trees 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age > 2 & fire.year <= 2010 & !is.na(tpa_max) & fire_sev_last != 255) %>%
                group_by(date, sev.bin, stand.age.bin) %>%
                summarize(tpa_max.mean = mean(tpa_max),
                          tpa_max.sd = sd(tpa_max), tpa_max.n = n()) %>%
                filter(tpa_max.n >= 175),
              mapping = aes(ymin=tpa_max.mean - 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            ymax=tpa_max.mean + 1.96*(tpa_max.sd / sqrt(tpa_max.n)),
                            x = as.Date(date), fill = sev.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'OrRd', name = 'Fire Severity') +
  scale_linetype(name = 'Fire Severity') +
  scale_fill_brewer(palette = 'OrRd') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.1, 0.7), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression(atop('Die-off Severity', '(trees ha'^-1*')'))) + xlab('Year') +                   
  facet_grid(. ~ stand.age.bin) 
p8

#Do tree cover recovery
p9 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  # geom_bin2d(alpha = 0.8, binwidth = c(365,5)) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(fire.year <= 2010 & !is.na(Tree_Cover) & fire_sev_last != 255 & stand.age > 2) %>%
              group_by(date, sev.bin, stand.age.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover), count = n()) %>%
              filter(if_else(stand.age.bin != '2001-2010', count >= 260, count >= 450)), 
            mapping = aes(x = as.Date(date), y = Tree_Cover.mean, color = sev.bin, linetype = sev.bin), 
            size = 1
  ) +
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age > 2 & fire.year <= 2010 & !is.na(Tree_Cover) & fire_sev_last != 255) %>%
                group_by(date, sev.bin, stand.age.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()) %>%
                filter(if_else(stand.age.bin != '2001-2010', Tree_Cover.n >= 260, Tree_Cover.n >= 450)),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)) ,
                            x = as.Date(date), fill = sev.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'OrRd', name = 'Fire Severity') +
  scale_linetype(name = 'Fire Severity') +
  scale_fill_brewer(palette = 'OrRd') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  ylab(expression('Tree Cover (%)')) + xlab('Year') +   
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  facet_grid(. ~ stand.age.bin)
p9

#Combine the figures together
f1 <- ggarrange(p8, p9, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(1, 1), align = "v", labels = c('a)', 'b)'))
f1
ggsave(filename = 'Fig8_Dieoff_fire_year_severity_time_series.png', height=12.5, width= 20, units = 'cm', dpi=900)

#Create a manual color scale
cols <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")
fills <- c("Shrub"="green","Herb"="brown","Tree"="forest green", "Bare" = "gray")

# #Figure of mean Cover changes by stand age
p10 <- ggplot() +
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = 'dashed') +
  #Create a shrub cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire_sev_last != 255) %>%
              group_by(stand.age, sev.bin) %>%
              summarize(Shrub_Cover.mean = mean(Shrub_Cover)), mapping = aes(x = stand.age, y = Shrub_Cover.mean, color = 'Shrub'), size = 1) +
  #Shrub Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 20 & !is.na(Shrub_Cover) & vi.year <= 2014 & fire_sev_last != 255) %>%
                group_by(stand.age, sev.bin) %>%
                summarize(Shrub_Cover.mean = mean(Shrub_Cover),
                          Shrub_Cover.sd = sd(Shrub_Cover), Shrub_Cover.n = n()),
              mapping = aes(ymin=Shrub_Cover.mean - 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            ymax=Shrub_Cover.mean + 1.96*(Shrub_Cover.sd / sqrt(Shrub_Cover.n)),
                            x = stand.age, fill = "Shrub"), alpha = 0.3) +
  #Create a Tree Cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 20 & !is.na(Tree_Cover) & vi.year <= 2014 & fire_sev_last != 255) %>%
              group_by(stand.age, sev.bin) %>%
              summarize(Tree_Cover.mean = mean(Tree_Cover)), mapping = aes(x = stand.age, y = Tree_Cover.mean, color = 'Tree'), size = 1) +
  #Tree Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 20 & !is.na(Tree_Cover) & vi.year <= 2014 & fire_sev_last != 255) %>%
                group_by(stand.age, sev.bin) %>%
                summarize(Tree_Cover.mean = mean(Tree_Cover),
                          Tree_Cover.sd = sd(Tree_Cover), Tree_Cover.n = n()),
              mapping = aes(ymin=Tree_Cover.mean - 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            ymax=Tree_Cover.mean + 1.96*(Tree_Cover.sd / sqrt(Tree_Cover.n)),
                            x = stand.age, fill = "Tree"), alpha = 0.3) +
  #Create an Herb cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 20 & !is.na(Herb_Cover) & vi.year <= 2014 & fire_sev_last != 255) %>%
              group_by(stand.age, sev.bin) %>%
              summarize(Herb_Cover.mean = mean(Herb_Cover)), mapping = aes(x = stand.age, y = Herb_Cover.mean, color = 'Herb'), size = 1) +
  #Herb Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 20 & !is.na(Herb_Cover) & vi.year <= 2014 & fire_sev_last != 255) %>%
                group_by(stand.age, sev.bin) %>%
                summarize(Herb_Cover.mean = mean(Herb_Cover),
                          Herb_Cover.sd = sd(Herb_Cover), Herb_Cover.n = n()),
              mapping = aes(ymin=Herb_Cover.mean - 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            ymax=Herb_Cover.mean + 1.96*(Herb_Cover.sd / sqrt(Herb_Cover.n)),
                            x = stand.age, fill = "Herb"), alpha = 0.3) +
  #Create a Bare cover line
  geom_line(data = pixel.data %>%
              filter(stand.age >= -10 & stand.age <= 20 & !is.na(Bare_Cover) & vi.year <= 2014 & fire_sev_last != 255) %>%
              group_by(stand.age, sev.bin) %>%
              summarize(Bare_Cover.mean = mean(Bare_Cover)), mapping = aes(x = stand.age, y = Bare_Cover.mean, color = 'Bare'), size = 1) +
  #Bare Cover 95% CI
  geom_ribbon(data = pixel.data %>% 
                filter(stand.age >= -10 & stand.age <= 20 & !is.na(Bare_Cover) & vi.year <= 2014 & fire_sev_last != 255) %>%
                group_by(stand.age, sev.bin) %>%
                summarize(Bare_Cover.mean = mean(Bare_Cover),
                          Bare_Cover.sd = sd(Bare_Cover), Bare_Cover.n = n()),
              mapping = aes(ymin=Bare_Cover.mean - 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            ymax=Bare_Cover.mean + 1.96*(Bare_Cover.sd / sqrt(Bare_Cover.n)),
                            x = stand.age, fill = "Bare"), alpha = 0.3) +
  scale_colour_manual(name="Vegetation Type",values=cols, aesthetics = 'color') +
  scale_fill_manual(values = fills) + 
  guides(fill = "none") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.65, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 6), legend.text = element_text(size = 5)) +
  # guides(color = guide_legend(ncol = 4)) +
  ylab(expression('Cover (%)')) + xlab('Years Since Fire') + facet_grid(. ~ factor(sev.bin, levels = c("Unchanged", "Low", "Mid", "High")))
p10

#Save the data
ggsave(filename = 'Fig10_veg_cover_stand_age.png', height=10, width= 20, units = 'cm', dpi=900)

summary(pixel.data)

#AET time series
p12 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  # geom_bin2d(alpha = 0.8, binwidth = c(365,5)) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(fire.year <= 2010 & !is.na(AET) & fire_sev_last != 255 & stand.age > 2) %>%
              group_by(date, sev.bin, stand.age.bin) %>%
              summarize(AET.mean = mean(AET)), mapping = aes(x = as.Date(date), y = AET.mean, color = sev.bin, linetype = sev.bin), 
            size = 1
  ) +
  #AET 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age > 2 & fire.year <= 2010 & !is.na(AET) & fire_sev_last != 255) %>%
                group_by(date, sev.bin, stand.age.bin) %>%
                summarize(AET.mean = mean(AET),
                          AET.sd = sd(AET), AET.n = n()),
              mapping = aes(ymin=AET.mean - 1.96*(AET.sd / sqrt(AET.n)),
                            ymax=AET.mean + 1.96*(AET.sd / sqrt(AET.n)),
                            x = as.Date(date), fill = sev.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'OrRd', name = 'Fire Severity') +
  scale_linetype(name = 'Fire Severity') +
  scale_fill_brewer(palette = 'OrRd') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Water Use (AET; mm yr'^-1*')')) + xlab('Year') +                   
  facet_grid(. ~ stand.age.bin) 
p12

#Soil Moisture time series
p13 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  # geom_bin2d(alpha = 0.8, binwidth = c(365,5)) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(fire.year <= 2010 & !is.na(Soil_Moisture) & fire_sev_last != 255 & stand.age > 2) %>%
              group_by(date, sev.bin, stand.age.bin) %>%
              summarize(Soil_Moisture.mean = mean(Soil_Moisture)), mapping = aes(x = as.Date(date), y = Soil_Moisture.mean, color = sev.bin, linetype = sev.bin), 
            size = 1
  ) +
  #Soil Moisture 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age > 2 & fire.year <= 2010 & !is.na(Soil_Moisture) & fire_sev_last != 255) %>%
                group_by(date, sev.bin, stand.age.bin) %>%
                summarize(Soil_Moisture.mean = mean(Soil_Moisture),
                          Soil_Moisture.sd = sd(Soil_Moisture), Soil_Moisture.n = n()),
              mapping = aes(ymin=Soil_Moisture.mean - 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            ymax=Soil_Moisture.mean + 1.96*(Soil_Moisture.sd / sqrt(Soil_Moisture.n)),
                            x = as.Date(date), fill = sev.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'OrRd', name = 'Fire Severity') +
  scale_linetype(name = 'Fire Severity') +
  scale_fill_brewer(palette = 'OrRd') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = "none", legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Soil Moisture (mm)')) + xlab('Year') +                  
  facet_grid(. ~ stand.age.bin) 
p13

#Water Stress time series
p14 <- ggplot() + 
  # geom_line(mapping = aes(group = .geo), color = 'dark gray', size = 0.2, alpha = 0.2) +
  # geom_bin2d(alpha = 0.8, binwidth = c(365,5)) +
  geom_hline(yintercept = 0) +
  geom_line(data = pixel.data %>%
              filter(fire.year <= 2010 & !is.na(Water_Stress) & fire_sev_last != 255 & stand.age > 2) %>%
              group_by(date, sev.bin, stand.age.bin) %>%
              summarize(Water_Stress.mean = mean(Water_Stress)), mapping = aes(x = as.Date(date), y = Water_Stress.mean, color = sev.bin, linetype = sev.bin), 
            size = 1
  ) +
  #Water Stress 95% CI
  geom_ribbon(data = pixel.data %>%
                filter(stand.age > 2 & fire.year <= 2010 & !is.na(Water_Stress) & fire_sev_last != 255) %>%
                group_by(date, sev.bin, stand.age.bin) %>%
                summarize(Water_Stress.mean = mean(Water_Stress),
                          Water_Stress.sd = sd(Water_Stress), Water_Stress.n = n()),
              mapping = aes(ymin=Water_Stress.mean - 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            ymax=Water_Stress.mean + 1.96*(Water_Stress.sd / sqrt(Water_Stress.n)),
                            x = as.Date(date), fill = sev.bin), alpha = 0.3) +
  #Do the Formatting
  scale_color_brewer(type = 'seq', palette = 'OrRd', name = 'Fire Severity') +
  scale_linetype(name = 'Fire Severity') +
  scale_fill_brewer(palette = 'OrRd') +
  guides(color = guide_legend(), linetype = guide_legend(), fill = 'none') +
  theme_dark() +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.1, 0.3), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  xlim(as.Date('1985-08-01'),as.Date('2020-01-01')) +
  ylab(expression('Water Stress (mm)')) + xlab('Year') +                  
  facet_grid(. ~ stand.age.bin) 
p14

#Combine the figures together
f2 <- ggarrange(p12, p13, p14, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f2
ggsave(filename = 'Fig9_Dieoff_fire_year_water_balance_time_series.png', height=18, width= 20, units = 'cm', dpi=900)

# pixel.data <- pixel.data %>% separate(system.index, sep = '_', into = c('system.index', NA))
# 
# pixel.data %>% dplyr::filter(fire_sev_last != 255 & !is.na(tpa_max)) %>% dplyr::group_by(system.index) %>% 
#   summarize(dTree = Tree_Cover[vi.year == 2016] - Tree_Cover[vi.year == 2012], tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017)], na.rm = TRUE), stand.age = stand.age[vi.year == 2010], sev.bin = getmode(sev.bin))

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# pixel.data$vi.year <- pixel.data$vi.year %>% as.factor()
summary(pixel.data)
#Correlation plot for ADS vs standage when accouting for 
p15 <- ggplot(data = pixel.data %>% dplyr::filter(fire_sev_last != 255 & !is.na(tpa_max) & stand.age > 2) %>% dplyr::group_by(system.index) %>% 
              summarize(dTree = Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015], tpa_max = max(tpa_max[vi.year %in% c(2012, 2013, 2014, 2015, 2016, 2017)], na.rm = TRUE), stand.age = stand.age[vi.year == 2010], sev.bin = sev.bin[vi.year == 2010])
              ) +
  geom_point(mapping = aes(x = stand.age, y = tpa_max, color = sev.bin), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = stand.age, y = tpa_max, color = sev.bin, linetype = sev.bin), size = 2) + 
  stat_cor(mapping = aes(x = stand.age, y = tpa_max, color = sev.bin)) +
  theme_bw()
p15

ggsave(filename = 'Fig10_Dieoff_dieoff_standage_fire_severity.png', height=18, width= 20, units = 'cm', dpi=900)

pixel.data %>% filter(vi.year == 2010)

p16 <- ggplot(data = pixel.data %>% dplyr::filter(fire_sev_last != 255 & stand.age > 2) %>% dplyr::group_by(system.index) %>% 
                summarize(dTree = Tree_Cover[vi.year == 2019] - Tree_Cover[vi.year == 2015], stand.age = stand.age[vi.year == 2010], sev.bin = sev.bin[vi.year == 2010])
) +
  geom_point(mapping = aes(x = stand.age, y = dTree, color = sev.bin), size = 1) + 
  geom_smooth(method = 'lm', mapping = aes(x = stand.age, y = dTree, color = sev.bin, linetype = sev.bin), size = 2) + 
  stat_cor(mapping = aes(x = stand.age, y = dTree, color = sev.bin)) +
  theme_bw()
p16

ggsave(filename = 'Fig11_Dieoff_dTree_standage_fire_300m_severity.png', height=18, width= 20, units = 'cm', dpi=900)

