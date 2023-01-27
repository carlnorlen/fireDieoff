#Author: Carl Norlen
#Date Created: January 23, 2023
#Date Updated: January 25, 2023
#Purpose: Create Pr-ET four-year versus dTree figures

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'zoo', 'segmented')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('zoo'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(segmented)
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the Wildfire data
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 
       'rlist', 'ggspatial', 'svglite', 'mgcv')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"
#Add the data
# pixel.data <- read.csv(file.path(dir_in, "Stratified_sample_stand_age_2012_no_fire_history_mask_20210629_30m_v2.csv"), header = TRUE, na.strings = "NaN") #v2 is for all of Sierra and Socal
# pixel.data <- read.csv(file.path(fire_in, "Stratified_sample_stand_age_no_fire_history_mask_01242022_30m.csv"), header = TRUE, na.strings = "NaN")
# pixel.data <- read.csv(file.path(dir_in, "frapsev_ecoregion_stratified_sample_100pts_30m_ts8_20220713.csv"), header = TRUE, na.strings = "NaN")
sev.data <- read.csv(file.path(dir_in, "fire_south_sierra_USFS_sevfire_400pt_ts8_300m_20230112.csv"), header = TRUE, na.strings = "NaN")
# fire.data$fire.year <- fire.data$perimeter_year
sev.data$treatment <- 'Disturb'
summary(sev.data)
# list.files(fire_in)
# list.files(fire_in)
sev.control.data <- read.csv(file.path(dir_in, "control_south_sierra_Sev_2km_buffer_400pt_ts16_300m_20230125.csv"), header = TRUE, na.strings = "NaN")
summary(control.data)
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

#Add buffer dummy columns for fire.data
# sev.data$buffer_type_2010 <- -9999
# sev.data$buffer_type_2019 <- -9999
# sev.data$buffer_year_2010 <- -9999
# sev.data$buffer_year_2019 <- -9999

# summary(control.data)
#Get a  of the data
# summary(pixel.data)
# pixel.data <- pixel.data %>% filter(fire.year >= 1919 & !is.na(stand.age) & !is.na(NDMI))
# control.data$fire.year <- control.data$perimeter_year
sev.control.data$treatment <- 'Control' #Try making this 1-km versus, 2-km

#Figure out why the two dataframes don't match
# sev.n <- sev.data %>% colnames()
# con.n <- control.data %>% colnames()
# sev.n
# con.n
# sev.n[!(sev.n %in% con.n)]

#Combine the data together
sev.pixel.data <- rbind(sev.data, sev.control.data)
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
# sev.pixel.data[sev.pixel.data$fire_type_2010 == -9999,]$fire_type_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_year_2010 == -9999,]$fire_year_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_ID_2010 == -9999,]$fire_ID_2010 <- NA
sev.pixel.data[sev.pixel.data$fire_count_2010 == -9999,]$fire_count_2010 <- NA
# sev.pixel.data[sev.pixel.data$fire_type_2019 == -9999,]$fire_type_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_year_2019 == -9999,]$fire_year_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_ID_2019 == -9999,]$fire_ID_2019 <- NA
sev.pixel.data[sev.pixel.data$fire_count_2019 == -9999,]$fire_count_2019 <- NA
# sev.pixel.data[sev.pixel.data$fire_type_2020 == -9999,]$fire_type_2020 <- NA
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
sev.pixel.data$fire.year <- sev.pixel.data$fire_year_2010
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
  treatment == 'Control' ~ 'No Fire',
  # fire.year >= 1910 & fire.year <=  1970 ~ '1910-1970',#'81-95',
  # fire.year >= 1936 & fire.year <= 1950 ~ '65-79',
  # fire.year >= 1951 & fire.year <= 1965 ~ '50-64',
  # fire.year >= 1951 & fire.year <= 1960 ~ '55-64',
  # fire.year >= 1971 & fire.year <= 1980 ~ '1971-1980',#'56-80',
  # fire.year >= 1985 & fire.year <= 1990 ~ '1985-1990',
  # fire.year >= 1991 & fire.year <= 2000 ~ '1991-2000',#'31-55', 
  # fire.year >= 1991 & fire.year <= 2000 ~ '15-24',
  fire.year >= 1985 & fire.year <= 2010 ~ '1985-2010',
  # fire.year >= 2001 & fire.year <= 2010 ~ '2001-2010',
  fire.year >= 2011 & fire.year <= 2018 ~ '2011-2017')) #,
# fire.year >= 2019 ~ '2019-2020'))#'0-4'))

#Fire Severity Bins
#With re-export type needs to be converted to sev
sev.pixel.data <- sev.pixel.data %>% mutate(sev.bin = case_when(
  fire_sev_2010 == '0' ~ 'No Fire',
  fire_sev_2010 == '1' | fire_sev_2010 == '2' ~ 'Unchanged or Low',
  fire_sev_2010 == '3' | fire_sev_2010 == '4' ~ 'Mid or High',
  fire_sev_2010 == '255' ~ 'Masked')) # end function

#Fire year bins for Fire Severity Data
sev.pixel.data$fire.year.bin = with(sev.pixel.data, factor(fire.year.bin, levels = c('2011-2017', '1985-2010', 'No Fire')))#c('0-4','5-30','31-55','56-80',

#Make the years bin lables in the correct order
sev.pixel.data$sev.bin = with(sev.pixel.data, factor(sev.bin, levels = c('No Fire','Masked', 'Unchanged or Low','Mid or High')))

#Recode the veg type data
sev.pixel.data$veg_name <- recode(.x=sev.pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
                                  '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
                                  '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

summary(sev.pixel.data)

sev.pixel.data$fire.year.bin = with(sev.pixel.data, factor(fire.year.bin, levels = c('2019-2020', '2011-2018', '1980-2010',  'No Fire')))#

#Recode the veg type data
# sev.pixel.data$veg_name <- recode(.x=sev.pixel.data$lf_evt_2001, .default = NA_character_, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
#                               '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
#                               '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')


#Filter the data into subsets for modeling
sev.pixel.filter <- sev.pixel.data %>% filter(fire.year <= 2010 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
                dplyr::group_by(system.index) %>% 
                summarize(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
                    RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
                    Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), 
                    sev.bin = sev.bin[vi.year == 2010],
                    treatment = treatment[vi.year == 2010])
sev.hi.control <- sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == "Mid or High")
sev.hi.disturb <- sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == "Mid or High")
sev.lo.control <- sev.pixel.filter %>% filter(treatment == 'Control' & sev.bin == "Unchanged or Low")
sev.lo.disturb <- sev.pixel.filter %>% filter(treatment == 'Disturb' & sev.bin == "Unchanged or Low")  
  

#Models for Wild Fire
sev.hi.control.lm <- lm(data = sev.hi.control, dTree~ Water_Stress) 
sev.hi.disturb.lm <- lm(data = sev.hi.disturb, dTree ~ Water_Stress) 

#Models for Rx Fire
sev.lo.control.lm <- lm(data = sev.lo.control, dTree~ Water_Stress) 
sev.lo.disturb.lm <- lm(data = sev.lo.disturb, dTree ~ Water_Stress) 

#Calculate the sgemented models
sev.hi.control.seg <- segmented(sev.hi.control.lm)
sev.hi.disturb.seg <- segmented(sev.hi.disturb.lm)
sev.lo.control.seg <- segmented(sev.lo.control.lm)
sev.lo.disturb.seg <- segmented(sev.lo.disturb.lm)

#Add predicted dNDMI values
sev.hi.control$dTree.predict = predict(sev.hi.control.seg)
sev.hi.disturb$dTree.predict = predict(sev.hi.disturb.seg)
sev.lo.control$dTree.predict = predict(sev.lo.control.seg)
sev.lo.disturb$dTree.predict = predict(sev.lo.disturb.seg)

#Add the segmented fits and Standard Errors
#Fits
sev.hi.control$dTree.fit = broken.line(sev.hi.control.seg)$fit
sev.hi.disturb$dTree.fit = broken.line(sev.hi.disturb.seg )$fit
sev.lo.control$dTree.fit = broken.line(sev.lo.control.seg)$fit
sev.lo.disturb$dTree.fit = broken.line(sev.lo.disturb.seg)$fit

#SE fit
sev.hi.control$dTree.se.fit = broken.line(sev.hi.control.seg)$se.fit
sev.hi.disturb$dTree.se.fit = broken.line(sev.hi.disturb.seg)$se.fit
sev.lo.control$dTree.se.fit = broken.line(sev.lo.control.seg)$se.fit
sev.lo.disturb$dTree.se.fit = broken.line(sev.lo.disturb.seg)$se.fit

#Recombine the data frames with the model fitted dNDMI as a column
sev.all.models <- rbind(sev.hi.control, sev.hi.disturb, sev.lo.control, sev.lo.disturb)

#R-Squared values for the four models
r2.a  <- format(summary(sev.hi.control.seg)$r.squared, digits = 2) #I could switch this back to segmented
r2.b <- format(summary(sev.hi.disturb.seg)$r.squared, digits = 2)
r2.c <- format(summary(sev.lo.control.seg)$r.squared, digits = 2)
r2.d <- format(summary(sev.lo.disturb.seg)$r.squared, digits = 2) #I could switch this back to segmented

#Create a data.frame of R.squared values
r2.text <- data.frame(
  label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 =r2.a)))), 
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.b)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.c)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.d))))
  ),
  treatment = c('Control', 'Disturb', 'Control', 'Disturb'),
  sev.bin = c('Mid or High', 'Mid or High', 'Unchanged or Low', 'Unchanged or Low'),
  x = c(3500, 3500, 3500, 3500),
  y = c(-20, -20, -20, -20)
)

# letter.text <- data.frame(label = c("a)", "b)", "c)", "d)"),
#                           sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
#                           drought = c('1999-2002', '2012-2015', '1999-2002',  '2012-2015'),
#                           y     = c(-0.3, -0.3, -0.3, -0.3),
#                           x     = c(-2400, -2400, -2400, -2400)
# )

#Create the figure
p1 <- ggplot(data = sev.all.models) +
  #Create the density layer
  geom_bin2d(binwidth = c(200, 1), mapping = aes(x = Water_Stress, y = dTree, group = ..count..)) +
  #Piecewise linear regression fit line
  geom_line(mapping = aes(x=Water_Stress, y=dTree.fit), size=2, color = 'black', linetype = 'dotdash') +
  #Piecewise fit uncertainty
  geom_ribbon(mapping = aes(x = Water_Stress, y = dTree.fit, ymax = dTree.fit + 1.96*dTree.se.fit, ymin = dTree.fit - 1.96*dTree.se.fit), alpha = 0.4) +  
  
  scale_fill_gradient2(limits = c(0,800), breaks = c(0,200,400,600), midpoint = 400, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
    facet_grid(sev.bin ~ treatment) +
  # scale_alpha(range = c(1, 1), limits = c(50, 1000), na.value = 0.4) +
  # stat_cor( mapping = aes(x = Water_Stress, y = dTree), color = 'black') + facet_grid(fire.type.bin ~ treatment) +
  labs(fill = "Grid Cells") +
  #Add the R^2 values
  geom_text(data = r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  #Add the R^2 text
  # geom_text(data = letter.text, mapping = aes(x = x, y = y, label = label), size = 5, fontface = "bold") +
  theme_bw() +
  xlab(expression('Four-year Pr-ET (mm 4yr'^-1*')')) + ylab('Die-off (Relative dTree %)')
p1

p2 <- p1 + theme(
  legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
  legend.justification = c(1, 0),
  legend.position = c(0.15, 0.55),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.direction = "vertical") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3,
                               title.position = "top", 
                               title.hjust = 0.5, 
                               ticks.colour = "black"))

p2

ggsave(filename = 'Fig6c_sev_water_stress_dTree_300m.png', height=16, width= 16, units = 'cm', dpi=900)
