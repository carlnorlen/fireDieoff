#Author: Carl Norlen
#Date Created: January 23, 2023
#Date Updated: January 24, 2023
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
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures')

#The data directory
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


#Filter the data into subsets for modeling
pixel.filter <- pixel.data %>% filter(fire.year <= 2010 & fire.year >= 1921 & Tree_Cover > 0 & (fire_year_2019 <= 2010 | is.na(fire_year_2019))) %>%
                dplyr::group_by(system.index) %>% 
                summarize(dTree = mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
                    RdTree = (mean(Tree_Cover[vi.year %in% c(2017, 2018)]) - mean(Tree_Cover[vi.year %in% c(2013,2014)])) / mean(Tree_Cover[vi.year %in% c(2013, 2014)]),
                    Water_Stress = sum(PrET[vi.year %in% c(2012,2013,2014,2015)]), 
                    fire.year.bin = fire.year.bin[vi.year == 2010],
                    treatment = treatment[vi.year == 2010],
                    fire.type.bin = fire.type.bin[vi.year == 2010])
wild.control <- pixel.filter %>% filter(fire.year.bin == 'No Fire' & fire.type.bin == "Wildfire")
wild.disturb <- pixel.filter %>% filter(fire.year.bin == '1980-2010' & fire.type.bin == "Wildfire")
rx.control <- pixel.filter %>% filter(fire.year.bin == 'No Fire' & fire.type.bin == "Rxfire")
rx.disturb <- pixel.filter %>% filter(fire.year.bin == '1980-2010' & fire.type.bin == "Rxfire")  
  

#Models for Wild Fire
wild.control.lm <- lm(data = wild.control, dTree~ Water_Stress) 
wild.disturb.lm <- lm(data = wild.disturb, dTree ~ Water_Stress) 

#Models for Rx Fire
rx.control.lm <- lm(data = rx.control, dTree~ Water_Stress) 
rx.disturb.lm <- lm(data = rx.disturb, dTree ~ Water_Stress) 

#Calculate the sgemented models
wild.control.seg <- segmented(wild.control.lm)
wild.disturb.seg <- segmented(wild.disturb.lm)
rx.control.seg <- segmented(rx.control.lm)
rx.disturb.seg <- segmented(rx.disturb.lm)

#Add predicted dNDMI values
wild.control$dTree.predict = predict(wild.control.seg)
wild.disturb$dTree.predict = predict(wild.disturb.seg)
rx.control$dTree.predict = predict(rx.control.seg)
rx.disturb$dTree.predict = predict(rx.disturb.seg)

#Add the segmented fits and Standard Errors
#Fits
wild.control$dTree.fit = broken.line(wild.control.seg)$fit
wild.disturb$dTree.fit = broken.line(wild.disturb.seg )$fit
rx.control$dTree.fit = broken.line(rx.control.seg)$fit
rx.disturb$dTree.fit = broken.line(rx.disturb.seg)$fit

#SE fit
wild.control$dTree.se.fit = broken.line(wild.control.seg)$se.fit
wild.disturb$dNDMI.se.fit = broken.line(wild.disturb.seg)$se.fit
rx.control$dNDMI.se.fit = broken.line(rx.control.seg)$se.fit
rx.disturb$dNDMI.se.fit = broken.line(rx.disturb.seg)$se.fit

#Recombine the data frames with the model fitted dNDMI as a column
all.models <- rbind(wild.control, wild.disturb, rx.control, rx.disturb)

#R-Squared values for the four models
r2.a  <- format(summary(wild.control.seg)$r.squared, digits = 2) #I could switch this back to segmented
r2.b <- format(summary(wild.disturb.seg)$r.squared, digits = 2)
r2.c <- format(summary(rx.control.seg)$r.squared, digits = 2)
r2.d <- format(summary(rx.disturb.seg)$r.squared, digits = 2) #I could switch this back to segmented

#Create a data.frame of R.squared values
r2.text <- data.frame(
  label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 =r2.a)))), 
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.b)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.c)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.d))))
  ),
  fire.year.bin = c('No Fire', '1980-2010', 'No Fire', '1980-2010'),
  fire.type.bin = c('Wildfire', 'Wildfire', 'Rxfire', 'Rxfire'),
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
p1 <- ggplot(data = all.models) +
  #Create the density layer
  geom_bin2d(binwidth = c(200, 1), mapping = aes(x = Water_Stress, y = dTree, group = ..count..)) +
  #Piecewise linear regression fit line
  geom_line(mapping = aes(x=Water_Stress, y=dTree.fit), size=2, color = 'black', linetype = 'dotdash') +
  #Piecewise fit uncertainty
  geom_ribbon(mapping = aes(x = Water_Stress, y = dTree.fit, ymax = dTree.fit + 1.96*dTree.se.fit, ymin = dTree.fit - 1.96*dNDMI.se.fit), alpha = 0.4) +  
  
  scale_fill_gradient2(limits = c(0,1200), breaks = c(0,300,600,900), midpoint = 600, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
    facet_grid(fire.type.bin ~ fire.year.bin) +
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

ggsave(filename = 'Fig6a_water_stress_dTree_300m.png', height=16, width= 16, units = 'cm', dpi=900)
