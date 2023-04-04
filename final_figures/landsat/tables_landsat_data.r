#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: May 12, 2020
#Purpose: Create merge split raster files

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/chrono
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/chrono
#Run the script: R < stand_age.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggplot2', 'magrittr', 'stats', 'patchwork','ggpmisc', 'raster', 'RStoolbox', 'quantreg','segmented', 'RColorBrewer',
	   'gt', 'gtsummary', 'webshot', 'stargazer', 'kableExtra', 'broom', 'svglite','sjPlot','purrr', 'sjmisc', 'magick', 'magrittr', 'knitr', 'xtable')
# install.packages('quantreg',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\CECS"
dir <- "D:\\Large_Files\\CECS"
memory.limit(32000)

# grand.mtbs <- read.csv(file.path(dir, 'CAfiresALL_LS578_median_annual_100m_v2.csv'))
# FRAP grand table
# stand.age <- read.csv(file.path(dir, 'California_stand_age_die-off_300m_v8.csv'))
# stand.age <- read.csv(file.path(dir, 'California_stand_age_die-off_450m.csv'))
stand.age <- read.csv(file.path(dir, 'Sierra_stand_age_die-off_450m_v6.csv'))
# stand.age.norcal <- read.csv(file.path(dir, 'NorCal_stand_age_300m_v2.csv'))
# stand.age.socal <- read.csv(file.path(dir, 'SoCal_stand_age_300m_v2.csv'))
# grand.frap <- read.csv(file.path(dir, 'FRAPsimpleALL_LS578_multireducer_LS578_annual_500m_reducer2.csv'))
# summary(grand.frap)
# head(grand.table)

stand.age$veg_name <- recode(.x=stand.age$type_2001_mode, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
									'2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
									'2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

# stand.age <- subset(stand.age, veg_name != 'Subalpine' & veg_name != 'Blue Oak Woodland' & veg_name != 'Oregon White Oak' & veg_name != 'Blue Oak - Digger Pine' & veg_name != 'Bristlecone Pine') 

stand.age$type_2001_mode <- factor(stand.age$type_2001_mode)

# stand.age.norcal$veg_name <- recode(.x=stand.age.norcal$type_2001_mode, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
									# '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
									# '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

# stand.age.norcal <- subset(stand.age.norcal, veg_name != 'Subalpine' & veg_name != 'Blue Oak Woodland' & veg_name != 'Oregon White Oak' & veg_name != 'Blue Oak - Digger Pine' & veg_name != 'Bristlecone Pine') 

# stand.age.norcal$type_2001_mode <- factor(stand.age.norcal$type_2001_mode)

# stand.age.socal$veg_name <- recode(.x=stand.age.socal$type_2001_mode, '2015' = 'Redwood', '2019' = 'Pinyon Juniper', '2020' = 'Bristlecone Pine', '2027' = 'Mixed Conifer', '2028' = 'White Fir', '2031' = 'Jeffrey Pine',
									# '2032' = 'Red Fir', '2033' = 'Subalpine', '2034' = 'Knobcone Pine', '2043' = 'Mixed Conifer', '2044' = 'Subalpine', '2045' = 'Mixed Conifer', 
									# '2053' = 'Ponderosa Pine', '2058' = 'Lodgepole Pine', '2061' = 'Mixed Conifer', '2112' = 'Blue Oak Woodland', '2172' = 'White Fir', '2173' = 'Lodgepole Pine', '2201' = 'Oregon White Oak', '2230' = 'Blue Oak - Digger Pine')

# stand.age.socal <- subset(stand.age.socal, veg_name != 'Subalpine' & veg_name != 'Blue Oak Woodland' & veg_name != 'Oregon White Oak' & veg_name != 'Blue Oak - Digger Pine' & veg_name != 'Bristlecone Pine') 

# stand.age.socal$type_2001_mode <- factor(stand.age.socal$type_2001_mode)

#Convert ADS to trees/hectare
stand.age$ADS_2017_mean <- stand.age$ADS_2017_mean * 2.41705
stand.age$dNDMI_2015_mean.transform <- stand.age$dNDMI_2015_mean
stand.age$dNDMI_2015_mean.transform[stand.age$dNDMI_2015_mean.transform > 0] <- 0
p1 <-  ggplot(data = stand.age, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # geom_point(mapping = aes(color = veg_name), shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 60) +
	   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_wrap(~veg_name, scale="free_y") +
	   theme_bw() + xlim(0, 140) +
	   # facet_grid(type_2001_mode ~ .) +
	   # theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) + #Presentation text sizes.
	   # ylim(-100, 500) + xlim(-100, 25) +
	   ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')	

f1 <- ggarrange(p1, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig4_stand_age_die-off_comparison.png', height=160, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig4_stand_age_die-off_comparison.eps', height=160, width=210, units = 'mm', dpi=300)

p2 <-  ggplot(data = stand.age, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 60) +
	   geom_smooth(method = "lm", formula = y ~ x , size = 2, se = FALSE, color = 'black', linetype = 'dashed') + 
	   # stat_cor(method = "pearson", color = 'black') +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   theme_bw() + xlim(0, 140) +
	   # facet_grid(type_2001_mode ~ .) +
	   # theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) + #Presentation text sizes.
	   # ylim(-100, 500) + xlim(-100, 25) +
	   ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')		

f2 <- ggarrange(p2, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig5_stand_age_die-off_comparison.png', height=120, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig5_stand_age_die-off_comparison.eps', height=120, width=210, units = 'mm', dpi=300)

# p3 <-  ggplot(data = stand.age.norcal, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # # geom_point(shape = 21) +
	   # geom_bin2d(alpha = 0.5, bins = 60) +
	   # geom_smooth(method = "lm", formula = y ~ x , size = 2, se = FALSE, color = 'black', linetype = 'dashed') + 
	   # stat_cor(method = "pearson", color = 'black') +
	   # geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # # facet_wrap(~veg_name, scale="free_y") +
	   # theme_bw() + xlim(0, 140) +
	   # # facet_grid(type_2001_mode ~ .) +
	   # # theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) + #Presentation text sizes.
	   # # ylim(-100, 500) + xlim(-100, 25) +
	   # ggtitle("Northern California") +
	   # ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')

# p4 <-  ggplot(data = stand.age.socal, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # # geom_point(shape = 21) +
	   # geom_bin2d(alpha = 0.5, bins = 60) +
	   # geom_smooth(method = "lm", formula = y ~ x , size = 1, se = FALSE, color = 'red') + 
	   # stat_cor(method = "pearson", color = 'red') +
	   # geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # # facet_wrap(~veg_name, scale="free_y") +
	   # theme_bw() + xlim(0, 140) +
	   # # facet_grid(type_2001_mode ~ .) +
	   # # theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) + #Presentation text sizes.
	   # # ylim(-100, 500) + xlim(-100, 25) +
	   # ggtitle("Southern California") +
	   # ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')

# f3 <- ggarrange(p3, p4, ncol = 1, nrow = 2, common.legend = TRUE, legend = 'bottom')

# ggsave(filename = 'Fig6_stand_age_die-off_comparison.png', height=220, width=210, units = 'mm', dpi=300)
# ggsave(filename = 'Fig6_stand_age_die-off_comparison.eps', height=220, width=210, units = 'mm', dpi=300)

# p5 <-  ggplot(data = stand.age.norcal, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # # geom_point(mapping = aes(color = veg_name), shape = 21) +
	   # geom_bin2d(alpha = 0.5, bins = 60) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   # geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~veg_name, scale="free_y") +
	   # theme_bw() + xlim(0, 140) +
	   # # facet_grid(type_2001_mode ~ .) +
	   # # theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) + #Presentation text sizes.
	   # # ylim(-100, 500) + xlim(-100, 25) +
	   # ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')	

# f4 <- ggarrange(p5, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

# ggsave(filename = 'Fig7_norcal_stand_age_die-off_comparison.png', height=160, width=210, units = 'mm', dpi=300)
# ggsave(filename = 'Fig7_norcal_stand_age_die-off_comparison.eps', height=160, width=210, units = 'mm', dpi=300)

# p6 <-  ggplot(data = stand.age.socal, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # # geom_point(mapping = aes(color = veg_name), shape = 21) +
	   # geom_bin2d(alpha = 0.5, bins = 60) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   # geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~veg_name, scale="free_y") +
	   # theme_bw() + xlim(0, 140) +
	   # # facet_grid(type_2001_mode ~ .) +
	   # # theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) + #Presentation text sizes.
	   # # ylim(-100, 500) + xlim(-100, 25) +
	   # ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')	

# f5 <- ggarrange(p6, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

# ggsave(filename = 'Fig8_socal_stand_age_die-off_comparison.png', height=160, width=210, units = 'mm', dpi=300)
# ggsave(filename = 'Fig8_socal_stand_age_die-off_comparison.eps', height=160, width=210, units = 'mm', dpi=300)

#Group the csv into bins by stand age.
stand.age <- stand.age %>% mutate(agegroup = case_when(stand_age_mean >= 0  & stand_age_mean <= 35 ~ '0-35',
                                             stand_age_mean > 35  & stand_age_mean <= 70 ~ '36-70',
                                             stand_age_mean > 70  & stand_age_mean <= 105 ~ '71-105',
											 stand_age_mean > 105 ~ '106-140+'))
											 # stand_age_mean > 120 & stand_age_mean <= 139 ~ '121-139',
											 # stand_age_mean > 100 & stand_age_mean <= 120 ~ '101-120',
											 # stand_age_mean > 120 & stand_age_mean <= 139 ~ '121-139',
											 # stand_age_mean > 139 ~ '140+')) # end function
stand.age$agegroup = with(stand.age, factor(agegroup, levels = c('0-35', '36-70', '71-105', '106-140+')))

# print(stand.age)

p7 <-  ggplot(data = stand.age) + geom_density(mapping = aes(x = dNDMI_2015_mean, after_stat(scaled))) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_wrap(~agegroup, scale="free_y") +
	   theme_bw() + #xlim(-0.6, 0.6) +
	   ggtitle('Stand Age') +
	   xlab('dNDMI 2016')	

f6 <- ggarrange(p7, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig9_stand_age_die-off_pdf.png', height=160, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig9_stand_age_die-off_pdf.eps', height=160, width=210, units = 'mm', dpi=300)

p8 <-  ggplot(data = stand.age) + geom_density(mapping = aes(x = dNDMI_2015_mean, after_stat(scaled))) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_grid(agegroup~veg_name) +
	   theme_bw() + #xlim(-0.6, 0.6) +
	   xlab('dNDMI 2016')	

f7 <- ggarrange(p8, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig10_stand_age_die-off_veg_type_pdf.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig10_stand_age_die-off_veg_type_pdf.eps', height=160, width=260, units = 'mm', dpi=300)

# p9 <-  ggplot(data = stand.age.socal, mapping = aes(x = PET_mean, y = dNDMI_2015_mean)) +
	   # # geom_point(mapping = aes(color = veg_name), shape = 21) +
	   # geom_bin2d(alpha = 0.5, bins = 60) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 2) +
	   # # stat_cor(method = "pearson") +
	   # geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~veg_name, scale="free_y") +
	   # theme_bw() + #ylim(-0.6, 0.6) + #xlim(0, 120) +
	   # ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Pr-ET 2015 (mm / 4 yr)')	

# f8 <- ggarrange(p9, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

# ggsave(filename = 'Fig11_PrET_die-off_comparison.png', height=160, width=210, units = 'mm', dpi=300)
# ggsave(filename = 'Fig11_PrET_die-off_comparison.eps', height=160, width=210, units = 'mm', dpi=300)

#Group the csv into bins by control group
stand.age <- stand.age %>% mutate(PET.control = case_when(PET_4yr_2015_mean >= 500 ~ '500+',
                                             PET_4yr_2015_mean >= 0  &  PET_4yr_2015_mean < 500 ~ '0 to 499',
                                             PET_4yr_2015_mean >= -500  & PET_4yr_2015_mean < 0 ~ '-1 to -500',
											 PET_4yr_2015_mean >= -1000 & PET_4yr_2015_mean < -500 ~ '-501 to -1000',
											 PET_4yr_2015_mean < -1000 ~ '< -1000')) # end function

stand.age <- stand.age %>% mutate(SPI48.control = case_when(spi48_2015_mean >= -2 ~ '>= -2',
                                             spi48_2015_mean >= -2.25  &  spi48_2015_mean < -2 ~ '-2.25 to -2',
                                             spi48_2015_mean >= -2.5  & spi48_2015_mean < -2.25 ~ '-2.5 to -2.25',
											 spi48_2015_mean >= -2.75 & PET_4yr_2015_mean < -2.5 ~ '-2.75 to -2.5',
											 spi48_2015_mean < -2.75 ~ '< -2.75')) # end function

stand.age <- stand.age %>% mutate(elevation.control = case_when(elevation_mean >= 2500 ~ '2500+',
											 elevation_mean >= 2000 & elevation_mean < 2500 ~ '2000-2499',
                                             elevation_mean >= 1500  &  elevation_mean < 2000 ~ '1500-1999',
                                             elevation_mean >= 1000  & elevation_mean < 1500 ~ '1000-1499',
											 elevation_mean < 1000 ~ '0-999')) # end function

stand.age$PET.control = with(stand.age, factor(PET.control, levels = c('< -1000', '-501 to -1000', '-1 to -500', '0 to 499', '500+')))
stand.age$SPI48.control = with(stand.age, factor(SPI48.control, levels = c('< -2.75', '-2.75 to -2.5', '-2.5 to -2.25', '-2.25 to -2', '>= -2')))
stand.age$elevation.control = with(stand.age, factor(elevation.control, levels = c('0-999', '1000-1499', '1500-1999', '2000-2499', '2500+')))

stand.age.controls <- stand.age %>% group_by(PET.control, elevation.control) %>% summarize(dNDMI_2015_mean.count = n(), dNDMI_2015_mean.control = mean(dNDMI_2015_mean), ADS_2017_mean.control = mean(ADS_2017_mean), biomass_2012_mean.control = mean(biomass_2012_mean))

stand.age.SPI48.controls <- stand.age %>% group_by(SPI48.control, elevation.control) %>% summarize(dNDMI_2015_mean.count = n(), dNDMI_2015_mean.control = mean(dNDMI_2015_mean), ADS_2017_mean.control = mean(ADS_2017_mean), biomass_2012_mean.control = mean(biomass_2012_mean))
# print(stand.age.controls)

#Create plot that create two rasters with each square being a potential control sample.
p10 <- ggplot(data = stand.age.controls, aes(x=PET.control, y=elevation.control)) + geom_raster(aes(fill = dNDMI_2015_mean.control)) +
	   scale_fill_viridis(name = "dNDMI", direction = -1, option = "magma") +
	   xlab('Pr-ET (mm)') + ylab('Elevation (m)') + theme_bw()
	  
p11 <- ggplot(data = stand.age.controls, aes(x=PET.control, y=elevation.control)) + geom_raster(aes(fill = dNDMI_2015_mean.count)) +
	   scale_fill_viridis(name = "Count", direction = 1, option = "inferno") +
	   xlab('Pr-ET (mm)') + ylab('Elevation (m)') + theme_bw()

f9 <- ggarrange(p10, p11, ncol = 2, nrow = 1, common.legend = FALSE, legend = 'bottom')

ggsave(filename = 'Fig12_PrET_elevation_controls_raster.png', height=120, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig12_PrET_elevation_controls_raster.eps', height=120, width=210, units = 'mm', dpi=300)

p10a <- ggplot(data = stand.age.SPI48.controls, aes(x=SPI48.control, y=elevation.control)) + geom_raster(aes(fill = dNDMI_2015_mean.control)) +
	   scale_fill_viridis(name = "dNDMI", direction = -1, option = "magma") +
	   xlab('SPI48') + ylab('Elevation (m)') + theme_bw()
	  
p11a <- ggplot(data = stand.age.SPI48.controls, aes(x=SPI48.control, y=elevation.control)) + geom_raster(aes(fill = dNDMI_2015_mean.count)) +
	   scale_fill_viridis(name = "Count", direction = 1, option = "inferno") +
	   xlab('SPI48)') + ylab('Elevation (m)') + theme_bw()

f9a <- ggarrange(p10a, p11a, ncol = 2, nrow = 1, common.legend = FALSE, legend = 'bottom')

ggsave(filename = 'Fig12a_SPI48_elevation_controls_raster.png', height=120, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig12a_SPI48_elevation_controls_raster.eps', height=120, width=210, units = 'mm', dpi=300)

#Make pdf plots of certain sub-samples of data
stand.age.sample <- subset(stand.age, PET_4yr_2015_mean <= -1000 & elevation_mean >= 1000 & stand_age_mean > 20)
stand.age.control <- subset(stand.age, PET_4yr_2015_mean >= -500 & elevation_mean >= 1000 & stand_age_mean > 20)

stand.age.sample.all <- subset(stand.age, PET_4yr_2015_mean <= -1000 & elevation_mean >= 1000)
# stand.age.sample.all <- subset(stand.age, spi48_2015_mean <= -2 & elevation_mean >= 1000)
stand.age.control.all <- subset(stand.age, PET_4yr_2015_mean >= -500 & elevation_mean >= 1000)
# stand.age.control.all <- subset(stand.age, spi48_2015_mean > -2 & elevation_mean >= 1000)

#Create binned chronosequences
stand.age.sample.chrono <- stand.age.sample.all %>% group_by(agegroup) %>% summarize(dNDMI_2015.count = n(), dNDMI_2015.mean = mean(dNDMI_2015_mean), dNDMI_2015.sd = sd(dNDMI_2015_mean), ADS_2017.mean = mean(ADS_2017_mean), 
																   ADS_2017.sd = sd(ADS_2017_mean), biomass_2012.mean = mean(biomass_2012_mean), biomass_2012.sd = sd(biomass_2012_mean), NDMI_2012.mean = mean(NDMI_2012_mean), 
																   NDMI_2012.sd = sd(NDMI_2012_mean), PET_4yr_2015.mean = mean(PET_4yr_2015_mean), PET_4yr_2015.sd = sd(PET_4yr_2015_mean), 
																   stand_age.mean = mean(stand_age_mean), stand_age.sd = sd(stand_age_mean))

stand.age.control.chrono <- stand.age.control.all %>% group_by(agegroup) %>% summarize(dNDMI_2015.count = n(), dNDMI_2015.mean = mean(dNDMI_2015_mean), dNDMI_2015.sd = sd(dNDMI_2015_mean), ADS_2017.mean = mean(ADS_2017_mean), 
																   ADS_2017.sd = sd(ADS_2017_mean), biomass_2012.mean = mean(biomass_2012_mean), biomass_2012.sd = sd(biomass_2012_mean), NDMI_2012.mean = mean(NDMI_2012_mean), 
																   NDMI_2012.sd = sd(NDMI_2012_mean), PET_4yr_2015.mean = mean(PET_4yr_2015_mean), PET_4yr_2015.sd = sd(PET_4yr_2015_mean), 
																   stand_age.mean = mean(stand_age_mean), stand_age.sd = sd(stand_age_mean))

q10 <- seq(0.1, 0.9, by = 0.2)

q20 <- seq(0.5, 0.9, by = 0.05)
															   
#Overall Trends by sample regions
p12 <- ggplot(data = stand.age.sample, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, , binwidth = c(5, 0.04)) +
	   scale_fill_gradient2(limits = c(0,150), midpoint = 75, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'red') +
	   geom_quantile(quantiles = q10) +
	   # stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~veg_name, scale="free_y") +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 140) + ggtitle('Exposed: Pr-ET <= -1000 (mm) & elevation >= 1000 (m)') +
	   ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')	

p13 <- ggplot(data = stand.age.control, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, , binwidth = c(5, 0.04)) +
	   scale_fill_gradient2(limits = c(0,150), midpoint = 75, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'red') +
	   geom_quantile(quantiles = q10) +
	   # stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~veg_name, scale="free_y") +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 140) + ggtitle('Not Exposed') +
	   ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')	

f10 <- ggarrange(p12, p13, ncol = 1, nrow = 2, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig13_controlled_stand_age_die-off_comparison.png', height=17, width=16, units = 'cm', dpi=900)
ggsave(filename = 'Fig13_controlled_stand_age_die-off_comparison.eps', height=17, width=16, units = 'cm', dpi=900)

p14 <-  ggplot(data = stand.age.sample.all) + geom_density(mapping = aes(x = dNDMI_2015_mean, after_stat(scaled), color = agegroup), size = 2, alpha = 0.3) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Stand Ages') + #values = c('black', 'red', 'blue', 'gray'), 
	   theme_dark() + ggtitle('Treatment') +
	   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
	   axis.title.y = element_text(size = 10), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.text=element_text(size=10), legend.title = element_text(size=10)) +
	   xlab('dNDMI 2017') + ylab('Scaled Density') + xlim(-0.35, 0.15)	

#f11 <- ggarrange(p14, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

# ggsave(filename = 'Fig14_sample_stand_age_die-off_vegtype_pdf.png', height=160, width=210, units = 'mm', dpi=600)
# ggsave(filename = 'Fig14_sample_stand_age_die-off_vegtype_pdf.eps', height=160, width=210, units = 'mm', dpi=600)

p17 <-  ggplot(data = stand.age.control.all) + geom_density(mapping = aes(x = dNDMI_2015_mean, after_stat(scaled), color = agegroup), size = 2, alpha = 0.3) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Stand Ages') +
	   theme_dark() + ggtitle('Not Exposed') +
	   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
       axis.title.y = element_text(size = 10), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.text=element_text(size=10), legend.title = element_text(size=10)) +
	   xlab('dNDMI 2017') + ylab('Scaled Density') + xlim(-0.35, 0.15)	

f14 <- ggarrange(p14, p17, ncol = 2, nrow = 1, common.legend = TRUE, labels = c('A', 'B'),  legend = 'bottom')

ggsave(filename = 'Fig17_control_stand_age_die-off_vegtype_pdf.png', height=8, width=16, units = 'cm', dpi=900)
ggsave(filename = 'Fig17_control_stand_age_die-off_vegtype_pdf.eps', height=8, width=16, units = 'cm', dpi=900)

p15 <-  ggplot(data = stand.age.sample) + geom_density(mapping = aes(x = dNDMI_2015_mean, after_stat(scaled))) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_grid(agegroup~veg_name) +
	   theme_bw() + ggtitle('Exposed: Pr-ET <= -1000 (mm) & elevation >= 1000 (m)') +
	   xlab('dNDMI 2017')	

f12 <- ggarrange(p15, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig15_stand_age_die-off_veg_type_age_pdf.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig15_stand_age_die-off_veg_type_age_pdf.eps', height=160, width=260, units = 'mm', dpi=300)

#Overall Trends by sample regions
p16 <- ggplot(data = stand.age, mapping = aes(x = stand_age_mean, y = dNDMI_2015_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 60) +
	   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 2, linetype = 'dashed') +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 2) +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 140) + ggtitle('Elevation (horizontal) vs Pr-ET (vertical) ') +
	   ylab('Die-off (NDMI 2017 minus NDMI 2010-12)') +  xlab('Stand Age (2017 - fire year)')	

f13 <- ggarrange(p16, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig16_stand_age_die-off_PET_elevation_pdf.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig16_stand_age_die-off_PET_elevation_pdf.eps', height=160, width=260, units = 'mm', dpi=300)

p18 <-  ggplot(data = stand.age.control) + geom_density(mapping = aes(x = dNDMI_2015_mean, after_stat(scaled))) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_grid(agegroup~veg_name) +
	   theme_bw() + ggtitle('Not Exposed: Pr-ET >= 500 (mm) & elevation >= 1000 (m)') +
	   xlab('dNDMI 2017')	

f15 <- ggarrange(p18, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig18_control_stand_age_die-off_veg_type_age_pdf.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig18_control_stand_age_die-off_veg_type_age_pdf.eps', height=160, width=260, units = 'mm', dpi=300)

#Create plot that create two rasters with each square being a potential control sample.
p19 <- ggplot(data = stand.age.controls, aes(x=PET.control, y=elevation.control)) + geom_raster(aes(fill = ADS_2017_mean.control)) +
	   scale_fill_viridis(name = "dead trees/ha", direction = 1, option = "magma") +
	   xlab('Pr-ET (mm)') + ylab('Elevation (m)') + theme_bw()
	  
p20 <- ggplot(data = stand.age.controls, aes(x=PET.control, y=elevation.control)) + geom_raster(aes(fill = biomass_2012_mean.control)) +
	   scale_fill_viridis(name = "Biomass (Mg/ha)", direction = 1, option = "inferno") +
	   xlab('Pr-ET (mm)') + ylab('Elevation (m)') + theme_bw()

f16 <- ggarrange(p19, p20, ncol = 2, nrow = 1, common.legend = FALSE, legend = 'bottom')

ggsave(filename = 'Fig19_ADS_biomass_by_controls_raster.png', height=120, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig19_ADS_biomass_by_controls_raster.eps', height=120, width=210, units = 'mm', dpi=300)

p21 <-  ggscatter(stand.age, x = "dNDMI_2015_mean", y = "ADS_2017_mean", point = FALSE,
	   #color = 'black', shape = 21, size = 1, # Points color, shape and size
	   # palette = 'viridis',
	   add = "reg.line",  # Add regression line
	   add.params = list(color = 'black', size =2, linetype = 'dotdash'), # Customize reg. line
	   conf.int = FALSE # Add confidence interval
	   ) +
	   geom_bin2d(alpha = 0.6, bins = 60) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   theme_bw() + #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), size = 3.5, label.y.npc="top", color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   scale_fill_gradient2(limits = c(0,200), midpoint = 100, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'red') +
	   # scale_fill_viridis() +
	   ylab('Mortality (trees/ha)') +  xlab('dNDMI 2017')
	   
p21a <-  ggscatter(stand.age, x = "dNDMI_2015_mean.transform", y = "ADS_2017_mean", point = FALSE,
	   #color = 'black', shape = 21, size = 1, # Points color, shape and size
	   # palette = 'viridis',
	   add = "reg.line",  # Add regression line
	   add.params = list(color = 'black', size =2, linetype = 'dotdash'), # Customize reg. line
	   conf.int = FALSE # Add confidence interval
	   ) +
	   geom_bin2d(alpha = 0.6, bins = 60) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   theme_bw() + #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), size = 3.5, label.y.npc="top", color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   scale_fill_gradient2(limits = c(0,200), midpoint = 100, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'red') +
	   # scale_fill_viridis() +
	   ylab('Mortality (trees/ha)') +  xlab('dNDMI 2017')

f17 <- ggarrange(p21, p21a, ncol = 2, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig20_dNDMI_ADS_comparison.png', device = 'png', height=8, width=14, units = 'cm', dpi=900)
ggsave(filename = 'Fig20_dNDMI_ADS_comparison.svg', device = 'svg', height=8, width=14, units = 'cm', dpi=900)

p22 <-  ggplot(data = stand.age) + geom_density(mapping = aes(x = ADS_2017_mean, after_stat(scaled))) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_grid(agegroup~veg_name) +
	   theme_bw() + 
	   ggtitle('Veg Type (horizontal) vs Stand Age (vertical)') +
	   ylab('Density') +  xlab('Die-off (trees/hectare)')	

f18 <- ggarrange(p22, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig21_stand_age_die-off_ADS_veg_type_pdf.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig21_stand_age_die-off_ADS_veg_type_pdf.eps', height=160, width=260, units = 'mm', dpi=300)

p23 <-  ggplot(data = stand.age, mapping = aes(x = stand_age_mean, y = ADS_2017_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 100) +
	   geom_smooth(method = "lm", formula = y ~ x , size = 1, se = FALSE, color = 'black', linetype = 'dashed') + 
	   # stat_cor(method = "pearson", color = 'black') +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   theme_bw() + xlim(0, 140) +
	   ylab('Die-off (trees/hectare)') +  xlab('Stand Age (2017 - fire year)')		

f19 <- ggarrange(p23, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig22_stand_age_die-off_ADS_comparison.png', height=120, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig22_stand_age_die-off_ADS_comparison.eps', height=120, width=210, units = 'mm', dpi=300)

#Overall Trends by sample regions
p24 <- ggplot(data = stand.age.sample, mapping = aes(x = stand_age_mean, y = ADS_2017_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 60) +
	   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 2, linetype = 'dashed') +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~veg_name, scale="free_y") +
	   theme_bw() + xlim(0, 140) + ggtitle('Exposed: Pr-ET <= -1000 (mm) & elevation >= 1000 (m)') +
	   ylab('Die-off (trees/hectare)') +  xlab('Stand Age (2017 - fire year)')	

p25 <- ggplot(data = stand.age.control, mapping = aes(x = stand_age_mean, y = ADS_2017_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 60) +
	   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 2, linetype = 'dashed') +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~veg_name, scale="free_y") +
	   theme_bw() + xlim(0, 140) + ggtitle('Not Exposed: Pr-ET >= 500 (mm) & elevation >= 1000 (m)') +
	   ylab('Die-off (trees/hectare)') +  xlab('Stand Age (2017 - fire year)')	

f20 <- ggarrange(p24, p25, ncol = 1, nrow = 2, common.legend = TRUE, labels = c('A', 'B'),  legend = 'bottom')

ggsave(filename = 'Fig23_controlled_stand_age_die-off_comparison.png', height=220, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig23_controlled_stand_age_die-off_comparison.eps', height=200, width=210, units = 'mm', dpi=300)

p26 <-  ggplot(data = stand.age.sample) + geom_density(mapping = aes(x = ADS_2017_mean, after_stat(scaled), color = agegroup), size = 2) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Stand Ages') +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~agegroup, scale="free_y") +
	   theme_dark() + ggtitle('Treatment') +
	   xlab('Die-off (trees/hectare)')	

p27 <-  ggplot(data = stand.age.control) + geom_density(mapping = aes(x = ADS_2017_mean, after_stat(scaled), color = agegroup), size =3 ) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Stand Ages') +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_wrap(~agegroup, scale="free_y") +
	   theme_dark() + ggtitle('Not Exposed') +
	   xlab('Die-off (trees/hectare)')	

f22 <- ggarrange(p26, p27, ncol = 2, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig25_control_stand_age_die-off_vegtype_pdf.png', height=8, width=16, units = 'cm', dpi=900)
ggsave(filename = 'Fig25_control_stand_age_die-off_vegtype_pdf.eps', height=8, width=16, units = 'cm', dpi=900)

p28 <-  ggplot(data = stand.age) + geom_density(mapping = aes(x = ADS_2017_mean, after_stat(scaled), color = agegroup)) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_wrap(~agegroup, scale="free_y") +
	   theme_bw() + xlab('Die-off (trees/hectare)')	

f23 <- ggarrange(p28, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig26_stand_age_die-off_all_veg_type_age_pdf.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig26_stand_age_die-off_all_veg_type_age_pdf.eps', height=160, width=260, units = 'mm', dpi=300)

p29 <-  ggplot(data = stand.age.sample) + geom_density(mapping = aes(x = ADS_2017_mean, after_stat(scaled))) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_grid(agegroup~veg_name) +
	   theme_bw() + ggtitle('Sample: Pr-ET <= -1000 (mm) & elevation >= 1000 (m)') +
	   xlab('Die-off (trees/hectare)')	

f24 <- ggarrange(p29, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig26_stand_age_die-off_ADS_veg_type_age_pdf.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig26_stand_age_die-off_ADS_veg_type_age_pdf.eps', height=160, width=260, units = 'mm', dpi=300)

p30 <-  ggplot(data = stand.age.control.all) + geom_density(mapping = aes(x = dNDMI_2015_mean.transform, after_stat(scaled), color = agegroup), alpha = 0.3) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   scale_x_reverse() +
	   # facet_wrap(~agegroup, scale="free_y") +
	   theme_bw() + ggtitle('Not Exposed: Pr-ET >= -500 (mm) & elevation >= 1000 (m)') +
	   xlab('dNDMI 2017')	

f25 <- ggarrange(p30, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig27_control_stand_age_die-off_pdf.png', height=160, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig27_control_stand_age_die-off_pdf.eps', height=160, width=210, units = 'mm', dpi=300)

p31 <-  ggplot(data = stand.age.sample.all) + geom_density(mapping = aes(x = dNDMI_2015_mean.transform, after_stat(scaled), color = agegroup), alpha = 0.3) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   scale_x_reverse() +
	   # facet_wrap(~agegroup, scale="free_y") +
	   theme_bw() + ggtitle('Not Exposed: Pr-ET <= -1000 (mm) & elevation >= 1000 (m)') +
	   xlab('dNDMI 2017')	

f26 <- ggarrange(p31, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig28_experimental_stand_age_die-off_pdf.png', height=160, width=210, units = 'mm', dpi=300)
ggsave(filename = 'Fig28_experimental_stand_age_die-off_pdf.eps', height=160, width=210, units = 'mm', dpi=300)

#Overall Trends by sample regions
p32 <- ggplot(data = stand.age, mapping = aes(x = stand_age_mean, y = biomass_2012_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 100) +
	   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 4, linetype = 'dashed') +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 2) +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 140) + ggtitle('Elevation (horizontal) vs Pr-ET (vertical) ') +
	   ylab('Pre-Drought Biomass (Mg/ha)') +  xlab('Stand Age (2017 - fire year)')	

f27 <- ggarrange(p32, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig29_stand_age_biomass.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig29_stand_age_biomass.eps', height=160, width=260, units = 'mm', dpi=300)

#Overall Trends by sample regions
p33 <- ggplot(data = stand.age, mapping = aes(x = stand_age_mean, y = biomass_2012_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 100) +
	   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 4, linetype = 'dashed') +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 140) + 
	   ylab('Pre-Drought Biomass (Mg/ha)') +  xlab('Stand Age (2017 - fire year)')	

f28 <- ggarrange(p33, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig30_stand_age_biomass.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig30_stand_age_biomass.eps', height=160, width=260, units = 'mm', dpi=300)

#Overall Trends by sample regions
p34 <- ggplot(data = stand.age, mapping = aes(x = stand_age_mean, y = NDMI_2012_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, bins = 100) +
	   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 4, linetype = 'dashed') +
	   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 2) +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 140) + 
	   ylab('Pre-Drought NDMI') +  xlab('Stand Age (2017 - fire year)')	

f29 <- ggarrange(p34, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig31_stand_age_NDMI.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig31_stand_age_NDMI.eps', height=160, width=260, units = 'mm', dpi=300)

#Overall Trends by sample regions
p35 <- ggplot(data = stand.age, mapping = aes(x = stand_age_mean, y = NDMI_2012_mean)) +
	   # geom_point(shape = 21) +
	   geom_bin2d(alpha = 0.5, , binwidth = c(5, 0.04)) +
	   geom_smooth() +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 4, linetype = 'dashed') +
	   # stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, size = 4) +
	   # stat_cor(method = "pearson") +
	   scale_fill_gradient2(limits = c(0,150), midpoint = 75, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'red') +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 140) + 
	   ylab('Pre-Drought NDMI') +  xlab('Stand Age (2017 - fire year)')	

f28 <- ggarrange(p35, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig32_stand_age_NDMI.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig32_stand_age_NDMI.eps', height=160, width=260, units = 'mm', dpi=300)

p36 <- ggplot(data = stand.age.sample.chrono, mapping = aes(x = stand_age.mean, y = dNDMI_2015.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=dNDMI_2015.mean-dNDMI_2015.sd, ymax=dNDMI_2015.mean+dNDMI_2015.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 145) + ylim(-0.2, 0.05) + ggtitle('Exposed: Pr-ET <= -1000 & Elevation >= 1000') +
	   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
       axis.title.y = element_text(size = 10), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.text=element_text(size=10), legend.title = element_text(size=10)) +
	   ylab('dNDMI') +  xlab('Stand Age (2017 - fire year)')	

p37 <- ggplot(data = stand.age.control.chrono, mapping = aes(x = stand_age.mean, y = dNDMI_2015.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=dNDMI_2015.mean-dNDMI_2015.sd, ymax=dNDMI_2015.mean+dNDMI_2015.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 145) + ylim(-0.2, 0.05) + ggtitle('Not Exposed: Pr-ET >= -500 & Elevation >= 1000') + 
	   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
       axis.title.y = element_text(size = 10), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.text=element_text(size=10), legend.title = element_text(size=10)) +
	   ylab('dNDMI') +  xlab('Stand Age (2017 - fire year)')

f29 <- ggarrange(p36, p37, ncol = 1, nrow = 2, labels = c('A', 'B', 'C'), common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig34_stand_age_dNDMI.png', height=160, width=260, units = 'mm', dpi=600)
ggsave(filename = 'Fig34_stand_age_dNDMI.eps', height=160, width=260, units = 'mm', dpi=600)

p38 <- ggplot(data = stand.age.sample.chrono, mapping = aes(x = stand_age.mean, y = ADS_2017.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=ADS_2017.mean-ADS_2017.sd, ymax=ADS_2017.mean+ADS_2017.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 150) + ggtitle('Exposed: Pr-ET <= -1000 & Elevation >= 1000') +
	   ylab('Mortality (trees/ha)') +  xlab('Stand Age (2017 - fire year)')	

p39 <- ggplot(data = stand.age.control.chrono, mapping = aes(x = stand_age.mean, y = ADS_2017.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=ADS_2017.mean-ADS_2017.sd, ymax=ADS_2017.mean+ADS_2017.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 150) + ggtitle('Not Exposed: Pr-ET >= -500 & Elevation >= 1000') + 
	   ylab('Mortality (trees/ha)') +  xlab('Stand Age (2017 - fire year)')

f30 <- ggarrange(p38, p39, ncol = 1, nrow = 2, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig35_stand_age_ADS.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig35_stand_age_ADS.eps', height=160, width=260, units = 'mm', dpi=300)

p40 <- ggplot(data = stand.age.sample.chrono, mapping = aes(x = stand_age.mean, y = biomass_2012.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=biomass_2012.mean-biomass_2012.sd, ymax=biomass_2012.mean+biomass_2012.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 150) + ggtitle('Exposed: Pr-ET <= -1000 & Elevation >= 1000') +
	   ylab('Biomass (Mg/ha)') +  xlab('Stand Age (2017 - fire year)')	

p41 <- ggplot(data = stand.age.control.chrono, mapping = aes(x = stand_age.mean, y = biomass_2012.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=biomass_2012.mean-biomass_2012.sd, ymax=biomass_2012.mean+biomass_2012.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 150) + ggtitle('Not Exposed: Pr-ET >= 500 & Elevation >= 1000') + 
	   ylab('Biomass (Mg/ha)') +  xlab('Stand Age (2017 - fire year)')

f31 <- ggarrange(p40, p41, ncol = 1, nrow = 2, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig36_stand_age_biomass.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig36_stand_age_biomass.eps', height=160, width=260, units = 'mm', dpi=300)

p42 <- ggplot(data = stand.age.sample.chrono, mapping = aes(x = stand_age.mean, y = NDMI_2012.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=NDMI_2012.mean-NDMI_2012.sd, ymax=NDMI_2012.mean+NDMI_2012.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 150) + ggtitle('Exposed: Pr-ET <= -1000 & Elevation >= 1000') +
	   ylab('NDMI') +  xlab('Stand Age (2017 - fire year)')	

p43 <- ggplot(data = stand.age.control.chrono, mapping = aes(x = stand_age.mean, y = NDMI_2012.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=NDMI_2012.mean-NDMI_2012.sd, ymax=NDMI_2012.mean+NDMI_2012.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 150) + ggtitle('Not Exposed: Pr-ET >= -500 & Elevation >= 1000') + 
	   ylab('NDMI') +  xlab('Stand Age (2017 - fire year)')

f32 <- ggarrange(p42, p43, ncol = 1, nrow = 2, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig37_stand_age_NDMI.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig37_stand_age_NDMI.eps', height=160, width=260, units = 'mm', dpi=300)

p44 <- ggplot(data = stand.age.sample.chrono, mapping = aes(x = stand_age.mean, y = PET_4yr_2015.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=PET_4yr_2015.mean-PET_4yr_2015.sd, ymax=PET_4yr_2015.mean+PET_4yr_2015.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 145) + ggtitle('Exposed: Pr-ET <= -1000 & Elevation >= 1000') +
	   ylab('Pr-ET (mm/4 yr)') +  xlab('Stand Age (2017 - fire year)')	

p45 <- ggplot(data = stand.age.control.chrono, mapping = aes(x = stand_age.mean, y = PET_4yr_2015.mean)) +
	   geom_point(mapping = aes(size = dNDMI_2015.count)) +
	   geom_line() +
	   geom_errorbar(aes(ymin=PET_4yr_2015.mean-PET_4yr_2015.sd, ymax=PET_4yr_2015.mean+PET_4yr_2015.sd), width = 2) +#, width=.2, position=position_dodge(.9)) 
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   # facet_grid(PET.control~elevation.control) +
	   theme_bw() + #ylim(-0.6, 0.6) + 
	   xlim(0, 145) + ggtitle('Not Exposed: Pr-ET >= -500 & Elevation >= 1000') + 
	   ylab('Pr-ET (mm/4 yr)') +  xlab('Stand Age (2017 - fire year)')

f33 <- ggarrange(p44, p45, ncol = 1, nrow = 2, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'Fig38_stand_age_PrET.png', height=160, width=260, units = 'mm', dpi=300)
ggsave(filename = 'Fig38_stand_age_PrET.eps', height=160, width=260, units = 'mm', dpi=300)

#Density plot for Moisture Overdraft
p46 <-  ggplot(data = stand.age.sample.all) + geom_density(mapping = aes(x = PET_4yr_2015_mean, after_stat(scaled), color = agegroup), size = 2, alpha = 0.3) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Stand Ages') +
	   theme_dark() + ggtitle('Treatment') +
	   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
	   axis.title.y = element_text(size = 10), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.text=element_text(size=10), legend.title = element_text(size=10)) +
	   xlab('Pr-ET (mm/4 yr)') + ylab('Scaled Density') #+ xlim(0.15, -0.35)	

#f34 <- ggarrange(p46, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

#ggsave(filename = 'Fig39_sample_stand_age_die-off_vegtype_pdf.png', height=160, width=210, units = 'mm', dpi=600)
#ggsave(filename = 'Fig39_sample_stand_age_die-off_vegtype_pdf.eps', height=160, width=210, units = 'mm', dpi=600)

p47 <-  ggplot(data = stand.age.control.all) + geom_density(mapping = aes(x = PET_4yr_2015_mean, after_stat(scaled), color = agegroup), size = 2, alpha = 0.3) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Stand Ages') +
	   theme_dark() + ggtitle('Not Exposed') +
	   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
       axis.title.y = element_text(size = 10), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.text=element_text(size=10), legend.title = element_text(size=10)) +
	   xlab('Pr-ET (mm/4 yr)') + ylab('Scaled Density') #+ xlim(0.15, -0.35)	

f35 <- ggarrange(p46, p47, ncol = 2, nrow = 1, common.legend = TRUE, labels = c('A', 'B'), legend = 'bottom')

ggsave(filename = 'Fig40_control_stand_age_die-off_vegtype_pdf.png', height=8, width=16, units = 'cm', dpi=900)
ggsave(filename = 'Fig40_control_stand_age_die-off_vegtype_pdf.eps', height=8, width=16, units = 'cm', dpi=900)

#Density plot for Moisture Overdraft
p48 <-  ggplot(data = stand.age.sample.all) + geom_density(mapping = aes(x = biomass_2012_mean, after_stat(scaled), color = agegroup), size = 2, alpha = 0.3) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Stand Ages') +
	   theme_dark() + ggtitle('Treatment') +
	   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
	   axis.title.y = element_text(size = 10), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.text=element_text(size=10), legend.title = element_text(size=10)) +
	   xlab('Pre-Drought Biomass (Mg/ha)') + ylab('Scaled Density') + xlim(0, 450)		

# f36 <- ggarrange(p48, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

# ggsave(filename = 'Fig41_sample_stand_age_die-off_vegtype_pdf.png', height=160, width=210, units = 'mm', dpi=600)
# ggsave(filename = 'Fig41_sample_stand_age_die-off_vegtype_pdf.eps', height=160, width=210, units = 'mm', dpi=600)

p49 <-  ggplot(data = stand.age.control.all) + geom_density(mapping = aes(x = biomass_2012_mean, after_stat(scaled), color = agegroup), size = 2, alpha = 0.3) +
	   # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
	   # stat_cor(method = "pearson") +
	   geom_vline(xintercept = 0, linetype='dashed') + geom_hline(yintercept = 0, linetype='dashed') + 
	   scale_color_brewer(type = 'seq', palette = 'Greens', name = 'Stand Ages') +
	   theme_dark() + ggtitle('Not Exposed') +
	   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
       axis.title.y = element_text(size = 10), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), legend.text=element_text(size=10), legend.title = element_text(size=10)) +
	   xlab('Pre-Drought Biomass (Mg/ha)') + ylab('Scaled Density') + xlim(0, 450)	

f37 <- ggarrange(p48, p49, ncol = 2, nrow = 1, common.legend = TRUE, labels = c('A', 'B'), legend = 'bottom')

ggsave(filename = 'Fig42_control_stand_age_die-off_vegtype_pdf.png', height=8, width=16, units = 'cm', dpi=900)
ggsave(filename = 'Fig42_control_stand_age_die-off_vegtype_pdf.eps', height=8, width=16, units = 'cm', dpi=900)

wg <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
c <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

age.dNDMI.rq <- rq(dNDMI_2015_mean ~ stand_age_mean, data = stand.age.sample, tau = q10)
print(age.dNDMI.rq %>% tidy())
tb1 <- age.dNDMI.rq %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 1: Quantile Regression, Die-off(dNDMI) ~ f(Stand Age)") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 5, file = "T1_dNDMI_stand_age_quantile_regression_results.png", zoom = 4.0)  


age.ADS.rq <- rq(ADS_2017_mean ~ stand_age_mean, data = stand.age.sample, tau = q10)
print(age.ADS.rq %>% tidy())
tb2 <- age.ADS.rq %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 2: Quantile Regression, Die-off(ADS) ~ f(Stand Age)") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 5, file = "T2_ADS_stand_age_quantile_regression_results.png", zoom = 4.0)  

stand.age.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age)

summary(stand.age.lm)
#Treatment Grid Cells
aov.dNDMI.stand.age.treatment <- aov(dNDMI_2015_mean ~ agegroup, data = stand.age.sample)
summary(aov.dNDMI.stand.age.treatment)

aov.PET4yr.stand.age.treatment <- aov(PET_4yr_2015_mean ~ agegroup, data = stand.age.sample)
summary(aov.PET4yr.stand.age.treatment)

aov.biomass.stand.age.treatment <- aov(biomass_2012_mean ~ agegroup, data = stand.age.sample)
summary(aov.biomass.stand.age.treatment)

tukey.dNDMI.stand.age.treatment <- TukeyHSD(aov.dNDMI.stand.age.treatment)
print(tukey.dNDMI.stand.age.treatment)

tukey.PET4yr.stand.age.treatment <- TukeyHSD(aov.PET4yr.stand.age.treatment)
print(tukey.PET4yr.stand.age.treatment)

tukey.biomass.stand.age.treatment <- TukeyHSD(aov.biomass.stand.age.treatment)
print(tukey.biomass.stand.age.treatment)

# anovas.treament <- anova(aov.dNDMI.stand.age.treatment, aov.PET4yr.stand.age.treatment, aov.biomass.stand.age.treatment)
# print(anovas.treatment) 

#Control Grid Cells
aov.dNDMI.stand.age.control <- aov(dNDMI_2015_mean ~ agegroup, data = stand.age.control)
summary(aov.dNDMI.stand.age.control)

aov.PET4yr.stand.age.control <- aov(PET_4yr_2015_mean ~ agegroup, data = stand.age.control)
summary(aov.PET4yr.stand.age.control)

aov.biomass.stand.age.control <- aov(biomass_2012_mean ~ agegroup, data = stand.age.control)
summary(aov.biomass.stand.age.control)

tukey.dNDMI.stand.age.control <- TukeyHSD(aov.dNDMI.stand.age.control)
print(tukey.dNDMI.stand.age.control)

tukey.PET4yr.stand.age.control <- TukeyHSD(aov.PET4yr.stand.age.control)
print(tukey.PET4yr.stand.age.control)

tukey.biomass.stand.age.control <- TukeyHSD(aov.biomass.stand.age.control)
print(tukey.biomass.stand.age.control)

tb1 <- tukey.dNDMI.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 1: Not Exposed, Tukey HSD, dNDMI ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 5, file = "Table1_Not_Exposed_Mortality_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb2 <- tukey.dNDMI.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 2: Exposed, Tukey HSD, dNDMI ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 5, file = "Table2_Exposed_Mortality_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb3 <- tukey.PET4yr.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 3: Not Exposed, Tukey HSD, four-year Pr-ET ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb3, width = 5, file = "Table3_Not_Exposed_Water_Deficit_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb4 <- tukey.PET4yr.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 4: Exposed, Tukey HSD, four-year Pr-ET ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb4, width = 5, file = "Table4_Exposed_Water_Deficit_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb5 <- tukey.biomass.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 5: Not Exposed, Tukey HSD, Biomass ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb5, width = 5, file = "Table5_Not_Exposed_Biomass_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb6 <- tukey.biomass.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 6: Exposed, Tukey HSD, Biomass ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb6, width = 5, file = "Table6_Exposed_Biomass_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

#Summary Statistics
summary.control <- stand.age.control %>% 
  group_by(agegroup) %>% 
  summarize(dNDMI_mean = mean(dNDMI_2015_mean),
            dNDMI_sd = sd(dNDMI_2015_mean),
			biomass_mean = mean(biomass_2012_mean),
			biomass_sd = sd(biomass_2012_mean))

print(summary.control)

summary.treatment <- stand.age.sample %>% 
  group_by(agegroup) %>% 
  summarize(dNDMI_mean = mean(dNDMI_2015_mean),
            dNDMI_sd = sd(dNDMI_2015_mean),
			biomass_mean = mean(biomass_2012_mean),
			biomass_sd = sd(biomass_2012_mean))

print(summary.treatment)

# stand.age.lm2 <- lm(dNDMI_2015_mean ~ stand_age_mean, data = stand.age)

# summary(stand.age.lm2)

# stand.age.socal.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age.socal)

# summary(stand.age.socal.lm)

# stand.age.norcal.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age.norcal)

# summary(stand.age.norcal.lm)

nrow(stand.age)
# summary(stand.age)
# nrow(stand.age.norcal)
# nrow(stand.age.socal)