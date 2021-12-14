#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: May 12, 2020
#Purpose: Create merge split raster files

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/chrono
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/chrono
#Run the script: R < grand_table.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggplot2', 'magrittr', 'stats', 'patchwork')
# install.packages('dplyr',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# install.packages(c('stats'),repo='https://cran.r-project.org/')
# require(ggpubr)
# require(viridis)
# require(tidyr)
# require(dplyr)
# # require(plyr)
# require(ggplot2)
# require(magrittr)
# # require(raster)
# # require(rgdal)
# # require(sp)
# # require(sf)
# # require(RStoolbox)
# # require(ncdf4)
# # require(tmap)
# # require(tmaptools)
# # require(gtools)
# # require(tigris)
# require(patchwork)
# require(stats)
#require(USAboundaries)

# dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\CECS"
dir <- "D:\\Large_Files\\CECS"
memory.limit(32000)

# grand.mtbs <- read.csv(file.path(dir, 'CAfiresALL_LS578_median_annual_100m_v2.csv'))
# FRAP grand table
grand.table <- read.csv(file.path(dir, 'FRAPbuffer_LS578_annual_newasset.csv'))
# grand.frap <- read.csv(file.path(dir, 'FRAPsimpleALL_LS578_multireducer_LS578_annual_500m_reducer2.csv'))
# summary(grand.frap)
# head(grand.table)

# summary(grand.table)
grand.table$vi.date <- as.Date(grand.table$date)
grand.table$vi.year <- format(grand.table$vi.date , '%Y')
grand.table$vi.month <- format(grand.table$vi.date , '%m')
grand.table$fire.date <- as.Date(grand.table$ALARM_DATE)
grand.table$fire.year <- format(grand.table$fire.date , '%Y')
grand.table$fire.month <- format(grand.table$fire.date , '%m')
grand.table$stand.age <- as.numeric(grand.table$vi.year) - as.numeric(grand.table$fire.year) 
# head(grand.table)

#Limit to only fires since 1950
grand.table <- grand.table %>% filter(fire.year >= 1950 & fire.year <= 2012)
# grand.table <- filter(grand.table, vi.year <= 2012)
#Limit the plots to only elevations between 1000 m and 2500 m
grand.table <- grand.table %>% filter(elevation_p50 >= 1000 & elevation_p50 <= 2500)
#Limit to only fires with Evergreen (4) or Mixed (5) Forest as the median.
grand.table <- grand.table %>% filter(remapped_p50 == 4 | remapped_p50 == 5)
# summary(grand.table)

p1 <- ggplot(data=grand.table, mapping = aes(x=stand.age, y=biomass_p50, color = OBJECTID)) + geom_point(size = 1) + 
	  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) + theme_bw() 
p2 <- ggplot(data=grand.table, mapping = aes(x=stand.age, y=biomass_p50, color = OBJECTID)) + #geom_point(size = 1) +
	  geom_line() +
	  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) + theme_bw() 
# (p1 / p3) | p2 +
	 # plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect') # heights = c(2,1))

f1 <- ggarrange(p1, p2, ncol = 1, nrow = 2, labels = c('A', 'B'), legend = 'bottom')
ggsave(filename = 'Fig1_California_chronosequence.png', height=210, width=210, units = 'mm', dpi=300)

# burn.morethan30 <- filter(grand.table, stand.age < 0)

burn.lessthan15 <- filter(grand.table, stand.age >= 0 & stand.age < 25)

burn.morethan15 <- filter(grand.table, stand.age >= 25 & stand.age < 50)

burn.morethan30 <- filter(grand.table, stand.age >= 50)
# summary(burn.morethan15)

# Biomass Response by stand age
p3 <- ggplot() +
	  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.2, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
	  geom_smooth(data=burn.morethan30, mapping = aes(x=vi.date, y=biomass_p50), method = "loess", formula = y ~ x, se = FALSE) + theme_bw() + scale_x_date(date_labels = "%Y") +
	  geom_point(data=burn.morethan30, mapping = aes(x=vi.date, y=biomass_p50)) #+
	  # xlim(as.Date('1984-06-01'), as.Date('2019-06-01')) + ylim(0, 400)
p4 <- ggplot() +
	  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.2, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
	  geom_smooth(data=burn.lessthan15, mapping = aes(x=vi.date, y=biomass_p50), method = "loess", formula = y ~ x, se = FALSE) + theme_bw() + scale_x_date(date_labels = "%Y") +
	  geom_point(data=burn.lessthan15, mapping = aes(x=vi.date, y=biomass_p50)) #+
	  # xlim(as.Date('1984-06-01'), as.Date('2019-06-01')) + ylim(0, 400)
p5 <- ggplot() +
	  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.2, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
	  geom_smooth(data=burn.morethan15, mapping = aes(x=vi.date, y=biomass_p50), method = "loess", formula = y ~ x, se = FALSE) + theme_bw() + scale_x_date(date_labels = "%Y") +
	  geom_point(data=burn.morethan15, mapping = aes(x=vi.date, y=biomass_p50))# +
	  # xlim(as.Date('1984-06-01'), as.Date('2019-06-01')) + ylim(0, 400)

f2 <- ggarrange(p3, p4, p5, ncol = 1, nrow = 3, labels = c('A', 'B', 'C'), legend = 'bottom')
ggsave(filename = 'Fig2_California_biomass_resopnse.png', height=210, width=210, units = 'mm', dpi=300)

# Biomass Response by stand age
p6 <- ggplot() +
	  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.2, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
	  geom_smooth(data=burn.morethan30, mapping = aes(x=vi.date, y=NDMI_mean), method = "loess", formula = y ~ x, se = FALSE) + theme_bw() + scale_x_date(date_labels = "%Y") +
	  geom_point(data=burn.morethan30, mapping = aes(x=vi.date, y=NDMI_mean, color = OBJECTID)) +
	  xlim(as.Date('1984-06-01'), as.Date('2019-06-01')) + ylim(-0.3, 0.6)
p7 <- ggplot() +
	  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.2, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
	  geom_smooth(data=burn.lessthan15, mapping = aes(x=vi.date, y=NDMI_mean), method = "loess", formula = y ~ x, se = FALSE) + theme_bw() + scale_x_date(date_labels = "%Y") +
	  geom_point(data=burn.lessthan15, mapping = aes(x=vi.date, y=NDMI_mean, color = OBJECTID)) +
	  xlim(as.Date('1984-06-01'), as.Date('2019-06-01')) + ylim(-0.3, 0.6)
p8 <- ggplot() +
	  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.2, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
	  geom_smooth(data=burn.morethan15, mapping = aes(x=vi.date, y=NDMI_mean), method = "loess", formula = y ~ x, se = FALSE) + theme_bw() + scale_x_date(date_labels = "%Y") +
	  geom_point(data=burn.morethan15, mapping = aes(x=vi.date, y=NDMI_mean, color = OBJECTID)) +
	  xlim(as.Date('1984-06-01'), as.Date('2019-06-01')) + ylim(-0.3, 0.6)

f3 <- ggarrange(p6, p7, p8, ncol = 1, nrow = 3, labels = c('A', 'B', 'C'), legend = 'bottom')
ggsave(filename = 'Fig3_California_die-off_response.png', height=210, width=210, units = 'mm', dpi=300)

