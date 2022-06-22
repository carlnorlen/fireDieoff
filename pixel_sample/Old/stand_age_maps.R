#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: May 12, 2020
#Purpose: Create merge split raster files

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < stand_age_maps.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 'rlist', 'ggspatial', 'svglite')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# land_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Landsat"
# socal_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\socal"
# landfire_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\LANDFIRE"
# frap_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\FRAP\\raster"
# wrcc_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC"
# data_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\All"
# data_30m_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\SPI48_30m"
# # dir_out <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\Output"
# dir_out <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\SPI48_30m"
# work_dir <- "C:\\Users\\Carl\\mystuff\\Goulden_Lab\\Forest_Dieback\\dieback\\figure_set\\map"
# dir_ca <- "C:\\Users\\Carl\\mystuff\\Large_Files\\TIGER\\ca-state-boundary"
# dir_usgs <- "C:\\Users\\Carl\\mystuff\\Large_Files\\USGS"

# # #Directory for drought monitor polygons
# dir_usdm <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Drought_Monitor\\equal_drought"

land_dir <- "D:\\Large_Files\\Landsat"
cecs_dir <- "D:\\Large_Files\\CECS"
socal_dir <- "D:\\Large_Files\\socal"
landfire_dir <- "D:\\Large_Files\\LANDFIRE"
frap_dir <- "D:\\Large_Files\\FRAP\\raster"
wrcc_dir <- "D:\\Large_Files\\WRCC"
data_dir <- "D:\\Large_Files\\WRCC\\All"
data_30m_dir <- "D:\\Large_Files\\WRCC\\SPI48_30m"
# dir_out <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\Output"
dir_out <- "D:\\Large_Files\\WRCC\\SPI48_30m"
work_dir <- "C:\\Users\\can02\\mystuff\\Goulden_Lab\\Forest_Dieback\\dieback\\figure_set\\final_figures_redo"
dir_ca <- "D:\\Large_Files\\TIGER\\ca-state-boundary"
dir_usgs <- "D:\\Large_Files\\USGS\\data"
dir_eco <- "D:\\Large_Files\\EcoRegion\\ca_eco_l3"

#Directory for drought monitor polygons
dir_usdm <- "D:\\Large_Files\\Drought_Monitor\\equal_drought"

#CSV version of landsat data directory
dir_in <- "D:\\Large_Files\\Landsat"

#ESPG 5070, proj4 crs
c <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#Read in a raster of general WGS 84 crs in PROJ4 code format
wg <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")

#Increase the memory limit for R. Helps with spatially explicit analyses.
memory.limit(32000)

#Add California Boundary shape file
us_states_20m <- states(cb = TRUE, resolution = "20m", class = "sf")
us_states_20m <- st_transform(us_states_20m, c)
ca_20m <- us_states_20m[us_states_20m$NAME == "California", ]
ca_20m <- st_transform(ca_20m, c)

ca.ext <- as(extent(ca_20m), 'SpatialPolygons')
crs(ca.ext) <- c

south.sierra.ext <- as(extent(-120.2, -117.5, 34.6,  37.5), 'SpatialPolygons')
crs(south.sierra.ext) <- wg
south.sierra.ext <- spTransform(south.sierra.ext, c)
south.sierra.ext <- st_as_sf(south.sierra.ext)

#Get the Sierra Nevada EcoRegion polygon and transform in to ESPG: 5070
eco.regions <- st_read(file.path(dir_eco, 'ca_eco_l3.shp'))
eco.regions <- st_read(file.path(dir_eco, 'ca_eco_l3.shp'))
sierra <- eco.regions[eco.regions$US_L3CODE ==5, ]
sierra <- st_transform(sierra, c)

sierra.ext <- as(extent(-120.2, -117.5, 34.6,  41.0), 'SpatialPolygons')
crs(sierra.ext) <- wg
sierra.ext <- spTransform(sierra.ext, c)
sierra.ext <- st_as_sf(sierra.ext)

south.sierra <- st_crop(sierra, south.sierra.ext)
# Get the sample pixels and transform them to EPSG: 5070
# sample.pixels <- raster(file.path(cecs_dir, 'Sample_pixels_Sierra_die-off_v3.tif'))
# crs(sample.pixels) <- c
# sample.pixels.m <- sample.pixels == 0
# sample.pixels.masked <- mask(sample.pixels, mask = sample.pixels.m, maskvalue = 1)

# #Get the control pixles and transform them to EPSG: 5070
# control.pixels <- raster(file.path(cecs_dir, 'Control_pixels_Sierra_die-off_v4.tif'))
# crs(control.pixels) <- c
# control.pixels.m <- control.pixels == 0
# control.pixels.masked <- mask(control.pixels, mask = control.pixels.m, maskvalue = 1)

# #Combined Control and Sample Pixels
# control.pixels.v2 <- control.pixels * 2

# combine.pixels <- sample.pixels + control.pixels.v2
# combine.pixels.m <- combine.pixels == 0
# combine.pixels.masked <- mask(combine.pixels, mask = combine.pixels.m, maskvalue = 1)

#Landsat dNDMI
dndmi.2017 <- raster(file.path(cecs_dir, 'Sierra_die-off_dNDMI_2015_v3.tif'))
crs(dndmi.2017) <- c
dndmi.2017.m <- dndmi.2017 == 0
dndmi.2017.mask <- mask(dndmi.2017, mask = dndmi.2017.m, maskvalue = 1)
dndmi.2017.crop <- dndmi.2017.mask %>% crop(sierra.ext)

#Landsat four-year Pr-ET
PrET.2015 <- raster(file.path(cecs_dir, 'Sierra_die-off_PET_4yr_2015_v3.tif'))
crs(PrET.2015) <- c
PrET.2015.m <- PrET.2015 == 0
PrET.2015.mask <- mask(PrET.2015, mask = PrET.2015.m, maskvalue = 1)
PrET.2015.crop <- PrET.2015.mask %>% crop(sierra.ext)

#Stand age
stand.age <- raster(file.path(cecs_dir, 'Sierra_die-off_stand_age_v3.tif'))
crs(stand.age) <- c
stand.age.m <- stand.age == 0
stand.age.mask <- mask(stand.age, mask = stand.age.m, maskvalue = 1)
stand.age.crop <- stand.age.mask %>% crop(sierra.ext)

stand.age.mask.reclass <- reclassify(stand.age.mask, c(0, 35, 1, 35, 70, 2, 70, 105, 3, 105, 300, 4)) 
stand.age.crop.reclass <- stand.age.mask.reclass %>% crop(sierra.ext)

#Stand age
stand.age.manage <- raster(file.path(cecs_dir, 'Sierra_die-off_management_stand_age.tif'))
crs(stand.age.manage) <- c
stand.age.manage.m <- stand.age.manage == 0
stand.age.manage.mask <- mask(stand.age.manage, mask = stand.age.manage.m, maskvalue = 1)
stand.age.manange.crop <- stand.age.manage.mask %>% crop(sierra.ext)

stand.age.manage.mask.reclass <- reclassify(stand.age.manage.mask, c(0, 9, 1, 9, 18, 2, 18, 27, 3, 27, 35, 4)) 
stand.age.crop.reclass <- stand.age.mask.reclass %>% crop(sierra.ext)

#Pr-ET metric
p3 <- ggplot() + 
	  geom_sf(data=sierra, color='black', fill=NA, alpha = 0.5) + #First drought 
	  ggR(img = PrET.2015.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + 
	  coord_sf() + #xlim(-120, -115.5) + ylim(33, 39.5) +
	  theme_dark() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8),
	  legend.title = element_text( size=8), legend.text=element_text(size=6)) + 
	  # scale_fill_viridis(name = 'Pr-ET four-year', direction = -1, option = "magma", limits = c(-2000, 3000), na.value = NA)
	  scale_fill_gradient2(name = "Pr-ET (mm/4yr)", limits = c(-2000, 3000), midpoint = 0, low = "red", mid = "cornsilk", high = "blue", na.value = NA)

#Die-off metric
p4 <- ggplot() + 
	  geom_sf(data=sierra, color='black', fill=NA, alpha = 0.5) + #First drought 
	  ggR(img = dndmi.2017.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + 
	  coord_sf() + #xlim(-120, -115.5) + ylim(33, 39.5) +
	  theme_dark() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8),
	  legend.title = element_text( size=8), legend.text=element_text(size=6)) + 
	  scale_fill_gradient2(name = "dNDMI", limits = c(-0.25, 0.2), midpoint = 0, low = "red", mid = "cornsilk", high = "green", na.value = NA)
	  # scale_fill_viridis(name = 'dNDMI', direction = -1, option = "magma", limits = c(-0.3, 0.1), na.value = NA)	  


# ggsave(filename = 'Fig34_Sierra_drought_stand_age_dNDMI.svg', height=8.5, width= 16, units = 'cm', dpi=900, device = 'svg')

#Stand age metric
p5 <- ggplot() + 
	  geom_sf(data=sierra, color='black', fill=NA, alpha = 0.5) + 
	  ggR(img = stand.age.mask.reclass, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) + 
	  coord_sf() + #xlim(-120, -115.5) + ylim(33, 39.5) +
	  theme_dark() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8),
	  legend.title = element_text( size=8), legend.text=element_text(size=6)) + guides(fill=guide_legend(title.position="top", title.hjust =0.5, ncol=4)) +
	  scale_fill_brewer(type = 'seq', palette = 'RdPu', name = 'Years Since Fire', breaks = c("1", "2", "3", "4"), labels = c("0-35", "36-70", "71-105", "106+"))
	  # scale_fill_gradient2(name = 'Years Since Fire', limits = c(0, 160), low = "white", high = "forestgreen", na.value = NA)

f1 <- ggarrange(p3, p4, p5, ncol = 3, nrow = 1, common.legend = FALSE, labels = c('A', 'B', 'C'), legend = 'bottom')
ggsave(filename = 'Fig34_Sierra_drought_dNDMI_PrET.png', height=8.5, width= 20, units = 'cm', dpi=900)

p6 <- ggplot() + 
	  geom_sf(data=sierra, color='black', fill=NA, alpha = 0.5) + 
	  ggR(img = stand.age.manage.mask.reclass, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) + 
	  coord_sf() + #xlim(-120, -115.5) + ylim(33, 39.5) +
	  theme_dark() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8),
	  legend.title = element_text( size=8), legend.text=element_text(size=6)) + guides(fill=guide_legend(title.position="top", title.hjust =0.5, ncol=4)) +
	  scale_fill_brewer(type = 'seq', palette = 'RdPu', name = 'Years Since Harvest', breaks = c("1", "2", "3", "4"), labels = c("0-9", "10-18", "19-27", "28-35"))
	  # scale_fill_gradient2(name = 'Years Since Fire', limits = c(0, 160), low = "white", high = "forestgreen", na.value = NA)

f2 <- ggarrange(p5, p6, ncol = 2, nrow = 1, common.legend = FALSE, labels = c('A', 'B'), legend = 'bottom')
ggsave(filename = 'Fig35_Sierra_drought_stand_age.png', height=8.5, width= 16, units = 'cm', dpi=900)
# ggsave(filename = 'Fig34_Sierra_drought_stand_age_dNDMI.svg', height=8.5, width= 16, units = 'cm', dpi=900, device = 'svg')