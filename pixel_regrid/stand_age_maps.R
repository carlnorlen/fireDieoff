#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: May 12, 2020
#Purpose: Create merge split raster files

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/chrono
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/chrono
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
sierra <- eco.regions[eco.regions$US_L3CODE ==5, ]
sierra <- st_transform(sierra, c)

south.sierra <- st_crop(sierra, south.sierra.ext)
# Get the sample pixels and transform them to EPSG: 5070
sample.pixels <- raster(file.path(cecs_dir, 'Sample_pixels_Sierra_die-off_v3.tif'))
crs(sample.pixels) <- c
sample.pixels.m <- sample.pixels == 0
sample.pixels.masked <- mask(sample.pixels, mask = sample.pixels.m, maskvalue = 1)

#Get the control pixles and transform them to EPSG: 5070
control.pixels <- raster(file.path(cecs_dir, 'Control_pixels_Sierra_die-off_v4.tif'))
crs(control.pixels) <- c
control.pixels.m <- control.pixels == 0
control.pixels.masked <- mask(control.pixels, mask = control.pixels.m, maskvalue = 1)

#Combined Control and Sample Pixels
control.pixels.v2 <- control.pixels * 2

combine.pixels <- sample.pixels + control.pixels.v2
combine.pixels.m <- combine.pixels == 0
combine.pixels.masked <- mask(combine.pixels, mask = combine.pixels.m, maskvalue = 1)

#Landsat dNDMI
dndmi.2017 <- raster(file.path(cecs_dir, 'Sierra_die-off_dNDMI_2015.tif'))
crs(dndmi.2017) <- c
dndmi.2017.m <- dndmi.2017 == 0
dndmi.2017.mask <- mask(dndmi.2017, mask = dndmi.2017.m, maskvalue = 1)


#Landsat four-year Pr-ET
PrET.2015 <- raster(file.path(cecs_dir, 'Sierra_die-off_PET_4yr_2015.tif'))
crs(PrET.2015) <- c
PrET.2015.m <- PrET.2015 == 0
PrET.2015.mask <- mask(PrET.2015, mask = PrET.2015.m, maskvalue = 1)

#Stand age
stand.age <- raster(file.path(cecs_dir, 'Sierra_die-off_stand_age.tif'))
crs(stand.age) <- c
stand.age.m <- stand.age == 0
stand.age.mask <- mask(stand.age, mask = stand.age.m, maskvalue = 1)

stand.age.mask.reclass <- reclassify(stand.age.mask, c(0, 35, 1, 35, 70, 2, 70, 105, 3, 105, 300, 4)) 

# #Create scene setting map figure.
# p1 <- ggplot() + 
	  # geom_sf(data=ca_20m, color='gray', fill=NA) + #Add the California state border
	  # geom_sf(data=sierra, color=NA, fill='black', alpha = 0.5) + #Add the Sierra Nevada polygon
	  # geom_sf(data = south.sierra.ext, color='black', fill=NA) +
	  # coord_sf() + #scalebar(1000) +
	  # annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
	  # annotation_north_arrow(location = "br", which_north = "true",
	  # height = unit(1, "cm"), width = unit(1, "cm"),
      # pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
      # style = north_arrow_minimal) + theme_bw() + 
	  # # scale_fill_manual(values = c(usdm.august2002.dataset = "red", usdm.august2015.dataset = "yellow")) +
	  # theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) 

# #Drought Study Region
# p2 <- ggplot() + 
	  # geom_sf(data=south.sierra, color='black', fill=NA, alpha = 0.5) + #First drought 
	  # ggR(img = sample.pixels.masked, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
	  # coord_sf() + #scalebar(1000) +
	  # annotation_scale(location = "br", height = unit(0.1, "cm"), width_hint = 0.2) +
	  # annotation_north_arrow(location = "bl", which_north = "true",
	  # height = unit(1, "cm"), width = unit(1, "cm"),
      # pad_x = unit(0.1, "cm"), pad_y = unit(0.5, "cm"),
      # style = north_arrow_minimal) + #theme_bw() +
	  # theme_bw() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),  legend.margin=margin(t = 0, unit='cm')) +
	  # scale_fill_manual(values = c('red'), name = "Study Area", breaks=c(1), labels=c("Treatment Pixels")) +
	  # guides(fill=guide_legend(title.position = "top", ncol = 2, nrow = 1, byrow=TRUE)) #+ ggtitle("SPI48 Within 0.5")

# p3 <- ggplot() + 
	  # geom_sf(data=south.sierra, color='black', fill=NA, alpha = 0.5) + #First drought 
	  # ggR(img = control.pixels.masked, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
	  # coord_sf() + #scalebar(1000) +
	  # annotation_scale(location = "br", height = unit(0.1, "cm"), width_hint = 0.2) +
	  # annotation_north_arrow(location = "bl", which_north = "true",
	  # height = unit(1, "cm"), width = unit(1, "cm"),
      # pad_x = unit(0.1, "cm"), pad_y = unit(0.5, "cm"),
      # style = north_arrow_minimal) + #theme_bw() +
	  # theme_bw() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8), plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),  legend.margin=margin(t = 0, unit='cm')) +
	  # scale_fill_manual(values = c('blue'), name = "Study Area", breaks=c(1), labels=c("Control Pixels")) +
	  # guides(fill=guide_legend(title.position = "top", ncol = 2, nrow = 1, byrow=TRUE)) #+ ggtitle("SPI48 Within 0.5")

# f1 <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1, common.legend = FALSE, labels = c('A', 'B', 'C'), legend = 'bottom', widths = c(0.3, 0.35, 0.35))
# ggsave(filename = 'Fig33_Sierra_control_samples.png', height=8.5, width= 14, units = 'cm', dpi=900)
# ggsave(filename = 'Fig33_Sierra_control_samples.svg', height=8.5, width= 14, units = 'cm', dpi=900, device = 'svg')

#Drought study region
p4 <- ggplot() + 
	  geom_sf(data = south.sierra, color='black', fill=NA, alpha = 0.5) +
	  ggR(img = combine.pixels.masked, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
	  coord_sf() + #scalebar(1000) +
	  annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
	  annotation_north_arrow(location = "br", which_north = "true",
	  height = unit(1, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
      style = north_arrow_minimal) + theme_bw() + 
	  theme_dark() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8),
	  legend.title = element_text( size=8), legend.text=element_text(size=6)) + guides(fill=guide_legend(title.position="top",title.hjust =0.5)) +
	  scale_fill_manual(values = c("1" = "red","2" = "blue"), breaks = c("1", "2"), name = 'Drought', labels = c("Exposed", "Not Exposed"), na.value = NA)

#Die-off metric
p5 <- ggplot() + 
	  geom_sf(data=south.sierra, color='black', fill=NA, alpha = 0.5) + #First drought 
	  ggR(img = dndmi.2017.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + 
	  coord_sf() + #scalebar(1000) +
	  theme_dark() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8),
	  legend.title = element_text( size=8), legend.text=element_text(size=6)) + 
	  scale_fill_viridis(name = 'dNDMI', direction = -1, option = "magma", limits = c(-0.2, 0.1), na.value = NA)	  

#Stand age metric
p6 <- ggplot() + 
	  geom_sf(data=south.sierra, color='black', fill=NA, alpha = 0.5) + 
	  ggR(img = stand.age.mask.reclass, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) + 
	  coord_sf() + #scalebar(1000) +
	  theme_dark() + theme(axis.title.y = element_blank(), axis.text.y = element_text(size=8), axis.title.x = element_blank(), axis.text.x = element_text(size=8),
	  legend.title = element_text( size=8), legend.text=element_text(size=6)) + guides(fill=guide_legend(title.position="top", title.hjust =0.5, nrow=2)) +
	  scale_fill_brewer(type = 'seq', palette = 'Greens', name = 'Years Since Fire', breaks = c("1", "2", "3", "4"), labels = c("0-35", "35-70", "70-105", "105+"))

f2 <- ggarrange(p4, p6, p5, ncol = 3, nrow = 1, common.legend = FALSE, labels = c('A', 'B', 'C'), legend = 'bottom')
ggsave(filename = 'Fig34_Sierra_drought_stand_age_dNDMI.png', height=8.5, width= 16, units = 'cm', dpi=900)
ggsave(filename = 'Fig34_Sierra_drought_stand_age_dNDMI.svg', height=8.5, width= 16, units = 'cm', dpi=900, device = 'svg')