#Author: Carl Norlen
#Date Created: June 2, 2022
#Date Update: June 23, 2023
#Purpose: Explore pixel sampling data with rgee.

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('sf', 'ggplot2', 'tidyterra', 'viridis', 'tigris','terra') 
# install.packages('tidyterra',repo='https://cran.r-project.org/')
# library(tidyterra)
# library(ggplot2)
# library(RStoolbox)
# library(sf)
# install.packages(p,repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(raster)
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/pixel_sample')

#Set up a python environment
# ee_install(py_env = "rgee")

#Upgrade the python API version
# ee_install_upgrade(version = '0.1.312')

#Intialize RGEE
# ee_Initialize(user = 'cnorlen@uci.edu', drive = TRUE)

#Install a python package
# py_install("jsbeautifier")

#Import a python package
# regex <- import("regex")
# jsb <- import("jsbeautifier")

#Load a GEE package
# palettes <- module('users/gena/packages:palettes')
# js.batch <- 'https://github.com/fitoprincipe/geetools-code-editor/blob/master/batch'
# batch <- module(js.batch)
# batch
#Some how the LST module works, but other ones don't
# lsmod <- 'users/sofiaermida/landsat_smw_lst:modules/Landsat_LST.js'
# mod <- module(lsmod)
# mod
# geom <- ee$Geometry$Rectangle(-8.91, 40.0, -8.3, 40.4)
# LST <- mod$collection("L8", "2018-05-15", "2018-05-31", geom, TRUE)
# print(LST$first)
# LST$mean
# Map$addLayer(LST$first, {}, 'LST')

# frap.year <- ee$Image('users/cnorlen/Fire_Dieoff/frap_year_bin')

# yearViz <- list(
#   min = 1919,
#   max = 2020,
#   palette = c("yellow", "orange", "red")
# )
# Map$addLayer()
# Map$addLayer(frap.year, yearViz, 'FRAP Year Data')

#Setting variable for ESPG 5070, proj4 crs
# c <- terra::crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
# terra::crs
#Add California Boundary shape file
us_states_20m <- states(cb = TRUE, resolution = "20m", class = "sf")
# us_states_20m <- st_transform(us_states_20m, c)
ca_20m <- us_states_20m[us_states_20m$NAME == "California", ]
ca_20m <- st_as_sf(ca_20m)
# ca_20m <- st_transform(ca_20m, c)

#Select USFS EcoRegion for South Sierra Nevada
usfs_in <- "D:\\Large_Files\\USFS\\data\\subsections"
frap_in <- "D:\\Large_Files\\FRAP\\fire21_1_shp"
usfs.regions <- st_read(file.path(usfs_in, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo') # | MAP_UNIT_S == 'M261Ev') #MAP_UNIT_S == 'M261Et' | 

#Merge Sierra Nevada polygons into one larger polygon
usfs.sierra.union <- usfs.sierra %>% st_union()
st_bbox(usfs.sierra.union)
usfs.sierra.union$bbox
sierra.extent <- st_bbox(usfs.sierra.union)
sierra.extent
c <- st_crs(usfs.sierra.union)
# frap.files <- list.files(file.path(frap_in))

#Get the FRAP and FRAP Rx data
frap <- read_sf("D:\\Large_Files\\FRAP\\fire21_1_shp\\firep21_1.shp")
st_is_valid(frap)
frap$valid <- st_is_valid(frap)

#Intersect may not be the best option
frap.sierra <- st_intersection(st_transform(frap[frap$valid == TRUE, ],c), usfs.sierra.union)
# frap
# list.files(file.path(frap_in))
#Load Prescribed Burn FRAP data
#The shape files won't load for some reason
rxburn <- read_sf("D:\\Large_Files\\FRAP\\fire21_1_shp\\rxburn21_1.shp")
# crs(sierra.extent) <- c
#Export the FRAP raster
# frap.raster <- ee_as_raster(image = frap.year, via = 'drive', container = 'Fire_Dieoff')

#Load in the exported data for my hard drive
data_in <- 'D://Fire_Dieoff'
files <- list.files(data_in)
files
# files[1]

#Import as spatial rasters
#Select FRAP fire rasters
# frap.year.1 <- raster::raster(file.path(data_in, files[4]))
# frap.year.2 <- raster::raster(file.path(data_in, files[5]))

#Select the frap year layers
# frap.year.1990 <- raster::raster(file.path(data_in, files[16]))
# frap.year.1990.m <- frap.year.1990 == 0
# frap.year.1990.mask <-  raster::mask(frap.year.1990, mask = frap.year.1990.m, maskvalue = 1)
# 
# frap.year.2000 <- raster::raster(file.path(data_in, files[17]))
# frap.year.2000.m <- frap.year.2000 == 0
# frap.year.2000.mask <-  raster::mask(frap.year.2000, mask = frap.year.2000.m, maskvalue = 1)
#FRAP Wildfire layer
frap.year.2010 <- terra::rast(file.path(data_in, 'FRAP_wildfire_2010_300m.tif'))
frap.year.2010.m <- frap.year.2010== 0
frap.year.2010.mask <-  terra::mask(frap.year.2010, mask = frap.year.2010.m, maskvalue = 1)
c <- crs(frap.year.2010.mask)
# st_crs(usfs.sierra.union)
# frap.year.2010.mask
#Hack to deal with this until I find a better way
sierra.extent <- ext(project(vect(usfs.sierra.union),c))
# sierra.extent <- ext(-119.9852, 34.8167, -117.8697, 38.82613)
frap.year.2010.mask.crop <- terra::crop(frap.year.2010.mask, sierra.extent)

frap.buffer.year.2010 <- terra::rast(file.path(data_in, 'FRAP_wildfire_buffer_2010_300m.tif'))
frap.buffer.year.2010.m <- frap.buffer.year.2010== 0
frap.buffer.year.2010.mask <-  terra::mask(frap.buffer.year.2010, mask = frap.buffer.year.2010.m, maskvalue = 1)
frap.buffer.year.2010.mask.crop <- terra::crop(frap.buffer.year.2010.mask, sierra.extent)

#FRAP Rx Wildfire layer
rx.year.2010 <- terra::rast(file.path(data_in, 'FRAP_Rxfire_2010_300m.tif'))
rx.year.2010.m <- rx.year.2010== 0
rx.year.2010.mask <-  terra::mask(rx.year.2010, mask = rx.year.2010.m, maskvalue = 1)

rx.buffer.year.2010 <- terra::rast(file.path(data_in, 'FRAP_Rxfire_buffer_2010_300m.tif'))
rx.buffer.year.2010.m <- rx.buffer.year.2010== 0
rx.buffer.year.2010.mask <-  terra::mask(rx.year.buffer.2010, mask = rx.year.buffer.2010.m, maskvalue = 1)

#USFS Sev fire layer
sev.year.2010 <- terra::rast(file.path(data_in, 'USFS_fire_severity_2010_300m.tif'))
sev.year.2010.m <- sev.year.2010 == 0
sev.year.2010.mask <-  raster::mask(sev.year.2010, mask = sev.year.2010.m, maskvalue = 1)

sev.buffer.year.2010 <- terra::rast(file.path(data_in, 'USFS_fire_severity_buffer_2010_300m.tif'))
sev.buffer.year.2010.m <- sev.buffer.year.2010 == 0
sev.buffer.year.2010.mask <-  terra::mask(sev.buffer.year.2010, mask = sev.buffer.year.2010.m, maskvalue = 1)

#FRAP 2010
p1 <- ggplot() + 
  geom_sf(data = frap.sierra %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010), mapping = aes(fill = YEAR_)) +
  # geom_spatraster(data = frap.buffer.year.2010.mask.crop, mapping = aes(fill = fire_type_last)) +
  # geom_spatraster(data = frap.year.2010.mask.crop, mapping = aes(fill = fire_type_last)) +
  # geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis_d(name = 'Fire Type', option = 'inferno', na.value = NA) + theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = 'right',
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p1

ggsave(filename = 'Fig1a_frap_2010_map.png', height=16, width= 12, units = 'cm', dpi=900)

#Make a figure of the raster
p2 <- ggplot() + 
geom_spatraster(data = frap.year.2000.mask, layer = 1, maxcell = 1e6) +
geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
coord_sf() + xlab('longitude') + ylab('latitude') +
scale_fill_viridis(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p2

ggsave(filename = 'Fig37_FRAP_year_2000_map.png', height=16, width= 12, units = 'cm', dpi=900)

p3 <- ggplot() + 
  ggR(img = frap.year.2010.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p3

ggsave(filename = 'Fig38_FRAP_year_2010_map.png', height=16, width= 12, units = 'cm', dpi=900)

p4 <- ggplot() + 
  ggR(img = frap.year.2020.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p4

ggsave(filename = 'Fig39_FRAP_year_2020_map.png', height=16, width= 12, units = 'cm', dpi=900)

#FRAP Count map
p5 <- ggplot() + 
  ggR(img = frap.count.1990.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = '# of Fires', option = 'viridis', direction = -1, na.value = NA, discrete = TRUE, na.translate = F) + theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.5),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p5

ggsave(filename = 'Fig40_FRAP_count_1990_map.png', height=16, width= 12, units = 'cm', dpi=900)

p6 <- ggplot() + 
  ggR(img = frap.count.2000.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = '# of Fires', option = 'viridis', direction = -1, na.value = NA, discrete = TRUE, na.translate = F) + theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.5),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p6

ggsave(filename = 'Fig41_FRAP_count_2000_map.png', height=16, width= 12, units = 'cm', dpi=900)

p7 <- ggplot() + 
  ggR(img = frap.count.2010.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = '# of Fires', option = 'viridis', direction = -1, na.value = NA, discrete = TRUE, na.translate = F) + theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.5),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p7

ggsave(filename = 'Fig42_FRAP_count_2010_map.png', height=16, width= 12, units = 'cm', dpi=900)

p8 <- ggplot() + 
  ggR(img = frap.count.2020.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = '# of Fires', option = 'viridis', direction = -1, na.value = NA, discrete = TRUE, na.translate = F) + theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.5),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p8

ggsave(filename = 'Fig43_FRAP_count_2020_map.png', height=16, width= 12, units = 'cm', dpi=900)

# frap.year.mask
#Create map of prescirbed burn versus 
p9 <- ggplot() + 
  ggR(img = frap.type.1990.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Fire Type', option = 'magma', begin = 0.2, end = 0.8, na.value = NA, discrete = TRUE, na.translate = F, breaks = c(1, 2), labels = c('Wild', 'Prescribed')) + 
  theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p9

ggsave(filename = 'Fig44_FRAP_type_1990_map.png', height=16, width= 12, units = 'cm', dpi=900)

p10 <- ggplot() + 
  ggR(img = frap.type.2000.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Fire Type', option = 'magma', begin = 0.2, end = 0.8, na.value = NA, discrete = TRUE, na.translate = F, breaks = c(1, 2), labels = c('Wild', 'Prescribed')) + 
  theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p10

ggsave(filename = 'Fig45_FRAP_type_2000_map.png', height=16, width= 12, units = 'cm', dpi=900)

p11 <- ggplot() + 
  ggR(img = frap.type.2010.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Fire Type', option = 'magma', begin = 0.2, end = 0.8, na.value = NA, discrete = TRUE, na.translate = F, breaks = c(1, 2), labels = c('Wild', 'Prescribed')) + 
  theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p11

ggsave(filename = 'Fig46_FRAP_type_2010_map.png', height=16, width= 12, units = 'cm', dpi=900)

p12 <- ggplot() + 
  ggR(img = frap.type.2020.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Fire Type', option = 'magma', begin = 0.2, end = 0.8, na.value = NA, discrete = TRUE, na.translate = F, breaks = c(1, 2), labels = c('Wild', 'Prescribed')) + 
  theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p12

ggsave(filename = 'Fig47_FRAP_type_2020_map.png', height=16, width= 12, units = 'cm', dpi=900)

#ADS Data
p13 <- ggplot() + 
  ggR(img = ads.2007.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Die-off (ADS)', option = 'magma', na.value = NA, limits = c(0,30)) + 
  theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p13

ggsave(filename = 'Fig48_ADS_2007_map.png', height=16, width= 12, units = 'cm', dpi=900)

p14 <- ggplot() + 
  ggR(img = ads.2011.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Die-off (ADS)', option = 'magma', na.value = NA, limits = c(0,30)) + 
  theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p14

ggsave(filename = 'Fig49_ADS_2011_map.png', height=16, width= 12, units = 'cm', dpi=900)

p15 <- ggplot() + 
  ggR(img = ads.2018.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE) +
  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('longitude') + ylab('latitude') +
  scale_fill_viridis(name = 'Die-off (ADS)', option = 'magma', na.value = NA, limits = c(0,100)) + 
  theme_bw() + 
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p15

ggsave(filename = 'Fig50_ADS_2018_map.png', height=16, width= 12, units = 'cm', dpi=900)