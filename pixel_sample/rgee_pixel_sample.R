#Author: Carl Norlen
#Date Created: June 2, 2022
#Date Update: June 2, 2022
#Purpose: Explore pixel sampling data with rgee.

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/pixel_sample
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/pixel_sample
#Run the script: R < pixel_sample.r --vanilla
p <- c('reticulate', 'rgee', 'rgeeExtra','terra', 'sf', 'ggplot2', 'RStoolbox', 'viridis', 'tigris') 
# # install.packages('terra',repo='https://cran.r-project.org/')
# library(tigris)
# library(ggplot2)
# library(RStoolbox)

# install.packages('RStoolbox',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(raster)
#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/pixel_sample')

#Set up a python environment
# ee_install(py_env = "rgee")

#Upgrade the python API version
# ee_install_upgrade(version = '0.1.312')

#Intialize RGEE
ee_Initialize(user = 'cnorlen@uci.edu', drive = TRUE)

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

frap.year <- ee$Image('users/cnorlen/Fire_Dieoff/frap_year_bin')

# yearViz <- list(
#   min = 1919,
#   max = 2020,
#   palette = c("yellow", "orange", "red")
# )
# Map$addLayer()
# Map$addLayer(frap.year, yearViz, 'FRAP Year Data')

#Setting variable for ESPG 5070, proj4 crs
c <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#Add California Boundary shape file
us_states_20m <- states(cb = TRUE, resolution = "20m", class = "sf")
us_states_20m <- st_transform(us_states_20m, c)
ca_20m <- us_states_20m[us_states_20m$NAME == "California", ]
ca_20m <- st_as_sf(ca_20m)
ca_20m <- st_transform(ca_20m, c)

#Select USFS EcoRegion for South Sierra Nevada
usfs_in <- "D:\\Large_Files\\USFS\\data\\subsections"
usfs.regions <- st_read(file.path(usfs_in, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo') # | MAP_UNIT_S == 'M261Ev') #MAP_UNIT_S == 'M261Et' | 

#Merge Sierra Nevada polygons into one larger polygon
usfs.sierra.union <- usfs.sierra %>% st_union()

#Export the FRAP raster
frap.raster <- ee_as_raster(image = frap.year, via = 'drive', container = 'Fire_Dieoff')

#Load in the exported data for my hard drive
data_in <- 'D://Fire_Dieoff'
files <- list.files(data_in)
# files[1]

#Import as spatial rasters
frap.year.1 <- raster(file.path(data_in, files[1]))
frap.year.2 <- raster(file.path(data_in, files[3]))


#Combine the two rasters
frap.year <- merge(frap.year.1, frap.year.2)
frap.year.m <- frap.year == 0
frap.year.mask <-  mask(frap.year, mask = frap.year.m, maskvalue = 1)

#Make a figure of the raster
p1 <- ggplot() + 
ggR(img = frap.year.mask, layer = 1, maxpixels = 1e6, geom_raster = TRUE, ggLayer = TRUE) +
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

p1

ggsave(filename = 'Fig36_FRAP_conifer_forest_map.png', height=16, width= 12, units = 'cm', dpi=900)
