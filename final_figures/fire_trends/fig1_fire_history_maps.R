#Author: Carl Norlen
#Date Created: June 2, 2022
#Date Update: June 26, 2023
#Purpose: Explore pixel sampling data with rgee.

#Run the script: R < pixel_sample.r --vanilla
p <- c('sf', 'ggplot2', 'tidyterra', 'viridis', 'tigris','terra', 'ggpubr', 'scales', 'dplyr', 'tidyr', 'svglite') 
install.packages('svglite',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)
# library(raster)
# library(svglite)

#Home Computer directories
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/fire_trends')
usfs_in <- "D:\\Large_Files\\USFS\\data\\subsections"
frap_in <- "D:\\Large_Files\\FRAP\\fire21_1_shp"
sev_in <- "D:\\Large_Files\\USFS\\Fire_Severity\\VebBurnSeverity18_1_shp"

#Add California Boundary shape file
us_states_20m <- states(cb = TRUE, resolution = "20m", class = "sf")
# us_states_20m <- st_transform(us_states_20m, c)
ca_20m <- us_states_20m[us_states_20m$NAME == "California", ]
ca_20m <- st_as_sf(ca_20m)
# ca_20m <- st_transform(ca_20m, c)

#Select USFS EcoRegion for South Sierra Nevada
usfs.regions <- st_read(file.path(usfs_in, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo') # | MAP_UNIT_S == 'M261Ev') #MAP_UNIT_S == 'M261Et' | 

#Merge Sierra Nevada polygons into one larger polygon
usfs.sierra.union <- usfs.sierra %>% st_union()
st_bbox(usfs.sierra.union)
usfs.sierra.union$bbox
sierra.extent <- st_bbox(usfs.sierra.union)
sierra.extent

# frap.files <- list.files(file.path(frap_in))

#Get the FRAP and FRAP Rx data
frap <- read_sf("D:\\Large_Files\\FRAP\\fire21_1_shp\\firep21_1.shp")
c <- st_crs(frap)
# st_crs(frap)
# st_is_valid(frap)
# frap$valid <- st_is_valid(frap)

#Add the Wildfire Perimeters
frap$intersects <- st_intersects(frap, st_transform(usfs.sierra.union,c)) %>% lengths > 0
frap$title <- 'Wild Fire Perimeters'
#Add the Prescribed Fire Perimeters
rxburn <- read_sf("D:\\Large_Files\\FRAP\\fire21_1_shp\\rxburn21_1.shp")
rxburn$intersects <- st_intersects(rxburn, st_transform(usfs.sierra.union,c)) %>% lengths > 0
rxburn$title <- 'Prescribed Fire Perimeters'
#Add the Fire Severity Perimeters
fire.sev <- read_sf(file.path(sev_in, "VegBurnSeverity18.shp"))
fire.sev$intersects <- st_intersects(fire.sev, st_transform(usfs.sierra.union,c)) %>% lengths > 0
fire.sev.perimeter <- read_sf("D:\\Large_Files\\Fire_Severity\\veg_severity_perimeters\\veg_severity_perimeters.shp")
fire.sev.perimeter$intersects <- st_intersects(fire.sev.perimeter, st_transform(usfs.sierra.union,c)) %>% lengths > 0
fire.sev.perimeter$title <- 'Fire Severity Perimeters'

#Number of FRAP fires
frap %>% filter(intersects == TRUE & YEAR_ <= 2010 & YEAR_ >= 1987) %>% count()

#Number of rx fires
rxburn %>% filter(intersects == TRUE & YEAR_ <= 2010 & YEAR_ >= 1987) %>% count()

#Number of Fire severity fires
fire.sev.perimeter %>% filter(intersects == TRUE & FIRE_YEAR <= 2010 & FIRE_YEAR >= 1987) %>% count()
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

#FRAP permeters
p1a <- ggplot() + 
  geom_sf(data = frap %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE), mapping = aes(fill = as.numeric(YEAR_))) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('Longitude') + ylab('Latitude') +
  scale_fill_viridis_c(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + facet_wrap(~title) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.3, 0.2),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")
p1a

#Prescribed Fire Perimeters
p1b <- ggplot() + 
  geom_sf(data = rxburn %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE), mapping = aes(fill = as.numeric(YEAR_))) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('Longitude') + ylab(NULL) +
  scale_fill_viridis_c(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + facet_wrap(~title) +
  theme(
    legend.justification = c(1, 0),
    legend.position = 'none',
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")
p1b

#Fire Severity Perimeters
p1c <- ggplot() + 
  # geom_sf(data = frap %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE), mapping = aes(fill = YEAR_), color = 'black', linewidth = 1) +
  geom_sf(data = fire.sev.perimeter %>% filter(FIRE_YEAR >= 1987 & FIRE_YEAR <= 2010 & intersects == TRUE), mapping = aes(fill = as.numeric(FIRE_YEAR))) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('Longitude') + ylab(NULL) +
  scale_fill_viridis_c(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + facet_wrap(~title) +
  theme(
    legend.justification = c(1, 0),
    legend.position = 'none',
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

p1c

#Add Fire colors
#Create the palette
# mypalette <- brewer_pal('seq', "Set2")(2)[1:1]
# brewer_pal('div', "Set2")(2)[1]
cols <- c("Wild"=brewer_pal('div', "Set2")(2)[2], "Prescribed" = brewer_pal('div', "Set2")(2)[1])
# lines <- c("Wild" = "dashed", "Prescribed" = "solid")

#Add the Burned Area Time Series
p1d <- ggplot() +
  #Line of total FRAP burned area in the South Sierra
  geom_line(data = frap %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE) %>%
group_by(YEAR_, .groups = 'keep') %>% reframe(Area = sum(Shape_Area)), 
mapping = aes(x = as.Date(as.character(YEAR_), format = "%Y"), y = Area / 10000, color = "Wild"), linewidth = 1) +  #, color = 'South Wildfire', linetype = 'South Wildfire'), size = 1) + 
  # Line of total RxBurn burned area in the South Sierra
  geom_line(data = rxburn %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE) %>%
              group_by(YEAR_, .groups = 'keep') %>% reframe(Area = sum(Shape_Area)),
            mapping = aes(x = as.Date(as.character(YEAR_), format = "%Y"), y = Area /10000, color = "Prescribed"), size = 1) +
  theme_bw() + 
  #Figure
  theme(legend.position = c(0.1, 0.8), legend.background = element_rect(colour = NA, fill = NA), legend.direction = "horizontal",
        legend.title = element_text(size = 8), legend.text = element_text(size = 6), axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.text.x = element_blank(), axis.title.x = element_blank()) +
  scale_colour_manual(name="Fire Type",values=cols, aesthetics = 'color') +
  # scale_linetype_manual(name="Fire Type (FRAP)", values = lines) +
  ylab('Burned Area (ha)') + xlab(NULL)
p1d

#Create the fire severity palette
mypalette <- brewer_pal('seq', "YlOrRd")(5)[2:5]

#Create a time series of Fire Severity burned area
p1e <- ggplot() +
  #Line of total USFS burned area in the South Sierra
  geom_line(data = fire.sev %>% filter(FIRE_YEAR >= 1987 & FIRE_YEAR <= 2010 & intersects == TRUE & BURNSEV != 255) %>%
              group_by(FIRE_YEAR, BURNSEV, .groups = 'keep') %>% reframe(Area = sum(Shape_Area)), 
            mapping = aes(x = as.Date(as.character(FIRE_YEAR), format = "%Y"), y = Area * 1/10000, 
                          color = as.factor(BURNSEV)), size = 1) + 
  #Do the black and white theme
  theme_bw() + 
  #Figure
  theme(legend.position = c(0.165, 0.7), legend.background = element_rect(colour = NA, fill = NA), legend.direction = "horizontal",
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_manual(name="Fire Severity", labels = c('Unchanged', 'Low', 'Moderate', 'High'), values = mypalette, aesthetics = 'color') +
  # scale_linetype_discrete(name="Fire Severity", labels = c('Unchanged', 'Low', 'Moderate', 'High')) +
  ylab('Burned Area (ha)') + xlab('Year')
p1e

#Combining the panels into one plot with patchwork
p2 <- (p1a | p1b | p1c) #/ 
p3 <- (p1d / p1e) #+  plot_layout(widths = 0.8)) + plot_annotation(tag_levels = 'a')

#layout the plots
layout <- c(
  area(t = 1, l = 1, b = 9, r = 4),
  area(t = 10.5, l = 1, b = 12.5, r = 4)
)

p2 / p3 +  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')

#Save the plots
ggsave(filename = 'Fig1_fire_maps.png', height=30, width= 30, units = 'cm', dpi=900)
# ggsave(filename = 'Fig1_fire_maps.svg', height=30, width= 30, units = 'cm', dpi=900)
