#Author: Carl A. Norlen
#Created Date: May 20, 2022
#Updated Date: May 20, 2022
#Analyzing fire trends in teh South Sierra Region

p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 
       'rgdal', 'sp', 'sf', 'RStoolbox', 'ncdf4', 'gtools', 'tigris', 'patchwork', 'tigris', 
       'rlist', 'ggspatial', 'svglite', 'mgcv', 'lubridate')
# install.packages(p,repo='https://cran.r-project.org/')

#Load the packages
lapply(p,require,character.only=TRUE)

#Set the working directory
setwd('C:/Users/can02/mystuff/fireDieoff/fire_trends')

#The data directory
dir_in <- "D:\\Large_Files\\CECS"
frap_in <- "D:\\Large_Files\\FRAP\\fire20_1_shp"
eco_in <- "D:\\Large_Files\\EcoRegion"
usfs_in <- "D:\\Large_Files\\USFS\\data\\subsections"
sev_in <- "D:\\Large_Files\\USFS\\Fire_Severity\\VebBurnSeverity18_1_shp"

#Add the data
#Load the FRAP data
frap <- read_sf(file.path(frap_in, "firep20_1.shp"))

#Load Prescribed Burn FRAP data
rxburn <- read_sf(file.path(frap_in, "rxburn_20_1.shp"))

#Extract the CRS from the FRAP data
frap.crs <- crs(frap)

#USFS Fire severity
fire.sev <- read_sf(file.path(sev_in, "VegBurnSeverity18.shp"))

#Transform the CRS to match FRAP
# eco.region <- st_transform(eco.region, crs(frap))

#Add California Boundary shape file
us_states_20m <- states(cb = TRUE, resolution = "20m", class = "sf")

ca_20m <- us_states_20m[us_states_20m$NAME == "California", ]
ca_20m <- st_as_sf(ca_20m)
ca_20m <- st_transform(ca_20m, frap.crs)

#Select the Sierra Polygon
# sierra <- eco.region[eco.region$US_L3NAME == 'Sierra Nevada', ]
# sierra

#Select USFS EcoRegion for South Sierra Nevada
usfs.regions <- st_read(file.path(usfs_in, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo') # | MAP_UNIT_S == 'M261Ev') #MAP_UNIT_S == 'M261Et' | 

#Merge Sierra Nevada polygons into one larger polygon
usfs.sierra.union <- usfs.sierra %>% st_union()
usfs.sierra.union <- st_transform(usfs.sierra.union, frap.crs)
usfs.sierra.sf <- st_as_sf(usfs.sierra.union)

#Check which FRAP polygons are valid
frap$valid <- sf::st_is_valid(frap)

#Add the fire date
frap$fire.date <- as.Date(frap$ALARM_DATE)
frap$year <- format(frap$fire.date, '%Y')
frap$Area.sf <- st_area(frap) #m^2 units

#Add date to FRAP Rx Burn
rxburn <- st_transform(rxburn, frap.crs)
rxburn$fire.date <- as.Date(rxburn$START_DATE)
rxburn$year <- format(rxburn$fire.date, '%Y')
rxburn$Area.sf <- st_area(rxburn) #m^2 units

#Add date to Fire Severity data
fire.sev <- st_transform(fire.sev, frap.crs)
fire.sev$Area.sf <- st_area(fire.sev)
fire.sev$Area.sf %>% as.numeric()
#Get FRAP polygons that touch the South Sierra USFS Regions
frap.sierra <- st_join(frap[frap$valid == TRUE, ], usfs.sierra.sf)
frap.sierra
#Get the prescribed burn polygons in the South Sierra USFS Regions
rxburn.sierra <- st_join(rxburn, usfs.sierra.sf)

#Join the Fire Severity and South Sierra Region
fire.sev.sierra <- st_join(fire.sev, usfs.sierra.sf)


#Create an EcoREgion map
p1 <- ggplot() + 
      coord_sf() + 
      theme_bw() + 
      #Add the California perimeter
      geom_sf(data = ca_20m, color = 'black', alpha = 0.0, size = 0.5) +
      #Add the South Sierra Perimeter
      geom_sf(data = usfs.sierra.sf,  color='black', fill = 'black', alpha = 1, size = 0.1) +
      #Add a scale bar
      annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
      #Add a North Arrow
      annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_minimal) + theme_bw() + guides(fill = guide_legend(title.position = "top"))
p1

ggsave(filename = 'Fig1_Ecoregion_map.png', height=12, width=9, units = 'cm', dpi=900)


#Add Fire colors
cols <- c("Wildfire"="black","RxBurn"="gray")
lines <- c("Wildfire" = "dotdash", "RxBurn" = "solid")

#Create a time series of FRAP burned area
p2 <- ggplot() +
  #Line of total FRAP burned area in the South Sierra
  geom_line(data = as.data.frame(frap.sierra[frap.sierra$year >= 1850 & frap.sierra$year <= 2020, ]) %>% 
              group_by(year, .groups = 'keep') %>% summarize(Area = sum(GIS_ACRES)), 
            mapping = aes(x = as.Date(as.character(year), format = "%Y"), y = Area * 1/2.471, color = 'Wildfire', linetype = 'Wildfire'), size = 1) + 
  #Line of total RxBurn burned area in the South Sierra
  geom_line(data = as.data.frame(rxburn.sierra[rxburn.sierra$year >= 1850 & rxburn.sierra$year <= 2020, ]) %>% 
              group_by(year, .groups = 'keep') %>% summarize(Area = sum(GIS_ACRES)), 
            mapping = aes(x = as.Date(as.character(year), format = "%Y"), y = Area * 1/2.471, color = 'RxBurn', linetype = 'RxBurn'), size = 1) +
  #Do the black and white theme
  theme_bw() + 
  #Figure
  theme(legend.position = c(0.15, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_colour_manual(name="Fire Type (FRAP)",values=cols, aesthetics = 'color') +
  scale_linetype_manual(name="Fire Type (FRAP)", values = lines) +
  ylab('Burned Area (ha)') + xlab('Year')
p2

ggsave(filename = 'Fig2_FRAP_time_series_South_Sierra.png', height=10, width=16, units = 'cm', dpi=900)

#Create a time series of Fire Severity burned area
p3 <- ggplot() +
  #Line of total USFS burned area in the South Sierra
  geom_line(data = as.data.frame(fire.sev.sierra) %>% filter(!is.na(BURNSEV) & BURNSEV != 255) %>%
              group_by(FIRE_YEAR, BURNSEV, .groups = 'keep') %>% summarize(Area = sum(Shape_Area)), 
            mapping = aes(x = as.Date(as.character(FIRE_YEAR), format = "%Y"), y = Area * 1/10000, 
                          color = as.factor(BURNSEV), linetype = as.factor(BURNSEV)), size = 1) + 
  #Line of total USFS burned area in the South Sierra
  geom_line(data = as.data.frame(fire.sev.sierra) %>% filter(!is.na(BURNSEV) & BURNSEV != 255) %>%
              group_by(FIRE_YEAR, .groups = 'keep') %>% summarize(Area = sum(Shape_Area)), 
            mapping = aes(x = as.Date(as.character(FIRE_YEAR), format = "%Y"), y = Area * 1/10000), 
            color = 'black', alpha = 0.4, size = 1.5) + 
  #Do the black and white theme
  theme_bw() + 
  #Figure
  theme(legend.position = c(0.15, 0.8), legend.background = element_rect(colour = NA, fill = NA),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_discrete(name="Fire Severity", labels = c('Unchanged', 'Low', 'Moderate', 'High'), aesthetics = 'color') +
  scale_linetype_discrete(name="Fire Severity", labels = c('Unchanged', 'Low', 'Moderate', 'High')) +
  ylab('Burned Area (ha)') + xlab('Year')
p3

ggsave(filename = 'Fig3_USFS_Fire_Severity_time_series_South_Sierra.png', height=10, width=16, units = 'cm', dpi=900)
