#Author: Carl Norlen
#Date Created: June 2, 2022
#Date Update: November 10, 2023
#Purpose: Explore pixel sampling data with rgee.

#Add the needed R packages
p <- c('sf', 'ggplot2', 'tidyterra', 'viridis', 'tigris','terra', 'ggpubr', 'scales', 'dplyr', 'tidyr', 'svglite', 'patchwork') 

#Load the packages
lapply(p,require,character.only=TRUE)

#Home Computer directories
setwd('C:/Users/can02/mystuff/fireDieoff/figures')
usfs_in <- "D:\\Large_Files\\USFS\\data\\subsections"
frap_in <- "D:\\Large_Files\\FRAP\\fire21_1_shp"
sev_in <- "D:\\Large_Files\\USFS\\Fire_Severity\\VebBurnSeverity18_1_shp"

#Add California Boundary shape file
us_states_20m <- states(cb = TRUE, resolution = "20m", class = "sf")
ca_20m <- us_states_20m[us_states_20m$NAME == "California", ]
ca_20m <- st_as_sf(ca_20m)

#Select USFS EcoRegion for South Sierra Nevada
usfs.regions <- st_read(file.path(usfs_in, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo') # | MAP_UNIT_S == 'M261Ev') #MAP_UNIT_S == 'M261Et' | 

#Merge Sierra Nevada polygons into one larger polygon
usfs.sierra.union <- usfs.sierra %>% st_union()

#Calculate the Area of the South Sierra polygons
st_area(usfs.sierra.union) * 0.0001

#Get the FRAP and FRAP Rx data
# frap <- read_sf("D:\\Large_Files\\FRAP\\fire21_1_shp\\firep21_1.shp")
frap <- read_sf("D:\\Large_Files\\FRAP\\fire21_1_shp\\firep21_1_repair.shp")
c <- st_crs(frap)
#Add the Wildfire Perimeters
frap$intersects <- st_intersects(frap, st_transform(usfs.sierra.union,c)) %>% lengths > 0
frap$title <- 'Wild Fire Perimeters'
frap.clip <- st_intersection(frap, st_transform(usfs.sierra.union,c))
frap.clip$area <- st_area(frap.clip)
frap.clip <- frap.clip %>% mutate(type = 'Wild')

#Add the Prescribed Fire Perimeters
rxburn <- read_sf("D:\\Large_Files\\FRAP\\fire21_1_shp\\rxburn21_1.shp")
rxburn$intersects <- st_intersects(rxburn, st_transform(usfs.sierra.union,c)) %>% lengths > 0
rxburn$title <- 'Prescribed Fire Perimeters'
rxburn.clip <- st_intersection(rxburn, st_transform(usfs.sierra.union,c))
rxburn.clip$area <- st_area(rxburn.clip)
rxburn.clip <- rxburn.clip %>% mutate(type = 'Prescribed')

#combined FRAP and Rx
frap.rxburn.clip <- dplyr::bind_rows(frap.clip, rxburn.clip)

#Add the Fire Severity Perimeters
fire.sev <- read_sf(file.path(sev_in, "VegBurnSeverity18.shp"))
fire.sev$intersects <- st_intersects(fire.sev, st_transform(usfs.sierra.union,c)) %>% lengths > 0
fire.sev.clip <- st_intersection(fire.sev, st_transform(usfs.sierra.union,c))
fire.sev.clip$area <- st_area(fire.sev.clip)

#Total perimeters
fire.sev.perimeter <- read_sf("D:\\Large_Files\\Fire_Severity\\veg_severity_perimeters\\veg_severity_perimeters.shp")
fire.sev.perimeter$intersects <- st_intersects(fire.sev.perimeter, st_transform(usfs.sierra.union,c)) %>% lengths > 0
fire.sev.perimeter$title <- 'Fire Severity Perimeters'
fire.sev.perimeter.clip <- st_intersection(fire.sev.perimeter, st_transform(usfs.sierra.union,c))
fire.sev.perimeter.clip$area <- st_area(fire.sev.perimeter.clip)
# plot(fire.sev.perimeter)
# plot(fire.sev.perimeter.clip)

#Number of FRAP fires
frap %>% filter(intersects == TRUE & YEAR_ <= 2010 & YEAR_ >= 1987) %>% count()

#Number of rx fires
rxburn %>% filter(intersects == TRUE & YEAR_ <= 2010 & YEAR_ >= 1987) %>% count()

#Number of Fire severity fires
fire.sev.perimeter %>% filter(intersects == TRUE & FIRE_YEAR <= 2010 & FIRE_YEAR >= 1987) %>% count()

#FRAP permeters
p1a <- ggplot() + 
  geom_sf(data = frap.clip %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE), mapping = aes(fill = as.numeric(YEAR_))) +
  geom_sf(data = rxburn.clip %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE), mapping = aes(fill = as.numeric(YEAR_))) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('Longitude') + ylab('Latitude') +
  scale_fill_viridis_c(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + #facet_wrap(~title) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12), plot.margin = unit(c(0,0,0,0), "pt"),
    axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
    legend.direction = "vertical")
p1a

p1.inset <- 
            ggplot() +
              geom_sf(data = ca_20m, color='black', size = 0.4,  fill = NA) +
              geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = 'black') +
                coord_sf() + theme_bw() +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill=NULL, colour=NULL))

p1a.inset <- (p1a + inset_element(p1.inset, 0.5, 0.75, 0.99, 0.99))


#Add Fire colors
#Create the palette
cols <- c(brewer_pal('div', "Set2")(2)[2], brewer_pal('div', "Set2")(2)[1])

#Add the Burned Area Time Series
p1b <- ggplot() +
  #Bar chart total FRAP burned area in the South Sierra
  geom_bar(stat = 'identity', data = frap.rxburn.clip %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010) %>% # intersects == TRUE & 
              group_by(YEAR_, type, .groups = 'keep') %>% reframe(Area = sum(as.numeric(area))), 
            mapping = aes(x = as.Date(as.character(YEAR_), format = "%Y"), y = Area * 1/10000, fill = as.factor(type)), linewidth = 1, alpha = 0.8) +  
  theme_bw() + 
  #Figure
  theme(legend.position = c(0.25, 0.8), legend.background = element_rect(colour = NA, fill = NA), legend.direction = "vertical",
        legend.title = element_text(size = 12), legend.text = element_text(size = 10), 
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        axis.text.x = element_blank(), axis.title.x = element_blank()) +
  scale_fill_manual(name="Fire Type",values=cols, aesthetics = 'fill') +
  scale_y_continuous(labels = comma) +
  ylab('Burned Area (ha)') + xlab(NULL)
p1b

#Create the fire severity palette
mypalette <- brewer_pal('seq', "YlOrRd")(5)[2:5]

#Create a time series of Fire Severity burned area
p1c <- ggplot() +
  geom_bar(stat = 'identity', data = fire.sev.clip %>% filter(FIRE_YEAR >= 1987 & FIRE_YEAR <= 2010 & BURNSEV != 255) %>% # intersects == TRUE &
              group_by(FIRE_YEAR, BURNSEV, .groups = 'keep') %>% reframe(Area = sum(as.numeric(area))), 
            mapping = aes(x = as.Date(as.character(FIRE_YEAR), format = "%Y"), y = Area * 1/10000, 
                          fill = as.factor(BURNSEV)), linewidth = 1, alpha = 0.8) + 
  #Do the black and white theme
  theme_bw() + 
  #Figure
  theme(legend.position = c(0.25, 0.7), legend.background = element_rect(colour = NA, fill = NA), legend.direction = "vertical",
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12)) +
  scale_fill_manual(name="Fire Severity", labels = c('Lowest', 'Low', 'Moderate', 'High'), values = mypalette, aesthetics = 'fill') +
  # scale_linetype_discrete(name="Fire Severity", labels = c('Unchanged', 'Low', 'Moderate', 'High')) +
  scale_y_continuous(labels = comma) +
  ylab('Burned Area (ha)') + xlab('Year')
p1c

#Combining the panels into one plot with patchwork
p2 <- (p1a.inset)
p3 <- (p1b / p1c) 

(p2 | p3) + plot_layout(ncol = 2, widths = c(1.1, 0.9)) + plot_annotation(tag_levels = list(c('a','','b','c')))

#Save the plots
ggsave(filename = 'Fig1_fire_maps.png', height=18, width= 18, units = 'cm', dpi=900)

#Supplementary Figure 1 
#Shows where the different Fire perimeters are
p2a <- ggplot() + 
  geom_sf(data = frap.clip %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE), mapping = aes(fill = as.numeric(YEAR_))) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('Longitude') + ylab('Latitude') +
  scale_fill_viridis_c(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + facet_wrap(~title) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.3, 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    strip.text.x = element_text(size = 12),
    axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
    legend.direction = "vertical")
p2a

#Prescribed Fire Perimeters
p2b <- ggplot() + 
  geom_sf(data = rxburn.clip %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE), mapping = aes(fill = as.numeric(YEAR_))) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('Longitude') + ylab(NULL) +
  scale_fill_viridis_c(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + facet_wrap(~title) +
  theme(
    legend.justification = c(1, 0),
    legend.position = 'none',
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    strip.text.x = element_text(size = 12),
    axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
    legend.direction = "vertical")
p2b

#Fire Severity Perimeters
p2c <- ggplot() + 
  # geom_sf(data = frap %>% filter(YEAR_ >= 1987 & YEAR_ <= 2010 & intersects == TRUE), mapping = aes(fill = YEAR_), color = 'black', linewidth = 1) +
  geom_sf(data = fire.sev.perimeter.clip %>% filter(FIRE_YEAR >= 1987 & FIRE_YEAR <= 2010 & intersects == TRUE), mapping = aes(fill = as.numeric(FIRE_YEAR))) +
  geom_sf(data = usfs.sierra.union, color='black', size = 0.4,  fill = NA) +
  coord_sf() + xlab('Longitude') + ylab(NULL) +
  scale_fill_viridis_c(name = 'Fire Year', option = 'inferno', na.value = NA) + theme_bw() + facet_wrap(~title) +
  theme(
    legend.justification = c(1, 0),
    legend.position = 'none',
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    strip.text.x = element_text(size = 12),
    axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
    legend.direction = "vertical")

p2c

p4 <- (p2a | p2b | p2c) + plot_annotation(tag_levels = 'a')

#Save the plots
ggsave(filename = 'FigS1_fire_maps.png', height=20, width= 30, units = 'cm', dpi=900)