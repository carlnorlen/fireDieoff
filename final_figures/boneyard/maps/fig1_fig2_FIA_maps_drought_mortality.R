#Author: Carl Norlen
#Date Created: August 23, 2022
#Date Edited: January 31, 2023
#Purpose: Create maps of FIA data for the Sierra Nevada (and SoCal Mountains?) with average StandAge total Basal Area and Dead Basal Area?

# Specify necessary packages
p <- c("RSQLite","dbplyr","ggplot2","dplyr","tidyr", "ggpubr", "RColorBrewer",  
       'gt', 'broom', 'sf')

# If necessary: install/update packages
# install.packages('sf',repo='https://cloud.r-project.org/')
# library("agricolae")
# library('sf')
# Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
#setwd("/C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/field_data")
#cd /C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/field_data
#cd /C/Users/can02/mystuff/fireDieoff/FIA
#Command for calling the script in the command line: R < stand_age_dieoff.r --vanilla
#INstalling packages: install.packages('RColorBrewer',repo='https://cran.cnr.berkeley.edu/')
# setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/maps')
setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/maps')
#Add Data Sets
sql_dir <- 'C:\\Users\\Carl\\mystuff\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
# sql_dir <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
# dir_usfs <- "C:\\Users\\Carl\\mystuff\\Large_Files\\USFS\\data\\subsections"
# dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"
# fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
fiaCA <- file.path(sql_dir, 'SQLite_FIADB_CA.db')
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = fiaCA)  

#Creating a plot of mortality by tree species in two time periods and drought sequences.
#Create a notin operator
`%notin%` <- Negate(`%in%`)

#Add USFS EcoRegion maps
# usfs_in <- "D:\\Large_Files\\USFS\\data\\subsections"
usfs_in <- "C:\\Users\\Carl\\mystuff\\Large_Files\\USFS\\data\\subsections"
usfs.regions <- st_read(file.path(usfs_in, 'S_USA.EcomapSubsections.shp'))
#South Sierra
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S =='M261Eq' | MAP_UNIT_S =='M261Eu' | MAP_UNIT_S =='M261Er'  | MAP_UNIT_S =='M261Eo'  | MAP_UNIT_S =='M261Es' | 
#North Sierra
                                                MAP_UNIT_S == 'M261Ea'  | MAP_UNIT_S =='M261Eb'  | MAP_UNIT_S =='M261Ec'  | MAP_UNIT_S =='M261Ed'  | MAP_UNIT_S =='M261Ef'  | MAP_UNIT_S =='M261Eg'  | MAP_UNIT_S =='M261Eh'  | MAP_UNIT_S =='M261Ej'  | MAP_UNIT_S =='M261Ek'  | MAP_UNIT_S =='M261El'  | MAP_UNIT_S =='M261Em'  | MAP_UNIT_S =='M261Et')

#North Sierra Union
south.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S =='M261Eq' | MAP_UNIT_S =='M261Eu' | MAP_UNIT_S =='M261Er'  | MAP_UNIT_S =='M261Eo'  | MAP_UNIT_S =='M261Es') 
south.sierra.merge <- south.sierra %>% st_union()

#South Sierra Union
north.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ea'  | MAP_UNIT_S =='M261Eb'  | MAP_UNIT_S =='M261Ec'  | MAP_UNIT_S =='M261Ed'  | MAP_UNIT_S =='M261Ef'  | MAP_UNIT_S =='M261Eg' | 
                         MAP_UNIT_S =='M261Eh'  | MAP_UNIT_S =='M261Ej'  | MAP_UNIT_S =='M261Ek'  | MAP_UNIT_S =='M261El'  | MAP_UNIT_S =='M261Em'  | MAP_UNIT_S =='M261Et') %>% st_union()
north.sierra.merge <- north.sierra %>% st_union()

#Add extension functions, allows more math functions
initExtension(db)				

# summary(db)
#Query for FIA live trees, 2014-2019
q1 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identified code
(t.carbon_ag * 2)*(t.tpa_unadj) live_biomass, -- biomass (lbs) of tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, t.invyr, r.common_name, t.spcd, c.fortypcd, 
c.fldtypcd, rft.meaning, c.stdage, c.fldage, t.totage, t.bhage,
c.dstrbcd1, c.dstrbyr1, p.ecosubcd, p.lat, p.lon
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 --2 means non-forest
--AND t.statuscd = 1 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 1.0 -- additional where_clause from ref_pop_attribute table
AND rft.value = c.fldtypcd
--All Sierra Eco SubCodes
--South Sierra
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es')-- OR
--NOrth Sierra
--P.ECOSUBCD = 'M261Ea' OR P.ECOSUBCD = 'M261Eb' OR P.ECOSUBCD = 'M261Ec' OR P.ECOSUBCD = 'M261Ed' OR P.ECOSUBCD = 'M261Ef' OR P.ECOSUBCD = 'M261Eg' OR P.ECOSUBCD = 'M261Eh' OR P.ECOSUBCD = 'M261Ej' OR P.ECOSUBCD = 'M261Ek' OR P.ECOSUBCD = 'M261El' OR P.ECOSUBCD = 'M261Em' OR P.ECOSUBCD = 'M261Et')
--Combined Sierra and SoCal Stand Age
--AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es' OR P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba')
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70) -- No Fires OR c.dstrbcd1 = 30 OR c.dstrbcd1 = 31 OR c.dstrbcd1 = 32)
")
#DSTRBCD1 == 30, 31, 32 reference fires
all <- dbFetch(q1, n = -1)
# dbDisconnect(db)
summary(all)

#Convert per acre to per hectare
all$count <- all$count * 2.47105 # Convert to per hectare
all$DIA <- all$DIA * (2.54) #Convert to centimeters
all$basal_area <- (((all$DIA / 2)^2) * pi)*(1/10000) * all$count

live <- all %>% filter(STATUSCD == 1) %>% group_by(INVYR, PLOT) %>% summarize(count.live = n(), tpa.live = sum(count), basal_area.live = sum(basal_area), STDAGE = median(STDAGE), ECOSUBCD = first(ECOSUBCD), latitude = median(LAT), longitude = median(LON))
live
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead <- all %>% filter(STATUSCD == 2 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")) %>% group_by(PLOT, INVYR) %>% summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
# dead <- dead %>% mutate(INVYR = MORTYR) & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")
dead
join <- left_join(live, dead, by = c('PLOT', 'INVYR'))

#Replace the NAs with 0s
join <- join %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0))
summary(join)
#Add the total basal area calculations
join$count.all <- join$count.live + join$count.dead
join$tpa.all <- join$tpa.live + join$tpa.dead
join$basal_area.all <- join$basal_area.live + join$basal_area.dead

summary(join)

spat.join <- join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & (!is.na(STDAGE) & STDAGE != 9999) & STDAGE > 0)
# summary(spat.join)
#Make the FIA data into spatila points
# coordinates(spat.join) <- ~ longitude + latitude
spat.join <- st_as_sf(spat.join, coords=c("longitude", "latitude"), crs="EPSG:4326")
# spat.join <- spat.join %>% st_as_sf()
# crs(spat.join) <- 
# plot(spat.join)
#Create the ecoregion summaries
ecosubcd.summary <- join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & (!is.na(STDAGE) & STDAGE != 9999) & STDAGE > 0) %>% group_by(ECOSUBCD) %>% summarize(BAA.all = mean(basal_area.all), BAA.dead = mean(basal_area.dead), stdage.mean = mean(STDAGE), tpa.all = mean(tpa.all), tpa.dead = mean(tpa.dead))

#REname the ECOSUBCD Column
ecos.sum <- ecosubcd.summary %>% rename(MAP_UNIT_S = ECOSUBCD)
ecos.sum
usfs.sierra
plot(south.sierra)
ecosubcd.join <- left_join(south.sierra, ecos.sum, by = 'MAP_UNIT_S')
ecosubcd.join

p1 <- ggplot() +
  geom_sf(data=ecosubcd.join, mapping = aes(fill=stdage.mean), lwd=0.8, alpha=0.6) +
  #geom_sf(data = north.sierra, lwd = 1.2, alpha = 0.0, color = 'dark gray') +
  geom_sf(data = south.sierra.merge, lwd = 1.2, alpha = 0.0, color = 'black') +
  # geom_sf(data = spat.join, mapping = aes(color = STDAGE), size = 1) +
  theme_bw()+
  # guides(fill = guide_colorbar(order = 1), color = guide_colorbar(order = 2)) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.2, 0.32), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_fill_viridis_c("Live Tree Age") #+
  # scale_color_viridis_c("Tree Age", option = 'B')
p1

# ggsave(filename = 'Fig20_FIA_average_tree_age.png', height=16, width= 12, units = 'cm', dpi=900)

p2 <- ggplot() +
  geom_sf(data=ecosubcd.join, mapping = aes(fill=BAA.all), lwd=0.8, alpha=0.6) +
  # geom_sf(data = north.sierra, lwd = 1.2, alpha = 0.0, color = 'dark gray') +
  geom_sf(data = south.sierra.merge, lwd = 1.2, alpha = 0.0, color = 'black') +
  # geom_sf(data = spat.join, mapping = aes(color = basal_area.all), size = 1) +
  theme_bw()+
  guides(fill = guide_colorbar(order = 1), color = guide_colorbar(order = 2)) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.23, 0.32), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_fill_viridis_c(expression('Basal Area (m'^2*' ha'^-1*')'), option = 'A', direction = -1) #+
  # scale_color_viridis_c("Plot \nBasal Area \n(m^2/ha)", option = 'B')
p2

# ggsave(filename = 'Fig21_FIA_average_basal_area.png', height=16, width= 12, units = 'cm', dpi=900)

p3 <- ggplot() +
  geom_sf(data=ecosubcd.join, mapping = aes(fill=BAA.dead), lwd=0.8, alpha=0.6) +
  # geom_sf(data = north.sierra, lwd = 1.2, alpha = 0.0, color = 'dark gray') +
  geom_sf(data = south.sierra.merge, lwd = 1.2, alpha = 0.0, color = 'black') +
  # geom_sf(data = spat.join, mapping = aes(color = basal_area.dead), size = 1) +
  theme_bw()+
  # guides(fill = guide_colorbar(order = 1), color = guide_colorbar(order = 2)) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.2, 0.32), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_fill_viridis_c(expression('Mortality (m'^2*' ha'^-1*')'), option = 'C') #+
  # scale_color_viridis_c("Plot \nMortality \n(m^2/ha)", option = 'B')
p3

f1 <- ggarrange(p2, p3, ncol = 2, nrow = 1, common.legend = FALSE, align = "h", labels = c('a)', 'b)'))
f1
ggsave(filename = 'fig1_FIA_south_sierra_summary.png', height=16, width= 24, units = 'cm', dpi=900)

#Plot data for FIA
p4 <- ggplot() +
  # geom_sf(data=ecosubcd.join, mapping = aes(fill=stdage.mean), lwd=0.8, alpha=0.6) +
  # geom_sf(data = north.sierra, lwd = 1.2, alpha = 0.0, color = 'dark gray') +
  geom_sf(data = south.sierra.merge, lwd = 1.2, alpha = 0.0, color = 'black') +
  geom_sf(data = spat.join, mapping = aes(color = STDAGE), size = 1) +
  theme_bw()+
  guides(fill = guide_colorbar(order = 1), color = guide_colorbar(order = 2)) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.2, 0.32), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  # scale_fill_viridis_c("Region \nTree Age") +
  scale_color_viridis_c("Live Tree Age")
p4

# ggsave(filename = 'Fig20_FIA_average_tree_age.png', height=16, width= 12, units = 'cm', dpi=900)

p5 <- ggplot() +
  # geom_sf(data=ecosubcd.join, mapping = aes(fill=BAA.all), lwd=0.8, alpha=0.6) +
  # geom_sf(data = north.sierra, lwd = 1.2, alpha = 0.0, color = 'dark gray') +
  geom_sf(data = south.sierra.merge, lwd = 1.2, alpha = 0.0, color = 'black') +
  geom_sf(data = south.sierra, lwd = 0.8, alpha = 0.0, color = 'black') +
  geom_sf(data = spat.join, mapping = aes(color = basal_area.all), size = 1) +
  theme_bw()+
  guides(fill = guide_colorbar(order = 1), color = guide_colorbar(order = 2)) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.23, 0.32), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  # scale_fill_viridis_c("Region \nBasal Area \n(m^2/ha)") +
  scale_color_viridis_c(expression('Basal Area (m'^2*' ha'^-1*')'), option = 'A', direction = -1)
p5

# ggsave(filename = 'Fig21_FIA_region_summary.png', height=16, width= 12, units = 'cm', dpi=900)

p6 <- ggplot() +
  # geom_sf(data=ecosubcd.join, mapping = aes(fill=BAA.dead), lwd=0.8, alpha=0.6) +
  # geom_sf(data = north.sierra, lwd = 1.2, alpha = 0.0, color = 'dark gray') +
  geom_sf(data = south.sierra.merge, lwd = 1.2, alpha = 0.0, color = 'black') +
  geom_sf(data = south.sierra, lwd = 0.8, alpha = 0.0, color = 'black') +
  geom_sf(data = spat.join, mapping = aes(color = basal_area.dead), size = 1) +
  theme_bw()+
  guides(fill = guide_colorbar(order = 1), color = guide_colorbar(order = 2)) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), legend.position = c(0.2, 0.32), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  # scale_fill_viridis_c("Region \nMortality \n(m^2/ha)") +
  scale_color_viridis_c(expression('Mortality (m'^2*' ha'^-1*')'), option = 'B')
p6

f2 <- ggarrange(p5, p6, ncol = 2, nrow = 1, common.legend = FALSE, align = "h", labels = c('a)', 'b)'))
f2
ggsave(filename = 'fig2_FIA_south_sierra_plot_summary.png', height=16, width= 24, units = 'cm', dpi=900)
