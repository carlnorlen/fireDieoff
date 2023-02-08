#Author: Carl Norlen
#Date Created: August 4, 2021
#Date Edited: February 8, 2023
#Purpose: Do an analysis of dead trees and Stand Age

# Specify necessary packages
p <- c("RSQLite","dbplyr","ggplot2","dplyr","tidyr", "ggpubr", "RColorBrewer",  
	   'gt', 'gtsummary', 'webshot', 'kableExtra', 'broom')

# If necessary: install/update packages
# install.packages('rlang',repo='https://cloud.r-project.org/')
# library("agricolae")
# Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
#setwd("/C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/field_data")
#cd /C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/field_data
#cd /C/Users/can02/mystuff/fireDieoff/FIA
#Command for calling the script in the command line: R < stand_age_dieoff.r --vanilla
#INstalling packages: install.packages('RColorBrewer',repo='https://cran.cnr.berkeley.edu/')
# setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/fia')
setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/fia')

#Add Data Sets
# sql_dir <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
# fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
sql_dir <- 'C:\\Users\\Carl\\mystuff\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
fiaCA <- file.path(sql_dir, 'SQLite_FIADB_CA.db')

#Add Data Sets
# sql_dir <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
# fiaCA <- file.path(sql_dir, 'FIADB_CA.db')

sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = fiaCA)  

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
c.dstrbcd1, c.dstrbyr1, c.owngrpcd
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
--South Sierra Nevada EcoRegions
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Es') --South Sierra Nevada 
--SoCal Mountains Ecoregions
--AND(P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba')
--Combined South Sierra and SoCal Mountains
--AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es' OR P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba')
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 30 OR c.dstrbcd1 = 31 OR c.dstrbcd1 = 32)
")
#DSTRBCD1 == 30, 31, 32 reference fires
all <- dbFetch(q1, n = -1)
# dbDisconnect(db)
summary(all)

#Convert per acre to per hectare
all$count <- all$count * 2.47105 # Convert to per hectare
all$DIA <- all$DIA * (2.54) #Convert to centimeters
all$basal_area <- (((all$DIA / 2)^2) * pi)*(1/10000) * all$count

#combined the data together
live <- all %>% filter(STATUSCD == 1) %>% group_by(INVYR, PLOT) %>% summarize(count.live = n(), tpa.live = sum(count), basal_area.live = sum(basal_area), STDAGE = first(STDAGE), 
                                                                              FORTYPCD = first(FORTYPCD), MEANING = first(MEANING), DSTRBCD1 = first(DSTRBCD1), 
                                                                              DSTRBYR1 = first(DSTRBYR1), OWNGRPCD = first(OWNGRPCD))

all %>% filter(STATUSCD == 1)
# live
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead <- all %>% filter(STATUSCD == 2 & AGENTCD %in% c(70, 30, 31, 32)) %>% group_by(PLOT, INVYR) %>% summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
# dead <- dead %>% mutate(INVYR = MORTYR) & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")
# dead
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
join$basal_area.dead.pct <- join$basal_area.dead / join$basal_area.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join <- join %>% dplyr::mutate(basal_area.dead.pct = replace(basal_area.dead.pct, is.na(basal_area.dead.pct), 0))

join$tpa.dead.pct <- join$tpa.dead / join$tpa.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join <- join %>% dplyr::mutate(tpa.dead.pct = replace(tpa.dead.pct, is.na(tpa.dead.pct), 0))

# join %>% summary()
join$years.disturb <- join$INVYR - join$DSTRBYR1

join %>% filter(DSTRBCD1 %in% c('31', '32') & INVYR %in% c('2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013')) %>% group_by(DSTRBCD1) %>% count()

join <- join %>% mutate(disturb.bin = case_when(
  # DSTRBCD1 %in% c(10, 11, 12, 54, 70) ~ 'Die-off',
  DSTRBCD1 %in% c(31) ~ 'Ground Fire',
  DSTRBCD1 %in% c(32) ~ 'Crown Fire',
  DSTRBCD1 == 0 ~'No Disturbance'))

#Test out the basal area
p1<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join %>% filter(DSTRBCD1 %in% c('30', '31', '32')) %>%
              group_by(INVYR) %>% summarize(BA.dead = mean(basal_area.dead)), mapping = aes(x = INVYR, y = BA.dead), color = 'black', linewidth = 1) +
  #95% CI Die-off
  geom_ribbon(data = join %>% filter(DSTRBCD1 %in% c('30', '31', '32')) %>%
                group_by(INVYR) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = INVYR), alpha = 0.3) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p1

p2 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join %>% filter(DSTRBCD1 %in% c('30', '31', '32')) %>%
              group_by(INVYR) %>% summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100, BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.dead.pct.mean),  linewidth = 1) +
  #The error bars (95% CI)
  geom_ribbon(data = join %>% filter(DSTRBCD1 %in% c('30', '31', '32')) %>%
                group_by(INVYR) %>%
                summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100,
                          BA.dead.pct.sd = sd(basal_area.dead.pct) * 100, 
                          BA.n = n()),
              mapping = aes(ymin=BA.dead.pct.mean - 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            ymax=BA.dead.pct.mean + 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            x = INVYR), alpha = 0.2) +
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Year') + ylab('Mortality (%)')
p2

# join %>% filter(STDAGE <= 10) %>% count()
p3 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join  %>% filter(DSTRBCD1 %in% c('30', '31', '32')) %>%
              group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), mapping = aes(x = INVYR, y = BA.all), color = 'black', linewidth = 1) +
  #The error bars
  geom_ribbon(data = join %>% filter(DSTRBCD1 %in% c('30', '31', '32')) %>%
                group_by(INVYR) %>%
                summarize(BA.mean = mean(basal_area.all),
                          BA.sd = sd(basal_area.all), BA.n = n()),
              mapping = aes(ymin=BA.mean - 1.96*(BA.sd / sqrt(BA.n)),
                            ymax=BA.mean + 1.96*(BA.sd / sqrt(BA.n)),
                            x = INVYR), alpha = 0.3) +
  
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) + theme_bw()
p3

f2 <- ggarrange(p1, p2, p3, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f2

ggsave(filename = 'fig15_FIA_BA_fire_mortality_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

#FIA time series split up species
all$tree_type <- recode(.x=all$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'juniper', 'California live oak' = 'oak', 'California sycamore' = 'deciduous', 'Coulter pine' = 'pine', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'pine',
                        'bigcone Douglas-fir' = 'fir', 'bigleaf maple' = 'deciduous', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'deciduous', 'incense-cedar' = 'cedar', 'interior live oak' = 'oak', 'limber pine' = 'pine',
                        'lodgepole pine' = 'pine', 'ponderosa pine' = 'pine', 'singleleaf pinyon' = 'pine', 'sugar pine' = 'pine', 'Utah juniper' = 'juniper', 'western juniper' = 'juniper', 'white alder' = 'deciduous', 'white fir' = 'fir', 'California laurel' = 'deciduous',
                        'California-laurel' = 'deciduous', 'Oregon ash' = 'deciduous', 'Douglas-fir' = 'fir', 'honey mesquite' = 'deciduous', 'desert ironwood' = 'deciduous', 'California red fir' = 'fir', 'California buckeye' = 'deciduous', 'Engelmann oak' = 'oak', 'grand fir' = 'fir', 'western white pine' = 'pine',
                        "western white pine" = 'pine', "whitebark pine" = 'pine', "mountain hemlock" = "other conifer", "gray or California foothill pine" = "pine", "foxtail pine" = 'pine', "blue oak" = 'oak', "California white oak" = 'oak', "quaking aspen" = 'deciduous',
                        "giant sequoia" = 'other conifer', "Unknown dead conifer" = 'other conifer', "ash spp." = 'deciduous', "black cottonwood" = 'deciduous', "California torreya (nutmeg)" = 'deciduous', "Oregon white oak" = 'oak', "Port-Orford-cedar" = 'cedar', "Pacific dogwood" = 'deciduous',
                        "red alder" = 'deciduous', "bitter cherry" = 'deciduous', 'Rocky Mountain maple' = 'deciduous', 'unknown dead conifer' = 'other conifer')
all$tree_type <- as.factor(all$tree_type)
# test <- all %>% select(PLOT, INVYR) %>% group_by(PLOT, INVYR) %>% summarize(count = n())

#Create a tree type summary by plot
live.sp <- all %>% filter(STATUSCD == 1) %>% 
  group_by(INVYR, PLOT, tree_type, .drop = FALSE) %>% 
  summarize(count.live = n(), tpa.live = sum(count), 
            basal_area.live = sum(basal_area), STDAGE = median(STDAGE), FORTYPCD = median (FORTYPCD), MEANING = first(MEANING), DSTRBCD1 = first(DSTRBCD1), 
            DSTRBYR1 = first(DSTRBYR1), OWNGRPCD = first(OWNGRPCD))
live.sp
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead.sp <- all %>% filter(STATUSCD == 2 & AGENTCD %in% c(70, 30, 31, 32)) %>%
  group_by(PLOT, INVYR, tree_type, .drop = FALSE) %>% 
  summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))

dead.sp
join.sp <- left_join(live.sp, dead.sp, by = c('PLOT', 'INVYR', 'tree_type'))

#Replace the NAs with 0s
join.sp <- join.sp %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0)
) %>% group_by(INVYR, PLOT, tree_type) %>%
  fill(STDAGE, .direction = c("up"))
# join
# summary(join)
#Add the total basal area calculations
join.sp$count.all <- join.sp$count.live + join.sp$count.dead
join.sp$tpa.all <- join.sp$tpa.live + join.sp$tpa.dead
join.sp$basal_area.all <- join.sp$basal_area.live + join.sp$basal_area.dead
join.sp$basal_area.dead.pct <- join.sp$basal_area.dead / join.sp$basal_area.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join.sp <- join.sp %>% dplyr::mutate(basal_area.dead.pct = replace(basal_area.dead.pct, is.na(basal_area.dead.pct), 0))

join.sp$tpa.dead.pct <- join.sp$tpa.dead / join.sp$tpa.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join.sp <- join.sp %>% dplyr::mutate(tpa.dead.pct = replace(tpa.dead.pct, is.na(tpa.dead.pct), 0))
join.sp %>% summary()

join.sp <- join.sp %>% mutate(disturb.bin = case_when(
  # DSTRBCD1 %in% c(10, 11, 12, 54, 70) ~ 'Die-off',
  DSTRBCD1 %in% c(31) ~ 'Ground Fire',
  DSTRBCD1 %in% c(32) ~ 'Crown Fire',
  DSTRBCD1 == 0 ~'No Disturbance'))

p4 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join.sp %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar', 'juniper')) %>% 
              group_by(INVYR, tree_type) %>% summarize(BA.dead = mean(basal_area.dead)), 
            mapping = aes(x = INVYR, y = BA.dead, color = tree_type, linetype = tree_type), linewidth = 1) +
  #95% CI Die-off
  geom_ribbon(data = join.sp %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar', 'juniper')) %>%  
                group_by(INVYR, tree_type) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = INVYR, fill = tree_type, linetype = tree_type), alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.15, 0.62), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  # scale_fill_viridis_d("Tree\nSpecies", option = 'B') +
  # scale_color_viridis_d("Tree\nSpecies", option = 'B') +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p4

p5 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.sp %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar', 'juniper')) %>%  
              group_by(INVYR, tree_type) %>% summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100, BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.dead.pct.mean, color = tree_type, linetype = tree_type),  linewidth = 1) +
  #The error bars
  geom_ribbon(data = join.sp %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar', 'juniper')) %>%   
                group_by(INVYR, tree_type) %>%
                summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100,
                          BA.dead.pct.sd = sd(basal_area.dead.pct) * 100, 
                          BA.n = n()),
              mapping = aes(ymin=BA.dead.pct.mean - 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            ymax=BA.dead.pct.mean + 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            x = INVYR, fill = tree_type), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Year') + ylab('Mortality (%)')
p5

# join %>% filter(STDAGE <= 10) %>% count()
p6 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.sp %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar', 'juniper')) %>%  
              group_by(INVYR, tree_type) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.all, color = tree_type, linetype = tree_type),  linewidth = 1) +
  #The error bars
  geom_ribbon(data = join.sp %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar', 'juniper')) %>%   
                group_by(INVYR, tree_type) %>%
                summarize(BA.mean = mean(basal_area.all),
                          BA.sd = sd(basal_area.all), BA.n = n()),
              mapping = aes(ymin=BA.mean - 1.96*(BA.sd / sqrt(BA.n)),
                            ymax=BA.mean + 1.96*(BA.sd / sqrt(BA.n)),
                            x = INVYR, fill = tree_type), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')'))
p6

f3 <- ggarrange(p4, p5, p6, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f3

ggsave(filename = 'Fig17_FIA_bySpecies_fire_mortality_BA_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

#Region wide counts of dead and live trees
p7 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join.sp %>% ungroup() %>%
               filter(!is.na(disturb.bin) & disturb.bin %in% c('Crown Fire', 'Ground Fire') & tree_type %in% c('pine', 'fir', 'oak')) %>%  
              group_by(disturb.bin, tree_type) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), 
             mapping = aes(x = disturb.bin, y = BA.dead, color = tree_type), size = 1, position = position_dodge(width = 0.90)) + 
  #95% CI Die-of
  geom_errorbar(data = join.sp %>% ungroup() %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('Crown Fire', 'Ground Fire') & tree_type %in% c('pine', 'fir', 'oak')) %>%  
                  group_by(disturb.bin, tree_type) %>%
                  summarize(BA.dead = mean(basal_area.dead),
                            BA.dead.sd = sd(basal_area.dead), BA.n = n()),
                mapping = aes(y = BA.dead, ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                              ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                              x = disturb.bin, color = tree_type), position = position_dodge(width = 0.90)) + 
  theme_bw() +
  # ylim(0, 40) +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.85, 0.7), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  # scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  xlab('Disturbance Type') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p7

p8 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join.sp %>% ungroup() %>%
               filter(!is.na(disturb.bin) & disturb.bin %in% c('Crown Fire', 'Ground Fire') & tree_type %in% c('pine', 'fir', 'oak')) %>%  
               group_by(disturb.bin, tree_type) %>% summarize(BA.all = mean(basal_area.all), BA.dead.pct = mean(basal_area.dead.pct)*100), 
             mapping = aes(x = disturb.bin, y = BA.dead.pct, color = tree_type), size = 1, position = position_dodge(width = 0.90)) +
  #95% CI Die-of
  geom_errorbar(data = join.sp %>% ungroup() %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('Crown Fire', 'Ground Fire') & tree_type %in% c('pine', 'fir', 'oak')) %>%  
                  group_by(disturb.bin, tree_type) %>%
                  summarize(BA.dead.pct = mean(basal_area.dead.pct)*100,
                            BA.dead.pct.sd = sd(basal_area.dead.pct)*100, BA.n = n()),
                mapping = aes(y = BA.dead.pct, ymin=BA.dead.pct - 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                              ymax=BA.dead.pct + 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                              x = disturb.bin, color = tree_type), position = position_dodge(width = 0.90)) +
  theme_bw() +
  # ylim(0, 40) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  # scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Disturbance Type') + ylab('Mortality (%)') 
p8

p9 <- ggplot() + 
  #Mean Die-off
  geom_point(data = join.sp %>% ungroup() %>%
               filter(!is.na(disturb.bin) & disturb.bin %in% c('Crown Fire', 'Ground Fire') & tree_type %in% c('pine', 'fir', 'oak')) %>%  
               group_by(disturb.bin, tree_type) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), 
             mapping = aes(x = disturb.bin, y = BA.all, color = tree_type), size = 1, position = position_dodge(width = 0.90)) + 
  #95% CI Die-of
  geom_errorbar(data = join.sp %>% ungroup() %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('Crown Fire', 'Ground Fire') & tree_type %in% c('pine', 'fir', 'oak')) %>%  
                  group_by(disturb.bin, tree_type) %>%
                  summarize(BA.all = mean(basal_area.all),
                            BA.all.sd = sd(basal_area.all), BA.n = n()),
                mapping = aes(y = BA.all, ymin=BA.all - 1.96*(BA.all.sd / sqrt(BA.n)),
                              ymax=BA.all + 1.96*(BA.all.sd / sqrt(BA.n)),
                              x = disturb.bin, color = tree_type), position = position_dodge(width = 0.90)) + 
  theme_bw() +
  # ylim(0, 40) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  # scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  theme(legend.position = 'none', axis.text.x = element_text(size = 8), axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), axis.text.y = element_text(size = 8)) +
  xlab('Disturbance Type') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) 
p9

f3 <- ggarrange(p7, p8, p9, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f3

ggsave(filename = 'Fig17_FIA_bySpecies_fire_mortality_BA_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

p2<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(!is.na(disturb.bin) & disturb.bin %in% c('No Disturbance', 'Crown Fire', 'Ground Fire')) %>%  
               group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), BA.live = mean(basal_area.live)), 
             mapping = aes(x = disturb.bin, y = BA.live, color = disturb.bin), size = 1) +
  #95% CI Die-of
  geom_errorbar(data = join %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('No Disturbance', 'Crown Fire', 'Ground Fire')) %>%  
                  group_by(disturb.bin) %>%
                  summarize(BA.live = mean(basal_area.live),
                            BA.live.sd = sd(basal_area.live), BA.n = n()),
                mapping = aes(y = BA.live, ymin=BA.live - 1.96*(BA.live.sd / sqrt(BA.n)),
                              ymax=BA.live + 1.96*(BA.live.sd / sqrt(BA.n)),
                              x = disturb.bin, color = disturb.bin)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Disturbance Type') + ylab(expression('Live Basal Area (m'^2*' ha'^-1*')')) 
p2

#Check on this for how many dead trees; See if I should calculate it a different way.
p3<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(!is.na(disturb.bin) & disturb.bin %in% c('No Disturbance', 'Crown Fire', 'Ground Fire')) %>%   
               group_by(disturb.bin) %>% 
               summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), 
             mapping = aes(x = disturb.bin, y = BA.dead, color = disturb.bin),  size = 1) +
  #95% CI Die-off
  geom_errorbar(data = join %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('No Disturbance', 'Crown Fire', 'Ground Fire')) %>%  
  group_by(disturb.bin) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(y = BA.dead, ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = disturb.bin, color = disturb.bin)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Disturbance Type') + ylab(expression('Fire Mortality (m'^2*' ha'^-1*')')) 
p3

# join %>% 
#   filter((!is.na(STDAGE) & STDAGE != 9999) ) %>% 
#   # filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999 & STDAGE != 9999) %>% 
#   group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE.mean = mean(STDAGE), count = n())

p4<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(!is.na(disturb.bin) & disturb.bin %in% c('No Disturbance', 'Crown Fire', 'Ground Fire') & (!is.na(STDAGE) & STDAGE != 9999)) %>% # & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
              group_by(disturb.bin) %>% 
               summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE.mean = mean(STDAGE)), 
             mapping = aes(x = disturb.bin, y = STDAGE.mean, color = disturb.bin), size = 1) +
  #95% CI Error bar
  geom_errorbar(data = join %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('No Disturbance', 'Crown Fire', 'Ground Fire') & (!is.na(STDAGE) & STDAGE != 9999)) %>%
                  group_by(disturb.bin) %>%
                  summarize(STDAGE.mean = mean(STDAGE),
                            STDAGE.mean.sd = sd(STDAGE), BA.n = n()),
                mapping = aes(y = STDAGE.mean, ymin=STDAGE.mean - 1.96*(STDAGE.mean.sd / sqrt(BA.n)),
                              ymax=STDAGE.mean + 1.96*(STDAGE.mean.sd / sqrt(BA.n)),
                              x = disturb.bin, color = disturb.bin)) +
  theme_bw() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Past Disturbance Type') + ylab('Average Tree Age') 
p4

f1 <- ggarrange(p1, p3, p4, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'c)', 'd)'))
f1

ggsave(filename = 'Fig16_FIA_fire_recovery_exploration.png', height=15, width= 10, units = 'cm', dpi=900)

p4<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_density(data = join %>% 
               filter(!is.na(disturb.bin) & disturb.bin %in% c('No Disturbance', 'Crown Fire', 'Ground Fire') & (!is.na(STDAGE) & STDAGE != 9999)), # & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
                 
                 mapping = aes(fill = disturb.bin, x = basal_area.all), size = 1, alpha =0.5) + theme_bw() +
  ylab('Density') + xlab(expression('Basal Area (m'^2*' ha'^-1*')'))
p4

p5 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_density(data = join %>% 
                 filter(!is.na(disturb.bin) & disturb.bin %in% c('Crown Fire', 'Ground Fire') & (!is.na(STDAGE) & STDAGE != 9999)), # & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
               
               mapping = aes(fill = disturb.bin, x = basal_area.dead), size = 1, alpha =0.5) + theme_bw() +
  ylab('Density') + xlab(expression('Mortality (m'^2*' ha'^-1*')'))
p5

p6 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_density(data = join %>% 
                 filter(!is.na(disturb.bin) & disturb.bin %in% c('No Disturbance', 'Crown Fire', 'Ground Fire') & (!is.na(STDAGE) & STDAGE != 9999)), # & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
               
               mapping = aes(fill = disturb.bin, x = STDAGE), size = 1, alpha =0.5) + theme_bw() +
  ylab('Density') + xlab('Average Tree Age')
p6

f2 <- ggarrange(p4, p5, p6, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'c)', 'd)'))
f2

ggsave(filename = 'Fig17_FIA_fire_recovery_desnity_plots_exploration.png', height=15, width= 10, units = 'cm', dpi=900)

#Do Plot by Ownership Type
# p5<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
#   #Mean Die-off
#   geom_point(data = join %>% 
#                filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%  
#                group_by(owncd.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), mapping = aes(x = owncd.bin, y = BA.all), color = 'black', size = 1) +
#   #95% CI Die-off
#   geom_errorbar(data = join %>%
#                   filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%
#                   group_by(owncd.bin) %>%
#                   summarize(BA.all = mean(basal_area.all),
#                             BA.all.sd = sd(basal_area.all), BA.n = n()),
#                 mapping = aes(y = BA.all, ymin=BA.all - 1.96*(BA.all.sd / sqrt(BA.n)),
#                               ymax=BA.all + 1.96*(BA.all.sd / sqrt(BA.n)),
#                               x = owncd.bin)) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
#   xlab('Ownership') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) 
# p5
# 
# join %>%
#   filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%
#   group_by(owncd.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), count = n())
# 
# # p6<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
# #   #Mean Die-off
# #   geom_point(data = join %>% 
# #                filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70)) %>% 
# #                group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), count = n()), 
# #              mapping = aes(x = disturb.bin, y = count), color = 'black', size = 1) +
# #   #95% CI Die-off
# #   # geom_ribbon(data = join %>% filter(!is.na(stdage.bin)) %>% 
# #   #               group_by(INVYR) %>%
# #   #               summarize(BA.dead = mean(basal_area.dead),
# #   #                         BA.dead.sd = sd(basal_area.dead), BA.n = n()),
# #   #             mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
# #   #                           ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
# #   #                           x = INVYR), alpha = 0.3) +
# #   theme_bw() +
# #   theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
# #   xlab('Disturbance Type') + ylab('# Plots') 
# # f2
# 
# p6 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
#   #Mean Die-off
#   geom_point(data = join %>% 
#                filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>% 
#                group_by(owncd.bin) %>% 
#                summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), 
#              mapping = aes(x = owncd.bin, y = BA.dead), color = 'black', size = 1) +
#   #95% CI Die-off
#   geom_errorbar(data = join %>%
#                   filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%
#                   group_by(owncd.bin) %>%
#                   summarize(BA.dead = mean(basal_area.dead),
#                             BA.dead.sd = sd(basal_area.dead), BA.n = n()),
#                 mapping = aes(y = BA.dead, ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
#                               ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
#                               x = owncd.bin)) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
#   xlab('Ownership') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
# p6
# 
# # join %>% 
# #   filter((!is.na(STDAGE) & STDAGE != 9999) ) %>% 
# #   # filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999 & STDAGE != 9999) %>% 
# #   group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE.mean = mean(STDAGE), count = n())
# 
# p7 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
#   #Mean Die-off
#   geom_point(data = join %>% 
#                filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>% 
#                group_by(owncd.bin) %>% 
#                summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE.mean = mean(STDAGE)), 
#              mapping = aes(x = owncd.bin, y = STDAGE.mean), color = 'black', size = 1) +
#   #95% CI Error bar
#   geom_errorbar(data = join %>%
#                   filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%
#                   group_by(owncd.bin) %>%
#                   summarize(STDAGE.mean = mean(STDAGE),
#                             STDAGE.mean.sd = sd(STDAGE), BA.n = n()),
#                 mapping = aes(y = STDAGE.mean, ymin=STDAGE.mean - 1.96*(STDAGE.mean.sd / sqrt(BA.n)),
#                               ymax=STDAGE.mean + 1.96*(STDAGE.mean.sd / sqrt(BA.n)),
#                               x = owncd.bin)) +
#   theme_bw() +
#   # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
#   xlab('Ownership') + ylab('Average Tree Age') 
# p7
# 
# f2 <- ggarrange(p5, p6, p7, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'c)', 'd)'))
# f2
# 
# ggsave(filename = 'Fig9_FIA_ownership_exploration_south_sierra.png', height=15, width= 10, units = 'cm', dpi=900)
