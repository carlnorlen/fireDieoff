#Author: Carl Norlen
#Date Created: August 4, 2021
#Date Edited: June 28, 2023
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

#Home computer layout
# setwd('C:/Users/can02/mystuff/fireDieoff/final_figures')
# sql_dir <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
# fiaCA <- file.path(sql_dir, 'FIADB_CA.db')

#Lab Computer layout
setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures')
sql_dir <- 'C:\\Users\\Carl\\mystuff\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
fiaCA <- file.path(sql_dir, 'SQLite_FIADB_CA.db')

#Add Data Sets


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
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 30 OR c.dstrbcd1 = 31 OR c.dstrbcd1 = 32 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70)
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

# all %>% filter(STATUSCD == 1)
all %>% summary()
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead <- all %>% filter(case_when(DSTRBCD1 %in% c(30,31,32) ~ STATUSCD == 2 & AGENTCD == 30, 
                                 DSTRBCD1 %in% c(10,11,12,54,70) ~ STATUSCD == 2 & 
                                   MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") & AGENTCD %in% c(10, 20, 50, 70))) %>% 
                group_by(PLOT, INVYR) %>% summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
# dead <- dead %>% mutate(INVYR = MORTYR) & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")
# dead
# dead.drought <- all %>% filter(STATUSCD == 2 & AGENTCD %in% c(10, 20, 50, 70) & 
#                        MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")) %>% group_by(PLOT, INVYR) %>% summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
# dead <- dead %>% mutate(INVYR = MORTYR) & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")


join <- left_join(live, dead, by = c('PLOT', 'INVYR'))
# join <-  left_join(join.1, dead, by = c('PLOT', 'INVYR'))
summary(join)
#Replace the NAs with 0s
join <- join %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0))
summary(join)
#Add the total basal area calculations
join$count.all <- join$count.live + join$count.dead
join$tpa.all <- join$tpa.live + join$tpa.dead
join$basal_area.all <- join$basal_area.live + join$basal_area.dead
join$basal_area.dead.pct <- join$basal_area.dead / join$basal_area.all * 100
#fill the NAs for basal_area.dead.pct, this could go earlier
join <- join %>% dplyr::mutate(basal_area.dead.pct = replace(basal_area.dead.pct, is.na(basal_area.dead.pct), 0))

join$tpa.dead.pct <- join$tpa.dead / join$tpa.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join <- join %>% dplyr::mutate(tpa.dead.pct = replace(tpa.dead.pct, is.na(tpa.dead.pct), 0))

# join %>% summary()
join$years.disturb <- join$INVYR - join$DSTRBYR1

# join %>% filter(DSTRBCD1 %in% c('31', '32') & INVYR %in% c('2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013')) %>% group_by(DSTRBCD1) %>% count()

join <- join %>% mutate(disturb.bin = case_when(
  DSTRBCD1 %in% c(10, 11, 12, 54, 70) ~ 'Die-off',
  DSTRBCD1 %in% c(30, 31, 32) ~ 'Fire',
  DSTRBCD1 == 0 ~'No Disturbance'))

#FIA time series split up species
all$tree_type <- recode(.x=all$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'juniper', 'California live oak' = 'oak', 'California sycamore' = 'deciduous', 'Coulter pine' = 'pine', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'pine',
                        'bigcone Douglas-fir' = 'fir', 'bigleaf maple' = 'deciduous', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'deciduous', 'incense-cedar' = 'cedar', 'interior live oak' = 'oak', 'limber pine' = 'pine',
                        'lodgepole pine' = 'pine', 'ponderosa pine' = 'pine', 'singleleaf pinyon' = 'pine', 'sugar pine' = 'pine', 'Utah juniper' = 'juniper', 'western juniper' = 'juniper', 'white alder' = 'deciduous', 'white fir' = 'fir', 'California laurel' = 'deciduous',
                        'California-laurel' = 'deciduous', 'Oregon ash' = 'deciduous', 'Douglas-fir' = 'fir', 'honey mesquite' = 'deciduous', 'desert ironwood' = 'deciduous', 'California red fir' = 'fir', 'California buckeye' = 'deciduous', 'Engelmann oak' = 'oak', 'grand fir' = 'fir', 'western white pine' = 'pine',
                        "western white pine" = 'pine', "whitebark pine" = 'pine', "mountain hemlock" = "other conifer", "gray or California foothill pine" = "pine", "foxtail pine" = 'pine', "blue oak" = 'oak', "California white oak" = 'oak', "quaking aspen" = 'deciduous',
                        "giant sequoia" = 'other conifer', "Unknown dead conifer" = 'other conifer', "ash spp." = 'deciduous', "black cottonwood" = 'deciduous', "California torreya (nutmeg)" = 'deciduous', "Oregon white oak" = 'oak', "Port-Orford-cedar" = 'cedar', "Pacific dogwood" = 'deciduous',
                        "red alder" = 'deciduous', "bitter cherry" = 'deciduous', 'Rocky Mountain maple' = 'deciduous', 'unknown dead conifer' = 'other conifer',
                        'velvet ash' = 'deciduous')
all$tree_type <- as.factor(all$tree_type)
# test <- all %>% select(PLOT, INVYR) %>% group_by(PLOT, INVYR) %>% summarize(count = n())

#Create a tree type summary by plot
live.sp <- all %>% filter(STATUSCD == 1) %>% 
  group_by(INVYR, PLOT, tree_type, .drop = FALSE) %>% 
  summarize(count.live = n(), tpa.live = sum(count), 
            basal_area.live = sum(basal_area), STDAGE = median(STDAGE), FORTYPCD = median (FORTYPCD), MEANING = first(MEANING), DSTRBCD1 = first(DSTRBCD1), 
            DSTRBYR1 = first(DSTRBYR1), OWNGRPCD = first(OWNGRPCD)) %>%
  ungroup()
live.sp
all %>% select(AGENTCD) %>% unique()

#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead.sp <- all %>% filter(case_when(DSTRBCD1 %in% c(30,31,32) ~ STATUSCD == 2 & AGENTCD == 30, 
                                     DSTRBCD1 %in% c(0,10,11,12,54,70) ~ STATUSCD == 2 & 
                                      MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") & AGENTCD %in% c(10, 20, 50, 70))) %>% 
  group_by(PLOT, INVYR, tree_type, .drop = FALSE) %>% 
  summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area)) %>%
  ungroup()

dead.sp
join.sp <- left_join(live.sp, dead.sp, by = c('PLOT', 'INVYR', 'tree_type'))

#Replace the NAs with 0s
join.sp <- join.sp %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0)
) %>% group_by(INVYR, PLOT, .drop = FALSE) %>%
  fill(DSTRBCD1, .direction = c("updown")) %>%
  ungroup()
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
  DSTRBCD1 %in% c(10, 11, 12, 54, 70) ~ 'Die-off',
  # DSTRBCD1 %in% c(30, 31, 32) ~ 'Fire',
  DSTRBCD1 %in% c(31, 32) ~ 'Fire',
  DSTRBCD1 == 0 ~'No Disturbance'))

#Create a tree_type factor variable and sort it to make the plots
join.sp$tree_type.f <- join.sp$tree_type
join.sp$tree_type.f <- factor(join.sp$tree_type.f, levels= c('pine', 'fir', 'oak', 'juniper', 'cedar'))
join.sp$basal_area.dead.pct <-join.sp$basal_area.dead.pct * 100

#Do the statistics by disturb.bin
join %>% ungroup %>% filter(disturb.bin %in% c('Die-off', 'Fire') & !is.na(disturb.bin)) %>% summary()
ttest.dead <- t.test(data = join %>% ungroup %>% filter(disturb.bin %in% c('Die-off', 'Fire') & !is.na(disturb.bin)), 
                basal_area.dead.pct ~ disturb.bin)
ttest.dead
#Tukey HSD
# dead.tHSD <- TukeyHSD(aov.dead)
# dead.tHSD

#ANOVA and Tukey HSD for basal are die-off by forest type, and disturb bin
type.aov.dead <- aov(data = join.sp %>% ungroup %>% filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off','Fire')& tree_type %in% c('pine', 'fir')), basal_area.dead.pct ~ tree_type * disturb.bin)
type.aov.dead %>% summary()

type.dead.tHSD <- TukeyHSD(type.aov.dead) 
type.dead.tHSD


# summary(join.sp)
join %>% ungroup() %>%
  # filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire')) %>%
  group_by(DSTRBCD1) %>%
  summarize(count = n())

#Create labels for the bar chart (a)
p4a_letters <- data.frame(label = c("a", "a"),
                       # sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                       #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                       y     = c(23, 27),
                       x     = c(1,2)
)

#Letters to indicate sample sizes
p4a_counts <- data.frame(label = c("n = 83", "n = 80"),
                       # sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       y     = c(21, 25),
                       x     = c(1, 2)
)

#Overall mortality by disturbance type
p4a <- ggbarplot(join %>% ungroup() %>%
                   filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire')), #& tree_type %in% c('pine', 'fir')),
                 x = "disturb.bin", y = "basal_area.dead.pct", #fill = 'tree_type.f', 
                 fill = 'gray', 
                 position = position_dodge(), add = "mean_se" , error.plot = "errorbar", alpha = 0.8, 
                 xlab = 'Disturbance', order = c('Die-off', "Fire")) +
  theme_bw() + 
  #guides(color = 'none', fill = guide_legend(title = "Tree Type", label.position = "bottom", title.position="top", title.hjust = 0.5)) +
  #scale_fill_discrete(labels = c("pine" = "Pine", "fir" = "Fir", "oak" = "Oak", "cedar" = "Cedar")) +
  geom_text(data = p4a_letters, mapping = aes(x = x, y = y, label = label), size = 5) +
  geom_text(data = p4a_counts, mapping = aes(x = x, y = y, label = label), size = 3) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.2, 0.55), legend.text = element_text(size = 8, angle = 45, vjust = 0.8), legend.title = element_text(size = 10),
        legend.direction = "horizontal", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), strip.background = element_blank(),
        strip.text.x = element_blank(), plot.margin = unit(c(2.5,0,0,5), "pt"), panel.spacing = unit(20, "pt"),
        plot.tag.position = c(0.2, 0.9), 
        plot.tag = element_text(face = "bold")) + ylab('Mortality (%)')
p4a

#Create labels for the bar chart (b)
p4b_letters <- data.frame(label = c("a", "b", "ab", "b"),
                          # sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                          # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                          #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                          y     = c(31, 12, 22, 15),
                          x     = c(0.76,1.24, 1.76, 2.24)
)

#Die-off Mortality as a % of basal area
p4b <- ggbarplot(join.sp %>% ungroup() %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire')& tree_type %in% c('pine', 'fir')),
                x = "disturb.bin", y = "basal_area.dead.pct", fill = 'tree_type.f', 
                color = "tree_type.f", 
                position = position_dodge(), add = "mean_se" , error.plot = "errorbar", alpha = 0.8, 
                xlab = 'Disturbance', order = c('Die-off', "Fire")) +
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree Type", label.position = "bottom", title.position="top", title.hjust = 0.5)) +
  scale_fill_discrete(labels = c("pine" = "Pine", "fir" = "Fir", "oak" = "Oak", "cedar" = "Cedar")) +
  geom_text(data = p4b_letters, mapping = aes(x = x, y = y, label = label), size = 5) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.9, 0.65), legend.text = element_text(size = 8, angle = 45, vjust = 0.8), legend.title = element_text(size = 10),
        legend.direction = "horizontal", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), strip.background = element_blank(),
        strip.text.x = element_blank(), plot.margin = unit(c(2.5,0,0,5), "pt"), panel.spacing = unit(20, "pt"),
        plot.tag.position = c(0.2, 0.9), 
        plot.tag = element_text(face = "bold")) + ylab('Mortality (%)')
p4b

#Combine the two figure panels into one	  
f4 <- ggarrange(p4a, p4b, ncol = 1, nrow = 2, align = "v", labels = c("a", "b"),  heights = c(0.95, 1), common.legend = FALSE)
f4

ggsave(filename = 'Fig6_fire_mortality_by_tree_type.png', height=16, width=16, units = 'cm', dpi=900)
