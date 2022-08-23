#Author: Carl Norlen
#Date Created: August 23, 2022
#Date Edited: August 23, 2022
#Purpose: Create maps of FIA data for the Sierra Nevada (and SoCal Mountains?) with average StandAge total Basal Area and Dead Basal Area?

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
setwd('C:/Users/can02/mystuff/fireDieoff/FIA')

#Add Data Sets
sql_dir <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"
fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = fiaCA)  

#Creating a plot of mortality by tree species in two time periods and drought sequences.
#Create a notin operator
`%notin%` <- Negate(`%in%`)

#Add USFS EcoRegion maps
usfs_in <- "D:\\Large_Files\\USFS\\data\\subsections"
usfs.regions <- st_read(file.path(usfs_in, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S =='M261Eq' | MAP_UNIT_S =='M261Eu' | MAP_UNIT_S =='M261Er'  | MAP_UNIT_S =='M261Eo'  | MAP_UNIT_S =='M261Es' | MAP_UNIT_S ==
'M261Ea'  | MAP_UNIT_S =='M261Eb'  | MAP_UNIT_S =='M261Ec'  | MAP_UNIT_S =='M261Ed'  | MAP_UNIT_S =='M261Ef'  | MAP_UNIT_S =='M261Eg'  | MAP_UNIT_S =='M261Eh'  | MAP_UNIT_S =='M261Ej'  | MAP_UNIT_S =='M261Ek'  | MAP_UNIT_S =='M261El'  | MAP_UNIT_S =='M261Em'  | MAP_UNIT_S =='M261Et')

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
c.dstrbcd1, c.dstrbyr1, p.ecosubcd
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
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es' OR
--NOrth Sierra
P.ECOSUBCD = 'M261Ea' OR P.ECOSUBCD = 'M261Eb' OR P.ECOSUBCD = 'M261Ec' OR P.ECOSUBCD = 'M261Ed' OR P.ECOSUBCD = 'M261Ef' OR P.ECOSUBCD = 'M261Eg' OR P.ECOSUBCD = 'M261Eh' OR P.ECOSUBCD = 'M261Ej' OR P.ECOSUBCD = 'M261Ek' OR P.ECOSUBCD = 'M261El' OR P.ECOSUBCD = 'M261Em' OR P.ECOSUBCD = 'M261Et')
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

live <- all %>% filter(STATUSCD == 1) %>% group_by(INVYR, PLOT) %>% summarize(count.live = n(), tpa.live = sum(count), basal_area.live = sum(basal_area), STDAGE = median(STDAGE), ECOSUBCD = first(ECOSUBCD))
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

ecosubcd.summary <- join %>% filter(!is.na(STDAGE)) %>% group_by(ECOSUBCD) %>% summarize(BAA.all = mean(basal_area.all), BAA.dead = mean(basal_area.dead), stdage.mean = mean(STDAGE), tpa.all = mean(tpa.all), tpa.dead = mean(tpa.dead))

#REname the ECOSUBCD Column
ecos.sum <- ecosubcd.summary %>% rename(MAP_UNIT_S = ECOSUBCD)
ecos.sum
usfs.sierra
ecosubcd.join <- left_join(usfs.sierra, ecos.sum, by = 'MAP_UNIT_S')
ecosubcd.join

p1 <- ggplot() +
  geom_sf(data=ecosubcd.join, mapping = aes(fill=stdage.mean), lwd=0.8, alpha=0.6) +
  theme_bw()+
  scale_fill_viridis_c("Average Tree Age")
p1

ggsave(filename = 'Fig20_FIA_average_tree_age.png', height=16, width= 12, units = 'cm', dpi=900)

p2 <- ggplot() +
  geom_sf(data=ecosubcd.join, mapping = aes(fill=BAA.all), lwd=0.8, alpha=0.6) +
  theme_bw()+
  scale_fill_viridis_c("Mean Basal Area (m^2/ha")
p2

ggsave(filename = 'Fig21_FIA_average_basal_area.png', height=16, width= 12, units = 'cm', dpi=900)

p3 <- ggplot() +
  geom_sf(data=ecosubcd.join, mapping = aes(fill=BAA.dead), lwd=0.8, alpha=0.6) +
  theme_bw()+
  scale_fill_viridis_c("Mean Mortality (m^2/ha")
p3

ggsave(filename = 'Fig21_FIA_average_mortality.png', height=16, width= 12, units = 'cm', dpi=900)
#Make tree type a factor so that I can fill in missing combinations
# all.forest$tree_type <- as.factor(all.forest$tree_type)
# all.forest.type <- all.forest %>% group_by(pltID, time.period, sequence, tree_type, .drop = FALSE) %>% 
#                                   summarize(BAA.all.sum = sum(BAA.all), BAA.live.sum = sum(BAA), BAA.dead.sum = sum(BAA.dead)) #%>%
#                                   
# #Calculate the percent mortality rate
# all.forest.type$BAA.mort <- all.forest.type$BAA.dead.sum / (all.forest.type$BAA.all.sum) * 100
# 
# #Summarize the data by forest type
# all.forest.type.summary <- all.forest.type %>% mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
#                                                       dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
#                                                 group_by(tree_type, time.period, sequence) %>% summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
#                                                                              BAA.live.mean = mean(BAA.live.sum), BAA.live.sd = sd(BAA.live.sum),
#                                                                              BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
#                                                                              BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
#                                                                              live.count = sum(live), dead.count = sum(dead))
# 
# #Calculate the conifer fraction to eliminate forest with no conifers from the sample
# all.summary <- all.forest %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100)
# 
# #Summarize the data on conifers per plot
# conifer.summary <- all.forest %>% filter(tree_type %in% c('pine', 'fir', 'juniper', 'cedar', 'other conifer')) %>% 
#                                   group_by(time.period, sequence, pltID) %>% summarize(BAA.all.conifer = sum(BAA.all), BAA.live.conifer = sum(BAA), BAA.dead.conifer = sum(BAA.dead))
# 
# #Join the overall and conifer summaries
# join.summary <- left_join(all.summary, conifer.summary, by = c('pltID', 'time.period', 'sequence'))
# 
# #Calculate the conifer fraction (with Basal Area)
# join.summary$conifer.frac <- join.summary$BAA.all.conifer / join.summary$BAA.all * 100
# 
# #Figure out which plots have 5% or greater conifer fraction
# plots <- join.summary %>% filter(conifer.frac >= 5) %>% ungroup() %>% pull(pltID) %>% unique()

#Do a summary for plots with at least 5% conifer fraction
# all.forest.type %>% filter(pltID %in% plots)
# conifer.forest.type.summary <- all.forest.type %>% filter(pltID %in% plots) %>% 
#   mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
#          dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
#   group_by(tree_type, time.period, sequence) %>% summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
#                                                            BAA.live.mean = mean(BAA.live.sum), BAA.live.sd = sd(BAA.live.sum),
#                                                            BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
#                                                            BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
#                                                            live.count = sum(live), dead.count = sum(dead))
# all.forest
# conifer.forest.all.summary <- all.forest %>% filter(pltID %in% plots) %>% group_by(time.period, sequence, pltID) %>%
#   summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100) %>%
#   mutate(live = case_when(BAA.all > 0 ~ 1, BAA.all == 0 ~ 0), 
#          dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
#   group_by(time.period, sequence) %>% summarize(BAA.all.mean = mean(BAA.all), BAA.all.sd = sd(BAA.all),
#                                                            BAA.live.mean = mean(BAA.live), BAA.live.sd = sd(BAA.live),
#                                                            BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
#                                                            BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort),
#                                                            count = n(),
#                                                            live.count = sum(live), dead.count = sum(dead))
# 
# conifer.forest.all.summary
# 
# #Create labels for the bar chart (a)
# p1_texta <- data.frame(label = c("a", "b", "b", "a"),
#                        sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
#                        # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
#                        #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
#                        y     = c(4.25, 1.15, 0.65, 3.55),
#                        x     = c(1, 2, 1, 2)
# )
# 
# #Letters to indicate sample sizes
# p1_textb <- data.frame(label = c("n = 47", "n = 31", "n = 206", "n = 188"),
#                        sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
#                        y     = c(3.95, 0.85, 0.35, 3.25),
#                        x     = c(1, 2, 1, 2)
# )

#Summary of Total Dead Basal Area
p1 <- ggbarplot(all.forest %>% filter(pltID %in% plots) %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.dead.sum", position = position_dodge(), color = "sequence", fill = 'gray',
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8, 
                ylab = expression('Mortality (m'^2*' ha'^-1*')'), 
                xlab = NULL, order = c("1999-2002", "2012-2015")) + 
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + labs(tag = 'b)') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.1), legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  # scale_x_discrete(labels = c("Response During\n1st Period", "Response During\n2nd Period")) +
  geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 3.5, x = 1.2, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')), 
             labeller = as_labeller(c('Both Droughts' = "Exposed to Both Droughts", '2nd Drought Only' = "Exposed to 2nd Drought Only"))) 
p1

#Create a tree_type factor variable and sort it to make the plots
all.forest.type$tree_type.f <- all.forest.type$tree_type
all.forest.type$tree_type.f <- factor(all.forest.type$tree_type.f, levels= c('pine', 'fir', 'oak', 'juniper', 'cedar'))
# all.forest.type <- all.forest.type %>% mutate(tree_type.f = as.factor(tree.type.f(levels = c('pine', 'fir', 'oak', 'juniper', 'cedar'))))

#Add text to the plot
p2_texta <- data.frame(label = c("a", "bc", "cd", "cd", "cd", "cd", "cd", "cd", "cd", "cd",
                                 "cd", "d", "d", "d", "d","ab", "ab", "cd", "d", "cd"),
                       sequence   = c('Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 
                                      'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only',
                                      '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only',
                                      '2nd Drought Only', '2nd Drought Only'),
                       # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                       #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                       y     = c(2.75, 1, 0.6, 0.4, 0.28, 0.6, 0.36, 0.22, 0.4, 0.16, 
                                 0.24, 0.2, 0.14, 0.14, 0.16, 1.58, 1.65, 0.27, 0.16, 0.44),
                       x     = c(0.615, 0.81, 1.01, 1.2, 1.38, 1.615, 1.81, 2.01, 2.2, 2.38,
                                 0.615, 0.81, 1.01, 1.2, 1.38, 1.615, 1.81, 2.01, 2.2, 2.38)
)

#Die-off Mortality as a % of basal area
p2 <- ggbarplot(all.forest.type %>% filter(pltID %in% plots & tree_type != 'other conifer' & tree_type != 'deciduous'), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.dead.sum", fill = 'tree_type.f', 
                color = "sequence", 
                position = position_dodge(), add = "mean_se" , error.plot = "errorbar", alpha = 0.8, 
                ylab = expression('Mortality (m'^2*' ha'^-1*')'), 
                xlab = NULL, order = c("1999-2002", "2012-2015")) +
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree Type", label.position = "bottom", title.position="top", title.hjust = 0.5)) +
  scale_color_manual(values = c("black", "black"), aesthetics = "color") + labs(tag = 'd)') +
  scale_fill_discrete(labels = c("pine" = "Pine", "fir" = "Fir", "juniper" = "Juniper", "oak" = "Oak", "cedar" = "Cedar")) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.45, 0.55), legend.text = element_text(size = 8, angle = 45, vjust = 0.8), legend.title = element_text(size = 10),
        legend.direction = "horizontal", axis.text.x = element_text(size = 10, color = 'black'), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), strip.background = element_blank(),
        strip.text.x = element_blank(), plot.margin = unit(c(2.5,0,0,5), "pt"), panel.spacing = unit(20, "pt"),
        plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold")) +
  scale_x_discrete(labels = c("Response During\n1st Period", "Response During\n2nd Period")) +
  geom_text(data = p2_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  # geom_text(data = data.frame(label = "Mean \n+/- SE", y = 1.7, x = 1.5, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')), 
             labeller = as_labeller(c('Both Droughts' = "Exposed to Both Droughts", '2nd Drought Only' = "Exposed to 2nd Drought Only"))) 
p2

#Combine the two figure panels into one	  
f1 <- ggarrange(p1, p2, ncol = 1, align = "v", labels = c("a)", "c)"), nrow = 2, heights = c(1, 0.95), common.legend = FALSE)
f1

ggsave(filename = 'Fig4_FIA_mortality_by_tree_type.png', height=14, width=16, units = 'cm', dpi=900)

#Create labels for the bar chart (a)
p3_texta <- data.frame(label = c("a", "a", "b", "b"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                       #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                       y     = c(28, 21, 38, 37.5),
                       x     = c(1, 2, 1, 2)
)

#Letters to indicate sample sizes
p3_textb <- data.frame(label = c("n = 47", "n = 31", "n = 206", "n = 188"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       y     = c(25, 18, 35, 34.5),
                       x     = c(1, 2, 1, 2)
)

#Summary of Total Basal Area
p3 <- ggbarplot(all.forest %>% filter(pltID %in% plots) %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all.sum = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.all.sum", position = position_dodge(), color = "sequence", fill = 'gray',
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8, ylab = expression('Basal Area (m'^2*' ha'^-1*')')) + 
  theme_bw() + guides(color = 'none', fill = 'none') +
  scale_color_manual(values = c("black", "black"), aesthetics = "color") + labs(tag = 'b)') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.15), legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), plot.margin = unit(c(2.5,0,0,5), "pt"), 
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  geom_text(data = p3_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  geom_text(data = p3_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 20.2, x = 1.2, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')),
             labeller = as_labeller(c('Both Droughts' = "Exposed to Both Droughts", '2nd Drought Only' = "Exposed to 2nd Drought Only")))
p3

#Create a data frame for adding the panel 4 text.
p4_texta <- data.frame(label = c("ad", "abd", "abd", "ab", "b", "abd", "abe", "abe", "ab", "ab",
                                 "c", "d", "b", "b", "b", "c", "de", "b", "b", "b"),
                       sequence   = c('Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 
                                      'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only',
                                      '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only',
                                      '2nd Drought Only', '2nd Drought Only'),
                       # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                       #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                       y     = c(11.1, 5.5, 7.7, 3.85, 1.7, 8.7, 5.4, 4.6, 5.7, 1.5, 
                                 17, 12.2, 3.5, 2.8, 3.6, 19.2, 11.3, 3.4, 2.25, 3.7),
                       x     = c(0.615, 0.81, 1.01, 1.2, 1.38, 1.615, 1.81, 2.01, 2.2, 2.38,
                                 0.615, 0.81, 1.01, 1.2, 1.38, 1.615, 1.81, 2.01, 2.2, 2.38)
)

#Total Basal Area
p4 <- ggbarplot(all.forest.type %>% filter(pltID %in% plots & tree_type != 'other conifer' & tree_type != 'deciduous'),
                x = "time.period", y = "BAA.all.sum", position = position_dodge(), fill = 'tree_type.f', color = "sequence",
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8, ylab = expression('Basal Area (m'^2*' ha'^-1*')'), 
                xlab = NULL, order = c('1999-2002', '2012-2015')) +  
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree Type",  label.position = "bottom", title.position="top", title.hjust = 0.5)) +
  scale_color_manual(values = c("black", "black"), aesthetics = "color") + labs(tag =("d)")) +
  scale_fill_discrete(labels = c("pine" = "Pine", "fir" = "Fir", "juniper" = "Juniper", "oak" = "Oak", "cedar" = "Cedar")) +
  theme(legend.background = element_rect(colour = NA, fill = NA), 
        legend.position = c(0.3, 0.75), legend.text = element_text(size = 8, angle = 45, vjust = 0.8), legend.title = element_text(size = 8),
        legend.direction = "horizontal",axis.text.x = element_text(size = 10, color = 'black'), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), 
        strip.background = element_blank(), strip.text.x = element_blank(), plot.margin = unit(c(2.5,0,0,5), "pt"), 
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold")) +
  scale_x_discrete(labels = c("Response During\n1st Period", "Response During\n2nd Period")) +
  geom_text(data = p4_texta, mapping = aes(x = x, y = y, label = label), size = 4) +
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')))
p4

f2 <- ggarrange(p3, p4, ncol = 1, labels = c('a)', 'c)'), align = "v", nrow = 2, heights = c(1, 0.95), common.legend = FALSE)
f2

ggsave(filename = 'SFig6_basal_area_boxplot.png', height=14, width=16, units = 'cm', dpi=900)

#Tree Mortality Tables
#ANOVA and Tukey HSD for basal area die-off by sequence and time period
aov.dead <- aov(data = join.summary %>% filter(pltID %in% plots), 
                BAA.dead.sum ~ time.period * sequence)

#Tukey HSD
dead.tHSD <- TukeyHSD(aov.dead)

#Basal Area Tables
#ANOVA and Tukey HSD for total basal area sequence and time period
all.forest.plot <- all.forest %>% filter(pltID %in% plots) %>% group_by(time.period, sequence, pltID) %>% 
            summarize(BAA.all.sum = sum(BAA.all), BAA.live = sum(BAA), 
            BAA.dead.sum = sum(BAA.dead), 
            BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100) 

#Summarize the overall data
conifer.forest.all.summary <- all.forest.plot %>% 
                              mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
                              dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
                              group_by(time.period, sequence) %>% 
                              summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
                                        BAA.live.mean = mean(BAA.live), BAA.live.sd = sd(BAA.live),
                                        BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
                                        BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
                                        live.count = sum(live), dead.count = sum(dead))

aov.all <- aov(data = all.forest.plot, BAA.all.sum ~ time.period * sequence)
summary(aov.all)

#Tukey HSD
all.tHSD <- TukeyHSD(aov.all)

#Combine Tukey HSD values
all.tHSD.combine <- list(dead.tHSD, #mort.tHSD, 
                 all.tHSD)

#Create a data frame
df.all.tHSD <- as.data.frame(map_df(all.tHSD.combine, tidy))

#Add a column with variable labels.
df.all.tHSD$variable <- c('Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)',
                        # 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)',
                          'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')

#Add Estimate 1 for Tukey HSD test
#Finish updating
df.all.tHSD$estimate.1 <- c(
  #Mortality
  mean((all.forest.plot %>% filter(time.period == '2012-2015'))$BAA.dead.sum), mean((all.forest.plot %>% filter( sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum),

  #Basal Area
  mean((all.forest.plot %>% filter(time.period == '2012-2015'))$BAA.all.sum), mean((all.forest.plot %>% filter(sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Add Estimate 2 for Tukey HSD test
df.all.tHSD$estimate.2 <- c(#Mortality
  mean((all.forest.plot %>% filter(time.period == '1999-2002'))$BAA.dead.sum), mean((all.forest.plot %>% filter(sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  
  #Basal Area
  mean((all.forest.plot %>% filter(time.period == '1999-2002'))$BAA.all.sum), mean((all.forest.plot %>% filter(sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Select variables and put them in order
df.all.tHSD.label <- df.all.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.all.tHSD.label) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')
# df.all.tHSD.label

#Combined ANOVA and Tukey HSD table
tb1 <- kbl(df.all.tHSD.label, format = 'html', caption = "Table S3: FIA ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 10, file = "STable3_FIA_tHSD_test_results.png", zoom = 5.0)

#Calculate the precentage changes based on the Tukey HSD test
df.all.tHSD$diff.pct <- df.all.tHSD$estimate / df.all.tHSD$estimate.2 * 100

df.all.tHSD$low.pct <- df.all.tHSD$conf.low / df.all.tHSD$estimate.2 * 100

df.all.tHSD$high.pct <- df.all.tHSD$conf.high / df.all.tHSD$estimate.2 * 100

#Select variables and put them in order
df.all.tHSD.sup <- df.all.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.all.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#Combined ANOVA and Tukey HSD table. This is the same data table as Sup Table 3, but with percentage changes. It is not included with the manuscript.
tb2 <- kbl(df.all.tHSD.sup, format = 'html', caption = "Table S7: Field ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 10, file = "STable7_FIA_tHSD_test_results.png", zoom = 5.0)

#Summary statistics for all the data
tb3 <- conifer.forest.all.summary %>% dplyr::select("time.period", "sequence", "count", "live.count", "dead.count", "BAA.all.mean", "BAA.all.sd",        
                                                     "BAA.live.mean", "BAA.live.sd", "BAA.dead.mean", "BAA.dead.sd") %>% 
  kbl(caption = "Table S9: FIA Overall Summary Statistics") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb3, width = 5, file = "STable8_Southern_California_mortality_summary_statistics_v1.png", zoom = 4.0) 



#Save summary statistics by tree species. This data table is not included with the manuscript
tb4 <- conifer.forest.type.summary %>% dplyr::select("tree_type", "time.period", "sequence", "count", "live.count", "dead.count", "BAA.all.mean", "BAA.all.sd",        
                                                 "BAA.live.mean", "BAA.live.sd", "BAA.dead.mean", "BAA.dead.sd") %>% 
  kbl(caption = "Table S10: FIA Tree Species Summary Statistics") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb4, width = 5, file = "STable9_Southern_California_mortality_summary_statistics_v1.png", zoom = 4.0) 

#Analayis by Tree Speices
#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
type.aov.all <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine', 'oak', 'fir', 'juniper', 'cedar')), BAA.all.sum ~ time.period * sequence * tree_type)
summary(type.aov.all)

type.all.tHSD <- TukeyHSD(type.aov.all) 
type.all.tHSD

#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
type.aov.dead <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine', 'oak', 'fir', 'juniper', 'cedar')), BAA.dead.sum ~ time.period * sequence * tree_type)
summary(type.aov.dead)

type.dead.tHSD <- TukeyHSD(type.aov.dead) 
type.dead.tHSD

#For the broad species analysis
#Combine Tukey HSD values
type.tHSD.combine <- list(type.dead.tHSD, #type.basal.dead.tHSD, 
                         type.all.tHSD)

#Create a data frame
df.type.tHSD <- as.data.frame(map_df(type.tHSD.combine, tidy))
print(df.type.tHSD)
                       
#Add a variable column
df.type.tHSD.1 <- df.type.tHSD %>% slice(1:298) %>% mutate(variable = 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)')
df.type.tHSD.2 <- df.type.tHSD %>% slice(299:596) %>% mutate(variable = 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')
df.type.tHSD.combine <- rbind(df.type.tHSD.1, df.type.tHSD.2)

#Select variables and put them in order
df.type.tHSD.label <- df.type.tHSD.combine %>% dplyr::select(variable, contrast, #estimate.1, estimate.2, 
                                                           estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.type.tHSD.label) <- c('Variable', 'Comparison', #'Estimate 1', 'Estimate 2', 
                                 'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#Combined ANOVA and Tukey HSD table
tb5 <- kbl(df.type.tHSD.label, format = 'html', caption = "Table S10: FIA ANOVA and Tukey HSD Results by Species Group", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
# tb5
as_image(x = tb5, width = 10, file = "STable10_FIA_tHSD_test_results_species.png", zoom = 5.0)

#Analayis by Just Pine trees
#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
pine.aov.all <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine')), BAA.all.sum ~ time.period * sequence)

pine.all.tHSD <- TukeyHSD(pine.aov.all) 

#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
pine.aov.dead <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine')), BAA.dead.sum ~ time.period * sequence)

pine.dead.tHSD <- TukeyHSD(pine.aov.dead) 

#For the broad species analysis
#Combine Tukey HSD values
pine.tHSD.combine <- list(pine.dead.tHSD, #type.basal.dead.tHSD, 
                          pine.all.tHSD)

#Create a data frame
df.pine.tHSD <- as.data.frame(map_df(pine.tHSD.combine, tidy))

#Add a column with variable labels.
df.pine.tHSD$variable <- c('Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)',

                          'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')

#Finish updating
df.pine.tHSD$estimate.1 <- c(#Mortality
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Add Estimate 2 for Tukey HSD test
df.pine.tHSD$estimate.2 <- c(#Mortality
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Select variables and put them in order
df.pine.tHSD.label <- df.pine.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, 
                                                             estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.pine.tHSD.label) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 
                                  'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#Combined ANOVA and Tukey HSD table
tb6 <- kbl(df.pine.tHSD.label, format = 'html', caption = "Table S5: Pine Tree FIA ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb6, width = 10, file = "STable11_FIA_tHSD_test_results_pine.png", zoom = 5.0)


#Calculate the precentage changes based on the Tukey HSD test
df.pine.tHSD$diff.pct <- df.pine.tHSD$estimate / df.pine.tHSD$estimate.2 * 100

df.pine.tHSD$low.pct <- df.pine.tHSD$conf.low / df.pine.tHSD$estimate.2 * 100

df.pine.tHSD$high.pct <- df.pine.tHSD$conf.high / df.pine.tHSD$estimate.2 * 100

#Select variables and put them in order
df.pine.tHSD.sup <- df.pine.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.pine.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#Combined ANOVA and Tukey HSD table. This is the same data table as Sup Table 5, but with percentage changes. It is not included with the manuscript.
tb7 <- kbl(df.pine.tHSD.sup, format = 'html', caption = "Table S12: FIA ANOVA and Tukey HSD Results for Pines", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb7, width = 10, file = "STable12_FIA_tHSD_test_results_pine.png", zoom = 5.0)

#Analysis Just for Firs
fir.aov.all <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir')), BAA.all.sum ~ time.period * sequence)

fir.all.tHSD <- TukeyHSD(fir.aov.all) 

#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
fir.aov.dead <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir')), BAA.dead.sum ~ time.period * sequence)

fir.dead.tHSD <- TukeyHSD(fir.aov.dead) 

#For the broad species analysis
#Combine Tukey HSD values
fir.tHSD.combine <- list(fir.dead.tHSD, #type.basal.dead.tHSD, 
                          fir.all.tHSD)

#Create a data frame
df.fir.tHSD <- as.data.frame(map_df(fir.tHSD.combine, tidy))

#Add a column with variable labels.
df.fir.tHSD$variable <- c('Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)',
                           
                           'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')

#Add Estimate 1 for Tukey HSD test
#Finish updating
df.fir.tHSD$estimate.1 <- c(#Mortality
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Add Estimate 2 for Tukey HSD test
df.fir.tHSD$estimate.2 <- c(#Mortality
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Select variables and put them in order
df.fir.tHSD.label <- df.fir.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, 
                                                     estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.fir.tHSD.label) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 
                                  'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#Combined ANOVA and Tukey HSD table
tb8 <- kbl(df.fir.tHSD.label, format = 'html', caption = "Table S6: Fir Tree FIA ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb8, width = 10, file = "STable13_FIA_tHSD_test_results_fir.png", zoom = 5.0)


#Calculate the precentage changes based on the Tukey HSD test
df.fir.tHSD$diff.pct <- df.fir.tHSD$estimate / df.fir.tHSD$estimate.2 * 100

df.fir.tHSD$low.pct <- df.fir.tHSD$conf.low / df.fir.tHSD$estimate.2 * 100

df.fir.tHSD$high.pct <- df.fir.tHSD$conf.high / df.fir.tHSD$estimate.2 * 100

#Select variables and put them in order
df.fir.tHSD.sup <- df.fir.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.fir.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#Combined ANOVA and Tukey HSD table. This is the same data table as Sup Table 3, but with percentage changes. It is not included with the manuscript.
tb9 <- kbl(df.fir.tHSD.sup, format = 'html', caption = "Table S14: FIA ANOVA and Tukey HSD Results for Firs", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb9, width = 10, file = "STable14_FIA_tHSD_test_results_fir.png", zoom = 5.0)
