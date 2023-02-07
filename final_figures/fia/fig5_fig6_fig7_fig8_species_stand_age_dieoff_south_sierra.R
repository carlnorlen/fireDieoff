#Author: Carl Norlen
#Date Created: December 10, 2021
#Date Edited: February 7, 2023
#Purpose: Do an analysis of dead trees and Stand Age

# Specify necessary packages
p <- c("RSQLite","dbplyr","ggplot2","dplyr","tidyr", "ggpubr", "RColorBrewer",  
	   'gt', 'gtsummary', 'webshot', 'kableExtra', 'broom')

# If necessary: install/update packages
# install.packages(p,repo='https://cloud.r-project.org/')

# Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
#setwd("/C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/field_data")
#cd /C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/field_data
#cd /C/Users/can02/mystuff/fireDieoff/FIA
#Command for calling the script in the command line: R < stand_age_dieoff.r --vanilla
#INstalling packages: install.packages('RColorBrewer',repo='https://cran.cnr.berkeley.edu/')
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/fia')
# setwd('C:/Users/Carl/mystuff/fireDieoff/final_figures/fia')
#Add Data Sets
sql_dir <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
# sql_dir <- 'C:\\Users\\Carl\\mystuff\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
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
c.fldtypcd, rft.meaning, c.stdage, c.fldage, t.totage, t.bhage, c.live_canopy_cvr_pct, c.live_missing_canopy_cvr_pct,
c.dstrbcd1, c.dstrbyr1
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
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es') --South Sierra Nevada P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba' OR 
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
live <- all %>% filter(STATUSCD == 1) %>% 
                group_by(INVYR, PLOT, tree_type, .drop = FALSE) %>% 
                summarize(count.live = n(), tpa.live = sum(count), 
                basal_area.live = sum(basal_area), STDAGE = median(STDAGE), FORTYPCD = median (FORTYPCD))
live
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead <- all %>% filter(STATUSCD == 2 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")) %>% 
                group_by(PLOT, INVYR, tree_type, .drop = FALSE) %>% 
                summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))

dead
join <- left_join(live, dead, by = c('PLOT', 'INVYR', 'tree_type'))

#Replace the NAs with 0s
join <- join %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0)
                               ) %>% group_by(INVYR, PLOT, tree_type) %>%
                 fill(STDAGE, .direction = c("up"))
# join
# summary(join)
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
join %>% summary()
#Region wide counts of dead and live trees
p1<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>% 
              group_by(INVYR, tree_type) %>% summarize(BA.dead = mean(basal_area.dead)), 
            mapping = aes(x = INVYR, y = BA.dead, color = tree_type, linetype = tree_type), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
                group_by(INVYR, tree_type) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = INVYR, fill = tree_type, linetype = tree_type), alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.25, 0.5), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  # scale_fill_viridis_d("Tree\nSpecies", option = 'B') +
  # scale_color_viridis_d("Tree\nSpecies", option = 'B') +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p1

p2 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              group_by(INVYR, tree_type) %>% summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100, BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.dead.pct.mean, color = tree_type, linetype = tree_type),  size = 1) +
  #The error bars
  geom_ribbon(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
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
p2

# join %>% filter(STDAGE <= 10) %>% count()
p3 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
  group_by(INVYR, tree_type) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), 
  mapping = aes(x = INVYR, y = BA.all, color = tree_type, linetype = tree_type),  size = 1) +
  #The error bars
  geom_ribbon(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
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
p3

f1 <- ggarrange(p1, p2, p3, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f1

ggsave(filename = 'Fig5_FIA_bySpecies_mortality_basal_area_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

#Region wide counts of dead and live trees (density)
p4<- ggplot() + 
  #Mean Die-off
  geom_line(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              group_by(INVYR, tree_type) %>% summarize(TPA.dead = mean(tpa.dead)), 
            mapping = aes(x = INVYR, y = TPA.dead, color = tree_type, linetype = tree_type), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                group_by(INVYR, tree_type) %>%
                summarize(TPA.dead = mean(tpa.dead),
                          TPA.dead.sd = sd(tpa.dead), TPA.n = n()),
              mapping = aes(ymin=TPA.dead - 1.96*(TPA.dead.sd / sqrt(TPA.n)),
                            ymax=TPA.dead + 1.96*(TPA.dead.sd / sqrt(TPA.n)),
                            x = INVYR, fill = tree_type), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.25, 0.5), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Year') + ylab(expression('Mortality (trees ha'^-1*')')) 
p4

# join %>% summary()
p5 <- ggplot() + 
  #The data mean
  geom_line(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
              group_by(INVYR, tree_type) %>% summarize(TPA.dead.pct.mean = mean(tpa.dead.pct) * 100, TPA.n = n()), 
            mapping = aes(x = INVYR, y = TPA.dead.pct.mean, color = tree_type, linetype = tree_type),  size = 1) +
  #The error bars (95% CI)
  geom_ribbon(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%    
                group_by(INVYR, tree_type) %>%
                summarize(TPA.dead.pct.mean = mean(tpa.dead.pct) * 100,
                          TPA.dead.pct.sd = sd(tpa.dead.pct) * 100, 
                          TPA.n = n()),
              mapping = aes(ymin=TPA.dead.pct.mean - 1.96*(TPA.dead.pct.sd / sqrt(TPA.n)),
                            ymax=TPA.dead.pct.mean + 1.96*(TPA.dead.pct.sd / sqrt(TPA.n)),
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
  geom_line(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              group_by(INVYR, tree_type) %>% summarize(TPA.all = mean(tpa.all), TPA.n = n()), mapping = aes(x = INVYR, y = TPA.all, color = tree_type, linetype = tree_type), size = 1) +
  #The error bars
  geom_ribbon(data = join %>% ungroup() %>% filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
                group_by(INVYR, tree_type) %>%
                summarize(TPA.mean = mean(tpa.all),
                          TPA.sd = sd(tpa.all), TPA.n = n()),
              mapping = aes(ymin=TPA.mean - 1.96*(TPA.sd / sqrt(TPA.n)),
                            ymax=TPA.mean + 1.96*(TPA.sd / sqrt(TPA.n)),
                            x = INVYR, fill = tree_type), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nSpecies", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Year') + ylab(expression('Density (trees ha'^-1*')'))
p6

f3 <- ggarrange(p4, p5, p6, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f3

ggsave(filename = 'fig6_bySpecies_FIA_density_mortality_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

#Figure out how to do a STAND AGE Time series.
# #Calculate the Quintiles of tree age (by trees sampled)
bhage.q <- as.data.frame(unname(quantile((all %>% filter(tree_type %in% c('pine', 'fir') & 
                                                  #INVYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") & 
                                                  !is.na(BHAGE)))$BHAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))

colnames(bhage.q) <- 'BHAGE'
bhage.q$'Quintile' <- c(0.0, 0.2, 0.4, 0.6,  0.8, 1.0)
bhage.q
f4 <- ggplot(data = all %>% filter(tree_type %in% c('pine', 'fir') & !is.na(BHAGE))) + # & INVYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019"))) + 
  geom_vline(xintercept = (bhage.q %>% filter(Quintile == 0.2) %>% dplyr::select(BHAGE) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') + 
    geom_vline(xintercept = (bhage.q %>% filter(Quintile == 0.4) %>% dplyr::select(BHAGE) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
    geom_vline(xintercept = (bhage.q %>% filter(Quintile == 0.6) %>% dplyr::select(BHAGE) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
  geom_vline(xintercept = (bhage.q %>% filter(Quintile == 0.8) %>% dplyr::select(BHAGE) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
geom_histogram(mapping = aes(x = BHAGE), bins = 60) + theme_bw()
f4 
ggsave(filename = 'Fig7_pine_fir_tree_age_histogram.png', height=12, width= 16, units = 'cm', dpi=900)

bhage.q
# all %>% summary()
#Add the stand age bins
all <- all %>% filter(!is.na(BHAGE)) %>% mutate(bhage.bin = case_when(
  BHAGE >= bhage.q %>% filter(Quintile == 0.8) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '171+',
  BHAGE >= bhage.q %>% filter(Quintile == 0.6) %>% dplyr::select(BHAGE) %>% as.numeric() &
    BHAGE < bhage.q %>% filter(Quintile == 0.8) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '111-170',
  BHAGE >= bhage.q %>% filter(Quintile == 0.4) %>% dplyr::select(BHAGE) %>% as.numeric() & 
    BHAGE < bhage.q %>% filter(Quintile == 0.6) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '78-110',
  BHAGE >= bhage.q %>% filter(Quintile == 0.2) %>% dplyr::select(BHAGE) %>% as.numeric() & 
    BHAGE < bhage.q %>% filter(Quintile == 0.4) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '55-77',
  BHAGE < bhage.q %>% filter(Quintile == 0.2) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '2-54'))
all %>% summary()
#Order the stand age bins
all$bhage.bin = with(all %>% filter(!is.na(BHAGE)), factor(bhage.bin, levels = c('2-54','55-77','78-110', '111-170', '171+')))

live.bhage <- all %>% filter(STATUSCD == 1 & tree_type %in% c('pine', 'fir') & !is.na(BHAGE)) %>% 
  group_by(INVYR, PLOT, bhage.bin, .drop = FALSE) %>% 
  summarize(count.live = n(), tpa.live = sum(count), 
            basal_area.live = sum(basal_area), STDAGE = median(STDAGE))
# live.bhage
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead.bhage <- all %>% filter(STATUSCD == 2 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") & 
                             tree_type %in% c('pine', 'fir') & !is.na(BHAGE)) %>% 
  group_by(PLOT, INVYR, bhage.bin, .drop = FALSE) %>% 
  summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))

# dead.bhage
join.bhage <- left_join(live.bhage, dead.bhage, by = c('PLOT', 'INVYR', 'bhage.bin'))

#Replace the NAs with 0s
join.bhage <- join.bhage %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0)
) %>% group_by(INVYR, PLOT) %>%
  fill(STDAGE, .direction = c("up"))


#Add the total basal area calculations
join.bhage$count.all <- join.bhage$count.live + join.bhage$count.dead
join.bhage$tpa.all <- join.bhage$tpa.live + join.bhage$tpa.dead
join.bhage$basal_area.all <- join.bhage$basal_area.live + join.bhage$basal_area.dead
join.bhage$basal_area.dead.pct <- join.bhage$basal_area.dead / join.bhage$basal_area.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join.bhage <- join.bhage %>% dplyr::mutate(basal_area.dead.pct = replace(basal_area.dead.pct, is.na(basal_area.dead.pct), 0))

join.bhage$tpa.dead.pct <- join.bhage$tpa.dead / join.bhage$tpa.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join.bhage <- join.bhage %>% dplyr::mutate(tpa.dead.pct = replace(tpa.dead.pct, is.na(tpa.dead.pct), 0))

summary(join.bhage)
#Region wide counts of dead and live trees
p7<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join.bhage %>% ungroup() %>% #filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>% 
              filter(!is.na(bhage.bin)) %>%
              group_by(INVYR, bhage.bin) %>% summarize(BA.dead = mean(basal_area.dead)), 
            mapping = aes(x = INVYR, y = BA.dead, color = bhage.bin, linetype = bhage.bin), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
                filter(!is.na(bhage.bin)) %>%
                group_by(INVYR, bhage.bin) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = INVYR, fill = bhage.bin, linetype = bhage.bin), alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.25, 0.5), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nAge") +
  # scale_fill_viridis_d("Tree\nAge", option = 'B') +
  # scale_color_viridis_d("Tree\nAge", option = 'B') +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p7

p8 <- ggplot() + #geom_line(data = join.bhage %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(bhage.bin)) %>%
              group_by(INVYR, bhage.bin) %>% summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100, BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.dead.pct.mean, color = bhage.bin, linetype = bhage.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                group_by(INVYR, bhage.bin) %>%
                summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100,
                          BA.dead.pct.sd = sd(basal_area.dead.pct) * 100, 
                          BA.n = n()),
              mapping = aes(ymin=BA.dead.pct.mean - 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            ymax=BA.dead.pct.mean + 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            x = INVYR, fill = bhage.bin), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nAge") +
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  # ylim(-2.5,20) +
  xlab('Year') + ylab('Mortality (%)')
p8

# join.bhage %>% filter(bhage <= 10) %>% count()
p9 <- ggplot() + #geom_line(data = join.bhage %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(bhage.bin)) %>%
              group_by(INVYR, bhage.bin) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.all, color = bhage.bin, linetype = bhage.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                filter(!is.na(bhage.bin)) %>%
                group_by(INVYR, bhage.bin) %>%
                summarize(BA.mean = mean(basal_area.all),
                          BA.sd = sd(basal_area.all), BA.n = n()),
              mapping = aes(ymin=BA.mean - 1.96*(BA.sd / sqrt(BA.n)),
                            ymax=BA.mean + 1.96*(BA.sd / sqrt(BA.n)),
                            x = INVYR, fill = bhage.bin), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nAge") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')'))
p9

f5 <- ggarrange(p7, p8, p9, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f5

ggsave(filename = 'Fig8_pine_fir_byTreeAge_FIA_mortality_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

#Does this actually make sense?
# p9a <- ggplot(data = all %>% filter(COMMON_NAME %in% c('ponderosa pine', 'California red fir', 'white fir', 'sugar pine') & !is.na(BHAGE) & STATUSCD == 2), mapping = aes(x = BHAGE, y = basal_area, color = COMMON_NAME)) + 
# geom_point() + geom_smooth(method = 'lm') + stat_cor() + theme_bw()
# p9a
# 
# p9b <- ggplot(data = all %>% filter(COMMON_NAME %in% c('ponderosa pine', 'California red fir', 'white fir', 'sugar pine') & !is.na(BHAGE) & STATUSCD == 1), mapping = aes(x = BHAGE, y = basal_area, color = COMMON_NAME)) + 
#   geom_point() + geom_smooth(method = 'lm') + stat_cor() + theme_bw()
# p9b
#Add Tree Density plots by tree age.
#Create the time series for FIA Pine and Fire diameter
p10 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join.bhage %>% ungroup() %>% #filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>% 
              filter(!is.na(bhage.bin)) %>%
              group_by(INVYR, bhage.bin) %>% summarize(TPA.dead = mean(tpa.dead)), 
            mapping = aes(x = INVYR, y = TPA.dead, color = bhage.bin, linetype = bhage.bin), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
                filter(!is.na(bhage.bin)) %>%
                group_by(INVYR, bhage.bin) %>%
                summarize(TPA.dead = mean(tpa.dead),
                          TPA.dead.sd = sd(tpa.dead), TPA.n = n()),
              mapping = aes(ymin=TPA.dead - 1.96*(TPA.dead.sd / sqrt(TPA.n)),
                            ymax=TPA.dead + 1.96*(TPA.dead.sd / sqrt(TPA.n)),
                            x = INVYR, fill = bhage.bin, linetype = bhage.bin), alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.25, 0.5), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nAge") +
  # scale_fill_viridis_d("Tree\nAge", option = 'B') +
  # scale_color_viridis_d("Tree\nAge", option = 'B') +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (trees ha'^-1*')')) 
p10

p11 <- ggplot() + #geom_line(data = join.bhage %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(bhage.bin)) %>%
              group_by(INVYR, bhage.bin) %>% summarize(TPA.dead.pct.mean = mean(tpa.dead.pct) * 100, TPA.n = n()), 
            mapping = aes(x = INVYR, y = TPA.dead.pct.mean, color = bhage.bin, linetype = bhage.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                group_by(INVYR, bhage.bin) %>%
                summarize(TPA.dead.pct.mean = mean(tpa.dead.pct) * 100,
                          TPA.dead.pct.sd = sd(tpa.dead.pct) * 100, 
                          TPA.n = n()),
              mapping = aes(ymin=TPA.dead.pct.mean - 1.96*(TPA.dead.pct.sd / sqrt(TPA.n)),
                            ymax=TPA.dead.pct.mean + 1.96*(TPA.dead.pct.sd / sqrt(TPA.n)),
                            x = INVYR, fill = bhage.bin), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nAge") +
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  ylim(-2.5,15) +
  xlab('Year') + ylab('Mortality (%)')
p11

# join.bhage %>% filter(bhage <= 10) %>% count()
p12 <- ggplot() + #geom_line(data = join.bhage %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(bhage.bin)) %>%
              group_by(INVYR, bhage.bin) %>% summarize(TPA.all = mean(tpa.all), TPA.n = n()), 
            mapping = aes(x = INVYR, y = TPA.all, color = bhage.bin, linetype = bhage.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.bhage %>% ungroup() %>% #filter(bhage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                filter(!is.na(bhage.bin)) %>%
                group_by(INVYR, bhage.bin) %>%
                summarize(TPA.mean = mean(tpa.all),
                          TPA.sd = sd(tpa.all), TPA.n = n()),
              mapping = aes(ymin=TPA.mean - 1.96*(TPA.sd / sqrt(TPA.n)),
                            ymax=TPA.mean + 1.96*(TPA.sd / sqrt(TPA.n)),
                            x = INVYR, fill = bhage.bin), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nAge") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Year') + ylab(expression('Basal Area (trees ha'^-1*')'))
p12

f6 <- ggarrange(p10, p11, p12, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f6

ggsave(filename = 'Fig9_pine_fir_byAge_FIA_mortality_density_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

#Figure out how to do a DBH time series for the pine and fir trees.
dia.q <- as.data.frame(unname(quantile((all %>% filter(tree_type %in% c('pine', 'fir')))$DIA, prob = seq(0,1, 1/4), type = 3, na.rm = TRUE)))

colnames(dia.q) <- 'DIA'
dia.q$'Quintile' <- c(0.0, 0.25, 0.5,  0.75, 1.0)
dia.q
f7 <- ggplot(data = all %>% filter(tree_type %in% c('pine', 'fir'))) + # & INVYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019"))) + 
  geom_vline(xintercept = (dia.q %>% filter(Quintile == 0.25) %>% dplyr::select(DIA) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') + 
  geom_vline(xintercept = (dia.q %>% filter(Quintile == 0.5) %>% dplyr::select(DIA) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
  geom_vline(xintercept = (dia.q %>% filter(Quintile == 0.75) %>% dplyr::select(DIA) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
  #geom_vline(xintercept = (dia.q %>% filter(Quintile == 0.8) %>% dplyr::select(DIA) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
  geom_histogram(mapping = aes(x = DIA), bins = 60) + theme_bw()
f7 
ggsave(filename = 'Fig10_pine_fir_diameter_histogram.png', height=12, width= 16, units = 'cm', dpi=900)

dia.q
#Add the stand age bins
all <- all %>% mutate(dia.bin = case_when(
  DIA >= dia.q %>% filter(Quintile == 0.75) %>% dplyr::select(DIA) %>% as.numeric() ~ '73.91+',
  DIA >= dia.q %>% filter(Quintile == 0.5) %>% dplyr::select(DIA) %>% as.numeric() &
    DIA < dia.q %>% filter(Quintile == 0.75) %>% dplyr::select(DIA) %>% as.numeric() ~ '45.47-73.90',
  DIA >= dia.q %>% filter(Quintile == 0.25) %>% dplyr::select(DIA) %>% as.numeric() & 
    DIA < dia.q %>% filter(Quintile == 0.5) %>% dplyr::select(DIA) %>% as.numeric() ~ '22.86-45.46',
  # DIA >= dia.q %>% filter(Quintile == 0.2) %>% dplyr::select(DIA) %>% as.numeric() & 
    # DIA < dia.q %>% filter(Quintile == 0.4) %>% dplyr::select(DIA) %>% as.numeric() ~ '19.30-34.30',
  DIA < dia.q %>% filter(Quintile == 0.25) %>% dplyr::select(DIA) %>% as.numeric() ~ '2.54-22.85'))
all
#Order the stand age bins
all$dia.bin = with(all, factor(dia.bin, levels = c('2.54-22.85','22.86-45.46','45.47-73.90', '73.91+')))
summary(all)
live.dia <- all %>% filter(STATUSCD == 1 & tree_type %in% c('pine', 'fir')) %>% 
  group_by(INVYR, PLOT, dia.bin, .drop = FALSE) %>% 
  summarize(count.live = n(), tpa.live = sum(count), 
            basal_area.live = sum(basal_area), BHAGE = median(BHAGE))
live.dia %>% summary()

#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead.dia <- all %>% filter(STATUSCD == 2 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") & tree_type %in% c('pine', 'fir')) %>% 
  group_by(PLOT, INVYR, dia.bin, .drop = FALSE) %>% 
  summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))

dead.dia %>% summary()
join.dia <- left_join(live.dia, dead.dia, by = c('PLOT', 'INVYR', 'dia.bin'))
summary(join.dia)
#Replace the NAs with 0s
join.dia <- join.dia %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                                             count.dead = replace(count.dead, is.na(count.dead), 0),
                                             tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0)
) %>% group_by(INVYR, PLOT) %>%
  fill(BHAGE, .direction = c("up"))


#Add the total basal area calculations
join.dia$count.all <- join.dia$count.live + join.dia$count.dead
join.dia$tpa.all <- join.dia$tpa.live + join.dia$tpa.dead
join.dia$basal_area.all <- join.dia$basal_area.live + join.dia$basal_area.dead
join.dia$basal_area.dead.pct <- join.dia$basal_area.dead / join.dia$basal_area.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join.dia <- join.dia %>% dplyr::mutate(basal_area.dead.pct = replace(basal_area.dead.pct, is.na(basal_area.dead.pct), 0))

join.dia$tpa.dead.pct <- join.dia$tpa.dead / join.dia$tpa.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join.dia <- join.dia %>% dplyr::mutate(tpa.dead.pct = replace(tpa.dead.pct, is.na(tpa.dead.pct), 0))
summary(join.dia)

#Create the time series for FIA Pine and Fire diameter
p13<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join.dia %>% ungroup() %>% #filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>% 
              filter(!is.na(dia.bin)) %>%
              group_by(INVYR, dia.bin) %>% summarize(BA.dead = mean(basal_area.dead)), 
            mapping = aes(x = INVYR, y = BA.dead, color = dia.bin, linetype = dia.bin), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
                filter(!is.na(dia.bin)) %>%
                group_by(INVYR, dia.bin) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = INVYR, fill = dia.bin, linetype = dia.bin), alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.25, 0.5), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nDiameter (cm)") +
  # scale_fill_viridis_d("Tree\nAge", option = 'B') +
  # scale_color_viridis_d("Tree\nAge", option = 'B') +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p13

p14 <- ggplot() + #geom_line(data = join.dia %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(dia.bin)) %>%
              group_by(INVYR, dia.bin) %>% summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100, BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.dead.pct.mean, color = dia.bin, linetype = dia.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                group_by(INVYR, dia.bin) %>%
                summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100,
                          BA.dead.pct.sd = sd(basal_area.dead.pct) * 100, 
                          BA.n = n()),
              mapping = aes(ymin=BA.dead.pct.mean - 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            ymax=BA.dead.pct.mean + 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            x = INVYR, fill = dia.bin), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nDiameter (cm)") +
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  #ylim(-2.5,20) +
  xlab('Year') + ylab('Mortality (%)')
p14

# join.stdage %>% filter(STDAGE <= 10) %>% count()
p15 <- ggplot() + #geom_line(data = join.stdage %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(dia.bin)) %>%
              group_by(INVYR, dia.bin) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.all, color = dia.bin, linetype = dia.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                filter(!is.na(dia.bin)) %>%
                group_by(INVYR, dia.bin) %>%
                summarize(BA.mean = mean(basal_area.all),
                          BA.sd = sd(basal_area.all), BA.n = n()),
              mapping = aes(ymin=BA.mean - 1.96*(BA.sd / sqrt(BA.n)),
                            ymax=BA.mean + 1.96*(BA.sd / sqrt(BA.n)),
                            x = INVYR, fill = dia.bin), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nDiameter (cm)") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')'))
p15

f8 <- ggarrange(p13, p14, p15, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f8

ggsave(filename = 'Fig11_pine_fir_byDIA_FIA_mortality_time_series_.png', height=18, width= 10, units = 'cm', dpi=900)

#Create the time series for FIA Pine and Fire diameter
p16 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join.dia %>% ungroup() %>% #filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>% 
              filter(!is.na(dia.bin)) %>%
              group_by(INVYR, dia.bin) %>% summarize(TPA.dead = mean(tpa.dead)), 
            mapping = aes(x = INVYR, y = TPA.dead, color = dia.bin, linetype = dia.bin), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
                filter(!is.na(dia.bin)) %>%
                group_by(INVYR, dia.bin) %>%
                summarize(TPA.dead = mean(tpa.dead),
                          TPA.dead.sd = sd(tpa.dead), TPA.n = n()),
              mapping = aes(ymin=TPA.dead - 1.96*(TPA.dead.sd / sqrt(TPA.n)),
                            ymax=TPA.dead + 1.96*(TPA.dead.sd / sqrt(TPA.n)),
                            x = INVYR, fill = dia.bin, linetype = dia.bin), alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.25, 0.5), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nDiameter (cm)") +
  # scale_fill_viridis_d("Tree\nAge", option = 'B') +
  # scale_color_viridis_d("Tree\nAge", option = 'B') +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (trees ha'^-1*')')) 
p16

p17 <- ggplot() + #geom_line(data = join.dia %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(dia.bin)) %>%
              group_by(INVYR, dia.bin) %>% summarize(TPA.dead.pct.mean = mean(tpa.dead.pct) * 100, TPA.n = n()), 
            mapping = aes(x = INVYR, y = TPA.dead.pct.mean, color = dia.bin, linetype = dia.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                group_by(INVYR, dia.bin) %>%
                summarize(TPA.dead.pct.mean = mean(tpa.dead.pct) * 100,
                          TPA.dead.pct.sd = sd(tpa.dead.pct) * 100, 
                          TPA.n = n()),
              mapping = aes(ymin=TPA.dead.pct.mean - 1.96*(TPA.dead.pct.sd / sqrt(TPA.n)),
                            ymax=TPA.dead.pct.mean + 1.96*(TPA.dead.pct.sd / sqrt(TPA.n)),
                            x = INVYR, fill = dia.bin), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nDiameter (cm)") +
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  #ylim(-2.5,20) +
  xlab('Year') + ylab('Mortality (%)')
p17

# join.stdage %>% filter(STDAGE <= 10) %>% count()
p18 <- ggplot() + #geom_line(data = join.stdage %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(dia.bin)) %>%
              group_by(INVYR, dia.bin) %>% summarize(TPA.all = mean(tpa.all), TPA.n = n()), 
            mapping = aes(x = INVYR, y = TPA.all, color = dia.bin, linetype = dia.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.dia %>% ungroup() %>% #filter(dia.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                filter(!is.na(dia.bin)) %>%
                group_by(INVYR, dia.bin) %>%
                summarize(TPA.mean = mean(tpa.all),
                          TPA.sd = sd(tpa.all), TPA.n = n()),
              mapping = aes(ymin=TPA.mean - 1.96*(TPA.sd / sqrt(TPA.n)),
                            ymax=TPA.mean + 1.96*(TPA.sd / sqrt(TPA.n)),
                            x = INVYR, fill = dia.bin), alpha = 0.2) +
  scale_color_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_fill_brewer(name = "Tree\nDiameter (cm)", palette = 'Dark2') +
  scale_linetype_discrete(name = "Tree\nDiameter (cm)") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Year') + ylab(expression('Basal Area (trees ha'^-1*')'))
p18

f9 <- ggarrange(p16, p17, p18, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f9

ggsave(filename = 'Fig12_pine_fir_byDIA_FIA_mortality_density_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

#Test of Mortality by common name species
live.pine <- all %>% filter(STATUSCD == 1 & tree_type %in% c('pine', 'fir')) %>% 
  group_by(INVYR, PLOT, COMMON_NAME, .drop = FALSE) %>% 
  summarize(count.live = n(), tpa.live = sum(count), 
            basal_area.live = sum(basal_area), STDAGE = median(STDAGE))
live.pine
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead.pine <- all %>% filter(STATUSCD == 2 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") & tree_type %in% c('pine', 'fir')) %>% 
  group_by(PLOT, INVYR, COMMON_NAME, .drop = FALSE) %>% 
  summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))

dead.pine
join.pine <- left_join(live.pine, dead.pine, by = c('PLOT', 'INVYR', 'COMMON_NAME'))

#Replace the NAs with 0s
join.pine <- join.pine %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0)
) %>% group_by(INVYR, PLOT) %>%
  fill(STDAGE, .direction = c("up"))
# join
# summary(join)
#Add the total basal area calculations
join.pine$count.all <- join.pine$count.live + join.pine$count.dead
join.pine$tpa.all <- join.pine$tpa.live + join.pine$tpa.dead
join.pine$basal_area.all <- join.pine$basal_area.live + join.pine$basal_area.dead
join.pine$basal_area.dead.pct <- join.pine$basal_area.dead / join.pine$basal_area.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join.pine <- join.pine %>% dplyr::mutate(basal_area.dead.pct = replace(basal_area.dead.pct, is.na(basal_area.dead.pct), 0))

join.pine$tpa.dead.pct <- join.pine$tpa.dead / join.pine$tpa.all
#fill the NAs for basal_area.dead.pct, this could go earlier
join.pine <- join.pine %>% dplyr::mutate(tpa.dead.pct = replace(tpa.dead.pct, is.na(tpa.dead.pct), 0))

#Mortality by specific pine species
#Figure out species level mortality

#Percentage mortality by Pine/Fir species
join.pine %>% filter(INVYR %in% c(2013,2014,2015,2016,2017,2018,2019)) %>% group_by(COMMON_NAME) %>% summarize(mortality = mean(basal_area.dead.pct))

p19 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join.pine %>% ungroup() %>% 
              filter(COMMON_NAME %in% c('ponderosa pine', 'sugar pine', 'California red fir', 'white fir', 'western white pine', 'Jeffrey pine', 'lodgepole pine', 'singleleaf pinyon')) %>% 
              group_by(INVYR, COMMON_NAME) %>% summarize(BA.dead = mean(basal_area.dead)), 
            mapping = aes(x = INVYR, y = BA.dead, color = COMMON_NAME, linetype = COMMON_NAME), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join.pine %>% ungroup() %>%
  filter(COMMON_NAME %in% c('ponderosa pine', 'sugar pine', 'California red fir', 'white fir', 'western white pine', 'Jeffrey pine', 'lodgepole pine', 'singleleaf pinyon')) %>%
                group_by(INVYR, COMMON_NAME) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = INVYR, fill = COMMON_NAME, linetype = COMMON_NAME), alpha = 0.2) +
  theme_bw() +
  guides(color=guide_legend(ncol=2)) +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.35, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_viridis_d(name = "Tree\nSpecies") +
  scale_fill_viridis_d(name = "Tree\nSpecies") +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  # scale_fill_viridis_d("Tree\nSpecies", option = 'B') +
  # scale_color_viridis_d("Tree\nSpecies", option = 'B') +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p19

# join.pine %>% filter(INVYR %in% c(2013,2014,2015,2016,2017,2018,2019)) %>% group_by(COMMON_NAME) %>% summarize(mortality = mean(basal_area.dead.pct))

p20 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.pine %>% ungroup() %>% 
              filter(COMMON_NAME %in% c('ponderosa pine', 'sugar pine', 'California red fir', 'white fir', 'western white pine', 'Jeffrey pine', 'lodgepole pine', 'singleleaf pinyon')) %>% 
              group_by(INVYR, COMMON_NAME) %>% summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100, BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.dead.pct.mean, color = COMMON_NAME, linetype = COMMON_NAME),  size = 1) +
  #The error bars
  geom_ribbon(data = join.pine %>% ungroup() %>% 
                filter(COMMON_NAME %in% c('ponderosa pine', 'sugar pine', 'California red fir', 'white fir', 'western white pine', 'Jeffrey pine', 'lodgepole pine', 'singleleaf pinyon')) %>%  
                group_by(INVYR, COMMON_NAME) %>%
                summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100,
                          BA.dead.pct.sd = sd(basal_area.dead.pct) * 100, 
                          BA.n = n()),
              mapping = aes(ymin=BA.dead.pct.mean - 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            ymax=BA.dead.pct.mean + 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            x = INVYR, fill = COMMON_NAME), alpha = 0.2) +
  scale_color_viridis_d(name = "Tree\nSpecies") +
  scale_fill_viridis_d(name = "Tree\nSpecies") +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  xlab('Year') + ylab('Mortality (%)')
p20

# join %>% filter(STDAGE <= 10) %>% count()
p21 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.pine %>% ungroup() %>% 
              filter(COMMON_NAME %in% c('ponderosa pine', 'sugar pine', 'California red fir', 'white fir', 'western white pine', 'Jeffrey pine', 'lodgepole pine', 'singleleaf pinyon')) %>%               group_by(INVYR, COMMON_NAME) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.all, color = COMMON_NAME, linetype = COMMON_NAME),  size = 1) +
  #The error bars
  geom_ribbon(data = join.pine %>% ungroup() %>% 
                filter(COMMON_NAME %in% c('ponderosa pine', 'sugar pine', 'California red fir', 'white fir', 'western white pine', 'Jeffrey pine', 'lodgepole pine', 'singleleaf pinyon')) %>%                 group_by(INVYR, COMMON_NAME) %>%
                summarize(BA.mean = mean(basal_area.all),
                          BA.sd = sd(basal_area.all), BA.n = n()),
              mapping = aes(ymin=BA.mean - 1.96*(BA.sd / sqrt(BA.n)),
                            ymax=BA.mean + 1.96*(BA.sd / sqrt(BA.n)),
                            x = INVYR, fill = COMMON_NAME), alpha = 0.2) +
  scale_color_viridis_d(name = "Tree\nSpecies") +
  scale_fill_viridis_d(name = "Tree\nSpecies") +
  scale_linetype_discrete(name = "Tree\nSpecies") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')'))
p21

f10 <- ggarrange(p19, p20, p21, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f10

ggsave(filename = 'Fig13_pine_fir_bySpecies_FIA_mortality_density_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

#Calculate everything by plot stand age
# live.stdage <- all %>% filter(STATUSCD == 1 & tree_type %in% c('pine', 'fir') & !is.na(BHAGE)) %>% 
#   group_by(INVYR, PLOT, stdage.bin, .drop = FALSE) %>% 
#   summarize(count.live = n(), tpa.live = sum(count), 
#             basal_area.live = sum(basal_area), STDAGE = median(STDAGE))
# # live.stdage
# #There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
# dead.stdage <- all %>% filter(STATUSCD == 2 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") & 
#                                tree_type %in% c('pine', 'fir') & !is.na(BHAGE)) %>% 
#   group_by(PLOT, INVYR, stdage.bin, .drop = FALSE) %>% 
#   summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
# 
# # dead.bhage
# join.stdage <- left_join(live.stdage, dead.stdage, by = c('PLOT', 'INVYR', 'stdage.bin'))
# 
# #Replace the NAs with 0s
# join.stdage <- join.stdage %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
#                                            count.dead = replace(count.dead, is.na(count.dead), 0),
#                                            tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0)
# ) %>% group_by(INVYR, PLOT) %>%
#   fill(STDAGE, .direction = c("up"))
# join %>% summary()
stdage.q <- as.data.frame(unname(quantile((join %>% filter(
                                                            #INVYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019") & 
                                                            !is.na(STDAGE) & STDAGE >  0))$STDAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))

colnames(stdage.q) <- 'STDAGE'
stdage.q$'Quintile' <- c(0.0, 0.2, 0.4, 0.6,  0.8, 1.0)
stdage.q
f11 <- ggplot(data = all %>% filter(!is.na(STDAGE) & STDAGE >  0)) + 
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') + 
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black', size = 1, linetype = 'dashed') +
  geom_histogram(mapping = aes(x = STDAGE), bins = 60) + theme_bw()
f11 
ggsave(filename = 'Fig14_pine_fir_stand_age_histogram.png', height=12, width= 16, units = 'cm', dpi=900)

stdage.q
# all %>% summary()
#Add the stand age bins
join.stdage <- join %>% filter(!is.na(STDAGE) & STDAGE > 0) %>% mutate(stdage.bin = case_when(
  STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '205+',
  STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() &
    STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '160-204',
  STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '115-159',
  STDAGE >= stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '90-114',
  STDAGE < stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '6-89'))

join.stdage %>% summary()

#Order the stand age bins
join.stdage$stdage.bin = with(join.stdage, factor(stdage.bin, levels = c('6-89','90-114','115-159', '160-204', '205+')))

#Region wide counts of dead and live trees
p22 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join.stdage %>% ungroup() %>% #filter(tree_type %in% c('pine', 'fir', 'oak', 'cedar')) %>% 
              filter(!is.na(stdage.bin)) %>%
              group_by(INVYR, stdage.bin) %>% summarize(BA.dead = mean(basal_area.dead)), 
            mapping = aes(x = INVYR, y = BA.dead, color = stdage.bin, linetype = stdage.bin), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join.stdage %>% ungroup() %>% #filter(stdage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
                filter(!is.na(stdage.bin)) %>%
                group_by(INVYR, stdage.bin) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = INVYR, fill = stdage.bin, linetype = stdage.bin), alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), legend.position = c(0.25, 0.5), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  scale_color_brewer(name = "Stand\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Stand\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Stand\nAge") +
  # scale_fill_viridis_d("Stand\nAge", option = 'B') +
  # scale_color_viridis_d("Stand\nAge", option = 'B') +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p22

p23 <- ggplot() + #geom_line(data = join.stdage %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.stdage %>% ungroup() %>% #filter(stdage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(stdage.bin)) %>%
              group_by(INVYR, stdage.bin) %>% summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100, BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.dead.pct.mean, color = stdage.bin, linetype = stdage.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.stdage %>% ungroup() %>% #filter(stdage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                group_by(INVYR, stdage.bin) %>%
                summarize(BA.dead.pct.mean = mean(basal_area.dead.pct) * 100,
                          BA.dead.pct.sd = sd(basal_area.dead.pct) * 100, 
                          BA.n = n()),
              mapping = aes(ymin=BA.dead.pct.mean - 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            ymax=BA.dead.pct.mean + 1.96*(BA.dead.pct.sd / sqrt(BA.n)),
                            x = INVYR, fill = stdage.bin), alpha = 0.2) +
  scale_color_brewer(name = "Stand\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Stand\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Stand\nAge") +
  theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  # ylim(-2.5,20) +
  xlab('Year') + ylab('Mortality (%)')
p23

# join.stdage %>% filter(stdage <= 10) %>% count()
p24 <- ggplot() + #geom_line(data = join.stdage %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join.stdage %>% ungroup() %>% #filter(stdage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%  
              filter(!is.na(stdage.bin)) %>%
              group_by(INVYR, stdage.bin) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.all, color = stdage.bin, linetype = stdage.bin),  size = 1) +
  #The error bars
  geom_ribbon(data = join.stdage %>% ungroup() %>% #filter(stdage.bin %in% c('pine', 'fir', 'oak', 'cedar')) %>%   
                filter(!is.na(stdage.bin)) %>%
                group_by(INVYR, stdage.bin) %>%
                summarize(BA.mean = mean(basal_area.all),
                          BA.sd = sd(basal_area.all), BA.n = n()),
              mapping = aes(ymin=BA.mean - 1.96*(BA.sd / sqrt(BA.n)),
                            ymax=BA.mean + 1.96*(BA.sd / sqrt(BA.n)),
                            x = INVYR, fill = stdage.bin), alpha = 0.2) +
  scale_color_brewer(name = "Stand\nAge", palette = 'Dark2') +
  scale_fill_brewer(name = "Stand\nAge", palette = 'Dark2') +
  scale_linetype_discrete(name = "Stand\nAge") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')'))
p24

f12 <- ggarrange(p22, p23, p24, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f12

ggsave(filename = 'Fig15_pine_fir_byStandAge_FIA_mortality_time_series.png', height=18, width= 10, units = 'cm', dpi=900)

ggplot(data =  join %>% filter(tree_type %in% c('pine', 'fir')), mapping= aes(x = STDAGE, y = basal_area.dead.pct * 100, color = as.factor(FORTYPCD))) + geom_point() + geom_smooth(method = 'lm')
