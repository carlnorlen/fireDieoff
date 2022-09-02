#Author: Carl Norlen
#Date Created: December 10, 2021
#Date Edited: August 29, 2022
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
setwd('C:/Users/can02/mystuff/fireDieoff/FIA')

#Add Data Sets
sql_dir <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Download from FIA DataMart
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
c.fldtypcd, rft.meaning, c.stdage, c.fldage, t.totage, t.bhage,
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
AND rft.value = c.fortypcd
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
all  %>% summary()

# all$tree_type <- recode(.x=all$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'juniper', 'California live oak' = 'oak', 'California sycamore' = 'deciduous', 'Coulter pine' = 'pine', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'pine',
#                                'bigcone Douglas-fir' = 'fir', 'bigleaf maple' = 'deciduous', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'deciduous', 'incense-cedar' = 'cedar', 'interior live oak' = 'oak', 'limber pine' = 'pine',
#                                'lodgepole pine' = 'pine', 'ponderosa pine' = 'pine', 'singleleaf pinyon' = 'pine', 'sugar pine' = 'pine', 'Utah juniper' = 'juniper', 'western juniper' = 'juniper', 'white alder' = 'deciduous', 'white fir' = 'fir', 'California laurel' = 'deciduous',
#                                'California-laurel' = 'deciduous', 'Oregon ash' = 'deciduous', 'Douglas-fir' = 'fir', 'honey mesquite' = 'deciduous', 'desert ironwood' = 'deciduous', 'California red fir' = 'fir', 'California buckeye' = 'deciduous', 'Engelmann oak' = 'oak', 'grand fir' = 'fir', 'western white pine' = 'pine',
#                                "western white pine" = 'pine', "whitebark pine" = 'pine', "mountain hemlock" = "other conifer", "gray or California foothill pine" = "pine", "foxtail pine" = 'pine', "blue oak" = 'oak', "California white oak" = 'oak', "quaking aspen" = 'deciduous',
#                                "giant sequoia" = 'other conifer', "Unknown dead conifer" = 'other conifer', "ash spp." = 'deciduous', "black cottonwood" = 'deciduous', "California torreya (nutmeg)" = 'deciduous', "Oregon white oak" = 'oak', "Port-Orford-cedar" = 'cedar', "Pacific dogwood" = 'deciduous',
#                                "red alder" = 'deciduous', "bitter cherry" = 'deciduous', 'Rocky Mountain maple' = 'deciduous', 'unknown dead conifer' = 'other conifer')
# all$tree_type <- as.factor(all$tree_type)
# test <- all %>% select(PLOT, INVYR) %>% group_by(PLOT, INVYR) %>% summarize(count = n())
live <- all %>% filter(STATUSCD == 1) %>% group_by(INVYR, PLOT, .drop = FALSE) %>% summarize(count.live = n(), tpa.live = sum(count), basal_area.live = sum(basal_area), STDAGE = median(STDAGE), FORTYPCD = median(FORTYPCD), MEANING = first(MEANING))
live
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead <- all %>% filter(STATUSCD == 2 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")) %>% group_by(PLOT, INVYR, .drop = FALSE) %>% summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
# dead <- dead %>% mutate(INVYR = MORTYR) & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")
dead
join <- left_join(live, dead, by = c('PLOT', 'INVYR'))
summary(join)
#Replace the NAs with 0s
join <- join %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0)
                               ) %>% group_by(INVYR, PLOT) %>%
                 fill(STDAGE, .direction = c("updown")) %>%
                 fill(FORTYPCD, .direction = c("updown")) %>%
                 fill(MEANING, .direction = c("updown"))
join
summary(join)
forest.type <- join %>% group_by(MEANING) %>% count()
join %>% ungroup() %>% count()

#Add the total basal area calculations
join$count.all <- join$count.live + join$count.dead
join$tpa.all <- join$tpa.live + join$tpa.dead
join$basal_area.all <- join$basal_area.live + join$basal_area.dead


# summary(join)
#Region wide counts of dead and live trees
f1<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & 
                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>% 
              group_by(INVYR, tree_type) %>% summarize(BA.dead = mean(basal_area.dead)), mapping = aes(x = INVYR, y = BA.dead, color = tree_type), size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & 
                                       MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%  
                group_by(INVYR, tree_type) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = INVYR, fill = tree_type), alpha = 0.3) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) 
f1

f2 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  geom_line(data = join  %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine')& 
                                      MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%   
              group_by(INVYR, tree_type) %>% summarize(BA.dead = mean(basal_area.dead), BA.n = n()), mapping = aes(x = INVYR, y = BA.n, color = tree_type), size = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab('# Plots') #+ ylim(0, 60) 
f2
# join %>% filter(STDAGE <= 10) %>% count()


f3 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join  %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & 
                                      MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%  
  group_by(INVYR, tree_type) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), mapping = aes(x = INVYR, y = BA.all, color = tree_type),  size = 1) +
  #The error bars
  geom_ribbon(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & 
                                       MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%  
                group_by(INVYR, tree_type) %>%
                summarize(BA.mean = mean(basal_area.all),
                          BA.sd = sd(basal_area.all), BA.n = n()),
              mapping = aes(ymin=BA.mean - 1.96*(BA.sd / sqrt(BA.n)),
                            ymax=BA.mean + 1.96*(BA.sd / sqrt(BA.n)),
                            x = INVYR, fill = tree_type), alpha = 0.3) +
  
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) + theme_bw()
f3

fig1 <- ggarrange(f1, f2, f3, ncol = 1, nrow = 3, common.legend = TRUE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
fig1

ggsave(filename = 'Fig9_mortality_time_series_FIA_bySpecies.png', height=18, width= 10, units = 'cm', dpi=900)


#Scatter plots versus stand age
p4 <- ggplot() + geom_point(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & 
                                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%   
                              filter(INVYR %in% c(2015,2016,2017,2018,2019) & STDAGE <= 250 & STDAGE > 0), mapping = aes(x = STDAGE, y = basal_area.all, color = tree_type)) +
      theme_bw()
p4

p5 <- ggplot() + geom_point(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & 
                                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%   
                              filter(INVYR %in% c(2015,2016,2017,2018,2019) & STDAGE <= 250 & STDAGE > 0), mapping = aes(x = STDAGE, y = tpa.all, color = tree_type)) +
      theme_bw()
p5

p6 <- ggplot() + geom_point(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & 
                                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%   
                              filter(INVYR %in% c(2015,2016,2017,2018,2019) & STDAGE <= 250 & STDAGE > 0), mapping = aes(x = STDAGE, y = basal_area.dead, color = tree_type)) +
  theme_bw() #+ geom_smooth(data = join %>% filter(INVYR %in% c(2013,2014,2015,2016,2017,2018,2019)), mapping = aes(x = STDAGE, y = basal_area.dead), method = 'lm')
p6

p7 <- ggplot() + geom_point(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & 
                                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>% 
                              filter(INVYR %in% c(2015,2016,2017,2018,2019) & STDAGE > 0), mapping = aes(x = basal_area.all, y = basal_area.dead, color = tree_type)) +
  theme_bw()
p7

fig2 <- ggarrange(p4, p5, p6, p7, ncol = 2, nrow = 2, common.legend = TRUE, align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
fig2

ggsave(filename = 'Fig10_mortality_standage_FIA.png', height=18, width= 18, units = 'cm', dpi=900)

# join %>% filter(INVYR %in% c(2013,2014,2015,2016,2017,2018,2019) & !is.na(STDAGE))
# join$stdage.bin <- as.factor(join$stdage.bin)

#Calculate the Quintiles of stand age
std.q <- as.data.frame(unname(quantile((join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0 &
                                                          MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>% distinct(PLOT, .keep_all = TRUE))$STDAGE, prob = seq(0,1, 1/20), type = 3, na.rm = TRUE)))
# precip.q
colnames(std.q) <- 'STDAGE'
std.q$'Quintile' <- c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0)
# temp.q
std.q

#Bin data by Stand Age. Bins are quintiles.
join <- join %>% mutate(std.bin = case_when(
  STDAGE >= std.q %>% filter(Quintile == 0.9) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '265+',
  STDAGE >= std.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() &
  STDAGE < std.q %>% filter(Quintile == 0.9) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '235-264',
  STDAGE >= std.q %>% filter(Quintile == 0.7) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '190-234',
  STDAGE >= std.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.7) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '165-189',
  STDAGE >= std.q %>% filter(Quintile == 0.5) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '152-164',
  STDAGE >= std.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.5) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '130-151',
  STDAGE >= std.q %>% filter(Quintile == 0.3) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '110-129',
  STDAGE >= std.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.3) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '100-109',
  STDAGE >= std.q %>% filter(Quintile == 0.1) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '85-99',
  STDAGE >= std.q %>% filter(Quintile == 0.05) %>% dplyr::select(STDAGE) %>% as.numeric() &
    STDAGE < std.q %>% filter(Quintile == 0.1) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '50-84',
  STDAGE < std.q %>% filter(Quintile == 0.05) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '0-49'))

#Order the stand age bins
join$std.bin = with(join, factor(std.bin, levels = c('0-49', '50-84', '85-99', '100-109', '110-129', '130-151',  '152-164', '165-189','190-234', '235-264','265+')))
# summary(live)
# MORTYR %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019) & 
p10 <- ggplot(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0 &
                                       MEANING %in% c( 'Canyon live oak','California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')), 
              mapping = aes(x = std.bin, y = basal_area.dead, color = tree_type)) +
       geom_point(stat = 'summary', position = position_dodge(width = .5)) + geom_errorbar(stat = 'summary', position = position_dodge(width = .5)) + 
  # scale_color_brewer(name = 'Average Tree Age Bins')
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Average Tree Age Bins') + ylab(expression('Mortality (m'^2*' ha'^-1*')'))
p10

p11 <- ggplot(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0 &
                                       MEANING %in% c('Canyon live oak','California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')),  
              mapping = aes(x = std.bin, y = basal_area.all, color = tree_type)) +
  geom_point(stat = 'summary', position = position_dodge(width = .5)) + geom_errorbar(stat = 'summary', position = position_dodge(width = .5)) + 
  # scale_color_brewer(name = 'Average Tree Age Bins')
  theme_bw() + xlab('Average Tree Age Bins') + ylab(expression('Basal Area (m'^2*' ha'^-1*')'))
p11

fig3 <- ggarrange(p10, p11, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
fig3

ggsave(filename = 'Fig11_species_mortality_standage_pointrange_FIA.png', height=18, width= 22, units = 'cm', dpi=900)

#Tree Density Version
p12 <- ggplot(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0 &
                                       MEANING %in% c('Canyon live oak','California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')),  
              mapping = aes(x = std.bin, y = tpa.dead, color = tree_type)) +
  geom_point(stat = 'summary', position = position_dodge(width = .5)) + geom_errorbar(stat = 'summary', position = position_dodge(width = .5)) + 
  # scale_color_brewer(name = 'Average Tree Age Bins')
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Average Tree Age Bins') + ylab(expression('Mortality (trees ha'^-1*')'))
p12

p13 <- ggplot(data = join %>% filter(tree_type %in% c('cedar', 'fir', 'oak', 'juniper', 'pine') & INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0 &
                                       MEANING %in% c('Canyon live oak','California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')),  
              mapping = aes(x = std.bin, y = tpa.all, color = tree_type)) +
  geom_point(stat = 'summary', position = position_dodge(width = .5)) + geom_errorbar(stat = 'summary', position = position_dodge(width = .5)) + 
  # scale_color_brewer(name = 'Average Tree Age Bins')
  theme_bw() + xlab('Average Tree Age Bins') + ylab(expression('Tree Density (tress ha'^-1*')'))
p13

fig4 <- ggarrange(p12, p13, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
fig4

ggsave(filename = 'Fig12_species_mortality_standage_pointrange_FIA.png', height=18, width= 22, units = 'cm', dpi=900)


