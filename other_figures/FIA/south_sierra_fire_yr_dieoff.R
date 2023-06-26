#Author: Carl Norlen
#Date Created: August 4, 2021
#Date Edited: September 1, 2022
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
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Eo' ) --South Sierra Nevada OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Es'
--SoCal Mountains Ecoregions
--AND(P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba')
--Combined South Sierra and SoCal Mountains
--AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es' OR P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba')
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70 OR c.dstrbcd1 = 30 OR c.dstrbcd1 = 31 OR c.dstrbcd1 = 32)
")
#DSTRBCD1 == 30, 31, 32 reference fires
live <- dbFetch(q1, n = -1)
# dbDisconnect(db)
summary(live)

#Convert per acre to per hectare
live$count <- live$count * 2.47105 # Convert to per hectare
live$DIA <- live$DIA * (2.54) #Convert to centimeters
live$basal_area <- (((live$DIA / 2)^2) * pi)*(1/10000) * live$count

#Get the live and dead values of the FIA data
# live %>% filter(!is.na(MORTYR)) %>% select(PLOT) %>% unique() %>% count()
# test <- live %>% select(PLOT, INVYR) %>% group_by(PLOT, INVYR) %>% summarize(count = n())
total <- live %>% group_by(INVYR, PLOT) %>% summarize(count = n(), tpa.all = sum(count), basal_area.all = sum(basal_area), STDAGE = median(STDAGE), 
                                                      DSTRBCD1 = median(DSTRBCD1), DSTRBYR1 = median(DSTRBYR1), OWNGRPCD = median(OWNGRPCD))
total
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
#Use my MORTYR stuff from before to fix this.
dead <- live %>% filter(STATUSCD == 2 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019")) %>% group_by(PLOT, INVYR) %>% summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
# dead <- dead %>% mutate(INVYR = MORTYR)
dead
join <- left_join(total, dead, by = c('PLOT', 'INVYR'))

#Replace the NAs with 0s
join <- join %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0))

join

#Test out the basal area
fa <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join  %>% # filter(DSTRBYR1 >= 2012 | is.na(DSTRBYR1)) %>%
              group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), mapping = aes(x = INVYR, y = BA.all), color = 'black', size = 1) +
  #The error bars
  geom_ribbon(data = join %>% 
                group_by(INVYR) %>%
                summarize(BA.mean = mean(basal_area.all),
                          BA.sd = sd(basal_area.all), BA.n = n()),
              mapping = aes(ymin=BA.mean - 1.96*(BA.sd / sqrt(BA.n)),
                            ymax=BA.mean + 1.96*(BA.sd / sqrt(BA.n)),
                            x = INVYR), alpha = 0.3) + #ylim(0, 45) +
  
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) + theme_bw()
fa


join$years.disturb <- join$INVYR - join$DSTRBYR1

join <- join %>% mutate(disturb.bin = case_when(
  DSTRBCD1 %in% c(10, 11, 12, 54, 70) & DSTRBYR1 > 2012 ~ 'Die-off',
  DSTRBCD1 %in% c(10, 11, 12, 54, 70) & DSTRBYR1 <= 2012 ~ 'Past Die-off', 
  DSTRBCD1 %in% c(30,31,32) & DSTRBYR1 > 2012 ~ 'Fire',
  DSTRBCD1 %in% c(30,31,32) & DSTRBYR1 <= 2012 ~ 'Past Fire',
  DSTRBCD1 == 0 ~'No Disturbance'))

#Ownership Bins
join <- join %>% mutate(owncd.bin = case_when(
  OWNGRPCD == 10 ~ 'USFS',
  OWNGRPCD == 20 ~ 'Other Federal', 
  OWNGRPCD == 30 ~ 'State & Local',
  OWNGRPCD == 40 ~ 'Private'))

# summary(join %>% filter(disturb.bin == 'Die-off'))
# join %>% filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999)
#Region white counts of dead and live trees
p1<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire', 'No Disturbance') & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>%  
              group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), mapping = aes(x = disturb.bin, y = BA.all), color = 'black', size = 1) +
  #95% CI Die-off
  geom_errorbar(data = join %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire', 'No Disturbance') & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
                  group_by(disturb.bin) %>%
                  summarize(BA.all = mean(basal_area.all),
                            BA.all.sd = sd(basal_area.all), BA.n = n()),
                mapping = aes(y = BA.all, ymin=BA.all - 1.96*(BA.all.sd / sqrt(BA.n)),
                              ymax=BA.all + 1.96*(BA.all.sd / sqrt(BA.n)),
                              x = disturb.bin)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Disturbance Type') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) 
p1

join %>%
  filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire', 'No Disturbance') & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
  group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), count = n())

p2<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire', 'No Disturbance') & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
              group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), count = n()), 
            mapping = aes(x = disturb.bin, y = count), color = 'black', size = 1) +
  #95% CI Die-off
  # geom_ribbon(data = join %>% filter(!is.na(stdage.bin)) %>% 
  #               group_by(INVYR) %>%
  #               summarize(BA.dead = mean(basal_area.dead),
  #                         BA.dead.sd = sd(basal_area.dead), BA.n = n()),
  #             mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
  #                           ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
  #                           x = INVYR), alpha = 0.3) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Disturbance Type') + ylab('# Plots') 
p2

p3<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire', 'No Disturbance') & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
               group_by(disturb.bin) %>% 
               summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), 
             mapping = aes(x = disturb.bin, y = BA.dead), color = 'black', size = 1) +
  #95% CI Die-off
  geom_errorbar(data = join %>%
                filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire', 'No Disturbance') & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
  group_by(disturb.bin) %>%
                summarize(BA.dead = mean(basal_area.dead),
                          BA.dead.sd = sd(basal_area.dead), BA.n = n()),
              mapping = aes(y = BA.dead, ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                            x = disturb.bin)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Disturbance Type') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p3

# join %>% 
#   filter((!is.na(STDAGE) & STDAGE != 9999) ) %>% 
#   # filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999 & STDAGE != 9999) %>% 
#   group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE.mean = mean(STDAGE), count = n())

p4<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire', 'No Disturbance') & INVYR %in% c("2015", "2016", "2017", "2018", "2019") & (!is.na(STDAGE) & STDAGE != 9999)) %>% # & INVYR %in% c("2015", "2016", "2017", "2018", "2019") ) %>% 
              group_by(disturb.bin) %>% 
               summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE.mean = mean(STDAGE)), 
             mapping = aes(x = disturb.bin, y = STDAGE.mean), color = 'black', size = 1) +
  #95% CI Error bar
  geom_errorbar(data = join %>%
                  filter(!is.na(disturb.bin) & disturb.bin %in% c('Die-off', 'Fire', 'No Disturbance') & INVYR %in% c("2015", "2016", "2017", "2018", "2019") & (!is.na(STDAGE) & STDAGE != 9999)) %>%
                  group_by(disturb.bin) %>%
                  summarize(STDAGE.mean = mean(STDAGE),
                            STDAGE.mean.sd = sd(STDAGE), BA.n = n()),
                mapping = aes(y = STDAGE.mean, ymin=STDAGE.mean - 1.96*(STDAGE.mean.sd / sqrt(BA.n)),
                              ymax=STDAGE.mean + 1.96*(STDAGE.mean.sd / sqrt(BA.n)),
                              x = disturb.bin)) +
  theme_bw() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Past Disturbance Type') + ylab('Average Tree Age') 
p4

f1 <- ggarrange(p1, p3, p4, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'c)', 'd)'))
f1

ggsave(filename = 'Fig8_FIA_fire_recovery_exploration_wo_high_elev.png', height=15, width= 10, units = 'cm', dpi=900)

#Do Plot by Ownership Type
p5<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%  
               group_by(owncd.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), mapping = aes(x = owncd.bin, y = BA.all), color = 'black', size = 1) +
  #95% CI Die-off
  geom_errorbar(data = join %>%
                  filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%
                  group_by(owncd.bin) %>%
                  summarize(BA.all = mean(basal_area.all),
                            BA.all.sd = sd(basal_area.all), BA.n = n()),
                mapping = aes(y = BA.all, ymin=BA.all - 1.96*(BA.all.sd / sqrt(BA.n)),
                              ymax=BA.all + 1.96*(BA.all.sd / sqrt(BA.n)),
                              x = owncd.bin)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Ownership') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) 
p5

join %>%
  filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%
  group_by(owncd.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), count = n())

# p6<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
#   #Mean Die-off
#   geom_point(data = join %>% 
#                filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70)) %>% 
#                group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), count = n()), 
#              mapping = aes(x = disturb.bin, y = count), color = 'black', size = 1) +
#   #95% CI Die-off
#   # geom_ribbon(data = join %>% filter(!is.na(stdage.bin)) %>% 
#   #               group_by(INVYR) %>%
#   #               summarize(BA.dead = mean(basal_area.dead),
#   #                         BA.dead.sd = sd(basal_area.dead), BA.n = n()),
#   #             mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
#   #                           ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
#   #                           x = INVYR), alpha = 0.3) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
#   xlab('Disturbance Type') + ylab('# Plots') 
# f2

p6 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>% 
               group_by(owncd.bin) %>% 
               summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), 
             mapping = aes(x = owncd.bin, y = BA.dead), color = 'black', size = 1) +
  #95% CI Die-off
  geom_errorbar(data = join %>%
                  filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%
                  group_by(owncd.bin) %>%
                  summarize(BA.dead = mean(basal_area.dead),
                            BA.dead.sd = sd(basal_area.dead), BA.n = n()),
                mapping = aes(y = BA.dead, ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                              ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
                              x = owncd.bin)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Ownership') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
p6

# join %>% 
#   filter((!is.na(STDAGE) & STDAGE != 9999) ) %>% 
#   # filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999 & STDAGE != 9999) %>% 
#   group_by(disturb.bin) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE.mean = mean(STDAGE), count = n())

p7 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% 
               filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>% 
               group_by(owncd.bin) %>% 
               summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE.mean = mean(STDAGE)), 
             mapping = aes(x = owncd.bin, y = STDAGE.mean), color = 'black', size = 1) +
  #95% CI Error bar
  geom_errorbar(data = join %>%
                  filter(INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & (!is.na(STDAGE) & STDAGE != 9999)) %>%
                  group_by(owncd.bin) %>%
                  summarize(STDAGE.mean = mean(STDAGE),
                            STDAGE.mean.sd = sd(STDAGE), BA.n = n()),
                mapping = aes(y = STDAGE.mean, ymin=STDAGE.mean - 1.96*(STDAGE.mean.sd / sqrt(BA.n)),
                              ymax=STDAGE.mean + 1.96*(STDAGE.mean.sd / sqrt(BA.n)),
                              x = owncd.bin)) +
  theme_bw() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Ownership') + ylab('Average Tree Age') 
p7

f2 <- ggarrange(p5, p6, p7, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'c)', 'd)'))
f2

ggsave(filename = 'Fig9_FIA_ownership_exploration_south_sierra.png', height=15, width= 10, units = 'cm', dpi=900)
