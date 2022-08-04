#Author: Carl Norlen
#Date Created: August 4, 2021
#Date Edited: August 4, 2022
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
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es') --South Sierra Nevada P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba' OR 
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

#Calculate the Quintiles of stand age
# stdage.q <- as.data.frame(unname(quantile(live$STDAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# # precip.q
# colnames(stdage.q) <- 'STDAGE'
# stdage.q$'Quintile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# # temp.q
# stdage.q
# 
# #Bin data by Stand Age. Bins are quintiles.
# live <- live %>% mutate(stdage.bin = case_when(
#   STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '210+',
#   STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
#     STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '160-209',
#   STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
#     STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '120-159',
#   STDAGE >= stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
#     STDAGE < stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '93-119',
#   STDAGE < stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '0-92'))
# 
# #Order the stand age bins
# live$stdage.bin = with(live, factor(stdage.bin, levels = c('0-92','93-119','120-159','160-209','210+')))
# # summary(live)
# 
# #Replace MORTYR that is NA with INVYR
# live[is.na(live$MORTYR), ]$MORTYR <- live[is.na(live$MORTYR), ]$INVYR 
# # live%>% dplyr::filter(stdage.bin == '138-154')
# #Calculate the Quintiles of tree age (breast height age)
# bhage.q <- as.data.frame(unname(quantile(live$BHAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# # precip.q
# colnames(bhage.q) <- 'BHAGE'
# bhage.q$'Quintile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# # temp.q
# bhage.q
# # live %>% filter(!is.na(BHAGE))
# #Bin data by Stand Age. Bins are quintiles.
# live <- live %>% filter(!is.na(BHAGE)) %>% mutate(bhage.bin = case_when(
#   BHAGE >= bhage.q %>% filter(Quintile == 0.8) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '171+',
#   BHAGE >= bhage.q %>% filter(Quintile == 0.6) %>% dplyr::select(BHAGE) %>% as.numeric() & 
#     BHAGE < bhage.q %>% filter(Quintile == 0.8) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '110-170',
#   BHAGE >= bhage.q %>% filter(Quintile == 0.4) %>% dplyr::select(BHAGE) %>% as.numeric() & 
#     BHAGE < bhage.q %>% filter(Quintile == 0.6) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '78-109',
#   BHAGE >= bhage.q %>% filter(Quintile == 0.2) %>% dplyr::select(BHAGE) %>% as.numeric()  & 
#     BHAGE < bhage.q %>% filter(Quintile == 0.4) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '57-77',
#   BHAGE < bhage.q %>% filter(Quintile == 0.2) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '6-56'))
# 
# #Order the stand age bins
# live$bhage.bin = with(live, factor(bhage.bin, levels = c('6-56','57-77','78-109','110-170','171+')))

#Get the live and dead values of the FIA data
# live %>% filter(!is.na(MORTYR)) %>% select(PLOT) %>% unique() %>% count()
# test <- live %>% select(PLOT, INVYR) %>% group_by(PLOT, INVYR) %>% summarize(count = n())
total <- live %>% group_by(INVYR, PLOT) %>% summarize(count = n(), tpa.all = sum(count), basal_area.all = sum(basal_area), STDAGE = median(STDAGE), DSTRBCD1 = median(DSTRBCD1), DSTRBYR1 = median(DSTRBYR1))
total
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead <- live %>% filter(STATUSCD == 2) %>% group_by(PLOT, INVYR) %>% summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
# dead <- dead %>% mutate(INVYR = MORTYR)
dead
join <- left_join(total, dead, by = c('PLOT', 'INVYR'))

#Replace the NAs with 0s
join <- join %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0))

join
# stdage.q <- as.data.frame(unname(quantile(join$STDAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# # precip.q
# colnames(stdage.q) <- 'STDAGE'
# stdage.q$'Quintile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# # temp.q
# stdage.q

#Bin data by Stand Age. Bins are quintiles.
# join <- join %>% mutate(stdage.bin = case_when(
#   STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '210+',
#   STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
#     STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '160-209',
#   STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
#     STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '120-159',
#   STDAGE >= stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
#     STDAGE < stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '93-119',
#   STDAGE < stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '0-92'))
# join
# #Order the stand age bins
# join$stdage.bin = with(join, factor(stdage.bin, levels = c('0-92','93-119','120-159',
#                                                            '160-209','210+')))
# join %>% group_by(stdage.bin) %>% summarize(basal_area = mean(basal_area.dead), count = n())
# join %>% filter(is.na(stdage.bin))
# join
# # live.plot
# bhage.q <- as.data.frame(unname(quantile(live$BHAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# live
#Mortality binned by stand age
#Add a new year column
# join <- join %>% mutate(year = case_when(is.na(MORTYR) ~ INVYR, !is.na(MORTYR) ~ MORTYR))

# join <- join %>% mutate(age.bin = case_when(
#   STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '210+',
#   STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
#     STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '160-209',
#   STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
#     STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '120-159',
#   STDAGE < 120 & STDAGE >= 93 ~ '93-119',
#   STDAGE >= 30 & STDAGE < 93 ~ '30-92',
#   STDAGE < 29 ~ '0-29'))
# 
# #Order the stand age bins
# join$age.bin = with(join, factor(age.bin, levels = c('0-29', '30-92', '93-119','120-159',
#                                                            '160-209','210+')))
# summary(join)

join$years.fire <- join$INVYR - join$DSTRBYR1
join %>% filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999)
#Region white counts of dead and live trees
f1<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join %>% filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999) %>% group_by(years.fire) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), mapping = aes(x = years.fire, y = BA.all), color = 'black', size = 1) +
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
  xlab('Years Since Fire') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) 
f1

f2<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join %>% filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999) %>% group_by(years.fire) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), count = n()), 
            mapping = aes(x = years.fire, y = count), color = 'black', size = 1) +
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
  xlab('Years Since Fire') + ylab('# Plots') 
f2

f3<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_point(data = join %>% filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999) %>% group_by(years.fire) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead)), 
             mapping = aes(x = years.fire, y = BA.dead), color = 'black', size = 1) +
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
  xlab('Years Since Fire') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) 
f3

f4<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join %>% filter((DSTRBCD1 == 30 | DSTRBCD1 == 31 | DSTRBCD1 == 32) & !is.na(DSTRBYR1) & DSTRBYR1 != 9999 & STDAGE != 9999) %>% group_by(years.fire) %>% summarize(BA.all = mean(basal_area.all), BA.dead = mean(basal_area.dead), STDAGE = mean(STDAGE)), 
             mapping = aes(x = years.fire, y = STDAGE), color = 'black', size = 1) +
  #95% CI Die-off
  # geom_ribbon(data = join %>% filter(!is.na(stdage.bin)) %>% 
  #               group_by(INVYR) %>%
  #               summarize(BA.dead = mean(basal_area.dead),
  #                         BA.dead.sd = sd(basal_area.dead), BA.n = n()),
  #             mapping = aes(ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
  #                           ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)),
  #                           x = INVYR), alpha = 0.3) +
  theme_bw() +
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Years Since Fire') + ylab('Average Tree Age') 
f4

f1 <- ggarrange(f1, f2, f3, f4, ncol = 1, nrow = 4, common.legend = FALSE, heights = c(0.9, 0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
f1

ggsave(filename = 'Fig8_FIA_fire_recovery_exploration.png', height=18, width= 10, units = 'cm', dpi=900)
