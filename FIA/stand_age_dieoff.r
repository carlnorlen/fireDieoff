#Author: Carl Norlen
#Date Created: December 10, 2021
#Date Edited: June 16, 2022
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
t.plot, t.statuscd, t.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning, c.stdage, c.fldage, t.totage, t.bhage
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 --2 means forest
--AND t.statuscd = 1 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 -- additional where_clause from ref_pop_attribute table
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es') --South Sierra Nevada P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba' OR 
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70)
")

live <- dbFetch(q1, n = -1)
# dbDisconnect(db)
summary(live)

#Convert per acre to per hectare
live$count <- live$count * 2.47105 # Convert to per hectare
live$DIA <- live$DIA * (2.54) #Convert to centimeters
live$basal_area <- (((live$DIA / 2)^2) * pi)*(1/10000) * live$count

# live %>% filter(STDAGE > 0 & !is.na(STDAGE)) %>% select(STDAGE) %>% unique()
# live %>% filter(INVYR == 2001) %>% select(PLOT) %>% unique() %>% count()
# 
# live %>% filter(STATUSCD == 1) %>% group_by(INVYR) %>% summarize(Live.count = sum(count))
# 
# live %>% filter(STATUSCD == 2 & !is.na(MORTYR)) %>% group_by(INVYR) %>% summarize(Dead.count = sum(count))

#Region white counts of dead and live trees
f1<- ggplot() + geom_line(data = live %>% filter(STATUSCD == 1) %>% group_by(INVYR, PLOT) %>% summarize(Live.BA = sum(basal_area)) %>% ungroup() %>% group_by(INVYR) %>% summarize(Live = mean(Live.BA)), mapping = aes(x = INVYR, y = Live), color = 'green') + 
           geom_line(data = live %>% filter(STATUSCD == 2 & !is.na(MORTYR)) %>% group_by(MORTYR, PLOT) %>% summarize(Dead.BA = sum(basal_area)) %>% ungroup() %>% group_by(MORTYR) %>% summarize(Dead = mean(Dead.BA)), mapping = aes(x = MORTYR, y = Dead), color = 'red') +
           xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) + theme_bw()
f1

ggsave(filename = 'Fig1_mortality_time_series_FIA.png', height=6, width= 10, units = 'cm', dpi=900)

#Calculate the Quintiles of stand age
stdage.q <- as.data.frame(unname(quantile(live$STDAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# precip.q
colnames(stdage.q) <- 'STDAGE'
stdage.q$'Quintile' <- c(0.0, 0.2,0.4, 0.6, 0.8, 1.0)
# temp.q
stdage.q

#Bin data by Stand Age. Bins are quintiles.
live <- live %>% mutate(stdage.bin = case_when(
  STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '210+',
  STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '160-209',
  STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '125-159',
  STDAGE >= stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '95-124',
  STDAGE < stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '0-94'))

#Order the stand age bins
live$stdage.bin = with(live, factor(stdage.bin, levels = c('0-94','95-124','125-159','160-209','210+')))
# summary(live)

#Replace MORTYR that is NA with INVYR
live[is.na(live$MORTYR), ]$MORTYR <- live[is.na(live$MORTYR), ]$INVYR 
# live%>% dplyr::filter(stdage.bin == '138-154')
#Calculate the Quintiles of tree age (breast height age)
bhage.q <- as.data.frame(unname(quantile(live$BHAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# precip.q
colnames(bhage.q) <- 'BHAGE'
bhage.q$'Quintile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# temp.q
bhage.q
# live %>% filter(!is.na(BHAGE))
#Bin data by Stand Age. Bins are quintiles.
live <- live %>% filter(!is.na(BHAGE)) %>% mutate(bhage.bin = case_when(
  BHAGE >= bhage.q %>% filter(Quintile == 0.8) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '171+',
  BHAGE >= bhage.q %>% filter(Quintile == 0.6) %>% dplyr::select(BHAGE) %>% as.numeric() & 
    BHAGE < bhage.q %>% filter(Quintile == 0.8) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '110-170',
  BHAGE >= bhage.q %>% filter(Quintile == 0.4) %>% dplyr::select(BHAGE) %>% as.numeric() & 
    BHAGE < bhage.q %>% filter(Quintile == 0.6) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '78-109',
  BHAGE >= bhage.q %>% filter(Quintile == 0.2) %>% dplyr::select(BHAGE) %>% as.numeric()  & 
    BHAGE < bhage.q %>% filter(Quintile == 0.4) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '57-77',
  BHAGE < bhage.q %>% filter(Quintile == 0.2) %>% dplyr::select(BHAGE) %>% as.numeric() ~ '6-56'))

#Order the stand age bins
live$bhage.bin = with(live, factor(bhage.bin, levels = c('6-56','57-77','78-109','110-170','171+')))


#Stand Age Histogram with data binned by stand age
f2 <- ggplot() + geom_histogram(data = (live %>% filter(STDAGE > 0 & !is.na(STDAGE)) %>% select(STDAGE)), mapping = aes(x =STDAGE), bins = 60) +
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black') + 
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black') +
  theme_bw() + xlab('Stand Age') + ylab('Count')

ggsave(filename = 'Fig2_Stand_Age_Quintiles_historgram.png', height=6, width= 10, units = 'cm', dpi=900)

live %>% filter(!is.na(MORTYR)) %>% select(PLOT) %>% unique() %>% count()
test <- live %>% select(PLOT, INVYR) %>% group_by(PLOT, INVYR) %>% summarize(count = n())
total <- live %>% group_by(INVYR, PLOT) %>% summarize(count = n(), tpa.all = sum(count), basal_area.all = sum(basal_area), STDAGE = median(STDAGE))
total
#There is a slightly different result when using INVYR instead of MORTYR to calculate annual mortality
dead <- live %>% filter(STATUSCD == 2) %>% group_by(MORTYR, PLOT) %>% summarize(count.dead = n(), tpa.dead = sum(count), basal_area.dead = sum(basal_area))
dead <- dead %>% mutate(INVYR = MORTYR)
dead
join <- left_join(total, dead, by = c('PLOT', 'INVYR'))

#Replace the NAs with 0s
join <- join %>% dplyr::mutate(basal_area.dead = replace(basal_area.dead, is.na(basal_area.dead), 0), 
                               count.dead = replace(count.dead, is.na(count.dead), 0),
                               tpa.dead = replace(tpa.dead, is.na(tpa.dead), 0))


stdage.q <- as.data.frame(unname(quantile(join$STDAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# precip.q
colnames(stdage.q) <- 'STDAGE'
stdage.q$'Quintile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# temp.q
stdage.q

#Bin data by Stand Age. Bins are quintiles.
join <- join %>% mutate(stdage.bin = case_when(
  STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '210+',
  STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '160-209',
  STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '120-159',
  STDAGE >= stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '95-119',
  STDAGE < stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '0-94'))

#Order the stand age bins
join$stdage.bin = with(join, factor(stdage.bin, levels = c('0-94','95-119','120-159',
                                                           '160-209','210+')))
join %>% group_by(stdage.bin) %>% summarize(count = n())
# join %>% filter(is.na(stdage.bin))
# join
# # live.plot
# bhage.q <- as.data.frame(unname(quantile(live$BHAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# live
#Mortality binned by stand age
# live %>% filter(STATUSCD == 2 & !is.na(MORTYR) & !is.na(STDAGE)) %>% group_by(MORTYR, PLOT) %>% summarize(basal_area = sum(basal_area))
#For some reason Right now this is showing the youngest stands with the most die-off. However, the youngest stands are pretty old
f3<- ggplot() + #geom_line(data = live %>% filter(STATUSCD == 1  & !is.na(STDAGE)) %>% group_by(INVYR, stdage.bin) %>% summarize(Live.count = sum(count)), mapping = aes(x = INVYR, y = Live.count), color = 'green') + 
  geom_line(data = join %>% filter(!is.na(stdage.bin)) %>% group_by(INVYR, stdage.bin) %>% summarize(BA.dead = mean(basal_area.dead)), mapping = aes(x = INVYR, y = BA.dead, color = stdage.bin)) +
  xlab('Year') + ylab(expression('Mortality (m'^2*'ha'^-1*')')) + theme_bw()# + facet_wrap(~stdage.bin, ncol = 5)

f3
ggsave(filename = 'Fig3_stand_age_mortality_time_series_FIA.png', height=6, width= 15, units = 'cm', dpi=900)

#Mortality binned by tree age
f4<- ggplot() + #geom_line(data = live %>% filter(STATUSCD == 1  & !is.na(STDAGE)) %>% group_by(INVYR, bhage.bin) %>% summarize(Live.count = sum(count)), mapping = aes(x = INVYR, y = Live.count), color = 'green') + 
  geom_line(data = live %>% filter(STATUSCD == 2 & !is.na(MORTYR) & !is.na(STDAGE)) %>% group_by(MORTYR, bhage.bin) %>% summarize(Dead.count = sum(count)), mapping = aes(x = MORTYR, y = Dead.count), color = 'red') +
  xlab('Year') + ylab(expression('Mortality (trees ha'^-1*')')) + theme_bw() + facet_wrap(~bhage.bin, ncol = 5)

f4
ggsave(filename = 'Fig4_tree_age_mortality_time_series_FIA.png', height=6, width= 15, units = 'cm', dpi=900)
