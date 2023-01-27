#Author: Carl Norlen
#Date Created: December 10, 2021
#Date Edited: January 23, 2023
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
setwd('C:/Users/can02/mystuff/fireDieoff/final_figures/landsat')

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
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M261Es' OR P.ECOSUBCD = 'M261Er') --South Sierra Nevada 
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

#combined the data together
live <- all %>% filter(STATUSCD == 1) %>% group_by(INVYR, PLOT) %>% summarize(count.live = n(), tpa.live = sum(count), basal_area.live = sum(basal_area), STDAGE = median(STDAGE), FORTYPCD = median(FORTYPCD), MEANING = first(MEANING))
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


stdage.q <- as.data.frame(unname(quantile((join %>% filter(INVYR %in% c(2015, 2016, 2017, 2108, 2019)))$STDAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# precip.q
colnames(stdage.q) <- 'STDAGE'
stdage.q$'Quintile' <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# temp.q
stdage.q

#Bin data by Stand Age. Bins are quintiles.
join <- join %>% mutate(stdage.bin = case_when(
  STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '210+',
  STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() &
    STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '165-209',
  STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '133-164',
  STDAGE >= stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '105-132',
  STDAGE < stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '0-104'))
join
#Order the stand age bins
join$stdage.bin = with(join, factor(stdage.bin, levels = c('0-104','105-132','133-164', '165-209', '210+')))

join %>% summary()
# join %>% group_by(stdage.bin) %>% summarize(basal_area = mean(basal_area.dead), count = n())
# join %>% filter(is.na(stdage.bin))
# join
# # live.plot
# bhage.q <- as.data.frame(unname(quantile(live$BHAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# live
#Mortality binned by stand age
#Add a new year column
# join <- join %>% mutate(year = case_when(is.na(MORTYR) ~ INVYR, !is.na(MORTYR) ~ MORTYR))

join <- join %>% mutate(age.bin = case_when(
  STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '210+',
  STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '160-209',
  STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '120-159',
  STDAGE < 120 & STDAGE >= 93 ~ '93-119',
  STDAGE >= 30 & STDAGE < 93 ~ '30-92',
  STDAGE < 29 ~ '0-29'))

#Order the stand age bins
join$age.bin = with(join, factor(age.bin, levels = c('0-29', '30-92', '93-119','120-159',
                                                           '160-209','210+')))
summary(join)
#Region wide counts of dead and live trees
p1<- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #Mean Die-off
  geom_line(data = join %>% #filter(MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%
              group_by(INVYR) %>% summarize(BA.dead = mean(basal_area.dead)), mapping = aes(x = INVYR, y = BA.dead), color = 'black', size = 1) +
  #95% CI Die-off
  geom_ribbon(data = join %>% 
                #filter( MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%
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
  geom_line(data = join  %>% 
              #filter( MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%
              group_by(INVYR) %>% summarize(BA.dead = mean(basal_area.dead), BA.n = n()), mapping = aes(x = INVYR, y = BA.n), color = 'black', size = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Year') + ylab('# Plots') #+ ylim(0, 60) 
p2
# join %>% filter(STDAGE <= 10) %>% count()
p3 <- ggplot() + #geom_line(data = join %>% group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all)), mapping = aes(x = INVYR, y = BA.all), color = 'green') + 
  #The data mean
  geom_line(data = join  %>% #filter( MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%
  group_by(INVYR) %>% summarize(BA.all = mean(basal_area.all), BA.n = n()), mapping = aes(x = INVYR, y = BA.all), color = 'black', size = 1) +
  #The error bars
  geom_ribbon(data = join %>% 
                #filter( MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>%
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

ggsave(filename = 'Fig2_mortality_time_series_FIA_conifer_forest.png', height=18, width= 10, units = 'cm', dpi=900)

#Stand Age Histogram with data binned by stand age
f3 <- ggplot() + geom_histogram(data = (join %>% filter(!is.na(STDAGE) & MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>% select(STDAGE)), 
                                mapping = aes(x =STDAGE), bins = 60) +
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black') + 
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black') +
  geom_vline(xintercept = (stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric()), color = 'black') +
  theme_bw() + xlab('Stand Age') + ylab('Count')
f3
ggsave(filename = 'Fig3_Stand_Age_Quintiles_historgram_forest_type.png', height=6, width= 10, units = 'cm', dpi=900)

#For some reason Right now this is showing the youngest stands with the most die-off. However, the youngest stands are pretty old
p4<- ggplot() + 
  #Do the mean line
  geom_line(data = join %>% filter(!is.na(stdage.bin)) %>% 
                            group_by(INVYR, stdage.bin) %>% 
                            summarize(BA.dead = mean(basal_area.dead), BA.n = n()), 
                            mapping = aes(x = INVYR, y = BA.dead, color = stdage.bin, linetype = stdage.bin), size = 1) +
  #Do the 95% CI Ribbon
  geom_ribbon(data = join %>% filter(!is.na(stdage.bin)) %>% 
                group_by(INVYR, stdage.bin) %>% 
                summarize(BA.dead = mean(basal_area.dead), BA.n = n(), BA.dead.sd = sd(basal_area.dead)), 
              mapping = aes(x = INVYR, ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
                            ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)), fill = stdage.bin), alpha = 0.3) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) + theme_bw()
p4

p5<- ggplot() + 
  #Do the mean line
  geom_line(data = join %>% filter(!is.na(stdage.bin)) %>% 
              group_by(INVYR, stdage.bin) %>% 
              summarize(BA.dead = mean(basal_area.dead), BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.n, color = stdage.bin, linetype = stdage.bin), size = 1) +
  #Do the 95% CI Ribbon
  # geom_ribbon(data = join %>% filter(!is.na(stdage.bin)) %>% 
  #               group_by(INVYR, stdage.bin) %>% 
  #               summarize(BA.dead = mean(basal_area.dead), BA.n = n(), BA.dead.sd = sd(basal_area.dead)), 
  #             mapping = aes(x = INVYR, ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
  #                           ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)), fill = stdage.bin), alpha = 0.3) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) + theme_bw()
p5

p6<- ggplot() + 
  #Do the mean line
  geom_line(data = join %>% filter(!is.na(stdage.bin)) %>% 
              group_by(INVYR, stdage.bin) %>% 
              summarize(BA.all = mean(basal_area.all), BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.all, color = stdage.bin, linetype = stdage.bin), size = 1) +
  #Do the 95% CI Ribbon
  geom_ribbon(data = join %>% filter(!is.na(stdage.bin)) %>% 
                group_by(INVYR, stdage.bin) %>% 
                summarize(BA.all = mean(basal_area.all), BA.n = n(), BA.all.sd = sd(basal_area.all)), 
              mapping = aes(x = INVYR, ymin=BA.all - 1.96*(BA.all.sd / sqrt(BA.n)),
                            ymax=BA.all + 1.96*(BA.all.sd / sqrt(BA.n)), fill = stdage.bin), alpha = 0.3) +
  xlab('Year') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) + theme_bw()
p6

f4 <- ggarrange(p4, p5, p6, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9, 1), align = "v", labels = c('a)', 'b)', 'c)'))
f4

ggsave(filename = 'Fig4_stand_age_mortality_time_series_FIA.png', height=6, width= 15, units = 'cm', dpi=900)

# summary(join)
f3a<- ggplot() + 
  #Do the mean line
  geom_line(data = join %>% filter(!is.na(stdage.bin)& MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>% 
              group_by(INVYR, stdage.bin) %>% 
              summarize(BA.dead = mean(basal_area.dead), BA.n = n()), 
            mapping = aes(x = INVYR, y = BA.n, color = stdage.bin, linetype = stdage.bin), size = 1) +
  #Do the 95% CI Ribbon
  # geom_ribbon(data = join %>% filter(!is.na(stdage.bin)) %>% 
  #               group_by(INVYR, stdage.bin) %>% 
  #               summarize(BA.dead = mean(basal_area.dead), BA.n = n(), BA.dead.sd = sd(basal_area.dead)), 
  #             mapping = aes(x = INVYR, ymin=BA.dead - 1.96*(BA.dead.sd / sqrt(BA.n)),
  #                           ymax=BA.dead + 1.96*(BA.dead.sd / sqrt(BA.n)), fill = stdage.bin), alpha = 0.3) +
  xlab('Year') + ylab('Count') + theme_bw()
f3a
ggsave(filename = 'Fig3a_stand_age_count_time_series_FIA.png', height=6, width= 15, units = 'cm', dpi=900)

#For some reason Right now this is showing the youngest stands with the most die-off. However, the youngest stands are pretty old
f4<- ggplot() + #geom_line(data = live %>% filter(STATUSCD == 1  & !is.na(STDAGE)) %>% group_by(INVYR, stdage.bin) %>% summarize(Live.count = sum(count)), mapping = aes(x = INVYR, y = Live.count), color = 'green') + 
  geom_line(data = join %>% filter(!is.na(stdage.bin) & MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>% 
              group_by(INVYR, stdage.bin) %>% summarize(tpa.dead = mean(tpa.dead)), mapping = aes(x = INVYR, y = tpa.dead, color = stdage.bin, linetype = stdage.bin), size = 1) +
  xlab('Year') + ylab(expression('Mortality (trees ha'^-1*')')) + theme_bw()# + facet_wrap(~stdage.bin, ncol = 5)
# %>% filter(!is.na(stdage.bin)) 
f4
ggsave(filename = 'Fig4_stand_age_count_mortality_time_series_FIA.png', height=6, width= 15, units = 'cm', dpi=900)


f5<- ggplot() + #geom_line(data = live %>% filter(STATUSCD == 1  & !is.na(STDAGE)) %>% group_by(INVYR, stdage.bin) %>% summarize(Live.count = sum(count)), mapping = aes(x = INVYR, y = Live.count), color = 'green') + 
  geom_line(data = join %>% filter(!is.na(age.bin) & MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')) %>% 
              group_by(INVYR, age.bin) %>% summarize(BA.dead = mean(basal_area.dead)), mapping = aes(x = INVYR, y = BA.dead, color = age.bin, linetype = age.bin), size = 1) +
  xlab('Year') + ylab(expression('Mortality (m'^2*' ha'^-1*')')) + theme_bw()# + facet_wrap(~stdage.bin, ncol = 5)
# %>% filter(!is.na(stdage.bin)) 
f5
ggsave(filename = 'Fig5_stand_age_basal_area_mortality_time_series_FIA.png', height=6, width= 15, units = 'cm', dpi=900)

p6 <- ggplot() + geom_point(data = join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & STDAGE <= 250 & 
                                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')), mapping = aes(x = STDAGE, y = basal_area.all)) +
      theme_bw()
p6

p7 <- ggplot() + geom_point(data = join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & STDAGE <= 250 & 
                                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')), mapping = aes(x = STDAGE, y = tpa.all)) +
      theme_bw()
p7

p8 <- ggplot() + geom_point(data = join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & STDAGE <= 250 & 
                                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')), mapping = aes(x = STDAGE, y = basal_area.dead)) +
  theme_bw() #+ geom_smooth(data = join %>% filter(INVYR %in% c(2013,2014,2015,2016,2017,2018,2019)), mapping = aes(x = STDAGE, y = basal_area.dead), method = 'lm')
p8

p9 <- ggplot() + geom_point(data = join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & STDAGE <= 250 & 
                                                     MEANING %in% c('California mixed conifer', 'White fir', 'Pinyon / juniper woodland', 'Ponderosa pine', 'Jeffrey pine')), mapping = aes(x = basal_area.all, y = basal_area.dead)) +
  theme_bw()
p9

fig2 <- ggarrange(p6, p7, p8, p9, ncol = 2, nrow = 2, common.legend = FALSE, align = "v", labels = c('a)', 'b)', 'c)', 'd)'))
fig2

ggsave(filename = 'Fig6_mortality_standage_FIA_forest_type.png', height=18, width= 18, units = 'cm', dpi=900)

join %>% filter(INVYR %in% c(2013,2014,2015,2016,2017,2018,2019) & !is.na(STDAGE) & MEANING %in% c('California mixed conifer',  'White fir','Pinyon / juniper woodland',  'Ponderosa pine', 'Jeffrey pine'))
join$stdage.bin <- as.factor(join$stdage.bin)

#Calculate the Quintiles of stand age
std.q <- as.data.frame(unname(quantile((join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0))$STDAGE, prob = seq(0,1, 1/20), type = 3, na.rm = TRUE)))
# precip.q
colnames(std.q) <- 'STDAGE'
std.q$'Quintile' <- c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0)
# temp.q
std.q

#Bin data by Stand Age. Bins are quintiles.
join <- join %>% mutate(std.bin = case_when(
  STDAGE >= std.q %>% filter(Quintile == 0.9) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '235+',
  STDAGE >= std.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() &
  STDAGE < std.q %>% filter(Quintile == 0.9) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '180-234',
  STDAGE >= std.q %>% filter(Quintile == 0.7) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '165-179',
  STDAGE >= std.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.7) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '145-164',
  STDAGE >= std.q %>% filter(Quintile == 0.5) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '130-144',
  STDAGE >= std.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.5) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '115-129',
  STDAGE >= std.q %>% filter(Quintile == 0.3) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '110-114',
  STDAGE >= std.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.3) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '100-109',
  STDAGE >= std.q %>% filter(Quintile == 0.1) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < std.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '90-99',
    STDAGE < std.q %>% filter(Quintile == 0.1) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '35-89'))
  # STDAGE < std.q %>% filter(Quintile == 0.05) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '0-49'))

#Order the stand age bins
join$std.bin = with(join, factor(std.bin, levels = c('35-89', '90-99', '100-109', '110-114', '115-129',  '130-144', '145-164','165-179', '180-234','235+')))
summary(join)
# MORTYR %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019) & 
p10 <- ggplot(data = join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE)), mapping = aes(x = std.bin, y = basal_area.dead)) +
       geom_point(stat = 'summary') + geom_errorbar(stat = 'summary') + 
  # scale_color_brewer(name = 'Average Tree Age Bins')
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Average Tree Age Bins') + ylab(expression('Mortality (m'^2*' ha'^-1*')'))
p10

p11 <- ggplot(data = join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0), mapping = aes(x = std.bin, y = basal_area.all)) +
  geom_point(stat = 'summary') + geom_errorbar(stat = 'summary') + 
  # scale_color_brewer(name = 'Average Tree Age Bins')
  theme_bw() + xlab('Average Tree Age Bins') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) #+ ylim(0, 55)
p11

fig3 <- ggarrange(p10, p11, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
fig3

ggsave(filename = 'Fig7_mortality_standage_pointrange_FIA_wo_high_elevation.png', height=18, width= 22, units = 'cm', dpi=900)

#Tree Density Version
p12 <- ggplot(data = join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0), mapping = aes(x = std.bin, y = tpa.dead)) +
  geom_point(stat = 'summary') + geom_errorbar(stat = 'summary') + 
  # scale_color_brewer(name = 'Average Tree Age Bins')
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  xlab('Average Tree Age Bins') + ylab(expression('Mortality (trees ha'^-1*')'))
p12

p13 <- ggplot(data = join %>% filter(INVYR %in% c(2015,2016,2017,2018,2019) & !is.na(STDAGE) & STDAGE > 0), mapping = aes(x = std.bin, y = tpa.all)) +
  geom_point(stat = 'summary') + geom_errorbar(stat = 'summary') + 
  # scale_color_brewer(name = 'Average Tree Age Bins')
  theme_bw() + xlab('Average Tree Age Bins') + ylab(expression('Tree Density (tress ha'^-1*')'))
p13

fig4 <- ggarrange(p12, p13, ncol = 1, nrow = 2, common.legend = FALSE, heights = c(0.9, 1), align = "v", labels = c('a)', 'b)'))
fig4

ggsave(filename = 'Fig8_mortality_standage_pointrange_FIA.png', height=18, width= 22, units = 'cm', dpi=900)


