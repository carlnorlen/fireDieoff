#Author: Carl Norlen
#Date Created: May 3, 2022
#Date Edited: June 16, 2022
#Purpose: Do an analysis of dead trees and Stand Age

# Specify necessary packages
p <- c("RSQLite","dbplyr","ggplot2","dplyr","tidyr", "ggpubr", "RColorBrewer",  
       'gt', 'gtsummary', 'webshot', 'kableExtra', 'broom', 'rFIA')

# If necessary: install/update packages
# install.packages('rlang',repo='https://cloud.r-project.org/')
# library("agricolae")
# Load packages
lapply(p,require,character.only=TRUE)

#Get the FIA directory
fiaCA <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_CSV' #Downloaedd from FIA DataMart
# dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"
# fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
# ref <- getFIA(states = 'ref', tables = c('FOREST_TYPE', 'FOREST_TYPE_GROUP'), dir = fiaCA)
ca <- readFIA(fiaCA)
# ca <- getFIA(states = 'CA', dir = 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_CSV')
# summary(ca)

#Creating a plot of mortality by tree species in two time periods and drought sequences.
#Create a notin operator
`%notin%` <- Negate(`%in%`)

#Possible way to deal with non-conifer forests
forest.names <- ca$REF_FOREST_TYPE
forest.names$FORTYPCD <- forest.names$VALUE
##Both Droughts regions estimates
#Doing a combined estimated of the Basal Area for Both Droughts during 1999-2002
#Total basal area and tpa estimates by species
tpa.all <- tpa(ca, byPlot = TRUE, treeType = 'dead', grpBy = 'STDAGE', treeDomain = DIA >= 5 & 
                   MORTYR %notin% c(""), #c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo', 'M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

tpa.tree <- tpa(ca, byPlot = TRUE, treeType = 'dead', grpBy = c('STDAGE', 'MORTYR'), treeDomain = DIA >= 5 & 
                 MORTYR %notin% c(""), #c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
               areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo', 'M261Es') &
                 DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
tpa.all
growMort(treeType)
mort <- growMort(ca, byPlot = TRUE, treeType = 'gs', stateVar = 'BAA', grpBy = 'STDAGE', treeDomain = DIA >= 5 & 
           MORTYR %notin% c(""), #c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo', 'M261Es') &
           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
summary(mort)

#Calculate the Quintiles of stand age
stdage.q <- as.data.frame(unname(quantile(mort$STDAGE, prob = seq(0,1, 1/5), type = 3, na.rm = TRUE)))
# precip.q
colnames(stdage.q) <- 'STDAGE'
stdage.q$'Quintile' <- c(0.0, 0.2,0.4, 0.6, 0.8, 1.0)
# temp.q
stdage.q

#Bin data by Stand Age. Bins are quintiles.
mort <- mort %>% mutate(stdage.bin = case_when(
  STDAGE >= stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '150+',
  STDAGE >= stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.8) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '102-149',
  STDAGE >= stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.6) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '80-101',
  STDAGE >= stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() & 
    STDAGE < stdage.q %>% filter(Quintile == 0.4) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '48-79',
  STDAGE < stdage.q %>% filter(Quintile == 0.2) %>% dplyr::select(STDAGE) %>% as.numeric() ~ '0-47'))

#Order the stand age bins
mort$stdage.bin = with(mort, factor(stdage.bin, levels = c('0-47','48-79','80-101','102-149','150+')))

ggplot() + geom_line(data = mort %>% filter(MORT_BAA > 0) %>%
                                     group_by(YEAR, stdage.bin) %>% 
                                     summarize(MORT_BAA = mean(MORT_BAA)), 
                                     mapping = aes(x = YEAR, y  = MORT_BAA, color = stdage.bin))

plotFIA(tpa.all, y = BAA)
summary(tpa.all)
