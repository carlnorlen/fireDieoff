#Author: Carl Norlen
#Date Created: May 3, 2022
#Date Edited: May 3, 2022
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
tpa.all <- tpa(ca, byPlot = TRUE, treeType = 'all', treeDomain = DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo', 'M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

plotFIA(tpa.all, y = BAA)
summary(tpa.all)
