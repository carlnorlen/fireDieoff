#Created by : Jonathan Wang
#Modified by: Carl A. Norlen
#Created Date: August 15, 2022
#Updated Date
#Code for doing a matching in R based on the control bins

library(data.table)
library(stringr)
library(ggplot2)

# load("G:/My Drive/Slackbits/FRAP_metadata_controlexport (1).RData")

setwd('C:/Users/can02/mystuff/fireDieoff/pixel_sample')

#The data directory
dir_in <- "D:\\Fire_Dieoff"
fire_in <- "D:\\Large_Files\\Fire_Dieoff"

#Add the data
fireSamples <- read.csv(file.path(dir_in, "fire_south_sierra_FRAP_dist_300pts_100elev_200precip_5temp_ts8_300m_20221025.csv"), header = TRUE, na.strings = "NaN")

controlSamples <- read.csv(file.path(dir_in, "control_south_sierra_no_FRAP_undist_100pts_100elev_200precip_5temp_ts8_300m_20221025.csv"), header = TRUE, na.strings = "NaN")

#controlSamples1 = fread("G:/My Drive/earthEngine_outputs/controlTimeSeries_climMatch.csv")
#controlSamples2 = fread("G:/My Drive/earthEngine_outputs/controlTimeSeries_climMatch2.csv", fill = T)
##controlSamples3 = fread("G:/My Drive/earthEngine_outputs/controlTimeSeries_mediumBMCC_LST3.csv")
#controlSamples = rbindlist(list(controlSamples1, controlSamples2), use.names=T)
#rm(controlSamples2)
#rm(controlSamples1)
# #controlSamples$slope = NULL
# #controlSamples$latbin = controlSamples$latbin
# controlSamples[aspect >= 45 & aspect < 135, aspectBin := "E"]
# controlSamples[aspect >= 135 & aspect < 225, aspectBin := "S"]
# controlSamples[aspect >= 225 & aspect < 315, aspectBin := "W"]
# controlSamples[aspect >= 315 | aspect < 45, aspectBin := "N"]
# controlSamples$aspect = NULL
# 
# controlSamples[slope < 15, slopeBin := "<15"]
# controlSamples[slope >= 15, slopeBin := ">15"]
# controlSamples$slope = NULL
#controlSamples$slopeBin = NULL
#controlSamples[slope >= 40, slopeBin := ">40"]

#Unstack the Data
controlSamples <- controlSamples %>% #dplyr::select(-c('latitude', 'longitude')) %>% 
  pivot_longer(cols = X10_AET:X9_tpa_max, names_to = c('year', '.value'), names_pattern = "X(\\d{1}|\\d{2})_(.*)", names_repair = "unique")

#Remove those columns
controlSamples$`system.index` = NULL
controlSamples$`.geo` = NULL
controlSamples$year <- as.numeric(controlSamples$year) + 1984 

#Convert missing TPA data to NAs
controlSamples[controlSamples$tpa_max < 0,]$tpa_max <- NA

#Convert to a Data Table
controlSamples <- as.data.table(controlSamples)
## fix variable names
# badnames_num = paste(2:33, 2:33,sep="_")
# badnames_exp = expand.grid(badnames_num, c('GPPgf_CAsites_monthly_sum','NBR','NDVI','NIRv','biomass','cancover','remapped'))
# badnames = paste(badnames_exp$Var1, badnames_exp$Var2, sep = "_")

# goodnames_exp = expand.grid(2:33, c('GPP','NBR','NDVI','NIRv','biomass','cancover','lc'))
# goodnames = paste(goodnames_exp$Var1, goodnames_exp$Var2, sep = "_")
# 
# setnames(controlSamples, badnames, goodnames)

## get climate bins
controlSamples[,c("elevbin","precipbin","tempbin"):=
                 .(elevation %/% 100 * 100, clm_precip_sum %/% 200 * 200, clm_temp_mean %/% 5 * 5)]
controlSamples
# Add the latitude bin
# controlSamples[,latbin := as.numeric(latitude) %/% 0.25 * 0.25]

# trying by bins for now
controlSamples[,c('lf_evt_2001','stratlayer'):= NULL]

## get mean/sd/n of each stratlayer/index/year
controlMeans = controlSamples[, lapply(.SD, base::mean,na.rm=T), 
                              by = c("elevbin", "precipbin","tempbin")]
controlMeans
# controlMedians = controlSamples[, lapply(.SD, stats::median,na.rm=T), 
#                                 by = c("elevbin", "precipbin","tempbin")]
controlSDs = controlSamples[, lapply(.SD, stats::sd,na.rm=T), 
                            by = c("elevbin", "precipbin","tempbin")]
controlN = controlSamples[, lapply(.SD, length), 
                          by = c("elevbin", "precipbin","tempbin")]

### melt to get date info
meanMelt = melt(controlMeans, id.vars = c("year","elevbin", "precipbin","tempbin"))
meanMelt
meanMelt[,c("year","band") := tstrsplit(variable,"_")]
meanMelt$year = as.integer(meanMelt$year)
# meanMelt[variable == "vpdmax_mean", band := "vpd_max"]
meanMelt[variable == "tmax", band := "temp"]
meanMelt[variable == "ppt", band := "prcp"]
meanMelt$variable = NULL
meanMelt

# medianMelt = melt(controlMedians, id.vars = c("latbin", "elevbin", "precipbin","tempbin"))
# medianMelt[,c("year","band") := tstrsplit(variable,"_")]
# medianMelt$year = as.integer(meanMelt$year)
# medianMelt[variable == "vpdmax_mean", band := "vpd_max"]
# medianMelt[variable == "tmean_mean", band := "temp_mean"]
# medianMelt[variable == "ppt_sum", band := "prcp_sum"]
# medianMelt$variable = NULL

sdMelt = melt(controlSDs, id.vars = c("latbin", "elevbin", "precipbin","tempbin"))
sdMelt[,c("year","band") := tstrsplit(variable,"_")]
sdMelt$year = as.integer(sdMelt$year)
sdMelt[variable == "vpdmax_mean", band := "vpd_max"]
sdMelt[variable == "tmean_mean", band := "temp_mean"]
sdMelt[variable == "ppt_sum", band := "prcp_sum"]
sdMelt$variable = NULL

NMelt = melt(controlN, id.vars = c("latbin", "elevbin", "precipbin","tempbin"))
NMelt[,c("year","band") := tstrsplit(variable,"_")]
NMelt$year = as.integer(sdMelt$year)
NMelt[variable == "vpdmax_mean", band := "vpd_max"]
NMelt[variable == "tmean_mean", band := "temp_mean"]
NMelt[variable == "ppt_sum", band := "prcp_sum"]
NMelt$variable = NULL

###c("latbin", "elevbin", "precipbin","tempbin")
setnames(meanMelt,"value","vimean")
# setnames(medianMelt,"value","vimedian")
setnames(sdMelt,"value","visd")
setnames(NMelt,"value","N")
setkey(meanMelt,latbin, elevbin,precipbin,tempbin, band, year)
setkey(medianMelt,latbin, elevbin,precipbin,tempbin, band, year)
setkey(sdMelt,latbin, elevbin,precipbin,tempbin, band, year)
setkey(NMelt,latbin, elevbin,precipbin,tempbin, band, year)
controlMelt = merge(merge(merge(meanMelt,medianMelt), sdMelt),NMelt)
#controlMelt$se = controlMelt$visd/sqrt(controlMelt$N)

controlMelt[,vimean := as.integer(vimean)]
controlMelt[,visd := as.integer(visd)]
controlMelt[,vimedian := as.integer(vimedian)]

#controlMelt = controlMelt[]
## save this for later
fwrite(controlMelt, "G:/My Drive/data/controlPixels/controlMeans_climMatch_withGPP.csv")

#ggplot(data = controlMelt[band=="NDVI" &  latbin == 33.5 & elevation == 15 & landcover == 4,], aes(x = year+1984, y = mean, ymin = mean -2*se, ymax = mean + 2*se)) +
#  geom_ribbon(alpha = 0.6) + 
#  geom_line()

#unique(controlMelt[,.(latbin,elevation,landcover)])
#10101 "controls"

#Match the data based on the control bins
fireMatch = fread("G:/My Drive/data/controlPixels/to_be_matched_FRAP_USFS_20220620.csv")
fireMatch[,latbin := latbin/100]

#Write the matched data sets
fireMatch = merge(fireMatch, controlWide, all.x = T, by = c("latbin","elevbin","precipbin","tempbin"))
fwrite(fireMatch, "G:/My Drive/data/controlPixels/firesMatchedToControls_100pts_20220623.csv")