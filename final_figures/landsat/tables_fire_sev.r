#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: April 3, 2023
#Purpose: Create merge split raster files

# cd /C/Users/Carl/mystuff/Goulden_Lab/CECS/chrono
# cd /C/Users/can02/mystuff/Goulden_Lab/CECS/chrono
#Run the script: R < stand_age.r --vanilla
p <- c('ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggplot2', 'magrittr', 'stats', 'patchwork','ggpmisc', 'raster', 'RStoolbox', 'quantreg','segmented', 'RColorBrewer',
	   'gt', 'gtsummary', 'webshot', 'stargazer', 'kableExtra', 'broom', 'svglite','sjPlot','purrr', 'sjmisc', 'magick', 'magrittr', 'knitr', 'xtable')
# install.packages('quantreg',repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

# dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\CECS"
dir <- "D:\\Large_Files\\CECS"
memory.limit(32000)




age.dNDMI.rq <- rq(dNDMI_2015_mean ~ stand_age_mean, data = stand.age.sample, tau = q10)
print(age.dNDMI.rq %>% tidy())
tb1 <- age.dNDMI.rq %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 1: Quantile Regression, Die-off(dNDMI) ~ f(Stand Age)") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 5, file = "T1_dNDMI_stand_age_quantile_regression_results.png", zoom = 4.0)  


age.ADS.rq <- rq(ADS_2017_mean ~ stand_age_mean, data = stand.age.sample, tau = q10)
print(age.ADS.rq %>% tidy())
tb2 <- age.ADS.rq %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 2: Quantile Regression, Die-off(ADS) ~ f(Stand Age)") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 5, file = "T2_ADS_stand_age_quantile_regression_results.png", zoom = 4.0)  

stand.age.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age)

summary(stand.age.lm)
#Treatment Grid Cells
aov.dNDMI.stand.age.treatment <- aov(dNDMI_2015_mean ~ agegroup, data = stand.age.sample)
summary(aov.dNDMI.stand.age.treatment)

aov.PET4yr.stand.age.treatment <- aov(PET_4yr_2015_mean ~ agegroup, data = stand.age.sample)
summary(aov.PET4yr.stand.age.treatment)

aov.biomass.stand.age.treatment <- aov(biomass_2012_mean ~ agegroup, data = stand.age.sample)
summary(aov.biomass.stand.age.treatment)

tukey.dNDMI.stand.age.treatment <- TukeyHSD(aov.dNDMI.stand.age.treatment)
print(tukey.dNDMI.stand.age.treatment)

tukey.PET4yr.stand.age.treatment <- TukeyHSD(aov.PET4yr.stand.age.treatment)
print(tukey.PET4yr.stand.age.treatment)

tukey.biomass.stand.age.treatment <- TukeyHSD(aov.biomass.stand.age.treatment)
print(tukey.biomass.stand.age.treatment)

# anovas.treament <- anova(aov.dNDMI.stand.age.treatment, aov.PET4yr.stand.age.treatment, aov.biomass.stand.age.treatment)
# print(anovas.treatment) 

#Control Grid Cells
aov.dNDMI.stand.age.control <- aov(dNDMI_2015_mean ~ agegroup, data = stand.age.control)
summary(aov.dNDMI.stand.age.control)

aov.PET4yr.stand.age.control <- aov(PET_4yr_2015_mean ~ agegroup, data = stand.age.control)
summary(aov.PET4yr.stand.age.control)

aov.biomass.stand.age.control <- aov(biomass_2012_mean ~ agegroup, data = stand.age.control)
summary(aov.biomass.stand.age.control)

tukey.dNDMI.stand.age.control <- TukeyHSD(aov.dNDMI.stand.age.control)
print(tukey.dNDMI.stand.age.control)

tukey.PET4yr.stand.age.control <- TukeyHSD(aov.PET4yr.stand.age.control)
print(tukey.PET4yr.stand.age.control)

tukey.biomass.stand.age.control <- TukeyHSD(aov.biomass.stand.age.control)
print(tukey.biomass.stand.age.control)

tb1 <- tukey.dNDMI.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 1: Not Exposed, Tukey HSD, dNDMI ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 5, file = "Table1_Not_Exposed_Mortality_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb2 <- tukey.dNDMI.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 2: Exposed, Tukey HSD, dNDMI ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 5, file = "Table2_Exposed_Mortality_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb3 <- tukey.PET4yr.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 3: Not Exposed, Tukey HSD, four-year Pr-ET ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb3, width = 5, file = "Table3_Not_Exposed_Water_Deficit_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb4 <- tukey.PET4yr.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 4: Exposed, Tukey HSD, four-year Pr-ET ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb4, width = 5, file = "Table4_Exposed_Water_Deficit_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb5 <- tukey.biomass.stand.age.control %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 5: Not Exposed, Tukey HSD, Biomass ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb5, width = 5, file = "Table5_Not_Exposed_Biomass_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

tb6 <- tukey.biomass.stand.age.treatment %>% tidy() %>% as.data.frame() %>% kbl(caption = "Table 6: Exposed, Tukey HSD, Biomass ~ Years Since Fire", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb6, width = 5, file = "Table6_Exposed_Biomass_Years_Fire_Tukey_HSD_.png", zoom = 4.0) 

#Summary Statistics
summary.control <- stand.age.control %>% 
  group_by(agegroup) %>% 
  summarize(dNDMI_mean = mean(dNDMI_2015_mean),
            dNDMI_sd = sd(dNDMI_2015_mean),
			biomass_mean = mean(biomass_2012_mean),
			biomass_sd = sd(biomass_2012_mean))

print(summary.control)

summary.treatment <- stand.age.sample %>% 
  group_by(agegroup) %>% 
  summarize(dNDMI_mean = mean(dNDMI_2015_mean),
            dNDMI_sd = sd(dNDMI_2015_mean),
			biomass_mean = mean(biomass_2012_mean),
			biomass_sd = sd(biomass_2012_mean))

print(summary.treatment)

# stand.age.lm2 <- lm(dNDMI_2015_mean ~ stand_age_mean, data = stand.age)

# summary(stand.age.lm2)

# stand.age.socal.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age.socal)

# summary(stand.age.socal.lm)

# stand.age.norcal.lm <- lm(dNDMI_2015_mean ~ stand_age_mean + I(stand_age_mean^2), data = stand.age.norcal)

# summary(stand.age.norcal.lm)

nrow(stand.age)
# summary(stand.age)
# nrow(stand.age.norcal)
# nrow(stand.age.socal)