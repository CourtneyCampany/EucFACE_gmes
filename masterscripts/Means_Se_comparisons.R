#calculate new drawdown:

setwd("C:/R-projects/EucFACE_gmes/masterscripts")
source("C:/R-projects/EucFACE_gmes/masterscripts/functions.R")

gmes <-read.csv("Master_data_file_clean-reconst.csv")
gmes2 <- gmes_format_func2(gmes)


library(doBy)
library(plotrix)

# !!!first we need ring means then we can average by treatment, to make sure the replication and se is calculated correctly.
# Dataset needst to be CO2 grow only
RingMeans <- summaryBy(leafw + epi_up + epi_low + mesolay.mean + meso.mean + stomdenad.mean + stomdenab.mean + stomlengthad.mean + stomlengthab.mean + spi.ad.mean + spi.ab.mean + sumlength.par.mean + meanlength.par.mean + gmes + Mean_gm.Eglob + Mean_gs + Mean_Ci + Mean_Cc + Drawdown + Photo + gs + Ci + Tleaf + LMA + N.perc + Vcmax + Jmax + Rd + Vcmax25 + Jmax25 + Rd25 + Openness + Stomratio + Nm + Na  ~ ring + co2grow + canopy,
          data=gmes2, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Photo.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Mean_gs.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(gmes.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Mean_gm.Eglob.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(N.perc.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Jmax25.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Openness.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

#anatomical variables
summaryBy(leafw.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(mesolay.mean.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(meanlength.par.mean.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Stomratio.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)


##if analyzing gmes, Cc and Drawdown, drop tree 422
gmes2 <- gmes2[gmes2$tree != 422,]

##calculate CC
#gmes_clean$Cc<- with(gmes_clean, Ci-Photo/gmes)

##calculate drawdown
#gmes_clean$drawdown2 <- with(gmes_clean, Ci-Cc)
#need to adjust vars before running

summaryBy(Drawdown.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Mean_Cc.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

# gmes<-summaryBy(leafw + epi_up + epi_low + stomdenad.mean + stomdenab.mean + stomlengthad.mean + stomlengthab.mean + spi.ad.mean + spi.ab.mean + mesolay.mean + meso.mean + sumlength.par.mean + meanlength.par.mean + gmes + Mean_gm.Eglob + Mean_gs + Mean_Ci + Mean_Cc + Drawdown + Photo + gs + Ci + Tleaf + LMA + N.perc + Vcmax + Jmax + Rd + Vcmax25 + Jmax25 + Rd25 + Openness + Stomratio 
#               ~ Conc + co2grow + co2meas + position + Tree + ring, data=data, FUN = mean)


#---------------------------------------------------------------------------------------------

# Now need to recalulate means for the 4 campaigns too...

across <-read.csv("Tree_aver_age_position_EucFACE_corr.csv") 
Ringavg <- summaryBy(Photo + gs + Ci + Tleaf + Vcmax + Jmax + LMA + Leaf.N + GapFr + Openness + log.Open.   ~ Ring + Campaign + C.treat + Position + Age, 
                       data=across, FUN=c(mean, std.error), na.rm=TRUE)

TreeMeans <- summaryBy(Photo.mean + gs.mean + Ci.mean + Tleaf.mean + Vcmax.mean + Jmax.mean + LMA.mean + Leaf.N.mean + GapFr.mean + Openness.mean + log.Open..mean   ~ C.treat + Position + Age, # replaced campaign with Age
                          data=Ringavg, FUN=c(mean, std.error), na.rm=TRUE)

names(TreeMeans)
names(TreeMeans) <- c("C.treat", "Position", "Age","Photo","gs", "Ci", "Tleaf", "Vcmax", "Jmax", "LMA", "LeafN", "GapFr", "Openness", "log.Open","se-Photo","se-gs", "se-Ci", "se-Tleaf", "se-Vcmax", "se-Jmax", "se-LMA", "se-LeafN", "se-GapFr", "se-Openness", "se-log.Open" )

write.csv(TreeMeans, "TreeMeans_EucFACE_Age.csv")

#-------------------------------------------------------------------------------------------
# checking specific ratio numbers in the text with revised means above
# 1. across campaigns

summaryBy(Photo ~ Position, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(Vcmax ~ Position, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(Jmax ~ Position, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(LMA ~ Position, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(Openness ~ Position, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(gs ~ C.treat, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Vcmax ~ Age, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(Jmax ~ Age, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(LMA ~ Age, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(LeafN ~ Age + C.treat, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(Vcmax ~ Age + C.treat, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(Jmax ~ Age + Position, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(LMA ~ Age + Position, data=TreeMeans, FUN=c(mean, std.error), na.rm=TRUE)

# 2. for Oct 2014

# first calculate treatment means from Ringmeans
CO2Means <- summaryBy(leafw.mean + epi_up.mean + epi_low.mean + mesolay.mean.mean + meso.mean.mean + stomdenad.mean.mean + stomdenab.mean.mean + stomlengthad.mean.mean + stomlengthab.mean.mean + spi.ad.mean.mean + spi.ab.mean.mean + sumlength.par.mean.mean + meanlength.par.mean.mean + gmes.mean + Mean_gm.Eglob.mean + Mean_gs.mean + Mean_Ci.mean + Mean_Cc.mean + Drawdown.mean + Photo.mean + gs.mean + Ci.mean + Tleaf.mean + LMA.mean + N.perc.mean + Vcmax.mean + Jmax.mean + Rd.mean + Vcmax25.mean + Jmax25.mean + Rd25.mean + Openness.mean + Stomratio.mean + Nm.mean + Na.mean  ~ co2grow + canopy,
                       data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)
names(CO2Means)
names(CO2Means) <- c("co2grow", "canopy" ,"leafw" , "epi_up" , "epi_low" , "mesolay.mean" , "meso.mean" , "stomdenad" , "stomdenab" , "stomlengthad" , "stomlengthab" , "spi.ad" , "spi.ab" , "sumlength.par" , "meanlength.par" , "gmes" , "Mean_gm.Eglob" , "Mean_gs" , "Mean_Ci" , "Mean_Cc" , "Drawdown" , "Photo" , "gs" , "Ci" , "Tleaf" , "LMA" , "N.perc" , "Vcmax" , "Jmax" , "Rd" , "Vcmax25" , "Jmax25" , "Rd25" , "Openness" , "Stomratio" , "Nm" , "Na", "se-leafw" , "se-epi_up" , "se-epi_low" , "se-mesolay.mean" , "se-meso.mean" , "se-stomdenad" , "se-stomdenab" , "se-stomlengthad" , "se-stomlengthab" , "se-spi.ad" , "se-spi.ab" , "se-sumlength.par" , "se-meanlength.par" , "se-gmes" , "se-Mean_gm.Eglob" , "se-Mean_gs" , "se-Mean_Ci" , "se-Mean_Cc" , "se-Drawdown" , "se-Photo" , "se-gs" , "se-Ci" , "se-Tleaf" , "se-LMA" , "se-N.perc" , "se-Vcmax" , "se-Jmax" , "se-Rd" , "se-Vcmax25" , "se-Jmax25" , "se-Rd25" , "se-Openness" , "se-Stomratio" , "se-Nm" , "se-Na") 
write.csv(CO2Means, "CO2Means_Oct2014.csv")

mean(CO2Means$Vcmax, na.rm=TRUE)
sd(CO2Means$Vcmax)/sqrt(4)
mean(CO2Means$Vcmax25, na.rm=TRUE)
sd(CO2Means$Vcmax25)/sqrt(4)

mean(CO2Means$Jmax, na.rm=TRUE)
sd(CO2Means$Jmax)/sqrt(4)
mean(CO2Means$Jmax25, na.rm=TRUE)
sd(CO2Means$Jmax25)/sqrt(4)

mean(CO2Means$Mean_gs, na.rm=TRUE)
sd(CO2Means$Mean_gs)/sqrt(4)
mean(CO2Means$gmes, na.rm=TRUE)
sd(CO2Means$gmes)/sqrt(4)
mean(CO2Means$Mean_gm.Eglob, na.rm=TRUE)
sd(CO2Means$Mean_gm.Eglob)/sqrt(4)

summaryBy(Openness ~ canopy, data=CO2Means, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(LMA ~ canopy, data=CO2Means, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(LMA ~ co2grow, data=CO2Means, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(Nm ~ co2grow + canopy, data=CO2Means, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(leafw ~ canopy, data=CO2Means, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(mesolay.mean ~ canopy, data=CO2Means, FUN=c(mean, std.error), na.rm=TRUE)
summaryBy(meso.mean ~ canopy, data=CO2Means, FUN=c(mean, std.error), na.rm=TRUE)

mean(CO2Means$Stomratio, na.rm=TRUE)
sd(CO2Means$Stomratio)/sqrt(4)

#across sides
#merge both sides into one column, then calculate
st <- read.csv("C:/R-projects/EucFACE_gmes/masterscripts/Stomata_means-across-sides.csv")

mean(st$stomlength, na.rm=TRUE)
sd(st$stomlength)/sqrt(8)
mean(st$spi, na.rm=TRUE)
sd(st$spi)/sqrt(8)
