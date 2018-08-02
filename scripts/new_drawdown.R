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

summaryBy(Openness ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

#anatomical variables
summaryBy(leafw.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(mesolay.mean.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(meanlength.par.mean.mean ~ co2grow + canopy,
          data=RingMeans, FUN=c(mean, std.error), na.rm=TRUE)

summaryBy(Stomratio.mean.mean ~ co2grow + canopy,
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
Ringavg <- summaryBy(Photo + gs + Ci + Tleaf + Vcmax + Jmax + LMA + Leaf.N + GapFr + Openness + log.Open.   ~ Ring + Campaign + C.treat + Position, # I don't need age because these are split by campaigns
                       data=across, FUN=c(mean, std.error), na.rm=TRUE)

TreeMeans <- summaryBy(Photo.mean + gs.mean + Ci.mean + Tleaf.mean + Vcmax.mean + Jmax.mean + LMA.mean + Leaf.N.mean + GapFr.mean + Openness.mean + log.Open..mean   ~ Campaign + C.treat + Position, # I don't need age because these are split by campaigns
                          data=Ringavg, FUN=c(mean, std.error), na.rm=TRUE)

names(TreeMeans)
names(TreeMeans) <- c("Campaign", "C.treat", "Position","Photo","gs", "Ci", "Tleaf", "Vcmax", "Jmax", "LMA", "LeafN", "GapFr", "Openness", "log.Open","se-Photo","se-gs", "se-Ci", "se-Tleaf", "se-Vcmax", "se-Jmax", "se-LMA", "se-LeafN", "se-GapFr", "se-Openness", "se-log.Open" )

write.csv(TreeMeans, "TreeMeans_EucFACE_Campaigns.csv")