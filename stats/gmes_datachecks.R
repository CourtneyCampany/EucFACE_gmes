## frequency distribution of variables:

source("masterscripts/functions.R")

gmes <-read.csv("masterscripts/Master_data_file_clean-reconst.csv")
gmes2 <- gmes_format_func2(gmes)

##if analyzing gmes, drop tree 422
gmes_clean <- gmes2[gmes2$tree != 422,]

#test each variable, evaluate for need to transform
#looking at distribution and shapiro.test carefully due to small samples sizes
library(car)
library(MASS)
library(rcompanion)

### 1. Likely need data transformations:----------

### I start with sqrt(), log(), inverse (1/x). Then try others

#gmes
plotNormalHistogram(gmes_clean$gmes)
qqPlot(gmes_clean$gmes)
shapiro.test(gmes_clean$gmes)

  #simple sqrt() works great
  plotNormalHistogram(sqrt(gmes_clean$gmes))
  shapiro.test(sqrt(gmes_clean$gmes))

#mesolayer.mean
plotNormalHistogram(gmes2$mesolay.mean) #  looks like a bimodal thing...
qqPlot(gmes2$mesolay.mean)
shapiro.test(gmes2$mesolay.mean)

  #inverse transformation may work best # not anymore
  plotNormalHistogram(sqrt(log(gmes2$mesolay.mean)))
  shapiro.test(sqrt(log(gmes2$mesolay.mean)))
  # let try to subset lower and upper and test them separately
  lower.meso <- subset(gmes2, position == "lower")
  plotNormalHistogram(lower.meso$mesolay.mean)
  shapiro.test(lower.meso$mesolay.mean)
  qqPlot(lower.meso$mesolay.mean)
  plotNormalHistogram(sqrt(log(lower.meso$mesolay.mean)))
  shapiro.test(log(lower.meso$mesolay.mean)) # I cannot get it better than 0.01!

  upper.meso <- subset(gmes2, position == "upper")
  plotNormalHistogram(upper.meso$mesolay.mean)
  shapiro.test(upper.meso$mesolay.mean) # p = 0.03
  plotNormalHistogram(log(1/(upper.meso$mesolay.mean)))
  shapiro.test(1/(log(upper.meso$mesolay.mean))) # i give up, it stays 0.03, nothing seems to work
  

#sumlength.par.mean
plotNormalHistogram(gmes2$sumlength.par.mean)
qqPlot(gmes2$sumlength.par.mean) #a few outliers
shapiro.test(gmes2$sumlength.par.mean)

  #sin transformation may work best
  plotNormalHistogram(sin(gmes2$sumlength.par.mean))
  shapiro.test(sin(gmes2$sumlength.par.mean))

#meanlength.par.mean
plotNormalHistogram(gmes2$meanlength.par.mean)
qqPlot(gmes2$meanlength.par.mean) #a few outliers
shapiro.test(gmes2$meanlength.par.mean)

  #nothing works here (but .086 could be worse) # now 0.05
  

  #drawdown
plotNormalHistogram(gmes2$Drawdown)
qqPlot(gmes2$Drawdown) #serious outliers
boxplot(Drawdown~co2grow ,data=gmes2 ) #outliers maybe in eCO2 => probably R4
shapiro.test(gmes2$Drawdown)

  #log transform works (log or log10)
  plotNormalHistogram(log10(gmes2$Drawdown))
  shapiro.test(log10(gmes2$Drawdown))

### 2. Appear to be ok:-----------

#Photosynthesis 
plotNormalHistogram(gmes2$Photo)
qqPlot(gmes2$Photo)
shapiro.test(gmes2$Photo)

#Stomatal conductance # sqrt is needed
plotNormalHistogram(sqrt(gmes2$gs))
qqPlot(gmes2$gs)
shapiro.test(sqrt(gmes2$gs))

#Ci
plotNormalHistogram(gmes2$Ci)
qqPlot(gmes2$Ci)
shapiro.test(gmes2$Ci)

#LMA
plotNormalHistogram(gmes2$LMA)
qqPlot(gmes2$LMA)
shapiro.test(gmes2$LMA)

#Nm
plotNormalHistogram(gmes2$Nm)
qqPlot(gmes2$Nm)
shapiro.test(gmes2$Nm)

#Na
plotNormalHistogram(gmes2$Na)
qqPlot(gmes2$Na)
shapiro.test(gmes2$Na)

#leafw
hist(gmes2$leafw)
qqPlot(gmes2$leafw)
shapiro.test(gmes2$leafw)

#epiup
plotNormalHistogram(gmes2$epi_up)
qqPlot(gmes2$epi_up) ##maybe one outlier
shapiro.test(gmes2$epi_up)

#epilow
plotNormalHistogram(gmes2$epi_low)
qqPlot(gmes2$epi_low) ##maybe one outlier
shapiro.test(gmes2$epi_low)

#stomdenad
plotNormalHistogram(gmes2$stomdenad)
qqPlot(gmes2$stomdenad) ##maybe a few outliers
shapiro.test(gmes2$stomdenad.mean)

#stomdenab
plotNormalHistogram(gmes2$stomdenab)
qqPlot(gmes2$stomdenab)
shapiro.test(gmes2$stomdenab.mean)

#stomlengthad
plotNormalHistogram(gmes2$stomlengthad)
qqPlot(gmes2$stomlengthad)
shapiro.test(gmes2$stomlengthad.mean)

#stomlengthab
plotNormalHistogram(gmes2$stomlengthab)
qqPlot(gmes2$stomlengthab)
shapiro.test(gmes2$stomlengthab.mean)

#meso.mean
plotNormalHistogram(gmes2$meso.mean)
qqPlot(gmes2$meso.mean)
shapiro.test(gmes2$meso.mean)

#epiupper.mean
plotNormalHistogram(gmes2$epiupper.mean)
qqPlot(gmes2$epiupper.mean) #maybe one outlier
shapiro.test(gmes2$epiupper.mean)

#epilower.mean
plotNormalHistogram(gmes2$epilower.mean)
qqPlot(gmes2$epilower.mean)
shapiro.test(gmes2$epilower.mean)

#vcmax25
plotNormalHistogram(gmes2$Vcmax25)
qqPlot(gmes2$Vcmax25)
shapiro.test(gmes2$Vcmax25)

#jmax25
plotNormalHistogram(gmes2$Jmax25)
qqPlot(gmes2$Jmax25)
shapiro.test(gmes2$Jmax25)

#Rd25
plotNormalHistogram(gmes2$Rd25)
qqPlot(gmes2$Rd25)
shapiro.test(gmes2$Rd25)

##############################################################################################
##############################################################################################
# Data checks for the 4 campaigns 
setwd("C:/R-projects/EucFACE_gmes/masterscripts")
#read in datafile
across <-read.csv("Tree_aver_age_position_EucFACE.csv") 

across$Na <- (across$LMA * across$Leaf.N) / 100


plotNormalHistogram(across$Photo)
qqPlot(across$Photo)
shapiro.test(across$Photo)
bwplot(Photo ~ Position | C.treat , data = across)
# There are a few outliers with photo < 10 in eCO2. This is incorrect!
normal <- subset(across, Photo > 10)
plotNormalHistogram(normal$Photo)
qqPlot(normal$Photo)
shapiro.test(normal$Photo) # normally distributed without these outliers!


# Variables that need transforming
# gs
plotNormalHistogram(across$gs)
qqPlot(across$gs)
shapiro.test(across$gs)

  plotNormalHistogram(sqrt(across$gs))
  qqPlot(sqrt(across$gs))
  shapiro.test(sqrt(across$gs)) # sqrt works great

#Vcmax
plotNormalHistogram(across$Vcmax)
qqPlot(across$Vcmax)
shapiro.test(across$Vcmax)
bwplot(Vcmax ~ Position | C.treat , data = across) # one outlier perhaps

  plotNormalHistogram(sqrt(across$Vcmax))
  qqPlot(sqrt(across$Vcmax))
  shapiro.test(sqrt(across$Vcmax)) # sqrt works great

# Na, again sqrt works
plotNormalHistogram(across$Na)
qqPlot(across$Na)
shapiro.test(across$Na)
  
  plotNormalHistogram(sqrt(across$Leaf.N))
  qqPlot(sqrt(across$Leaf.N))
  shapiro.test(sqrt(across$Leaf.N))


# variables that don't need transforming
#Ci
plotNormalHistogram(across$Ci)
qqPlot(across$Ci)
shapiro.test(across$Ci)

#Jmax
plotNormalHistogram(across$Jmax)
qqPlot(across$Jmax)
shapiro.test(across$Jmax)

#LMA
plotNormalHistogram(across$LMA)
qqPlot(across$LMA)
shapiro.test(across$LMA)

# Leaf N (massbased)
plotNormalHistogram(across$Leaf.N)
qqPlot(across$Leaf.N)
shapiro.test(across$Leaf.N)

#Openness for lower canopy only
plotNormalHistogram(across$Openness)
qqPlot(across$Openness)
lower <- subset(across, Position =="lower")
plotNormalHistogram(lower$Openness)
qqPlot(lower$Openness)
shapiro.test(lower$Openness)

