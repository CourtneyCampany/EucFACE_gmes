rm(list = ls())
# read and format data ----------------------------------------------------
setwd("C:/R-projects/EucFACE_gmes/masterscripts")
gmes <-read.csv("Master_data_file_clean-reconst.csv")
# This is the file with tree level data
# make tree averages within canopy position
library(doBy)
library(lattice)
library(lme4)
library(car)
library(visreg)
library(lmerTest)
library(sciplot)

gmes$Date <- as.Date(gmes$Date)
gmes$treatment <- with(gmes, paste(co2grow, position, sep="-"))
gmes$Nm <- gmes$N.perc *10           # mass-based leaf N
gmes$Na <- gmes$LMA * gmes$Nm / 1000 # Narea in g m-2


#separate dataset for CO2 grow only, 
#which means Co2grow = amb and Co2meas = amb + CO2grow = elev and CO2meas = elev!
ambco2 <- subset(gmes, co2grow == "amb" & co2meas == "amb")
elevco2 <- subset(gmes, co2grow == "elev" & co2meas == "elev")
growCO2 <- rbind(ambco2,elevco2)
growCO2$Nm <- growCO2$N.perc *10
growCO2$Na <- growCO2$LMA * growCO2$Nm / 1000 # Narea in g m-2


# add some transformed variables such as for gmes, drawdown, mesolay.mean abd sumlenght.par.mean
growCO2$sqrt.gs <- sqrt(growCO2$Mean_gs)
growCO2$sqrt.gm <- sqrt(growCO2$gmes)
growCO2$log.dd <- log10(growCO2$Drawdown)
growCO2$sinsumlength <- sin(growCO2$sumlength.par.mean)
growCO2$sqrt.gmEgl <- sqrt(growCO2$Mean_gm.Eglob)
growCO2$log.Open <- log10(growCO2$Openness)

# plot objects -------------------------------------------------------------
palette(c("black", "red"))
pchs <- c(16,17)
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
walllab <- "Cell wall thickness  (units)"
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)


# now we redo the linear mixed model analyses as done before but with the appropriate transformations
# This data analyses in on old leaves only, measured in October 2014
# stats gmes --------------------------------------------------------------
mod_gmes <- lmer(sqrt.gm ~ co2grow*position + (1|ring/tree), data=growCO2, 
                 na.action = na.omit)
Anova(mod_gmes, test = "F")
VarCorr(mod_gmes)

plot(mod_gmes) # pretty good
qqPlot(residuals(mod_gmes)) # bit of an upwards tail on the left
test <- leveneTest(sqrt.gm ~ co2grow * position, data = growCO2)
summary(test) # not signficant so variance are equal
bwplot(gmes ~ position | co2grow , data = growCO2) # all ns different

# Now we analyse gmes with the measured CO2 concentrations (short term)
mod_gmes_meas <- lmer(sqrt.gm ~ co2meas*position + (1|ring/tree), data=gmes, 
                      na.action = na.omit)
Anova(mod_gmes_meas, test = "F")
VarCorr(mod_gmes_meas)
# and as seen before, there  is now a significant CO2 effect on gmes (P = 0.021) 
#where gm in eCO2 > gm in amb
visreg(mod_gmes_meas, "co2meas", by = "position", overlay = T, ylab = gmlab)
bargraph.CI(co2meas, gmes, position, data = gmes, legend = T, ylab = gmlab)
bargraph.CI(co2meas, gmes, data = gmes, legend = T, ylab = gmlab, xlab = "measured CO2")
bargraph.CI(co2grow, gmes, data = growCO2, legend = T, ylab = gmlab, xlab = "growth CO2")

# we should be seeing the same trends for gmes using globulus parameters
#(although now species-specific data is available)
mod_gmes_glob <- lmer(Mean_gm.Eglob ~ co2grow*position + (1|ring/tree), data=growCO2, 
                      na.action = na.omit)
Anova(mod_gmes_glob, test = "F")
VarCorr(mod_gmes_glob)

mod_gmes_glob_meas <- lmer(Mean_gm.Eglob ~ co2meas*position + (1|ring/tree), data=gmes, na.action = na.omit)
Anova(mod_gmes_glob_meas, test = "F")
# and that is the case: sign CO2 effect with CO2meas and ns with CO2grow
bargraph.CI(co2meas, Mean_gm.Eglob, data = gmes, legend = T, ylab = gmlab, xlab = "measured CO2")
# significance is stronger using gm.Eglob compared to gmes


# stats gs ----------------------------------------------------------------
mod_gs <- lmer(Mean_gs ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_gs, test = "F")
VarCorr(mod_gs)
# no Co2 by position interaction
visreg(mod_gs, "co2grow", by= "position", overlay =T, ylab = condlab)

mod_gs_meas<- lmer(Mean_gs ~ co2meas*position + (1|ring/tree), data=gmes, na.action = na.omit)
Anova(mod_gs_meas, test = "F")
# there are no differces in gs using co2meas - that is good!


# stats photosynthesis ----------------------------------------------------

mod_vcmax<- lmer(Vcmax25 ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_vcmax, test = "F")
VarCorr(mod_vcmax)

mod_jmax<- lmer(Jmax25 ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_jmax, test = "F")

mod_photo <- lmer(Photo ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_photo, test = "F")
visreg(mod_photo, "co2grow", by= "position", overlay =T, ylab = "Photo")

mod_Ci <- lmer(Ci ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_Ci, test = "F")

mod_Cc <- lmer(Mean_Cc ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_Cc, test = "F")
#drawdown
mod_dd <- lmer(log.dd ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_dd, test = "F")


# stats chemistry and lma -------------------------------------------------

mod_lma<- lmer(LMA ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_lma, test = "F")

mod_nitro <- lmer(N.perc ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_nitro, test = "F")
visreg(mod_nitro, "co2grow", by= "position", overlay =T, ylab = "N.perc")
# leaf N is reduced in eCO2 in the upper canopy only, 
# but same values in the lower canopy between the 2 cO2 treatments

mod_open <- lmer(log.Open ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_open, test = "F")


# stats leaf anatomy ------------------------------------------------------
mod_thick <- lmer(leafw ~ co2grow*position + (1|ring/Tree), data=growCO2, 
                  na.action = na.omit)
Anova(mod_thick, test = "F")
# used the leaf measurement tab from the anatomy, consistent across anatomical variables 
# values from the leaf measurements and cross section measurements are very similar
# strong position effect for leaf thickness (p = 0.039), logical

# test for difference in mesophyll layers and the average thickness of the mesophyll
mod_mesolayer <- lmer(mesolay.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_mesolayer, test = "F")
# again a strong position effect (p = 0.020)

mod_mesothick <- lmer(meso.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_mesothick, test = "F")
# again a strong position effect (p = 0.020)
# both have strong position effects (others that follow are ns)

# parenchyma cells: length and sum
mod_parlen <- lmer(length.par1.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_parlen, test = "F")

mod_parsum <- lmer(sinsumlength ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
Anova(mod_parsum, test = "F")
# these are not significant for any effects

# stomatal density: also ns
mod_sdup <- lmer(stomdenad.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit) #underside stomata
Anova(mod_sdup, test = "F")

mod_sdunder<- lmer(stomdenab.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit) #upperside stomata
Anova(mod_sdunder, test = "F")

mod_slup <- lmer(stomlengthad.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit) #underside stomata
Anova(mod_slup, test = "F")

mod_sdlow <- lmer(stomlengthab.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit) #underside stomata
Anova(mod_sdlow, test = "F")

mod_spiup <- lmer(spi.ad.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit) #underside stomata
Anova(mod_sdup, test = "F")

#check need for transformation!
mod_stomrat <- lmer(Stomratio ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit) #underside stomata
Anova(mod_stomrat, test = "F")

# end of linear mixed model analyses, separate script for figures and for function relationships using simple linear regressions





