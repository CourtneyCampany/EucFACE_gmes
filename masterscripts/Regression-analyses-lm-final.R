rm(list = ls())
### Linear regression analyses to determine functional relationships between physiological and anatomical variables
setwd("C:/R-projects/EucFACE_gmes/masterscripts")
source("C:/R-projects/EucFACE_gmes/masterscripts/functions.R")

gmes <-read.csv("Master_data_file_clean-reconst.csv")
gmes2 <- gmes_format_func2(gmes)

##if analyzing gmes, drop tree 422
gmes_clean <- gmes2[gmes2$tree != 422,]


#stats-------------------------------
library(lme4)
library(car)
library(lmerTest)
library(LMERConvenienceFunctions)
library(MuMIn)
library(lattice)


levels(gmes_clean$co2grow)
palette(c("blue","red")) # with blue for ambient and red for elevated CO2
# in this case, the canopy position often helps extend the relationship axes but for completeness we will have to test
# significant models for interaction with canopy position, like we have done with CO2.
# if I have the overview right, then there are six relationships signficant out of the 18 tested!

#-------------------------------------------------------------------------------------------------------
gmes_clean$sqrt.gs <- sqrt(gmes_clean$Mean_gs)
gmes_clean$sqrt.gm <- sqrt(gmes_clean$gmes)
gmes_clean$log.dd <- log10(gmes_clean$Drawdown)
gmes_clean$sinsumlength <- sin(gmes_clean$sumlength.par.mean)
gmes_clean$sqrt.gm <- sqrt(gmes_clean$gmes)
gmes_clean$log.open <- log10(gmes_clean$Openness)


plot(gmes ~meanlength.par.mean, data = gmes_clean, pch=19, col = co2grow)
model1 <- lm(gmes ~meanlength.par.mean, data = gmes_clean)
coef(model1) #this is what I want, but now with R2 and p-value so we need really the summary of the model
summary(model1)
confint(model1)
polygon(c(0.3218, 1.1528),col = "grey75", border = FALSE) # does not work, need predictions of the model?
abline(model1b)
# so the intercept = 0.60 and slope = -0.013 (negative relationship), weak (R2 = 0.13) but significant (p = 0.034) relationship
# given the significan relationship, we investigate whether treatments are different (test in slope differences)
lmfit1 <- lm(gmes~meanlength.par.mean  * co2grow, data = gmes_clean)
summary(lmfit1)
# in this case the slope difference is P= 0.29
# now check for canopy position interaction
lmfit1c <- lm(gmes~meanlength.par.mean  * canopy, data = gmes_clean)
summary(lmfit1c) # no slope difference for canopy


plot(gmes~sumlength.par.mean, data = gmes_clean, pch=19, col = co2grow)
model1b <- lm(gmes~sumlength.par.mean, data = gmes_clean)
summary(model1b) # significant p = 0.034, not sure what the difference in between meanlenght and sumlength.par
# they are exactly the same except for the numbers on the x-axis

#Relationship between gm and leaf thickness
plot(gmes~leafw, data = gmes_clean, pch=19, col = co2grow)
model2 <- lm(gmes~leafw, data = gmes_clean)
abline(model2)
summary(model2)
#non-significant relationships p = 0.69

# relationship between gm and mean mesophyll thickness
plot(gmes~meso.mean, data = gmes_clean, pch=19, col = co2grow)
model3 <- lm(gmes~meso.mean, data = gmes_clean)
abline(model3)
summary(model3) #non-significant relationships p = 0.48

plot(gmes~mesolay.mean, data = gmes_clean, pch=19, col = co2grow)
model3c <- lm(gmes~mesolay.mean, data = gmes_clean)
abline(model3c)
summary(model3c) #non-significant relationships p = 0.63

#-----------------------------------------------------------------------
# relationship between gm and gs
plot(gmes~gs, data = gmes_clean, pch=19, col = co2grow)
model4 <- lm(gmes~gs, data = gmes_clean)
abline(model4)
summary(model4) #non-significant relationships p = 0.70

# relationship between gm and Ci-Cc (drawdown)
plot(gmes~Drawdown, data = gmes_clean, pch=19, col = co2grow)
model5 <- lm(gmes~Drawdown, data = gmes_clean)
abline(model5)
summary(model5) # not linear so need a different fit or we transform the vars

loggm <- (log10(gmes_clean$gmes))
logdrawd <- (log10(gmes_clean$Drawdown))
plot(loggm~logdrawd, data = gmes_clean, pch=19, col = co2grow)
model.log <- lm(loggm~logdrawd, data = gmes_clean)
abline(model.log)
summary(model.log) # good relationship R2 = 0.68, P<0.0001
lmfit5d <- lm(loggm~logdrawd * co2grow, data = gmes_clean)
summary(lmfit5d) # no difference in slope or intercept with treatments - good

plot(gmes~Mean_Cc, data = gmes_clean, pch=19, col = co2grow)
model5b <- lm(gmes~Mean_Cc, data = gmes_clean)
abline(model5b)
summary(model5b) # very significant relationship, as expected, R2 = 0.45, p = <0.0001
#Test for slope differences
lmfit5b <- lm(gmes~Mean_Cc * co2grow, data = gmes_clean)
summary(lmfit5b) #and that shows no differences, so the plot is fit across CO2 treatments
lmfit5c <- lm(gmes~Mean_Cc * canopy, data = gmes_clean)
summary(lmfit5c) # and no slope differences between canopy positions

#------------------------------------------------------------------------
# LMA and gm and LMA and leaf thickness (leafw)
plot(gmes~LMA, data = gmes_clean, pch=19, col = co2grow)
model6 <- lm(gmes~LMA, data = gmes_clean)
abline(model6)
summary(model6) #non-significant relationships p = 0.67

plot(gmes~log.open, data = gmes_clean, pch=19, col = co2grow)
model6b <- lm(gmes~log.open, data = gmes_clean)
abline(model6b)
summary(model6b) 

plot(LMA~leafw, data=gmes_clean, pch=19, col = co2grow)
model7 <- lm(LMA~leafw, data = gmes_clean)
abline(model7)
summary(model7) #significant relationships p = 0.004, R2 = 0.25
# test for slope differences
lmfit7b <- lm(LMA~leafw * co2grow, data = gmes_clean)
summary(lmfit7b) #and that both slope and intercept marginal differences (P = 0.075)

library(visreg)
visreg(lmfit7b, "leafw", by="co2grow", overlay=TRUE)
# so the weak relationship is driven by ambient CO2 while elevated CO2 does not change LMA with CO2 treatment (this is a bit strange)
lmfit7c <- lm(LMA~leafw * canopy, data = gmes_clean)
summary(lmfit7c) # and no slope differences between canopy positions

plot(LMA~meso.mean, data = gmes_clean, pch=19, col = co2grow)
model15<- lm(LMA~meso.mean, data = gmes_clean)
abline(model15)
summary(model15) # weak but significant, R2 = 0.22 and p = 0.007, seems driven by relationship in aCO2.
lmfit15b<- lm(LMA~meso.mean * co2grow, data = gmes_clean)
summary(lmfit15b)
visreg(lmfit15b, "meso.mean", by="co2grow", overlay=TRUE)

plot(LMA~sumlength.par.mean, data = gmes_clean, pch=19, col = co2grow)
model14<- lm(LMA~sumlength.par.mean, data = gmes_clean)
abline(model14)
summary(model14) #non-significant relationships p = 0.20

# photosynthesis with gm and gs relationships
plot(Photo~gs, data=gmes_clean, pch=19, col = co2grow)
model8 <- lm(Photo~gs, data = gmes_clean)
abline(model8)
summary(model8) # r2 = 0.36 AND P = 0.0004
# again a test if slopes are different with co2 treatment
lmfit2 <- lm(Photo~gs * co2grow, data = gmes_clean)
summary(lmfit2) #shows no different slope and a marginal difference in intercept (p=0.064), will ignore
lmfit2c <- lm(Photo~gs * canopy, data = gmes_clean)
summary(lmfit2c) #no differences in canopy

plot(Photo~gmes, data=gmes_clean, pch=19, col = co2grow)
model9 <- lm(Photo~gmes, data = gmes_clean)
abline(model9)
summary(model9)  # marginally significant relationships p = 0.07
#BUT when fit with mean_gm.Eglob, then the relationship is very significant!! p = 0.001 and R2 =0.31
plot(Photo~Mean_gm.Eglob, data=gmes_clean, pch=19, col = co2grow)
model9b <- lm(Photo~ Mean_gm.Eglob, data = gmes_clean)
abline(model9b)
summary(model9b)  #yes! r2 = 0.31 and p = 0.0012

lmfit4 <- lm(Photo~Mean_gm.Eglob*co2grow, data = gmes_clean)
summary(lmfit4) # sign intercept difference
visreg(lmfit4, "Mean_gm.Eglob", by="co2grow", overlay=TRUE) #nice!
lmfit4c <- lm(Photo~Mean_gm.Eglob*canopy, data = gmes_clean)
summary(lmfit4c) # and no sign difference so relationship across canopy is fine.


# gm-spi.ad
plot(gmes ~ spi.ad.mean, data=gmes_clean, pch=19, col = co2grow)
model10 <- lm(gmes ~ spi.ad.mean, data = gmes_clean)
abline(model10)
summary(model10) # not significant

plot(gs ~ spi.ad.mean, data=gmes_clean, pch=19, col = co2grow)
model10b <- lm(gs ~ spi.ad.mean, data = gmes_clean)
abline(model10b)
summary(model10b) # not significant, even when lower point of t523 is removed.

plot(gs ~ stomdenab.mean, data=gmes_clean, pch=19, col = co2grow)
model10c <- lm(gs ~ stomdenab.mean, data = gmes_clean)
abline(model10c)
summary(model10c) # not significant

plot(gs ~ Stomratio, data=gmes_clean, pch=19, col = co2grow)
model10d <- lm(gs ~ Stomratio, data = gmes_clean)
abline(model10d)
summary(model10d) # not significant

#----------------------------------------
# relationships with Vcmax25
# and nitrogen, Na
# and gm
# and LMA
plot(Vcmax25 ~ Na, data=gmes_clean, pch=19, col = co2grow)
model11 <- lm(Vcmax25 ~ Na, data = gmes_clean)
abline(model11)
summary(model11) #not significant p = 0.76

plot(gmes~Vcmax25, data=gmes_clean, pch=19, col = co2grow)
model12 <- lm(gmes~Vcmax25, data = gmes_clean)
abline(model12)
summary(model12) #not significant p = 0.96

plot(Vcmax25 ~ LMA, data=gmes_clean, pch=19, col = co2grow)
model13 <- lm(Vcmax25 ~ LMA, data = gmes_clean)
abline(model13)
summary(model13) #not significant p = 0.73

# we can redo these relationships including the co2measurement points as well. It usually does not make a big difference, see table in my notebook
# would be nice to add confidence intervals if possible, with polygon


