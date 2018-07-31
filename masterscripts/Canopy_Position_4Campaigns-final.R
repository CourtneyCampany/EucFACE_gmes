
### Analysis of EucFACE canopy paper across 4 campaigns where upper and lower canopy positions were measured
# Clean up workspace
rm(list = ls())
library(doBy)
library(lattice)
library(lme4)
library(car)
library(visreg)
library(lmerTest)
library(sciplot)

# Set working directory
setwd("C:/R-projects/EucFACE_gmes/masterscripts")
#read in datafile
across <-read.csv("Tree_aver_age_position_EucFACE_corr.csv") 
#data for four campaigns where we have upper and lower canopy results, including analyses of light environment

str(across)
across$Year <- as.factor(across$Year)
across$Ring <- as.factor(across$Ring)
across$Na <- (across$LMA * across$Leaf.N) / 100

#Create transformed variables based on the datachecks
# Vcmax, gs and Na need a square root transformation
across$sqrt.Vc <- sqrt(across$Vcmax)
across$sqrt.gs <- sqrt(across$gs)
across$sqrt.Na <- sqrt(across$Na)


### Mixed model analyses ######
### Variable : Photo
####################

m1f2b <- lmer(Photo ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f2b, test = "F")
# sign C.treat at P = 0.02 and strong Position (P < 0.0001), no interactions

plot(m1f2b) 
qqPlot(residuals(m1f2b))
# looks very nice
test <- leveneTest(Photo ~ C.treat * Campaign, data = across)
summary(test) # significant

visreg(m1f2b, "Age", by = "Position", overlay = T, ylab = "Photo")
bargraph.CI(Age, Photo, Position, data = across, legend = T, ylab = "Photo") 

boxplot(Photo~C.treat, data=across)
boxplot(Photo~Position, data=across)
boxplot(Photo~Age+Position, data=across, ylab = "Photo")


#----------------------------------------------------------------------------------------------------
##################
# Variable: Vcmax
##################

m1f3b <- lmer(sqrt.Vc ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f3b, test = "F")
# so both age and position show very sign difference, no C.treat:Age interaction

# Testing the assumptions 
plot(m1f3b) 
qqPlot(residuals(m1f3b))
# looks good
test <- leveneTest(sqrt.Vc ~ C.treat * Campaign, data = across)
summary(test) # when significant, then there are no equal variances
## so qqplot shows normality, residuals are fine but variances are not equal ==> how to fix?

#Visualising the data 
visreg(m1f3b, "Position", by = "C.treat", overlay = T, ylab = "Vcmax")
bwplot(Vcmax ~ Position | Age:C.treat, data=across)
bargraph.CI(Age, Vcmax, Position, data = across, legend = T, ylab = "Vcmax")
# no interaction Vcmax is higher in new compared to old leaves and always higher in upper canopy than lower canopy regardless of leaf age

# well I think I settled on what mixed model to use but I  need posthoc test 
# POSTHOC tests ###

#install.packages("multcomp")
library(multcomp)
# Tukey is only usefull without the interactions, so let's look at some multiple comparisons for Campaign 
# to understand what is going on with the main factors
m2Vcmax <- lmer(sqrt.Vc ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)
summary(glht(m2Vcmax, linfct=mcp(Position = "Tukey")))# very significant!!
summary(glht(m2Vcmax, linfct=mcp(Age = "Tukey"))) # very significant, age here is across campaigns
summary(glht(m2Vcmax, linfct=mcp(C.treat = "Tukey")))# P = 0.066 with transformed variable


#-----------------------------------------------------------------------------------------------------
# Variable Jmax
################

# Mixed model 
m1f4b <- lmer(Jmax ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f4b, test = "F")

## Visualising data
m1f4c <- lmer(Jmax ~ C.treat*Age* Position + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f4c, test = "F")
visreg(m1f4c, "Age", by = "Position", overlay = T, ylab = "Jmax")
bargraph.CI(Age, Jmax, Position, data = across, legend = T, ylab = "Jmax") 
# Position:Age interaction explained by reduction in Jmax in upper campaign from new to old but not in the lower.

## Testing assumptions
plot(m1f4b) 
qqPlot(residuals(m1f4b))
# looks good
test <- leveneTest(Jmax ~ C.treat * Campaign, data = across)
summary(test) # not significant here

## further posthoc tests
m2Jmax <- lmer(Jmax ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)
summary(glht(m2Jmax, linfct=mcp(Position = "Tukey")))# very significant!!
summary(glht(m2Jmax, linfct=mcp(Age = "Tukey"))) # very significant, age here is across campaigns
summary(glht(m2Jmax, linfct=mcp(C.treat = "Tukey")))# ns

#-------------------------------------------------------------------------------------------------
# LMA ################
######################

m1f5b <- lmer(LMA ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f5b, test = "F")
# Strong postion effect and sign Age effect with strong Position:Age interaction (and a slight C.treat:Age interaction)
visreg(m1f5b, "C.treat", by = "Age", overlay = T, ylab = "LMA")
# LMA only increased in eCO2 in new leaves, but no CO2 effect in old leaves where LMA's between C.treat are the same
# So perhaps only a CO2 effect in upper new leaves
bwplot(LMA ~ C.treat | Age:Position, data=across)
bwplot(LMA ~ C.treat | PosAge, data=across)

visreg(m1f5b, "Age", by = "Position", overlay = T, ylab = "LMA")
# no LMA differences in new leaves but LMA sign. increased in old leaves in upper canopy (but not in lower canopy)
# explained by significuant position by age effect
bargraph.CI(Age, LMA, Position, data = across, legend = T, ylab = "LMA") 
bargraph.CI(PosAge, LMA, C.treat, data = across, legend = T, ylab = "LMA") 

## Testing assumptions
plot(m1f5b) 
qqPlot(residuals(m1f5b))
# looks good
test <- leveneTest(LMA ~ C.treat * Campaign, data = across)
summary(test) # not significant here

# some posthoc tests - drop interactions from the model
phLMA2 <- lmer(LMA ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)

summary(glht(phLMA2, linfct=mcp(Age = "Tukey")))      # P = 0.03
summary(glht(phLMA2, linfct=mcp(Position = "Tukey"))) # very significant!!
summary(glht(phLMA2, linfct=mcp(C.treat = "Tukey")))  # ns

#------------------------------------------------------------------------------------------------
# gs ##########
###############
m1f6b <- lmer(sqrt.gs ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f6b, test = "F")
# nothing is different except for a CO2 effect (P = 0.04) and slight position effect (P = 0.065)
boxplot(sqrt.gs~C.treat, data=across)
visreg(m1f6b,"C.treat", by = "Position", overlay = T, ylab = "gs")
# lower has significantly reduced gs in eCO2 whereas upper canopy leaves do not differ in gs between cO2 treatments

## Testing assumptions
plot(m1f6b) 
qqPlot(residuals(m1f6b))
# looks great
test <- leveneTest(sqrt.gs ~ C.treat * Position, data = across)
summary(test) #  not significant 

## More visualising
visreg(m1f6b, "Age", by = "Position", overlay = T, ylab = "gs")
bargraph.CI(Age, gs, Position, data = across, legend = T, ylab = "gs") 
bargraph.CI(Position, gs, data = across, legend = T, ylab = "gs") # upper gs > lower gs
bargraph.CI(C.treat, gs, data = across, legend = T, ylab = "gs") # CO2 higher in 0C than +C

# some posthoc tests - drop interactions from the model
phgs2 <- lmer(sqrt.gs ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)

summary(glht(phgs2, linfct=mcp(Age = "Tukey")))      # ns
summary(glht(phgs2, linfct=mcp(Position = "Tukey"))) # 0.06
summary(glht(phgs2, linfct=mcp(C.treat = "Tukey")))  # sign. 0.0025

#--------------------------------------------------------------------------------------------
# leaf N  ##########
###############
m1f7b <- lmer(Leaf.N ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f7b, test = "F")
# the model shows a sign. Age effect (P = 0.024) and a sign. CO2:Age interaction (P = 0.043)
boxplot(Leaf.N~Age, data = across)
visreg(m1f7b,"C.treat", by = "Age", overlay = T, ylab = "Leaf N")
# this interaction shows a sign CO2 effect in new leaves but not in Old leaves => do posthoc test
## Testing assumptions
plot(m1f7b) 
qqPlot(residuals(m1f7b))
# looks great
test <- leveneTest(Leaf.N ~ C.treat * Position, data = across)
summary(test) #  not significant
bargraph.CI(PosAge, Leaf.N, C.treat, data = across, legend = T, ylab = "Leaf N") # CO2 effect in upper new leaves only
visreg(m1f7b,"C.treat", by = "Age", overlay = T, ylab = "Leaf N")
visreg(m1f7b,"C.treat", by = "Position", overlay = T, ylab = "Leaf N")

# some posthoc tests - drop interactions from the model
phN <- lmer(Leaf.N ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)
summary(glht(phN, linfct=mcp(Age = "Tukey")))      # P= 0.038
summary(glht(phN, linfct=mcp(Position = "Tukey"))) # ns
summary(glht(phN, linfct=mcp(C.treat = "Tukey")))  # ns

# Narea
########
m1f8 <- lmer(sqrt.Na ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f8, test = "F")
# significant Age and position but also strongly significant interaction between age and position
visreg(m1f8, "Age", by = "Position", overlay = T, ylab = "Na")
#increase of Na when leaves get older in the upper canopy is and LMA effect while the decrease of Na in lower canopy as leaves get older is a leafN effect. Correct?


# Canopy Openness
##################
m1f9 <- lmer(log.Open. ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f9, test = "F")
visreg(m1f9,"C.treat", by = "Position", overlay = T, ylab = "Canopy Openness")


#------------------------------------------------------------------------------------------------- 

# and some linear relationships across campaigns
across$Na <- (across$LMA * across$Leaf.N) / 100

levels(across$C.treat)
palette(c("red","blue")) # here blue = ambient CO2 and red = elevated!!

plot(Vcmax ~ Na, data=across, pch=19, col = C.treat)
model1 <- lm(Vcmax ~ Na, data = across)
abline(model1)
summary(model1) #not significant p = 0.82 ==> split old and new leaves
    old_Vc <- subset(across, Age == "old")
    plot(Vcmax ~ Na, data=old_Vc, pch=19, col = C.treat)
    model1b <- lm(Vcmax ~ Na, data = old_Vc)
    abline(model1b)
    summary(model1b) # R2 = 0.17 and p = 0.0006, this relationship is weak but driven by old leaves
    
    new_Vc <- subset(across, Age == "new")
    plot(Vcmax ~ Na, data=new_Vc, pch=19, col = C.treat)
    model1c <- lm(Vcmax ~ Na, data = new_Vc)
    abline(model1c)
    summary(model1c) # P = 0.97 non-significant for new leaves!!
    
plot(Vcmax ~ LMA, data=across, pch=19, col = C.treat)
model1d <- lm(Vcmax ~ LMA, data = across)
abline(model1d)
summary(model1d) #ns, p = 0.06 with R2 = 0.02!
    plot(Vcmax ~ LMA, data=old_Vc, pch=19, col = C.treat)
    model1d2 <- lm(Vcmax ~ LMA, data = old_Vc)
    abline(model1d2)
    summary(model1d2) #ns, p = 0.01 with R2 = 0.09, not worth it.

plot(LMA ~ Na, data=across, pch=19, col = C.treat)
model2 <- lm(LMA ~ Na, data = across)
abline(model2)
summary(model2) #significant p < 0.0001 and R2 = 0.41 indicating the LMA and NA covary in the canopy. High LMA and high N go together

plot(LMA ~ log.Open., data=across, pch=19, col = C.treat)
model3 <- lm(LMA ~ log.Open., data = across)
abline(model3)
summary(model3) #significant p = 0.0001 and R2 = 0.19, so weak relationship but openness does correlate with LMA

plot(Na ~ log.Open., data=across, pch=19, col = C.treat)
model3b <- lm(Na ~ log.Open., data = across)
abline(model3b)
summary(model3b) #significant p = 0.002 and R2 = 0.11, weaker than LMA

plot(Jmax ~ Vcmax, data=across, pch=19, col = C.treat)
model4 <- lm(Jmax ~ Vcmax, data = across)
abline(model4)
summary(model4) #very significant p < 0.0001 and R2 = 0.70 across canopy and CO2
lmfit4 <- lm(Jmax ~ Vcmax  * Position, data = across)  # no CO2 difference, but a different intercept for canopy position (p = 0.040)
summary(lmfit4)
  up_Vc <- subset(across, Position == "upper")
  plot(Jmax ~ Vcmax, data=up_Vc, pch=19, col = C.treat)
  model4b <- lm(Jmax ~ Vcmax, data = up_Vc)
  abline(model4b)
  summary(model4b) # upper position leaves P = 0.65 and R2 < 0.0001
  
  low_Vc <- subset(across, Position == "lower")
  plot(Jmax ~ Vcmax, data=low_Vc, pch=19, col = C.treat)
  model4c <- lm(Jmax ~ Vcmax, data = low_Vc)
  abline(model4c)
  summary(model4c) # lower position leaves P = 0.73 and R2 < 0.0001. lower has the higher intercept but that may be due to different sample size?
  

plot(Photo ~ Na, data=across, pch=19, col = C.treat)
model5 <- lm(Photo ~ Na, data = across)
abline(model5)
summary(model5) #significant (P = 0.029) but R2 = 0.036...
lmfit5 <- lm(Photo ~ Na  * Age, data = across)  # relationship depends on age, both in intercept and slopes
summary(lmfit5)
    plot(Photo ~ Na, data=old_Vc, pch=19, col = C.treat)
    model5b <- lm(Photo ~ Na, data = old_Vc)
    abline(model5b)
    summary(model5b) # R2 = 0.22 and P = 0.0001 across CO2 treatments but they have diff intercepts?
    lmfit5b <- lm(Photo ~ Na  * C.treat, data = old_Vc)  # testing this shows NOT, so can draw across treatments
    summary(lmfit5b)
    
    
    plot(Photo ~ Na, data=new_Vc, pch=19, col = C.treat)
    model5c <- lm(Photo ~ Na, data = new_Vc)
    abline(model5c)
    summary(model5c) #no significant relationship in new leaves flat line!)