##gmes and anatomy 


#gmes and leaf internal anatomy
source("C:/R-projects/EucFACE_gmes/masterscripts/functions.R")

# read and format data ----------------------------------------------------
#data <-read.csv("C:/R-projects/EucFACE_gmes/masterscripts/Master_data_file_clean.csv")
#data2 <-read.csv("C:/R-projects/EucFACE_gmes/masterscripts/Tree-means_Gm-master2.csv")
data <-read.csv("Master_data_file_clean-reconst.csv")

gmes <- gmes_format_func(data)

ac <- gmes[gmes$co2grow == "amb",]
ec <- gmes[gmes$co2grow == "elev",]
low <- gmes[gmes$canopy == "lower",]
upp <- gmes[gmes$canopy == "upper",]

#stats-------------------------------
# library(lme4)
# library(car)
# library(lmerTest)
# library(LMERConvenienceFunctions)
# library(MuMIn)
# 
# #gm and sum parachencyma
# fit_parasum <- lmer(gmes~sumlength.par.mean*co2grow*canopy + (1|ring/tree),
#                     data=gmes2,na.action = na.omit)
# Anova(fit_parasum, test = "F")
# r.squaredGLMM(fit_parasum)
# #overall gm is correlated with sum para cell length P=0.01
# 
# #gm and mean parachenyma cell length
# fit_meanlength <- lmer(gmes~meanlength.par.mean*co2grow*canopy + (1|ring/tree),
#                           data=gmes2,na.action = na.omit)
# Anova(fit_meanlength, test = "F") #ignoring .07 interactions (depends on what we do)
# r.squaredGLMM(fit_meanlength)
# ##same as para sum length (maybe not use?????? as they are similar traits)
# 
# fit_meso<- lmer(gmes~meso.mean.y*co2grow*canopy + (1|ring/tree),
#                        data=gmes2,na.action = na.omit)
# Anova(fit_meso, test = "F") #intertaction with mesophyll thickness and co2 treatment
# r.squaredGLMM(fit_meso)
# mesoac <- mean(ac$meso.mean.y)
# mesoec <- mean(ec$meso.mean.y)
#  #mesopyll thickness higher in eco2
# fit_meso2 <- lmer(gmes~meso.mean.y*co2grow + (1|ring/tree),
#                   data=gmes2,na.action = na.omit)
# Anova(fit_meso2, test = "F") #intertaction with mesophyll thickness and co2 treatment
# r.squaredGLMM(fit_meso2)


# plotting----------------------------------------
##2 panels with gm and parenca thickness and mesophyll length

palette(c("black", "red"))
pchs <- c(16,17)
pchs2 <- c(1,2)
alab <-expression(italic(A)[net]~~(umol~m^-2~s^-1))
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
gslab <-expression(italic(g)[s]~~(mol~m^-2~s^-1))
tpc_lab <- expression(Parenchyma~cell~thickness~~(mu*m))
paralab <- expression(Total~parenchyma~cell~length~~(mu*m))
mesolab <- expression(Mesophyll~thickness~~(mu*m))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)
legpch2 <- c(1,1,1,2)

#linear models for regression line
fit_Ags <- lm(Photo~Mean_gs,data=gmes)
fit_Agm <- lm(Photo~gmes,data=gmes)
fir_AgmEg <- lm(Photo~Mean_gm.Eglob,data=gmes)
library(scales)
library(plotrix)

#2panel plots-# need extra panel---------------------------------------
windows(10,6)
par(mfrow=c(1,2), las=1, mgp=c(3,1,0), oma=c(5,5,1,1)) #I changed to 3 but left everything else the same

par(mar=c(0,0,0,0),xpd=TRUE )
plot(Photo ~ Mean_gs, data=gmes ,type='n', ylim=c(0, .55),xlim=c(0, 0.50))
ablineclip(fit_Ags, x1=min(gmes$Mean_gs),x2=max(gmes$Mean_gs), # not working
           lwd=2, lty=2)
points(gmes ~ Mean_gs, data=gmes, col=co2grow, pch=pchs[canopy], cex=1.25)
mtext(side=2, at=.275, line=3,text=alab, xpd=TRUE, las=3, cex=1.25)
mtext(side=1, at=0.25, line=3,text=gslab, xpd=TRUE, las=1, cex=1.25) 
text('A', x=0.02, y=.55, cex=1.25)

par(mar=c(0,0,0,0),xpd=TRUE )
plot(Photo~gmes, data=gmes, ylim=c(0, .55), yaxt='n', xlim=c(0, 0.5), type='n')
axis(2, labels=FALSE)
ablineclip(fit_Agm, x1 = min(gmes$gmes), x2=max(gmes$gmes), lty=2, lwd=2) #not working
points(Photo ~ gmes, data=gmes, col=co2grow, pch=pchs2[canopy], cex=1.25)
points(Photo ~ gmes, data=ac, col=co2grow, pch=pchs[canopy], cex=1.25)
mtext(side=1, at=0.3, line=3,text=gmlab, xpd=TRUE, las=1, cex=1.25)
text('B', x=0.02, y=.55, cex=1.25)
legend("topright", leglab, pch=legpch2, col=allcols,inset = 0.01, bty='n',cex=1)

dev.copy2pdf(file= "master_scripts/A-gs_gm_fig3.pdf")
dev.off()

