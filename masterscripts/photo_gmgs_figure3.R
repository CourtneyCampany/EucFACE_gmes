#photo vs gm/gs panel
source("masterscripts/functions.R")
setwd("C:/R-projects/EucFACE_gmes/masterscripts")
# read and format data ----------------------------------------------------
gmes <-read.csv("Master_data_file_clean-reconst.csv")
gmes2 <- gmes_format_func2(gmes)
##if analyzing gmes, drop tree 422
gmes3 <- gmes2[gmes2$tree != 422,]

ac <- gmes3[gmes3$co2grow == "amb",]
ec <- gmes3[gmes3$co2grow == "elev",]
low <- gmes3[gmes3$canopy == "lower",]
upp <- gmes3[gmes3$canopy == "upper",]

#stats-------------------------------------
# library(lme4)
# library(car)
# library(lmerTest)
# library(LMERConvenienceFunctions)
# library(MuMIn)

##gs and photo
# mod_photo_cond <- lmer(Photo ~ mean_gs*co2grow*canopy + (1|ring/tree), 
#                        data=gmes2,na.action = na.omit)
# Anova(mod_photo_cond, test = "F") #all leaves p<0.001
# r.squaredGLMM(mod_photo_cond)
# 
# ##gm and photo
# mod_photo_gm <- lmer(Photo ~ gmes*co2grow*canopy  + (1|ring/tree), data=gmes2,
#                        na.action = na.omit)
# Anova(mod_photo_gm, test = "F") #nothing with gmes
# r.squaredGLMM(mod_photo_gm)
# 
# ##gm and gs
# mod_gs_gm <- lmer(mean_gs ~ gmes*co2grow*canopy + (1|ring/tree), data=gmes2,
#                      na.action = na.omit)
# Anova(mod_gs_gm, test = "F") #nothing
# r.squaredGLMM(mod_gs_gm)

#simple linear models for best fit lines----------------------
fit_ags <- lm(Photo ~ Mean_gs, data = gmes3) 
fit_agm <- lm(Photo ~ gmes, data = gmes3) 


#plotpobjects--------------------------------------
palette(c("black", "red"))
pchs <- c(16,17)
pchs2 <- c(1,2)
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)
legpch2 <- c(1,1,1,2)
library(plotrix)

#2panel plots----------------------------------------
# windows(10,6)
par(mfrow=c(1,2), las=1, mgp=c(3,1,0), oma=c(5,5,1,1))

par(mar=c(0,0,0,0),xpd=TRUE )
plot(Photo ~ Mean_gs, data=gmes3,ylim=c(0,30), xlim=c(0, .55), type='n', ylab="", xlab="")
ablineclip(fit_ags, x1=min(gmes3$Mean_gs),x2=max(gmes3$Mean_gs), lwd=2, lty=2)
points(Photo ~ Mean_gs, data=gmes3, col=co2grow, pch=pchs[canopy], cex=1.25)
mtext(side=2, at=15, line=3,text=photolab, xpd=TRUE, las=3, cex=1.25)
mtext(side=1, at=.2, line=3,text=condlab, xpd=TRUE, las=1, cex=1.25)
text('A', x=0, y=30, cex=1.25)

par(mar=c(0,0,0,0), xpd=TRUE)
plot(Photo ~ gmes, data=gmes3, type='n', ylim=c(0,30), xlim=c(0,.55),yaxt='n')
axis(2, labels=FALSE)
#ablineclip(fit_agm, x1=min(gmes3$gmes),x2=max(gmes3$gmes), lwd=2, lty=2)
legend("topright", leglab, pch=legpch2, col=allcols,inset = 0.01, bty='n',cex=1)
points(Photo ~ gmes, data=gmes3, col=co2grow, pch=pchs2[canopy], cex=1.25)
mtext(side=1, at=.28, line=3,text=gmlab, xpd=TRUE, las=1, cex=1.25)
text('B', x=0, y=30, cex=1.25)


dev.copy2pdf(file= "masterscripts/photo_gmgs.pdf")
dev.off()
