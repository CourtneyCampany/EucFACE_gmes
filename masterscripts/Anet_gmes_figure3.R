##gmes and anatomy 


#gmes and leaf internal anatomy
# source("C:/R-projects/EucFACE_gmes/masterscripts/functions.R")
source("masterscripts/functions.R")

# read and format data ----------------------------------------------------
#data <-read.csv("C:/R-projects/EucFACE_gmes/masterscripts/Master_data_file_clean.csv")
#data2 <-read.csv("C:/R-projects/EucFACE_gmes/masterscripts/Tree-means_Gm-master2.csv")
data <-read.csv("masterscripts/Master_data_file_clean-reconst.csv")

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


# plotting----------------------------------------
##2 panels with gm and parenca thickness and mesophyll length

palette(c("black", "red"))
pchs <- c(16,17)
pchs2 <- c(1,2)
alab <-expression(italic(A)[net]~~(umol~m^-2~s^-1))
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
gmlab2 <-expression(italic(g)[m_E.globulus]~~(mol~m^-2~s^-1))
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
fit_AgmEg <- lm(Photo~Mean_gm.Eglob,data=gmes)
library(scales)
library(plotrix)
##I looked at the paper and we state that there are general relationships
##with A-gs, A-gm_glob (marginal with A-gm) across all treatments
##im not going to add in the mixed model scripts here as they are not needed
##just draw the regression with simple models.

#3panel plots-# need extra panel---------------------------------------
# windows(12,6)
png(filename = "output/Figure3.png", height = 6, width = 12, units = "in", res= 400)

par(mfrow=c(1,3), las=1, mgp=c(3,1,0), oma=c(5,5,1,1)) #I changed to 3 but left everything else the same

#a-gs
par(mar=c(0,0,0,0),xpd=TRUE )
plot(Photo ~ Mean_gs, data=gmes ,type='n',xlab="", ylab="", 
     xlim=c(0, .5), ylim=c(0, 30))
ablineclip(fit_Ags, x1=min(gmes$Mean_gs), x2=max(gmes$Mean_gs), lty=2, lwd=2)
points(Photo ~ Mean_gs, data=gmes, col=co2grow, pch=pchs[canopy], cex=1.25)
mtext(side=2, at=15, line=3,text=alab, xpd=TRUE, las=3, cex=1)
mtext(side=1, at=0.25, line=3,text=gslab, xpd=TRUE, las=1, cex=1) 
text('A', x=0., y=30, cex=1.5)

#a-gm (tobbaco)
par(mar=c(0,0,0,0),xpd=TRUE )
plot(Photo~gmes, data=gmes, yaxt='n', ylab="", xlab="",
     xlim=c(0, 0.5), ylim=c(0, 30),type='n')
axis(2, labels=FALSE)
ablineclip(fit_Agm, x1 = min(gmes$gmes, na.rm=TRUE), x2=max(gmes$gmes, na.rm=TRUE),
           lty=2, lwd=2) #not working
points(Photo ~ gmes, data=gmes, col=co2grow, pch=pchs[canopy], cex=1.25)
mtext(side=1, at=0.25, line=3,text=gmlab, xpd=TRUE, las=1, cex=1)
text('B', x=0, y=30, cex=1.5)

#a=gm(globulus)
par(mar=c(0,0,0,0),xpd=TRUE )
plot(Photo~gmes, data=gmes, yaxt='n', ylab="", xlab="",
     xlim=c(0, 0.5), ylim=c(0, 30),type='n')
axis(2, labels=FALSE)
ablineclip(fit_AgmEg, x1 = min(gmes$Mean_gm.Eglob, na.rm=TRUE),
           x2=max(gmes$Mean_gm.Eglob, na.rm=TRUE),lty=2, lwd=2)
points(Photo ~ Mean_gm.Eglob, data=gmes, col=co2grow, pch=pchs[canopy], cex=1.25)
mtext(side=1, at=0.25, line=3,text=gmlab2, xpd=TRUE, las=1, cex=1)
text('C', x=0, y=30, cex=1.5)
legend("topright", leglab, pch=legpch2, col=allcols,inset = 0.01, bty='n',cex=1.25)

# dev.copy2pdf(file= "output/Figure3.pdf")
dev.off()

