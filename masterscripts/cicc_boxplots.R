#boxplots of gm gs An with canopy and eco2

source("masterscripts/functions.R")
setwd("C:/R-projects/EucFACE_gmes/masterscripts")
# read and format data ----------------------------------------------------
gmes <-read.csv("Master_data_file_clean-reconst.csv")
gmes2 <- gmes_format_func2(gmes)
##if analyzing gmes, drop tree 422
gmes3 <- gmes2[gmes2$tree != 422,]

#plot objects-----------------------
cibarlab2 <- expression(bar(Ci)~~(ppm))
cilab <- expression(italic(C)[i]~~(ppm))
cclab <- expression(italic(C)[c]~~(ppm))
cclab2 <- expression(bar(italic(C)[c])~~(ppm))
drawdownlab <- expression(italic(C)[i]-italic(C)[c]~~(mu*mol~mol^-1))
drawdownlab3 <- expression(italic(C)[i]-italic(C)[c]~~(paste(mu,bar,sep="")))
cols <- c("black", "black", "red", "red")

aclc<- expression(atop(Ambient~CO[2]), Low~canopy)
acuc<- expression(atop(Ambient~CO[2]), High~canopy)
eclc<- expression(atop(Elevated~CO[2]), Low~canopy)
ecuc<- expression(atop(Elevated~CO[2]), High~canopy)

#plotting---------------------------------------
# windows(12,6)
par(mfrow=c(1,3), mar=c(5,5,1,1), cex.lab=1.25)

boxplot(Mean_Ci~treatment, gmes3, ylab=cilab,names=FALSE, ylim=c(0,425), 
        outline=FALSE, border=cols)
mtext(aclc, side=1,at=1, line=2.5, cex=.7)
mtext(acuc, side=1,at=2, line=2.5, cex=.7)
mtext(eclc, side=1,at=3, line=2.5, cex=.7)
mtext(ecuc, side=1,at=4, line=2.5, cex=.7)
text(x=.55, y=425, "A", cex=1.25)

boxplot(Mean_Cc~treatment, gmes3, ylab=cclab,names=FALSE, ylim=c(0,425), 
        outline=FALSE, border=cols)
mtext(aclc, side=1,at=1, line=2.5, cex=.7)
mtext(acuc, side=1,at=2, line=2.5, cex=.7)
mtext(eclc, side=1,at=3, line=2.5, cex=.7)
mtext(ecuc, side=1,at=4, line=2.5, cex=.7)
text(x=.55, y=425, "B", cex=1.25)

boxplot(Drawdown~treatment, gmes3, ylab=drawdownlab,names=FALSE, ylim=c(0,425), 
        outline=FALSE, border=cols)
mtext(aclc, side=1,at=1, line=2.5, cex=.7)
mtext(acuc, side=1,at=2, line=2.5, cex=.7)
mtext(eclc, side=1,at=3, line=2.5, cex=.7)
mtext(ecuc, side=1,at=4, line=2.5, cex=.7)
text(x=.55, y=425, "C", cex=1.25)

dev.copy2pdf(file= "C:/R-projects/EucFACE_gmes/masterscripts/cicc_3box.pdf")
dev.off()