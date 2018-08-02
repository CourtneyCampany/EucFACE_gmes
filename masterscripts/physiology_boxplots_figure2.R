##physiology boxplots

source("masterscripts/functions.R")

# setwd("C:/R-projects/EucFACE_gmes/masterscripts")
#gmes and leaf internal anatomy
# source("C:/R-projects/EucFACE_gmes/masterscripts/functions.R")

# read and format data ----------------------------------------------------
#data <-read.csv("C:/R-projects/EucFACE_gmes/masterscripts/Master_data_file_clean.csv")
#data2 <-read.csv("C:/R-projects/EucFACE_gmes/masterscripts/Tree-means_Gm-master2.csv")
# data <-read.csv("Master_data_file_clean-reconst.csv")
data <-read.csv("masterscripts/Master_data_file_clean-reconst.csv")

gmes <- gmes_format_func(data)

##can choose here which bad points to delete
not422 <- gmes[gmes$tree != "422",]
#not408 <- gmes[gmes$tree != "408",]

#plot objects-----------------------
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
cols <- c("black", "black", "red", "red")
cilab <- expression(italic(C)[i]~~(ppm))
cclab <- expression(italic(C)[c]~~(ppm))
cols <- c("black", "black", "red", "red")

lc<- "Low"
uc<- "High"
ac<- expression(Ambient~CO[2])
ec<- expression(Elevated~CO[2])

canopy <- c(lc, uc, lc, uc)
co2lab <- c(ac, ec)

#plotting---------------------------------------
# windows()
png(filename = "output/Figure2.png", height = 11.3, width = 8, units = "in", res= 400)
par(mfrow=c(2,2), oma=c(5,5,1,5),mar=c(0,0,0,0), cex.lab=1.25)
#gs
boxplot(Mean_gs~treatment, not422, ylab="",names=FALSE, ylim=c(0,.55), 
        outline=FALSE, border=cols, xaxt='n')
text(x=.55, y=.55, "A", cex=1.25)
mtext(condlab, side=2, line=3)

#gm
boxplot(gmes~treatment, not422, ylab="",names=FALSE, ylim=c(0,.55), 
        outline=FALSE, border=cols, xaxt='n', yaxt='n')
text(x=.55, y=.55, "B", cex=1.25)
mtexti(gmlab, 4, outer=TRUE, cex=1.25, off=.6)
axis(4, labels=TRUE)

#ci
boxplot(Mean_Ci~treatment, not422, ylab="",names=FALSE, ylim=c(0,435), 
        outline=FALSE, border=cols)
mtext(canopy, side=1,at=1:4, line=1, cex=.9)
mtext(co2lab, side=1, at=c(1.5, 3.5),line=3, cex=.9)
text(x=.55, y=430, "C", cex=1.25)
mtext(cilab, side=2, line=3)

#cc
boxplot(Mean_Cc~treatment, not422, ylab="",names=FALSE, ylim=c(0,435), 
        outline=FALSE, border=cols, yaxt='n')
text(x=.55, y=430, "D", cex=1.25)
axis(4, labels=TRUE)
mtexti(cclab, 4, outer=TRUE, cex=1.25,off=.6)
mtext(canopy, side=1,at=1:4, line=1, cex=.9)
mtext(co2lab, side=1, at=c(1.5, 3.5),line=3, cex=.9)

# dev.copy2pdf(file= "output/Figure2.pdf")
dev.off()

