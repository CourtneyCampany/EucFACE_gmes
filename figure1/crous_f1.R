means<-read.csv("TreeMeans_EucFACE_Age.csv")

new <- subset(means, Age == "new")
old <- subset(means, Age == "old")


newup <- subset(new, Position == "upper")
newlow <- subset(new, Position == "lower")
oldup <- subset(old, Position == "upper")
oldlow <- subset(old, Position == "lower")


#now for a figure with panels

Nlab <- expression(italic(N)[mass] ~~ (mg~g^-1))
# Alab <- expression(italic(A)[net] ~~ (mu*mol~m^-2~s^-1))
Alab <- expression(atop(italic(A)[net], ~~ (mu*mol~m^-2~s^-1))) # atop if you need a return for the axis (2 lines)
jlab <- expression(atop(Apparent~J[max],~~(mu*mol~m^-2~s^-1)))
vlab <- expression(atop(Apparent~V[cmax],~~(mu*mol~m^-2~s^-1)))
lmalab <- expression(LMA~~(g~cm^-2))

#standard sizes for plot bits (only need to change here)
cex_pts <- 1.8
cex_axs <- 1.2
pch1 <- 19 #just switch this to c(1,19)
pch2 <- 17
cextitle <- .9
cex_ylab <- .9


#make figure

pdf(file="Fig1remake2.pdf", width=6,height=8)

op1 <- par(oma=c(5,7,3,5), mfrow=c(4,2), mar=c(0,0,0,0), lwd=2) #omd = c(.15, 0.95, .15, 0.95)

#panel1
plot(1:2,newup$Photo,pch=pch1, cex = cex_pts, cex.axis=cex_axs,xlab="",ylab="",xaxt="n",xlim=c(0.5,2.5), ylim = c(0, 31)) # can use pch= c(1,19) for closed and open symbols
  lines(rbind(1:2,1:2,NA),rbind(newup$Photo-newup$se.Photo,newup$Photo+newup$se.Photo,NA), lwd = 3)
  points(1:2,newlow$Photo,pch=pch2, cex = cex_pts,cex.axis=cex_axs, ylab="",xaxt="n",xlim=c(0.5,2.5),ylim=c(0,31), cex.lab = 1.6) # can use pch= c(2,17) for closed (=eCO2) and open symbol (=ambCO2)
  lines(rbind(1:2,1:2,NA),rbind(newlow$Photo-newlow$se.Photo,newlow$Photo+newlow$se.Photo,NA), lwd=3)
  axis(side=1,labels = F, at=1:2,las=1, cex.axis=2, lwd.ticks = 0)
  mtext(Alab, side=2, line=2, cex=cex_ylab)
  mtext("New leaves", 3, line=1, cex=cextitle)
  text(0.55,28, "A", cex=1.4)
  legend("bottomleft", levels(newup$Position), pch=c(17,19),cex=1.6, bty="n")

#panel2
plot(1:2,oldup$Photo,pch=pch1, cex = cex_pts, cex.axis=cex_axs,yaxt='n',xaxt="n",xlim=c(0.5,2.5), ylim = c(0, 31), cex.lab = 1.6)
lines(rbind(1:2,1:2,NA),rbind(oldup$Photo-oldup$se.Photo,oldup$Photo+oldup$se.Photo,NA), lwd = 3)
points(1:2,oldlow$Photo,pch=pch2, cex = cex_pts,cex.axis=cex_axs, yaxt='n',xaxt="n",xlim=c(0.5,2.5),ylim=c(0,31), cex.lab = 1.6)
lines(rbind(1:2,1:2,NA),rbind(oldlow$Photo-oldlow$se.Photo,oldlow$Photo+oldlow$se.Photo,NA), lwd=3)
axis(2, labels=FALSE, tcl=.5)
mtext("Old leaves", 3, line=1, cex=cextitle)
text(0.55,28, "B", cex=1.4)

#panel3
plot(1:2,newup$Vcmax,pch=pch1, cex = cex_pts, cex.axis=cex_axs,xlab="",ylab="",xaxt="n",xlim=c(0.5,2.5), ylim = c(0, 170), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(newup$Vcmax-newup$se.Vcmax,newup$Vcmax+newup$se.Vcmax,NA), lwd = 3)
  points(1:2,newlow$Vcmax,pch=pch2, cex = cex_pts,cex.axis=cex_axs, ylab="",xaxt="n",xlim=c(0.5,2.5),ylim=c(0,170), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(newlow$Vcmax-newlow$se.Vcmax,newlow$Vcmax+newlow$se.Vcmax,NA), lwd=3)
  axis(side=1,at=1:2,labels=F,las=1, cex.axis=2, lwd.ticks = 0)
  mtext(vlab, side=2, line=2, cex=cex_ylab)
  text(0.55,160, "C", cex=1.4)

#panel4
plot(1:2,oldup$Vcmax,pch=pch1, cex = cex_pts, cex.axis=cex_axs,yaxt='n',xaxt="n",xlim=c(0.5,2.5), ylim = c(0, 170), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(oldup$Vcmax-oldup$se.Vcmax,oldup$Vcmax+oldup$se.Vcmax,NA), lwd = 3)
  points(1:2,oldlow$Vcmax,pch=pch2, cex = cex_pts,cex.axis=cex_axs, yaxt='n',xaxt="n",xlim=c(0.5,2.5),ylim=c(0,170), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(oldlow$Vcmax-oldlow$se.Vcmax,oldlow$Vcmax+oldlow$se.Vcmax,NA), lwd=3)
  axis(2, labels=FALSE, tcl=.5)
  text(0.55,160, "D", cex=1.4)

#panel5
plot(1:2,newup$Jmax,pch=pch1, cex = cex_pts, cex.axis=cex_axs,xlab="",ylab=jlab,xaxt="n",xlim=c(0.5,2.5), ylim = c(0, 190), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(newup$Jmax-newup$se.Jmax,newup$Jmax+newup$se.Jmax,NA), lwd = 3)
  points(1:2,newlow$Jmax,pch=pch2, cex = cex_pts,cex.axis=cex_axs, xlab=(expression(CO[2]~treatment)),ylab="",xaxt="n",xlim=c(0.5,2.5),ylim=c(0,190), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(newlow$Jmax-newlow$se.Jmax,newlow$Jmax+newlow$se.Jmax,NA), lwd=3)
  axis(side=1,at=1:2,labels=F,las=1, cex.axis=2, lwd.ticks = 0)
  mtext(jlab, side=2, line=2, cex=cex_ylab)
  text(0.55,180, "E", cex=1.4)

#panel6
plot(1:2,oldup$Jmax,pch=pch1, cex = cex_pts, cex.axis=cex_axs,yaxt='n',xaxt="n",xlim=c(0.5,2.5), ylim = c(0, 190), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(oldup$Jmax-oldup$se.Jmax,oldup$Jmax+oldup$se.Jmax,NA), lwd = 3)
  points(1:2,oldlow$Jmax,pch=pch2, cex = cex_pts,cex.axis=cex_axs, yaxt='n',xaxt="n",xlim=c(0.5,2.5),ylim=c(0,190), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(oldlow$Jmax-oldlow$se.Jmax,oldlow$Jmax+oldlow$se.Jmax,NA), lwd=3)
  axis(2, labels=FALSE, tcl=.5)
  text(0.55,180, "F", cex=1.4)

#panel 7 (here we do want the labels!)
plot(1:2,newup$LMA,pch=pch1, cex = cex_pts, cex.axis=cex_axs,xlab="", ylab="",xaxt="n",xlim=c(0.5,2.5), ylim = c(0, 230), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(newup$LMA-newup$se.LMA,newup$LMA+newup$se.LMA,NA), lwd = 3)
  points(1:2,newlow$LMA,pch=pch2, cex = cex_pts,cex.axis=cex_axs, xlab=(expression(CO[2]~treatment)),ylab="",xaxt="n",xlim=c(0.5,2.5),ylim=c(0,230), cex.lab = 1.6)
  lines(rbind(1:2,1:2,NA),rbind(newlow$LMA-newlow$se.LMA,newlow$LMA+newlow$se.LMA,NA), lwd=3)
  axis(side=1,at=1:2,labels=newup$C.treat,las=1, cex.axis=cex_axs, lwd.ticks = 0)
  mtext(lmalab, side=2, line=3, cex=cex_ylab)
  mtext((expression(CO[2]~treatment)),side = 1, line=3, cex=cextitle)
  text(0.55,220, "G", cex=1.4)

#panel 8
plot(1:2,oldup$LMA,pch=pch1, cex = cex_pts, cex.axis=cex_axs,xaxt="n",yaxt='n',xlim=c(0.5,2.5), ylim = c(0, 230), cex.lab = 1.6)
lines(rbind(1:2,1:2,NA),rbind(oldup$LMA-oldup$se.LMA,oldup$LMA+oldup$se.LMA,NA), lwd = 3)
points(1:2,oldlow$LMA,pch=pch2, cex = cex_pts,cex.axis=cex_axs, xaxt="n",yaxt='n',xlim=c(0.5,2.5),ylim=c(0,230), cex.lab = 1.6)
lines(rbind(1:2,1:2,NA),rbind(oldlow$LMA-oldlow$se.LMA,oldlow$LMA+oldlow$se.LMA,NA), lwd=3)
axis(side=1,at=1:2,labels=oldup$C.treat,las=1, cex.axis=cex_axs, lwd.ticks = 0)
axis(2, labels=F, tcl=.5)
mtext((expression(CO[2]~treatment)),side = 1, line=3, cex=cextitle)
text(0.55,220, "H", cex=1.4)

par(op1)

dev.off()
