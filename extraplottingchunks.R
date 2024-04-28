```{r luminance plot, include=FALSE, results='hide'}

load(file=paste0(datadir,'AveragedataLum.RData'))

# generate plot showing the group pupillometry data for the luminance condition
if (processdata > 0){

allPLumratios <- abs(cleanmeansP[2,])/abs(cleanmeansP[1,])
allPLum2ratios <- abs(cleanmeansP2[2,])/abs(cleanmeansP2[1,])

# plot figure
plotlims <- c(15,40,0,3)
ticklocsx <- contrastsdB
ticklabelsx <-c(6,12,24,48,96)
ticklocsy <- seq(0,3,0.5)    # locations of tick marks on y axis
ticklabelsy <- ticklocsy

postscript("SumLum.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Bin:Mon ratio", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(contrastsdB,allPLumratios, col=colpal[1], lwd=3, cex=0.5)
lines(contrastsdB,allPLum2ratios, col=colpal[2], lwd=3, cex=0.5)
points(contrastsdB,allPLumratios, pch = 21, col='black', bg=colpal[1], cex=1.6, lwd=3)
points(contrastsdB,allPLum2ratios, pch = 22, col='black', bg=colpal[2], cex=1.6, lwd=3)

legend(contrastsdB[4],3,c('Lum 1F','Lum 2F'), pch=21:24, pt.bg=colpal[1:2],box.lwd=2,pt.cex=1.6,pt.lwd=3)
text(17,2.8,'(c)',adj=0.5,cex=2.5)

dev.off()

plotlims <- c(15,40,0,0.15)
plotlims2 <- c(15,40,0,0.015)
ticklocsx <- contrastsdB    # locations of tick marks on x axis
ticklocsy <- seq(0,0.15,0.05)
ticklocsy2 <- seq(0,0.015,0.005)    # locations of tick marks on y axis
ticklabelsx <-c(6,12,24,48,96)        # set labels for x ticks
ticklabelsy <- ticklocsy
ticklabelsy2 <- ticklocsy2     # set labels for y ticks

postscript("CRF1p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP[cond,]),contrastsdB,cleanmeansPCI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP[cond,]),contrastsdB,cleanmeansPCI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP[cond,]), pch = 20+cond, col='black', bg=colpal[cond], cex=1.6, lwd=3)   # draw the data points themselves
}
legend(22,0.15,c('Monocular','Binocular','Dichoptic'),pch=21:23,pt.bg=colpal[1:3],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.14,'(d)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF2p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

condlist <- c(1,5,6)
pointlist <- c(21,24,25)
for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = pointlist[cond], col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)
}
legend(22,0.15,c('Monocular','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.14,'(e)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF3p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.4Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 4:6){
  lines(contrastsdB,abs(cleanmasksP[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmasksP[cond,]),contrastsdB,cleanmasksPCI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmasksP[cond,]),contrastsdB,cleanmasksPCI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmasksP[cond,]), pch = pointlist[cond-3], col='black', bg=colpal[cond], cex=1.6, lwd=3)
}
legend(22,0.15,c('Monocular 0.4Hz','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[4:6],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.14,'(f)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF1p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP2[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP2[cond,]),contrastsdB,cleanmeansP2CI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP2[cond,]),contrastsdB,cleanmeansP2CI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP2[cond,]), pch = 20+cond, col='black', bg=colpal[cond], cex=1.6, lwd=3)   # draw the data points themselves
}
legend(22,0.015,c('Monocular','Binocular','Dichoptic'),pch=21:23,pt.bg=colpal[1:3],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(g)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF2p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

condlist <- c(1,5,6)
pointlist <- c(21,24,25)
for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = pointlist[cond], col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)
}
legend(22,0.015,c('Monocular','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(h)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF3p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.8Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 4:6){
  lines(contrastsdB,abs(cleanmasksP2[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmasksP2[cond,]),contrastsdB,cleanmasksP2CI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmasksP2[cond,]),contrastsdB,cleanmasksP2CI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmasksP2[cond,]), pch = pointlist[cond-3], col='black', bg=colpal[cond], cex=1.6, lwd=3)
}
legend(22,0.015,c('Monocular 0.4Hz','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[4:6],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(i)',adj=0.5,cex=2.5)

dev.off()

postscript("SpecP.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 8)

frequencies <- (0:299)/10

plotlims <- c(0,2,0,0.15)
ticklocsx <- seq(0,2,0.25)    # locations of tick marks on x axis
ticklocsy <- seq(0,0.15,0.05)    # locations of tick marks on y axis
ticklabelsx <-ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Frequency (Hz)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

polygon(frequencies[c(3:21,21:3)],c(meanspectraPCI[1,3:21],meanspectraPCI[2,21:3]),col=colpal[2],border=NA)

lines(frequencies[3:21],abs(meanspectraP[3:21]), col=colpal[2], lwd=3, cex=0.5)

text(0.1,0.14,'(a)',adj=0.5,cex=2.5)

dev.off()

postscript("timecourseP.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 8)

times <- seq((1/120)-1,13,1/120)
sinewave <- sin(0.5*times * 2*pi)
sinewave[1:120] <- 0
sinewave[1560:1680] <- 0

plotlims <- c(-1,13,-1,0.5)
ticklocsx <- seq(-1,13,1)    # locations of tick marks on x axis
ticklocsy <- seq(-1,0.5,0.5)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Time (s)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

filt <- butter(3,0.083)
filtwaveP <- signal::filter(filt, meanwavesP)
filtU <- signal::filter(filt,meanwavesPCI[1,])
filtL <- signal::filter(filt,meanwavesPCI[2,])
polygon(times[c(1:length(times),length(times):1)],c(filtL,filtU[length(filtU):1]),col=colpal[2],border=NA)
lines(times, filtwaveP, col=colpal[2], lwd=1.5)
lines(times, (sinewave/10)-0.9, col='black', lwd=3)

text(-0.7,0.4,'(b)',adj=0.5,cex=2.5)

dev.off()

PostScriptTrace('CRF1p.ps')
p1 <- readPicture('CRF1p.ps.xml')
PostScriptTrace('CRF2p.ps')
p2 <- readPicture('CRF2p.ps.xml')
PostScriptTrace('CRF3p.ps')
p3 <- readPicture('CRF3p.ps.xml')
PostScriptTrace('CRF1p2.ps')
p4 <- readPicture('CRF1p2.ps.xml')
PostScriptTrace('CRF2p2.ps')
p5 <- readPicture('CRF2p2.ps.xml')
PostScriptTrace('CRF3p2.ps')
p6 <- readPicture('CRF3p2.ps.xml')
PostScriptTrace('SpecP.ps')
p7 <- readPicture('SpecP.ps.xml')
PostScriptTrace('timecourseP.ps')
p8 <- readPicture('timecourseP.ps.xml')
PostScriptTrace('SumLum.ps')
p9 <- readPicture('SumLum.ps.xml')

for (n in 1:length(p7@paths)){
  temp <- class(p7@paths[n]$path)[1]
  if (pmatch(temp,"PictureFill",nomatch=0)){
    if (sum(col2rgb(p7@paths[n]$path@rgb))<765){p7@paths[n]$path@rgb <- addalpha(p7@paths[n]$path@rgb,alpha=0.2)}}}

for (n in 1:length(p8@paths)){
  temp <- class(p8@paths[n]$path)[1]
  if (pmatch(temp,"PictureFill",nomatch=0)){
    if (sum(col2rgb(p8@paths[n]$path@rgb))<765){p8@paths[n]$path@rgb <- addalpha(p8@paths[n]$path@rgb,alpha=0.2)}}}

pdf(paste0(figdir,"pupildataLum.pdf"), bg="transparent", height = 20, width = 15)

par(mar=c(0.1,0.1,0.1,0.1))
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,1), ylim=c(0,1))

grid.picture(p1,x=0.17,y=0.4,width=0.32,height=1)
grid.picture(p2,x=0.5,y=0.4,width=0.32,height=1)
grid.picture(p3,x=0.83,y=0.4,width=0.32,height=1)
grid.picture(p4,x=0.17,y=0.15,width=0.32,height=1)
grid.picture(p5,x=0.5,y=0.15,width=0.32,height=1)
grid.picture(p6,x=0.83,y=0.15,width=0.32,height=1)
grid.picture(p7,x=0.25,y=0.83,width=0.5,height=1)
grid.picture(p8,x=0.25,y=0.63,width=0.5,height=1)
grid.picture(p9,x=0.75,y=0.71,width=0.5,height=1)

text(0.5,0.98,'Luminance',cex=4)

dev.off()

file.remove(c('CRF1p.ps','CRF2p.ps','CRF3p.ps','CRF1p2.ps','CRF2p2.ps','CRF3p2.ps','SpecP.ps','timecourseP.ps','SumLum.ps'))
file.remove(c('CRF1p.ps.xml','CRF2p.ps.xml','CRF3p.ps.xml','CRF1p2.ps.xml','CRF2p2.ps.xml','CRF3p2.ps.xml','SpecP.ps.xml','timecourseP.ps.xml','SumLum.ps.xml'))

  }
```

```{r L-M plot, include=FALSE, results='hide'}

load(file=paste0(datadir,'AveragedataLM.RData'))

# generate plot showing the group pupillometry data for the L-M experiment
if (processdata > 0){

allPLMratios <- abs(cleanmeansP[2,])/abs(cleanmeansP[1,])
allPLM2ratios <- abs(cleanmeansP2[2,])/abs(cleanmeansP2[1,])

# plot figure
plotlims <- c(15,40,0,4)
ticklocsx <- contrastsdB
ticklabelsx <-c(6,12,24,48,96)
ticklocsy <- seq(0,4,0.5)    # locations of tick marks on y axis
ticklabelsy <- ticklocsy

postscript("SumLM.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Bin:Mon ratio", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(contrastsdB,allPLMratios, col=colpal[1], lwd=3, cex=0.5)
lines(contrastsdB,allPLM2ratios, col=colpal[2], lwd=3, cex=0.5)
points(contrastsdB,allPLMratios, pch = 21, col='black', bg=colpal[1], cex=1.6, lwd=3)
points(contrastsdB,allPLM2ratios, pch = 22, col='black', bg=colpal[2], cex=1.6, lwd=3)

legend(contrastsdB[4],4,c('L-M 1F','L-M 2F'), pch=21:24, pt.bg=colpal[1:2],box.lwd=2,pt.cex=1.6,pt.lwd=3)
text(17,3.8,'(c)',adj=0.5,cex=2.5)

dev.off()

plotlims <- c(15,40,0,0.02)
plotlims2 <- c(15,40,0,0.015)
ticklocsx <- contrastsdB    # locations of tick marks on x axis
ticklocsy <- seq(0,0.02,0.005)
ticklocsy2 <- seq(0,0.015,0.005)    # locations of tick marks on y axis
ticklabelsx <-c(6,12,24,48,96)        # set labels for x ticks
ticklabelsy <- ticklocsy
ticklabelsy2 <- ticklocsy2     # set labels for y ticks

postscript("CRF1p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP[cond,]),contrastsdB,cleanmeansPCI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP[cond,]),contrastsdB,cleanmeansPCI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP[cond,]), pch = 20+cond, col='black', bg=colpal[cond], cex=1.6, lwd=3)   # draw the data points themselves
}
legend(22,0.02,c('Monocular','Binocular','Dichoptic'),pch=21:23,pt.bg=colpal[1:3],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(d)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF2p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

condlist <- c(1,5,6)
pointlist <- c(21,24,25)
for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = pointlist[cond], col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)
}
legend(22,0.02,c('Monocular','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(e)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF3p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.4Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 4:6){
  lines(contrastsdB,abs(cleanmasksP[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmasksP[cond,]),contrastsdB,cleanmasksPCI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmasksP[cond,]),contrastsdB,cleanmasksPCI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmasksP[cond,]), pch = pointlist[cond-3], col='black', bg=colpal[cond], cex=1.6, lwd=3)
}
legend(22,0.02,c('Monocular 0.4Hz','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[4:6],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(f)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF1p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP2[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP2[cond,]),contrastsdB,cleanmeansP2CI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP2[cond,]),contrastsdB,cleanmeansP2CI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP2[cond,]), pch = 20+cond, col='black', bg=colpal[cond], cex=1.6, lwd=3)   # draw the data points themselves
}
legend(22,0.015,c('Monocular','Binocular','Dichoptic'),pch=21:23,pt.bg=colpal[1:3],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(g)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF2p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

condlist <- c(1,5,6)
pointlist <- c(21,24,25)
for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = pointlist[cond], col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)
}
legend(22,0.015,c('Monocular','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(h)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF3p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.8Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 4:6){
  lines(contrastsdB,abs(cleanmasksP2[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmasksP2[cond,]),contrastsdB,cleanmasksP2CI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmasksP2[cond,]),contrastsdB,cleanmasksP2CI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmasksP2[cond,]), pch = pointlist[cond-3], col='black', bg=colpal[cond], cex=1.6, lwd=3)
}
legend(22,0.015,c('Monocular 0.4Hz','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[4:6],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(i)',adj=0.5,cex=2.5)

dev.off()

postscript("SpecP.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 8)

frequencies <- (0:299)/10

plotlims <- c(0,2,0,0.015)
ticklocsx <- seq(0,2,0.25)    # locations of tick marks on x axis
ticklocsy <- seq(0,0.015,0.005)    # locations of tick marks on y axis
ticklabelsx <-ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Frequency (Hz)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

polygon(frequencies[c(3:21,21:3)],c(meanspectraPCI[1,3:21],meanspectraPCI[2,21:3]),col=colpal[2],border=NA)

lines(frequencies[3:21],abs(meanspectraP[3:21]), col=colpal[2], lwd=3, cex=0.5)

text(0.1,0.014,'(a)',adj=0.5,cex=2.5)

dev.off()

postscript("timecourseP.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 8)

times <- seq((1/120)-1,13,1/120)
sinewave <- sin(0.5*times * 2*pi)
sinewave[1:120] <- 0
sinewave[1560:1680] <- 0

plotlims <- c(-1,13,-0.5,0.5)
ticklocsx <- seq(-1,13,1)    # locations of tick marks on x axis
ticklocsy <- seq(-0.5,0.5,0.5)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Time (s)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

filt <- butter(3,0.083)
filtwaveP <- signal::filter(filt, meanwavesP)
filtU <- signal::filter(filt,meanwavesPCI[1,])
filtL <- signal::filter(filt,meanwavesPCI[2,])
polygon(times[c(1:length(times),length(times):1)],c(filtL,filtU[length(filtU):1]),col=colpal[2],border=NA)
lines(times, filtwaveP, col=colpal[2], lwd=1.5)
lines(times, (sinewave/10)-0.4, col='black', lwd=3)

text(-0.7,0.4,'(b)',adj=0.5,cex=2.5)

dev.off()

PostScriptTrace('CRF1p.ps')
p1 <- readPicture('CRF1p.ps.xml')
PostScriptTrace('CRF2p.ps')
p2 <- readPicture('CRF2p.ps.xml')
PostScriptTrace('CRF3p.ps')
p3 <- readPicture('CRF3p.ps.xml')
PostScriptTrace('CRF1p2.ps')
p4 <- readPicture('CRF1p2.ps.xml')
PostScriptTrace('CRF2p2.ps')
p5 <- readPicture('CRF2p2.ps.xml')
PostScriptTrace('CRF3p2.ps')
p6 <- readPicture('CRF3p2.ps.xml')
PostScriptTrace('SpecP.ps')
p7 <- readPicture('SpecP.ps.xml')
PostScriptTrace('timecourseP.ps')
p8 <- readPicture('timecourseP.ps.xml')
PostScriptTrace('SumLM.ps')
p9 <- readPicture('SumLM.ps.xml')

for (n in 1:length(p7@paths)){
  temp <- class(p7@paths[n]$path)[1]
  if (pmatch(temp,"PictureFill",nomatch=0)){
    if (sum(col2rgb(p7@paths[n]$path@rgb))<765){p7@paths[n]$path@rgb <- addalpha(p7@paths[n]$path@rgb,alpha=0.2)}}}

for (n in 1:length(p8@paths)){
  temp <- class(p8@paths[n]$path)[1]
  if (pmatch(temp,"PictureFill",nomatch=0)){
    if (sum(col2rgb(p8@paths[n]$path@rgb))<765){p8@paths[n]$path@rgb <- addalpha(p8@paths[n]$path@rgb,alpha=0.2)}}}

pdf(paste0(figdir,"pupildataLM.pdf"), bg="transparent", height = 20, width = 15)

par(mar=c(0.1,0.1,0.1,0.1))
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,1), ylim=c(0,1))

grid.picture(p1,x=0.17,y=0.4,width=0.32,height=1)
grid.picture(p2,x=0.5,y=0.4,width=0.32,height=1)
grid.picture(p3,x=0.83,y=0.4,width=0.32,height=1)
grid.picture(p4,x=0.17,y=0.15,width=0.32,height=1)
grid.picture(p5,x=0.5,y=0.15,width=0.32,height=1)
grid.picture(p6,x=0.83,y=0.15,width=0.32,height=1)
grid.picture(p7,x=0.25,y=0.83,width=0.5,height=1)
grid.picture(p8,x=0.25,y=0.63,width=0.5,height=1)
grid.picture(p9,x=0.75,y=0.71,width=0.5,height=1)

text(0.5,0.98,'L-M',cex=4)

dev.off()

file.remove(c('CRF1p.ps','CRF2p.ps','CRF3p.ps','CRF1p2.ps','CRF2p2.ps','CRF3p2.ps','SpecP.ps','timecourseP.ps','SumLM.ps'))
file.remove(c('CRF1p.ps.xml','CRF2p.ps.xml','CRF3p.ps.xml','CRF1p2.ps.xml','CRF2p2.ps.xml','CRF3p2.ps.xml','SpecP.ps.xml','timecourseP.ps.xml','SumLM.ps.xml'))

  }
```

```{r S plot, include=FALSE, results='hide'}

load(file=paste0(datadir,'AveragedataS.RData'))

# generate plot showing the group pupillometry data for the S experiment
if (processdata > 0){

allPSratios <- abs(cleanmeansP[2,])/abs(cleanmeansP[1,])
allPS2ratios <- abs(cleanmeansP2[2,])/abs(cleanmeansP2[1,])

# plot figure
plotlims <- c(15,40,0,3)
ticklocsx <- contrastsdB
ticklabelsx <-c(6,12,24,48,96)
ticklocsy <- seq(0,3,0.5)    # locations of tick marks on y axis
ticklabelsy <- ticklocsy

postscript("SumS.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Bin:Mon ratio", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(contrastsdB,allPSratios, col=colpal[1], lwd=3, cex=0.5)
lines(contrastsdB,allPS2ratios, col=colpal[2], lwd=3, cex=0.5)
points(contrastsdB,allPSratios, pch = 21, col='black', bg=colpal[1], cex=1.6, lwd=3)
points(contrastsdB,allPS2ratios, pch = 22, col='black', bg=colpal[2], cex=1.6, lwd=3)

legend(contrastsdB[4],3,c('S 1F','S 2F'), pch=21:24, pt.bg=colpal[1:2],box.lwd=2,pt.cex=1.6,pt.lwd=3)
text(17,2.8,'(c)',adj=0.5,cex=2.5)

dev.off()

plotlims <- c(15,40,0,0.02)
plotlims2 <- c(15,40,0,0.015)
ticklocsx <- contrastsdB    # locations of tick marks on x axis
ticklocsy <- seq(0,0.02,0.005)
ticklocsy2 <- seq(0,0.015,0.005)    # locations of tick marks on y axis
ticklabelsx <-c(6,12,24,48,96)        # set labels for x ticks
ticklabelsy <- ticklocsy
ticklabelsy2 <- ticklocsy2     # set labels for y ticks

postscript("CRF1p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP[cond,]),contrastsdB,cleanmeansPCI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP[cond,]),contrastsdB,cleanmeansPCI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP[cond,]), pch = 20+cond, col='black', bg=colpal[cond], cex=1.6, lwd=3)   # draw the data points themselves
}
legend(22,0.02,c('Monocular','Binocular','Dichoptic'),pch=21:23,pt.bg=colpal[1:3],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(d)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF2p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

condlist <- c(1,5,6)
pointlist <- c(21,24,25)
for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = pointlist[cond], col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)
}
legend(22,0.02,c('Monocular','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(e)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF3p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.4Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 4:6){
  lines(contrastsdB,abs(cleanmasksP[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmasksP[cond,]),contrastsdB,cleanmasksPCI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmasksP[cond,]),contrastsdB,cleanmasksPCI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmasksP[cond,]), pch = pointlist[cond-3], col='black', bg=colpal[cond], cex=1.6, lwd=3)
}
legend(22,0.02,c('Monocular 0.4Hz','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[4:6],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(f)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF1p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP2[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP2[cond,]),contrastsdB,cleanmeansP2CI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP2[cond,]),contrastsdB,cleanmeansP2CI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP2[cond,]), pch = 20+cond, col='black', bg=colpal[cond], cex=1.6, lwd=3)   # draw the data points themselves
}
legend(22,0.02,c('Monocular','Binocular','Dichoptic'),pch=21:23,pt.bg=colpal[1:3],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(g)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF2p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

condlist <- c(1,5,6)
pointlist <- c(21,24,25)
for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = pointlist[cond], col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)
}
legend(22,0.02,c('Monocular','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(h)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF3p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.8Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 4:6){
  lines(contrastsdB,abs(cleanmasksP2[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmasksP2[cond,]),contrastsdB,cleanmasksP2CI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmasksP2[cond,]),contrastsdB,cleanmasksP2CI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmasksP2[cond,]), pch = pointlist[cond-3], col='black', bg=colpal[cond], cex=1.6, lwd=3)
}
legend(22,0.02,c('Monocular 0.4Hz','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[4:6],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(i)',adj=0.5,cex=2.5)

dev.off()

postscript("SpecP.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 8)

frequencies <- (0:299)/10

plotlims <- c(0,2,0,0.015)
ticklocsx <- seq(0,2,0.25)    # locations of tick marks on x axis
ticklocsy <- seq(0,0.015,0.005)    # locations of tick marks on y axis
ticklabelsx <-ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Frequency (Hz)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

polygon(frequencies[c(3:21,21:3)],c(meanspectraPCI[1,3:21],meanspectraPCI[2,21:3]),col=colpal[2],border=NA)

lines(frequencies[3:21],abs(meanspectraP[3:21]), col=colpal[2], lwd=3, cex=0.5)

text(0.1,0.014,'(a)',adj=0.5,cex=2.5)

dev.off()

postscript("timecourseP.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 8)

times <- seq((1/120)-1,13,1/120)
sinewave <- sin(0.5*times * 2*pi)
sinewave[1:120] <- 0
sinewave[1560:1680] <- 0

plotlims <- c(-1,13,-0.5,0.5)
ticklocsx <- seq(-1,13,1)    # locations of tick marks on x axis
ticklocsy <- seq(-0.5,0.5,0.5)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Time (s)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

filt <- butter(3,0.083)
filtwaveP <- signal::filter(filt, meanwavesP)
filtU <- signal::filter(filt,meanwavesPCI[1,])
filtL <- signal::filter(filt,meanwavesPCI[2,])
polygon(times[c(1:length(times),length(times):1)],c(filtL,filtU[length(filtU):1]),col=colpal[2],border=NA)
lines(times, filtwaveP, col=colpal[2], lwd=1.5)
lines(times, (sinewave/10)-0.4, col='black', lwd=3)

text(-0.7,0.4,'(b)',adj=0.5,cex=2.5)

dev.off()

PostScriptTrace('CRF1p.ps')
p1 <- readPicture('CRF1p.ps.xml')
PostScriptTrace('CRF2p.ps')
p2 <- readPicture('CRF2p.ps.xml')
PostScriptTrace('CRF3p.ps')
p3 <- readPicture('CRF3p.ps.xml')
PostScriptTrace('CRF1p2.ps')
p4 <- readPicture('CRF1p2.ps.xml')
PostScriptTrace('CRF2p2.ps')
p5 <- readPicture('CRF2p2.ps.xml')
PostScriptTrace('CRF3p2.ps')
p6 <- readPicture('CRF3p2.ps.xml')
PostScriptTrace('SpecP.ps')
p7 <- readPicture('SpecP.ps.xml')
PostScriptTrace('timecourseP.ps')
p8 <- readPicture('timecourseP.ps.xml')
PostScriptTrace('SumS.ps')
p9 <- readPicture('SumS.ps.xml')

for (n in 1:length(p7@paths)){
  temp <- class(p7@paths[n]$path)[1]
  if (pmatch(temp,"PictureFill",nomatch=0)){
    if (sum(col2rgb(p7@paths[n]$path@rgb))<765){p7@paths[n]$path@rgb <- addalpha(p7@paths[n]$path@rgb,alpha=0.2)}}}

for (n in 1:length(p8@paths)){
  temp <- class(p8@paths[n]$path)[1]
  if (pmatch(temp,"PictureFill",nomatch=0)){
    if (sum(col2rgb(p8@paths[n]$path@rgb))<765){p8@paths[n]$path@rgb <- addalpha(p8@paths[n]$path@rgb,alpha=0.2)}}}

pdf(paste0(figdir,"pupildataS.pdf"), bg="transparent", height = 20, width = 15)

par(mar=c(0.1,0.1,0.1,0.1))
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,1), ylim=c(0,1))

grid.picture(p1,x=0.17,y=0.4,width=0.32,height=1)
grid.picture(p2,x=0.5,y=0.4,width=0.32,height=1)
grid.picture(p3,x=0.83,y=0.4,width=0.32,height=1)
grid.picture(p4,x=0.17,y=0.15,width=0.32,height=1)
grid.picture(p5,x=0.5,y=0.15,width=0.32,height=1)
grid.picture(p6,x=0.83,y=0.15,width=0.32,height=1)
grid.picture(p7,x=0.25,y=0.83,width=0.5,height=1)
grid.picture(p8,x=0.25,y=0.63,width=0.5,height=1)
grid.picture(p9,x=0.75,y=0.71,width=0.5,height=1)

text(0.5,0.98,'S-(L+M)',cex=4)

dev.off()

file.remove(c('CRF1p.ps','CRF2p.ps','CRF3p.ps','CRF1p2.ps','CRF2p2.ps','CRF3p2.ps','SpecP.ps','timecourseP.ps','SumS.ps'))
file.remove(c('CRF1p.ps.xml','CRF2p.ps.xml','CRF3p.ps.xml','CRF1p2.ps.xml','CRF2p2.ps.xml','CRF3p2.ps.xml','SpecP.ps.xml','timecourseP.ps.xml','SumS.ps.xml'))

  }
```

```{r melanopsin plot, include=FALSE, results='hide'}

load(file=paste0(datadir,'AveragedataMel.RData'))

# generate plot showing the group pupillometry data for the melanopsin experiment
if (processdata > 0){

allPMelratios <- abs(cleanmeansP[2,])/abs(cleanmeansP[1,])
allPMel2ratios <- abs(cleanmeansP2[2,])/abs(cleanmeansP2[1,])

# plot figure
plotlims <- c(15,40,0,5)
ticklocsx <- contrastsdB
ticklabelsx <-c(6,12,24,48,96)
ticklocsy <- seq(0,5,0.5)    # locations of tick marks on y axis
ticklabelsy <- ticklocsy

postscript("SumMel.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Bin:Mon ratio", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(contrastsdB,allPMelratios, col=colpal[1], lwd=3, cex=0.5)
lines(contrastsdB,allPMel2ratios, col=colpal[2], lwd=3, cex=0.5)
points(contrastsdB,allPMelratios, pch = 21, col='black', bg=colpal[1], cex=1.6, lwd=3)
points(contrastsdB,allPMel2ratios, pch = 22, col='black', bg=colpal[2], cex=1.6, lwd=3)

legend(contrastsdB[2],5,c('Mel 1F','Mel 2F'), pch=21:24, pt.bg=colpal[1:2],box.lwd=2,pt.cex=1.6,pt.lwd=3)
text(17,4.8,'(c)',adj=0.5,cex=2.5)

dev.off()

plotlims <- c(15,40,0,0.02)
plotlims2 <- c(15,40,0,0.015)
ticklocsx <- contrastsdB    # locations of tick marks on x axis
ticklocsy <- seq(0,0.02,0.005)
ticklocsy2 <- seq(0,0.015,0.005)    # locations of tick marks on y axis
ticklabelsx <-c(6,12,24,48,96)        # set labels for x ticks
ticklabelsy <- ticklocsy
ticklabelsy2 <- ticklocsy2     # set labels for y ticks

postscript("CRF1p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP[cond,]),contrastsdB,cleanmeansPCI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP[cond,]),contrastsdB,cleanmeansPCI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP[cond,]), pch = 20+cond, col='black', bg=colpal[cond], cex=1.6, lwd=3)   # draw the data points themselves
}
legend(22,0.02,c('Monocular','Binocular','Dichoptic'),pch=21:23,pt.bg=colpal[1:3],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(d)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF2p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

condlist <- c(1,5,6)
pointlist <- c(21,24,25)
for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = pointlist[cond], col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)
}
legend(22,0.02,c('Monocular','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(e)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF3p.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.4Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 4:6){
  lines(contrastsdB,abs(cleanmasksP[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmasksP[cond,]),contrastsdB,cleanmasksPCI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmasksP[cond,]),contrastsdB,cleanmasksPCI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmasksP[cond,]), pch = pointlist[cond-3], col='black', bg=colpal[cond], cex=1.6, lwd=3)
}
legend(22,0.02,c('Monocular 0.4Hz','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[4:6],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.019,'(f)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF1p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP2[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP2[cond,]),contrastsdB,cleanmeansP2CI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP2[cond,]),contrastsdB,cleanmeansP2CI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP2[cond,]), pch = 20+cond, col='black', bg=colpal[cond], cex=1.6, lwd=3)   # draw the data points themselves
}
legend(22,0.015,c('Monocular','Binocular','Dichoptic'),pch=21:23,pt.bg=colpal[1:3],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(g)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF2p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

condlist <- c(1,5,6)
pointlist <- c(21,24,25)
for (cond in 1:3){
  lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = pointlist[cond], col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)
}
legend(22,0.015,c('Monocular','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(h)',adj=0.5,cex=2.5)

dev.off()

postscript("CRF3p2.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 5.5)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims2[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1)
title(xlab="Target contrast (% of max)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Amplitude at 0.8Hz (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (cond in 4:6){
  lines(contrastsdB,abs(cleanmasksP2[cond,]), col=colpal[cond], lwd=3, cex=0.5)
  arrows(contrastsdB,abs(cleanmasksP2[cond,]),contrastsdB,cleanmasksP2CI[1,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  arrows(contrastsdB,abs(cleanmasksP2[cond,]),contrastsdB,cleanmasksP2CI[2,cond,],lwd=3,col=colpal[cond],length=0.05,angle=90)
  points(contrastsdB,abs(cleanmasksP2[cond,]), pch = pointlist[cond-3], col='black', bg=colpal[cond], cex=1.6, lwd=3)
}
legend(22,0.015,c('Monocular 0.4Hz','Binocular cross','Dichoptic cross'),pch=pointlist,pt.bg=colpal[4:6],pt.lwd=3,pt.cex=1.6,box.lwd=2)
text(17,0.014,'(i)',adj=0.5,cex=2.5)

dev.off()

postscript("SpecP.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 8)

frequencies <- (0:299)/10

plotlims <- c(0,2,0,0.015)
ticklocsx <- seq(0,2,0.25)    # locations of tick marks on x axis
ticklocsy <- seq(0,0.015,0.005)    # locations of tick marks on y axis
ticklabelsx <-ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Frequency (Hz)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

polygon(frequencies[c(3:21,21:3)],c(meanspectraPCI[1,3:21],meanspectraPCI[2,21:3]),col=colpal[2],border=NA)

lines(frequencies[3:21],abs(meanspectraP[3:21]), col=colpal[2], lwd=3, cex=0.5)

text(0.1,0.014,'(a)',adj=0.5,cex=2.5)

dev.off()

postscript("timecourseP.ps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 5.5, width = 8)

times <- seq((1/120)-1,13,1/120)
sinewave <- sin(0.5*times * 2*pi)
sinewave[1:120] <- 0
sinewave[1560:1680] <- 0

plotlims <- c(-1,13,-0.5,0.5)
ticklocsx <- seq(-1,13,1)    # locations of tick marks on x axis
ticklocsy <- seq(-0.5,0.5,0.5)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Time (s)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude (mm)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

filt <- butter(3,0.083)
filtwaveP <- signal::filter(filt, meanwavesP)
filtU <- signal::filter(filt,meanwavesPCI[1,])
filtL <- signal::filter(filt,meanwavesPCI[2,])
polygon(times[c(1:length(times),length(times):1)],c(filtL,filtU[length(filtU):1]),col=colpal[2],border=NA)
lines(times, filtwaveP, col=colpal[2], lwd=1.5)
lines(times, (sinewave/10)-0.4, col='black', lwd=3)

text(-0.7,0.4,'(b)',adj=0.5,cex=2.5)

dev.off()

PostScriptTrace('CRF1p.ps')
p1 <- readPicture('CRF1p.ps.xml')
PostScriptTrace('CRF2p.ps')
p2 <- readPicture('CRF2p.ps.xml')
PostScriptTrace('CRF3p.ps')
p3 <- readPicture('CRF3p.ps.xml')
PostScriptTrace('CRF1p2.ps')
p4 <- readPicture('CRF1p2.ps.xml')
PostScriptTrace('CRF2p2.ps')
p5 <- readPicture('CRF2p2.ps.xml')
PostScriptTrace('CRF3p2.ps')
p6 <- readPicture('CRF3p2.ps.xml')
PostScriptTrace('SpecP.ps')
p7 <- readPicture('SpecP.ps.xml')
PostScriptTrace('timecourseP.ps')
p8 <- readPicture('timecourseP.ps.xml')
PostScriptTrace('SumMel.ps')
p9 <- readPicture('SumMel.ps.xml')

for (n in 1:length(p7@paths)){
  temp <- class(p7@paths[n]$path)[1]
  if (pmatch(temp,"PictureFill",nomatch=0)){
    if (sum(col2rgb(p7@paths[n]$path@rgb))<765){p7@paths[n]$path@rgb <- addalpha(p7@paths[n]$path@rgb,alpha=0.2)}}}

for (n in 1:length(p8@paths)){
  temp <- class(p8@paths[n]$path)[1]
  if (pmatch(temp,"PictureFill",nomatch=0)){
    if (sum(col2rgb(p8@paths[n]$path@rgb))<765){p8@paths[n]$path@rgb <- addalpha(p8@paths[n]$path@rgb,alpha=0.2)}}}

pdf(paste0(figdir,"pupildataMel.pdf"), bg="transparent", height = 20, width = 15)

par(mar=c(0.1,0.1,0.1,0.1))
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,1), ylim=c(0,1))

grid.picture(p1,x=0.17,y=0.4,width=0.32,height=1)
grid.picture(p2,x=0.5,y=0.4,width=0.32,height=1)
grid.picture(p3,x=0.83,y=0.4,width=0.32,height=1)
grid.picture(p4,x=0.17,y=0.15,width=0.32,height=1)
grid.picture(p5,x=0.5,y=0.15,width=0.32,height=1)
grid.picture(p6,x=0.83,y=0.15,width=0.32,height=1)
grid.picture(p7,x=0.25,y=0.85,width=0.5,height=1)
grid.picture(p8,x=0.25,y=0.63,width=0.5,height=1)
grid.picture(p9,x=0.75,y=0.73,width=0.5,height=1)

text(0.5,0.98,'Melanopsin',cex=4)

dev.off()

file.remove(c('CRF1p.ps','CRF2p.ps','CRF3p.ps','CRF1p2.ps','CRF2p2.ps','CRF3p2.ps','SpecP.ps','timecourseP.ps','SumMel.ps'))
file.remove(c('CRF1p.ps.xml','CRF2p.ps.xml','CRF3p.ps.xml','CRF1p2.ps.xml','CRF2p2.ps.xml','CRF3p2.ps.xml','SpecP.ps.xml','timecourseP.ps.xml','SumMel.ps.xml'))

  }
```