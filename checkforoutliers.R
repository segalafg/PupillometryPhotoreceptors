
processdata <- 2

library(FourierStats)
# cut off for data inclusion (Mahalanobis distance for complex data)
SDthresh <<- 99
nbootstraps <<- 1000

localdir <- 'local/'    # all files are stored in the project directory /local/ which git is told to ignore
if (!file.exists(localdir)){dir.create(localdir)}   # create a local directory to store data and outputs
rawdir <- 'local/rawdata/'
if (!file.exists(rawdir)){dir.create(rawdir)}   # create a local directory to store raw data
figdir <- 'Figures/'
if (!file.exists(figdir)){dir.create(figdir)}   # create a local directory to store figures
datadir <- 'local/processeddata/'
if (!file.exists(datadir)){dir.create(datadir)}   # create a local directory to store processed data

# files from P301-P324 are for S-cone stimulation
# files from P501-P524 are for melanopsin stimulation
# files from P601-P624 are for luminance stimulation
# files from P701-P724 are for (L-M) cone stimulation

# load in individual participant data and average, save group data
if (processdata > 1){
  
  prefixlist <- c('P6','P7','P3','P5')
  exptnames <- c('AveragedataLumA.RData','AveragedataLMA.RData','AveragedataSA.RData','AveragedataMelA.RData')
  
  includedvalues <- array(0,dim=c(4,24,6,5))
  includedvalues2 <- array(0,dim=c(4,24,6,5))
  
  for (expt in 1:4){
    d <- dir(datadir,pattern=prefixlist[expt], full.names = TRUE)
    
    tempmasksP <- array(0,dim=c(length(d),6,5))
    tempmeansP <- tempmasksP
    tempmeansP2 <- tempmasksP
    tempmasksP2 <- tempmasksP
    tempspectraP <- array(0,dim=c(length(d),300))
    tempwavesP <- array(0,dim=c(length(d),1680))
    
    for (s in 1:length(d)){
      
      load(d[s])
      
      tempmasksP[s,,] <- cleanmasksP
      tempmeansP[s,,] <- cleanmeansP
      tempmasksP2[s,,] <- cleanmasksP2
      tempmeansP2[s,,] <- cleanmeansP2
      tempspectraP[s,] <- meanspectraP
      tempwavesP[s,] <- meanwavesP
    }
    
    # tempspectraP <- tempspectraP[-c(2,4,13,20,26),]
    
    cleanmeansP <- matrix(0,nrow=6,ncol=5)
    cleanmeansPCI <- array(0,c(2,6,5))
    cleanmasksP <- matrix(0,nrow=6,ncol=5)
    cleanmasksPCI <- array(0,c(2,6,5))
    cleanmeansP2 <- matrix(0,nrow=6,ncol=5)
    cleanmeansP2CI <- array(0,c(2,6,5))
    cleanmasksP2 <- matrix(0,nrow=6,ncol=5)
    cleanmasksP2CI <- array(0,c(2,6,5))
    
    for (cond in 1:6){
      for (level in 1:5){
        
        temp <- tempmeansP[,cond,level]
        cartdata <- data.frame(Re(temp),Im(temp))
        if (sum(cartdata)!=0){
          D <- sqrt(stats::mahalanobis(cartdata, colMeans(cartdata), cov(cartdata)))
          i <- which(D<SDthresh)
          includedvalues[expt,i,cond,level] <- 1
          mbs <- amperrors((temp[i]),method='boot',quantiles=68,nresamples=nbootstraps)
          cleanmeansP[cond,level] <- mean(abs(temp[i]))
          cleanmeansPCI[1,cond,level] <- mbs$lowerCI
          cleanmeansPCI[2,cond,level] <- mbs$upperCI
        }
        
        temp <- tempmasksP[,cond,level]
        cartdata <- data.frame(Re(temp),Im(temp))
        if (sum(cartdata)!=0){
          D <- sqrt(stats::mahalanobis(cartdata, colMeans(cartdata), cov(cartdata)))
          i <- which(D<SDthresh)
          mbs <- amperrors((temp[i]),method='boot',quantiles=68,nresamples=nbootstraps)
          cleanmasksP[cond,level] <- mean(abs(temp[i]))
          cleanmasksPCI[1,cond,level] <- mbs$lowerCI
          cleanmasksPCI[2,cond,level] <- mbs$upperCI
        }
        
        temp <- tempmeansP2[,cond,level]
        cartdata <- data.frame(Re(temp),Im(temp))
        if (sum(cartdata)!=0){
          D <- sqrt(stats::mahalanobis(cartdata, colMeans(cartdata), cov(cartdata)))
          i <- which(D<SDthresh)
          mbs <- amperrors((temp[i]),method='boot',quantiles=68,nresamples=nbootstraps)
          cleanmeansP2[cond,level] <- mean(abs(temp[i]))
          cleanmeansP2CI[1,cond,level] <- mbs$lowerCI
          cleanmeansP2CI[2,cond,level] <- mbs$upperCI
        }
        
        temp <- tempmasksP2[,cond,level]
        cartdata <- data.frame(Re(temp),Im(temp))
        if (sum(cartdata)!=0){
          D <- sqrt(stats::mahalanobis(cartdata, colMeans(cartdata), cov(cartdata)))
          i <- which(D<SDthresh)
          includedvalues2[expt,i,cond,level] <- 1
          mbs <- amperrors((temp[i]),method='boot',quantiles=68,nresamples=nbootstraps)
          cleanmasksP2[cond,level] <- mean(abs(temp[i]))
          cleanmasksP2CI[1,cond,level] <- mbs$lowerCI
          cleanmasksP2CI[2,cond,level] <- mbs$upperCI
        }
      }
    }
    
    meanspectraP <- (1:300)*0
    meanspectraPCI <- array(0,c(2,300))
    
    for (f in 2:300){
      temp <- tempspectraP[,f]
      cartdata <- data.frame(Re(temp),Im(temp))
      if (sum(cartdata)!=0){
        D <- sqrt(stats::mahalanobis(cartdata, colMeans(cartdata), cov(cartdata)))
        i <- which(D<SDthresh) 
        mbs <- amperrors((temp[i]),method='boot',quantiles=68,nresamples=nbootstraps)
        meanspectraP[f] <- mbs$meanamp
        meanspectraPCI[1,f] <- mbs$lowerCI
        meanspectraPCI[2,f] <- mbs$upperCI
      }
    }
    
    meanwavesP <- NULL
    meanwavesPCI <- array(0,dim=c(2,dim(tempwavesP)[2]))
    
    nsubjs <- dim(tempwavesP)[1]
    for (t in 1:dim(tempwavesP)[2]){
      temp <- tempwavesP[,t]
      i <- which(abs(temp-mean(temp))<(SDthresh*sd(temp)))
      meanwavesP[t] <- mean(temp[i])
      bspop <- NULL
      for (s in 1:nbootstraps){bspop[s] <- mean(sample(temp[i],length(i),replace=TRUE))}
      meanwavesPCI[,t] <- quantile(bspop,c(0.16,0.84),na.rm=TRUE)
    }
    
    save(file=paste0(datadir,exptnames[expt]),list=c('meanwavesP','meanwavesPCI','cleanmeansP','cleanmeansPCI','cleanmasksP','cleanmasksPCI','cleanmeansP2','cleanmeansP2CI','cleanmasksP2','cleanmasksP2CI','meanspectraP','meanspectraPCI','tempmeansP','tempmeansP2','tempmasksP','tempmasksP2'))
  }
  
  save(file=paste0(datadir,'includedA.RData'),list=c('includedvalues','includedvalues2'))
  
}




# create plots of spectra, cone absorption, waveforms and Fourier spectra
if (processdata > 0){
  
  contrastsdB <- 20*log10(c(6,12,24,48,96))
  colpal <- c('#FE5000','#8783CF','#228B22','#808080','#6d008b','#8B8000')
  colpalmod <- c('#808080','#8B8000','#FE5000','#8783CF')
  
  # these are approximate values for isolumiant red, green, blue and yellow
  stimcolours <- c(rgb(0.02,0.02,0.02),rgb(0.98,0.98,0.98),rgb(0,0.61,0.5),rgb(1,0.39,0.5),rgb(0.35,0.63,0),rgb(0.65,0.37,1),'grey','cornflowerblue')
  
  contrastsfinedB <- 2:40
  
  condlist <- c(1,2,6)
  condlist2 <- c(1,2,4)
  plotlims <- c(15,40,0,0.15)
  plotlims2 <- c(15,40,0,0.025) 
  ticklocsx <- contrastsdB    # locations of tick marks on x axis
  ticklocsy <- seq(0,0.15,0.05)
  ticklocsy2 <- seq(0,0.025,0.005)    # locations of tick marks on y axis
  ticklabelsx <-c(6,12,24,48,96)        # set labels for x ticks
  ticklabelsy <- ticklocsy
  ticklabelsy2 <- ticklocsy2     # set labels for y ticks
  
  pdf(paste0(figdir,"CRFsA.pdf"), bg="transparent", height = 6, width = 14)
  par(mfcol=c(2,4), mar=c(3,6,2,2))
  
  
  load(file=paste0(datadir,'AveragedataLumA.RData'))
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)
  title(main='Luminance',cex.main=2)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsLum[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  legend(15,0.125,c('Monocular','Binocular','Dichoptic cross'),pch=21:23,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2,cex=1.2)
  text(17,0.14,'(a)',adj=0.5,cex=2.5)
  
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsLum2[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(e)',adj=0.5,cex=2.5)
  
  
  load(file=paste0(datadir,'AveragedataLMA.RData'))
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  title(main='L-M cone',cex.main=2)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsLM[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(b)',adj=0.5,cex=2.5)
  
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsLM2[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(f)',adj=0.5,cex=2.5)
  
  
  load(file=paste0(datadir,'AveragedataSA.RData'))
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  title(main='S cone',cex.main=2)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsS[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(c)',adj=0.5,cex=2.5)
  
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsS2[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(g)',adj=0.5,cex=2.5)
  
  
  load(file=paste0(datadir,'AveragedataMelA.RData'))
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  title(main='Melanopsin',cex.main=2)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsMel[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(d)',adj=0.5,cex=2.5)
  
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsMel2[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(h)',adj=0.5,cex=2.5)
  
  
  
  dev.off()  
  
}





# create plots of spectra, cone absorption, waveforms and Fourier spectra
if (processdata > 0){
  
  contrastsdB <- 20*log10(c(6,12,24,48,96))
  colpal <- c('#FE5000','#8783CF','#228B22','#808080','#6d008b','#8B8000')
  colpalmod <- c('#808080','#8B8000','#FE5000','#8783CF')
  
  # these are approximate values for isolumiant red, green, blue and yellow
  stimcolours <- c(rgb(0.02,0.02,0.02),rgb(0.98,0.98,0.98),rgb(0,0.61,0.5),rgb(1,0.39,0.5),rgb(0.35,0.63,0),rgb(0.65,0.37,1),'grey','cornflowerblue')
  
  contrastsfinedB <- 2:40
  
  condlist <- c(1,2,6)
  condlist2 <- c(1,2,4)
  plotlims <- c(15,40,0,0.15)
  plotlims2 <- c(15,40,0,0.015) 
  ticklocsx <- contrastsdB    # locations of tick marks on x axis
  ticklocsy <- seq(0,0.15,0.05)
  ticklocsy2 <- seq(0,0.015,0.005)    # locations of tick marks on y axis
  ticklabelsx <-c(6,12,24,48,96)        # set labels for x ticks
  ticklabelsy <- ticklocsy
  ticklabelsy2 <- ticklocsy2     # set labels for y ticks
  
  pdf(paste0(figdir,"CRFsC.pdf"), bg="transparent", height = 6, width = 14)
  par(mfcol=c(2,4), mar=c(3,6,2,2))
  
  
  load(file=paste0(datadir,'AveragedataLum.RData'))
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)
  title(main='Luminance',cex.main=2)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsLum[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  legend(15,0.125,c('Monocular','Binocular','Dichoptic cross'),pch=21:23,pt.bg=colpal[condlist],pt.lwd=3,pt.cex=1.6,box.lwd=2,cex=1.2)
  text(17,0.14,'(a)',adj=0.5,cex=2.5)
  
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsLum2[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(e)',adj=0.5,cex=2.5)
  
  
  load(file=paste0(datadir,'AveragedataLM.RData'))
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  title(main='L-M cone',cex.main=2)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsLM[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(b)',adj=0.5,cex=2.5)
  
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsLM2[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(f)',adj=0.5,cex=2.5)
  
  
  load(file=paste0(datadir,'AveragedataS.RData'))
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  title(main='S cone',cex.main=2)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsS[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(c)',adj=0.5,cex=2.5)
  
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsS2[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(g)',adj=0.5,cex=2.5)
  
  
  load(file=paste0(datadir,'AveragedataMel.RData'))
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 0.5Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  title(main='Melanopsin',cex.main=2)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsMel[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP[condlist[cond],]),contrastsdB,cleanmeansPCI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(d)',adj=0.5,cex=2.5)
  
  
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims2[1:2], ylim=plotlims2[3:4])   
  axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
  axis(2, at=ticklocsy2, tck=0.01, lab=F, lwd=2)
  mtext(text = ticklabelsx, side = 1, at=ticklocsx, line=0.3)     # add the tick labels
  mtext(text = ticklabelsy2, side = 2, at=ticklocsy2, line=0.2, las=1) 
  title(xlab="Target contrast (%)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
  title(ylab="Amplitude at 1Hz (mm)", col.lab=rgb(0,0,0), line=3.5, cex.lab=1.5)
  for (cond in 1:3){
    # lines(contrastsfinedB,modrespsMel2[condlist2[cond],], col=colpal[condlist[cond]], lwd=3, cex=0.5)
    lines(contrastsdB,abs(cleanmeansP2[condlist[cond],]), col=colpal[condlist[cond]], lwd=3, cex=0.5)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[1,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    # arrows(contrastsdB,abs(cleanmeansP2[condlist[cond],]),contrastsdB,cleanmeansP2CI[2,condlist[cond],],lwd=3,col=colpal[condlist[cond]],length=0.05,angle=90)
    points(contrastsdB,abs(cleanmeansP2[condlist[cond],]), pch = 20+cond, col='black', bg=colpal[condlist[cond]], cex=1.6, lwd=3)   
  }  
  text(17,0.014,'(h)',adj=0.5,cex=2.5)
  
  
  
  dev.off()  
  
}


load(file=paste0(datadir,'AveragedataLM.RData'))
xs <- 10*Re(tempmeansP)
ys <- 10*Im(tempmeansP)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.022,0.022), ylim=c(-0.022,0.022))   

rangles <- seq(0,360)*(pi/180)
r <- seq(0.005,0.02,0.005)
for (n in 1:length(r)){
  lines(r[n]*cos(rangles),r[n]*sin(rangles),col='gray')
}
lines(c(0,0),c(-0.02,0.02),lwd=2)
lines(c(-0.02,0.02),c(0,0),lwd=2)

text(0.015,-0.001,'Amplitude',cex=1.25,adj=0.5)
text(0.021,0,'Phase',cex=1.25,adj=0.5,srt=-90)

points(xs[,1,5]/10,ys[,1,5]/10,pch=21,col=stimcolours[1],bg=stimcolours[2],cex=2,lwd=3)



plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.022,0.022), ylim=c(-0.022,0.022))   

rangles <- seq(0,360)*(pi/180)
r <- seq(0.005,0.02,0.005)
for (n in 1:length(r)){
  lines(r[n]*cos(rangles),r[n]*sin(rangles),col='gray')
}
lines(c(0,0),c(-0.02,0.02),lwd=2)
lines(c(-0.02,0.02),c(0,0),lwd=2)

text(0.015,-0.001,'Amplitude',cex=1.25,adj=0.5)
text(0.021,0,'Phase',cex=1.25,adj=0.5,srt=-90)


points(xs[,2,5]/10,ys[,2,5]/10,pch=22,col=stimcolours[2],bg=stimcolours[3],cex=2,lwd=3)






load(file=paste0(datadir,'AveragedataLum.RData'))
xs <- Re(tempmeansP)
ys <- Im(tempmeansP)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.022,0.022), ylim=c(-0.022,0.022))   

rangles <- seq(0,360)*(pi/180)
r <- seq(0.005,0.02,0.005)
for (n in 1:length(r)){
  lines(r[n]*cos(rangles),r[n]*sin(rangles),col='gray')
}
lines(c(0,0),c(-0.02,0.02),lwd=2)
lines(c(-0.02,0.02),c(0,0),lwd=2)

text(0.015,-0.001,'Amplitude',cex=1.25,adj=0.5)
text(0.021,0,'Phase',cex=1.25,adj=0.5,srt=-90)

points(xs[,1,5]/10,ys[,1,5]/10,pch=21,col=stimcolours[1],bg=stimcolours[2],cex=2,lwd=3)



plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.022,0.022), ylim=c(-0.022,0.022))   

rangles <- seq(0,360)*(pi/180)
r <- seq(0.005,0.02,0.005)
for (n in 1:length(r)){
  lines(r[n]*cos(rangles),r[n]*sin(rangles),col='gray')
}
lines(c(0,0),c(-0.02,0.02),lwd=2)
lines(c(-0.02,0.02),c(0,0),lwd=2)

text(0.015,-0.001,'Amplitude',cex=1.25,adj=0.5)
text(0.021,0,'Phase',cex=1.25,adj=0.5,srt=-90)


points(xs[,2,5]/10,ys[,2,5]/10,pch=22,col=stimcolours[2],bg=stimcolours[3],cex=2,lwd=3)


