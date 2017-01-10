# SNAP track R code adapted by Matthew Scales and Marc Henrion (Institute of Cancer Research, London, UK) from:
# 
#              Diabetes Genetics Initiative of Broad Institute of Harvard and MIT, Lund University and 
#                                  Novartis Institutes of BioMedical Research
#        Whole-genome association analysis identifies novel loci for type 2 diabetes and triglyceride levels
#                             Science 2007 Jun 1;316(5829):1331-6. Epub 2007 Apr 26 
#

snap_track_with_breaks <- function(snp,data,recomb,snp2=NA,data2=NA,ylab,ylab.las,cex.annot,gaps.list,gaps.sizes,gaps.sizes.offset.x,gap_width,SP,EP,snap.cex.axis=1,mai,ylim=NA,snap.snp.offset.x=0,snap.snp.offset.y=0,add.SNP=FALSE,r2.hue=0,snp2.r2.hue=0.6665,snap.r2.legend.x.offset=0,snap.snp.label.cex=1.2,...){  
	# parameters that are used in the function that still have an effect:
	size.onehundredkb = 50000 
	
	## Sort out the association data for the 1st SNP
	
	# Effective position is supplied position or HapMap coordinate.
	data$Position[is.na(data$Position)] <- data$Coordinate[is.na(data$Position)]
	
	# Default r^2 to zero if not known
	data$RSquared[is.na(data$RSquared)] <- 0
	data$RSquared[data$RSquared<0] <- 0
	data$RSquared[data$RSquared>1] <- 0
	
	data <- subset(data, (!is.na(data$PValue) & !is.na(data$Position)))
	data$PValue[which(data$PValue < 0)]<-1
	
	# Prepare data for plotting reference SNP name
	locusname <- paste(snp, " region", sep="")
	locus <- data
	hit <- locus[snp,]
  
	# Filter out relevant region
	templocus <- subset(locus, locus$Position >= SP & locus$Position < EP)
	locus<-templocus
	
	best.pval = min(data$PValue)  
	
	# Set various parameters
	if(is.na(ylim)){range <- max(c(3.5, ceiling((-log10(best.pval))*1.05+0.5)))}else {range <-ylim[2]}
	offset <- 0  # Now the offset is 0 as there is a separate gene track in the web plotting interface
  
	big.range <- range + offset 
  

	## Prepare the parameters for the recombination rate data
	
	if(is.na(ylim)){ylim<-c(0,range)}
	ystart.recomb <- 0  
	keep.recomb <- subset(recomb, recomb[,"POSITION"] > SP & recomb[,"POSITION"] < EP)
	
	## Plot the axis & recombination rate
	
	if(nrow(keep.recomb)>0){
		tmp.max <- max(keep.recomb[,2],na.rm=TRUE)
		yend.recomb <- 5 * ceiling(tmp.max/5)
		recomb.range<-yend.recomb-ystart.recomb
  
		if(add.SNP==FALSE){
			breaks.info <- plot_with_breaks(x=keep.recomb[,1],y=ystart.recomb+(keep.recomb[,2]*big.range/recomb.range),type="n",col="lightblue",lwd=1,xlim=c(SP,EP),ylim=ylim,mai=mai,plot.axes='n',side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gaps.sizes.offset.x=gaps.sizes.offset.x,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,ylab=ylab,cex.annot=cex.annot,ylab.las=ylab.las,...)
			lines_with_breaks(keep.recomb[,1],ystart.recomb +(keep.recomb[,2]*big.range/recomb.range),col="lightblue",lwd=1,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
		}
  	}else{
		breaks.info <- plot_with_breaks(x=0,y=0,type="n", col="lightblue",lwd=1, xlim=c(SP,EP), ylim=ylim,mai=mai,plot.axes='n',side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gaps.sizes.offset.x=gaps.sizes.offset.x,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,ylab=ylab,cex.annot=cex.annot,ylab.las=ylab.las,...)
	}

	# Add axes and titles
	per.tick <- ceiling(range/5)
	axis(2, at=seq(0,range,per.tick), labels=seq(0,range,per.tick),las=ylab.las,cex.axis=snap.cex.axis)

	if(nrow(keep.recomb)>0){
		axis(4, at=seq(0,yend.recomb,5)*big.range/recomb.range, labels=format(seq(0,yend.recomb,5),digits=2), las=ylab.las,cex.axis=snap.cex.axis)
	}
	
	mtext("Recombination rate (cM/Mb)", side=4, at=(-offset+big.range/2),line=3, cex=ifelse(ylab.las==0||ylab.las==3,cex.annot*par("cex"),cex.annot),las=ylab.las)
	
	# Draw the horizontal, dotted line at y = 0
	segments_with_breaks(x0=SP,x1=EP,y0=0,y1=0, lty="dotted", lwd=1, col="black",ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
	
	# Add the SNP data
	add.SNP(locus=locus,snp=snp,r2.hue=r2.hue,breaks.info=breaks.info,hit=hit,range=range,SP=SP,EP=EP,snap.snp.offset.x=snap.snp.offset.x,snap.snp.offset.y=snap.snp.offset.y,snp.number=1,snap.snp.label.cex=snap.snp.label.cex)
	
	## Sort out the association data for the 2nd SNP
    if(!is.na(snp2)){
		data <- snp2$data
		snp <- snp2$snp
		
		# Effective position is supplied position or HapMap coordinate.
		data$Position[is.na(data$Position)] <- data$Coordinate[is.na(data$Position)]
      
		# Default r^2 to zero if not known
		data$RSquared[is.na(data$RSquared)] <- 0
		
		data <- subset(data, (!is.na(data$PValue) & !is.na(data$Position)))
		data$PValue[which(data$PValue < 0)]<-1
		
		# Prepare data for plotting reference SNP name
		locusname <- paste(snp, " region", sep="")
		locus <- data
		hit <- locus[snp,]
		
		# Filter out relevant region
		templocus <- subset(locus, locus$Position >= SP & locus$Position < EP)
		locus<-templocus  
		
		# Add the SNP data to the plot
		add.SNP(locus=locus,snp=snp,r2.hue=snp2.r2.hue,breaks.info=breaks.info,hit=hit,range=range,SP=SP,EP=EP,snap.snp.offset.x=snp2$offset.x,snap.snp.offset.y=snp2$offset.y,snp.number=2,snap.snp.label.cex=snap.snp.label.cex) 
		
		# Add legend 
		add.legend.text(SP,EP,2.5, range, breaks.info, snap.cex.axis, snap.r2.legend.x.offset)
	} else{
		# Add legend
		add.legend.text(SP,EP,1.3, range, breaks.info, snap.cex.axis, snap.r2.legend.x.offset)
	}
      
	# Recombination rate legend (out-commented)
	# legend(SP, range-step.y, c("recombination rate"), col="lightblue", lwd=0.8, box.lwd=0.5, box.lty=3)
}


	############################
	## SNAP plot sub-routines ##
	############################
	
	add.SNP <- function (locus, snp, r2.hue, breaks.info, hit, range, SP, EP, snap.snp.offset.x=0, snap.snp.offset.y=0,snp.number=1,snap.snp.label.cex=1) {
		
		if(is.na(snap.snp.offset.x)){snap.snp.offset.x<-0}
		if(is.na(snap.snp.offset.y)){snap.snp.offset.y<-0}
		
		size.pos<-sum(breaks.info$ranges_x[,2]-breaks.info$ranges_x[,1])+(breaks.info$gap_width_x*(length(breaks.info$ranges_x[,2])-1))
    
		## Sorting the types of markers
		
		# genotyped markers
		markers.in.strong.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.8 & tolower(gsub(' ','',locus$SnpType)) == "typed"))
		markers.in.moderate.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.5 & locus$RSquared < 0.8 & tolower(gsub(' ','',locus$SnpType)) == "typed"))
		markers.in.weak.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.2 & locus$RSquared < 0.5 & tolower(gsub(' ','',locus$SnpType)) == "typed"))
		markers.not.in.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared < 0.2 & tolower(gsub(' ','',locus$SnpType)) == "typed"))
		
		# imputed SNPs
		imputed.in.strong.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.8 & tolower(gsub(' ','',locus$SnpType)) == "imputed"))
		imputed.in.moderate.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.5 & locus$RSquared < 0.8 & tolower(gsub(' ','',locus$SnpType)) == "imputed"))
		imputed.in.weak.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.2 & locus$RSquared < 0.5 & tolower(gsub(' ','',locus$SnpType)) == "imputed"))
		imputed.not.in.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared < 0.2 & tolower(gsub(' ','',locus$SnpType)) == "imputed"))
		
		# other SNPs of unrecognized type
		others.in.strong.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.8 & tolower(gsub(' ','',locus$SnpType)) != "typed" & tolower(gsub(' ','',locus$SnpType)) != "imputed"))
		others.in.moderate.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.5 & locus$RSquared < 0.8 & tolower(gsub(' ','',locus$SnpType)) != "typed" & tolower(gsub(' ','',locus$SnpType)) != "imputed"))
		others.in.weak.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared >= 0.2 & locus$RSquared < 0.5 & tolower(gsub(' ','',locus$SnpType)) != "typed" & tolower(gsub(' ','',locus$SnpType)) != "imputed"))
		others.not.in.ld <- subset(locus, (row.names(locus) != snp & locus$RSquared < 0.2 & tolower(gsub(' ','',locus$SnpType)) != "typed" & tolower(gsub(' ','',locus$SnpType)) != "imputed"))
		
		## Plotting Markers for a given SNP

		# plot the other markers
		points_with_breaks(others.not.in.ld$Position, -(log10(others.not.in.ld$PValue)), pch=22, cex=1.0, bg=hsv(r2.hue,others.not.in.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(others.in.weak.ld$Position, -(log10(others.in.weak.ld$PValue)), pch=22, cex=1.0, bg=hsv(r2.hue,others.in.weak.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(others.in.moderate.ld$Position, -(log10(others.in.moderate.ld$PValue)), pch=22, cex=1.0, bg=hsv(r2.hue,others.in.moderate.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(others.in.strong.ld$Position, -(log10(others.in.strong.ld$PValue)), pch=22, cex=1.0, bg=hsv(r2.hue,others.in.strong.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
	
		# plot the imputed SNPs
		points_with_breaks(imputed.not.in.ld$Position, -(log10(imputed.not.in.ld$PValue)), pch=21, cex=0.7, bg=hsv(r2.hue,imputed.not.in.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(imputed.in.weak.ld$Position, -(log10(imputed.in.weak.ld$PValue)), pch=21, cex=0.7, bg=hsv(r2.hue,imputed.in.weak.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(imputed.in.moderate.ld$Position, -(log10(imputed.in.moderate.ld$PValue)), pch=21, cex=0.8, bg=hsv(r2.hue,imputed.in.moderate.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(imputed.in.strong.ld$Position,-(log10(imputed.in.strong.ld$PValue)),pch=21,cex=1.0,bg=hsv(r2.hue,imputed.in.strong.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)

		# plot the genotyped markers
		points_with_breaks(markers.not.in.ld$Position, -(log10(markers.not.in.ld$PValue)), pch=24, cex=1.0, bg=hsv(r2.hue,markers.not.in.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(markers.in.weak.ld$Position, -(log10(markers.in.weak.ld$PValue)), pch=24, cex=1.25, bg=hsv(r2.hue,markers.in.weak.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(markers.in.moderate.ld$Position, -(log10(markers.in.moderate.ld$PValue)), pch=24, cex=1.25, bg=hsv(r2.hue,markers.in.moderate.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(markers.in.strong.ld$Position, -(log10(markers.in.strong.ld$PValue)), pch=24, cex=1.25, bg=hsv(r2.hue,markers.in.strong.ld$RSquared,1),lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		
		# plot the hit
		hit.ypos <- min(-log10(hit$PValue), range)
		pval.text.pos <- 4
    
		# MTS added if !is.na
		if(!is.na(hit$Position)){
			if (hit$Position > SP + 0.75*(EP - SP)){pval.text.pos <- 2}
			
			if(hit$SnpType=="typed"){
				points_with_breaks(hit$Position, hit.ypos, pch=24, cex=2.5, bg=hsv(r2.hue,1,1), col="black",lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
			}else if(hit$SnpType=="imputed"){
				points_with_breaks(hit$Position, hit.ypos, pch=21, cex=2.5, bg=hsv(r2.hue,1,1), col="black",lwd=0.4,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
			}
			text_with_breaks(hit$Position+snap.snp.offset.x , hit.ypos + snap.snp.offset.y,labels=snp,cex=snap.snp.label.cex,font=3,ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		} 
    
		## RSquare legend bar
		step.y <- strheight('w',cex=1.5)# range / 30 # changed from 25
		step.x <- strwidth('w',cex=1.5)
		
		x.pos.r2 <-  EP-(size.pos * 0.005)-((snp.number-1)*step.x)
		
		points_with_breaks(x.pos.r2, range - (step.y * 11), pch=22, cex=2.5, bg=hsv(r2.hue,0,1), col=hsv(r2.hue,0,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 10), pch=22, cex=2.5, bg=hsv(r2.hue,0.1,1), col=hsv(r2.hue,0.1,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 9), pch=22, cex=2.5, bg=hsv(r2.hue,0.2,1), col=hsv(r2.hue,0.2,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 8), pch=22, cex=2.5, bg=hsv(r2.hue,0.3,1), col=hsv(r2.hue,0.3,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 7), pch=22, cex=2.5, bg=hsv(r2.hue,0.4,1), col=hsv(r2.hue,0.4,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 6), pch=22, cex=2.5, bg=hsv(r2.hue,0.5,1), col=hsv(r2.hue,0.5,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 5), pch=22, cex=2.5, bg=hsv(r2.hue,0.6,1), col=hsv(r2.hue,0.6,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 4), pch=22, cex=2.5, bg=hsv(r2.hue,0.7,1), col=hsv(r2.hue,0.7,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 3), pch=22, cex=2.5, bg=hsv(r2.hue,0.8,1), col=hsv(r2.hue,0.8,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - (step.y * 2), pch=22, cex=2.5, bg=hsv(r2.hue,0.9,1), col=hsv(r2.hue,0.9,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
		points_with_breaks(x.pos.r2, range - step.y, pch=22, cex=2.5, bg=hsv(r2.hue,1.0,1), col=hsv(r2.hue,1.0,1),ranges_x=breaks.info$ranges_x, ranges_y=breaks.info$ranges_y, gap_width_x=breaks.info$gap_width_x, gap_width_y=breaks.info$gap_width_y )
	}
	
	
	add.legend.text <- function (SP,EP,num.snps, range, breaks.info, legends.txt.cex, snap.r2.legend.x.offset=0) {
		step.y <- strheight('w',cex=1.5)
		step.x <- strwidth('w',cex=1.5)
		size.pos<-sum(breaks.info$ranges_x[,2]-breaks.info$ranges_x[,1])+(breaks.info$gap_width_x*(length(breaks.info$ranges_x[,2])-1))  
		x.pos.r2 <-  (EP-(size.pos * 0.005)-((num.snps)*step.x))+snap.r2.legend.x.offset
		text_with_breaks(x.pos.r2, range - (step.y * 0), cex=legends.txt.cex, labels=c(paste("1.0")), pos=1,ranges_x=breaks.info$ranges_x,  ranges_y=breaks.info$ranges_y,  gap_width_x=breaks.info$gap_width_x,  gap_width_y=breaks.info$gap_width_y)
		#text_with_breaks(x.pos.r2, range - (step.y * 2), cex=legends.txt.cex, labels=c(paste("0.8")), pos=1,ranges_x=breaks.info$ranges_x,  ranges_y=breaks.info$ranges_y,  gap_width_x=breaks.info$gap_width_x,  gap_width_y=breaks.info$gap_width_y)
		text_with_breaks(x.pos.r2, range - (step.y * 5), cex=legends.txt.cex, labels=c(paste("0.5")), pos=1,ranges_x=breaks.info$ranges_x,  ranges_y=breaks.info$ranges_y,  gap_width_x=breaks.info$gap_width_x,  gap_width_y=breaks.info$gap_width_y)
		text_with_breaks(x.pos.r2, range - (step.y * 10), cex=legends.txt.cex, labels=c(expression(paste(r^2))), pos=1,ranges_x=breaks.info$ranges_x,  ranges_y=breaks.info$ranges_y,  gap_width_x=breaks.info$gap_width_x,  gap_width_y=breaks.info$gap_width_y)
	}
	