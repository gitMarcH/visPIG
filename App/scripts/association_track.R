association_track_with_breaks<-function(assoc.dat,chr,SP,EP,col.vect=colorRampPalette(c("white","red","darkred"))(256),lwd.bars=1e-3,gaps=NULL,pval.scale=NULL,assoc.ylab="",...){
	# plots association data as a track with breaks on the x-axis
	# INPUT arguments:
	#	assoc.dat = association data file: column 1 is chr, columns 2 is the position of each SNP, column 3 is the association p-value
	#	chr = chromosome to plot
	#	SP = starting position of the region to plot
	#	EP = end position of the region to plot
	#	col.vect = graphical parameter giving the vector of colours to use to show how strong interactions are; NB needs to have length 256
	#	lwd.bars = width for drawing the vertical lines
	#	gaps = list of gaps on the x-axis
	#	pval.scale = vector with minimum and maximum of the colour scale used to plot p-values; if set to NULL, these are computed from the input data
	#	assoc.ylab = label for the y-axis
	#	... = further parameters for plot_with_breaks()

	# process data
	assoc.dat<-assoc.dat[assoc.dat[,1]==chr & assoc.dat[,2]>=SP & assoc.dat[,2]<=EP,]
		if(nrow(assoc.dat)>0){
		
		if(!is.null(gaps)){
			if(!is.null(gaps[[1]])){
				for(i in 1:nrow(gaps[[1]])){
					assoc.dat<-assoc.dat[assoc.dat[,2]<=gaps[[1]][i,1] | assoc.dat[,2]>=gaps[[1]][i,2],]
				}
			}
		}
		
		assoc.dat<-assoc.dat[order(-assoc.dat[,3]),]
		neglogpvals<-(-log10(assoc.dat[,3]))
		
		# compute colours
		if(is.null(pval.scale)){
			col.idx<-ceiling(0.001+254*(neglogpvals-min(neglogpvals))/(max(neglogpvals)-min(neglogpvals)))
		}else{
			minneglogP<-min(-log10(pval.scale))
			maxneglogP<-max(-log10(pval.scale))
			neglogpvals[neglogpvals<minneglogP]<-minneglogP
			neglogpvals[neglogpvals>maxneglogP]<-maxneglogP
			col.idx<-ceiling(0.001+254*(neglogpvals-minneglogP)/(maxneglogP-minneglogP))
		}
	
		# plot the data
		breaks.info<-plot_with_breaks(type="h",assoc.dat[,2],rep(1,nrow(assoc.dat)),ylab=assoc.ylab,xlab="",col=col.vect[col.idx],xlim=c(SP,EP),ylim=c(0,1),yaxt="n",lwd=lwd.bars,gaps=gaps,...)
	}else{
		plot_with_breaks(x=0,y=0,type="n",plot.axes='n',xlab="",ylab=assoc.ylab,main="",xlim=c(SP,EP),ylim=c(0,0),gaps=gaps,...)
	}
}
