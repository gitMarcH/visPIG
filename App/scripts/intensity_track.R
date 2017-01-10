int_track_with_breaks<-function(int.dat,chr,SP,EP,col.vect,lwd.line=1,gaps=NULL,plot.axes="y",ylab="",int.ylim=NULL,int.cex.axis=1,int.style="barplot",int.refbox=NA,int.refbox.col="green",int.refbox.density=25,int.ref2box=NA,int.refbox.lines.lwd=1,int.ref2box.col="red",int.ref2box.density=25,int.ref2box.lines.lwd=1,smooth.window=0,smooth.method="avg",...){
	# plots intenisty data as a barplot or lines
	# INPUT arguments:
	#	int.dat = association data file: column 1 is chr, columns 2 is the start position of each sample, column 3 the end position and column 4 is the intensity or score
	#	chr = chromosome to plot
	#	SP = starting position of the region to plot
	#	EP = end position of the region to plot
	#	col.vect = vector with colours to plot the data
	#	lwd.line = width of lines
	#	gaps = list of gaps
	#	plot.axes = as in plot_with_breaks()
	#	ylab = as in plot()
	#	int.ylim = ylim from plot()
	#	int.cex.axis = cex for axis labels as in usual plot()
	#	int.style = one of "barplot" (default) or "lineplot"; the intensities are either plotted as bar spanning (on the x-axis) the two positions and of height given by the intensities (barplot) or as points centred on the middle of the two positions and connected to each other (lineplot)  
	#	int.refbox = a matrix with 2 columns specifying SP & EP of the reference bin(s) to be indicated on the plot; one row per bin; if NA, no such reference bins are indicated (default)
	#	int.refbox.col = colour to be used for displaying the reference bin(s)
	#	int.refbox.density = density of the shading lines to indicate the reference bin(s)
	#	int.ref2box = a matrix with 2 columns specifying SP & EP of the secondary reference bin(s) to be indicated on the plot; one row per bin; if NA, no such reference bins are indicated (default)
	#	int.ref2box.col = colour to be used for displaying the secondary reference bin(s)
	#	int.ref2box.density = density of the shading lines to indicate the secondary reference bin(s)
	#	smooth.window = size (in bp) of the window over which to smooth values
	#	smooth.method = method for smoothing; one of "avg" (average), "min" (minimum) or "max" (maximum)
	#	... = optional parameters for plot_with_breaks()

	# process data
	int.dat<-int.dat[!is.na(int.dat[,1]) & !is.na(int.dat[,2]) & !is.na(int.dat[,3]) & !is.na(int.dat[,4]),]

	col.vect<-col.vect[int.dat[,1]==chr & int.dat[,2]>=(SP-min(1e6,abs(EP-SP)/3)) & int.dat[,2]<=(EP+min(1e6,abs(EP-SP)/3)) & int.dat[,3]>=(SP-min(1e6,abs(EP-SP)/3)) & int.dat[,3]<=(EP+min(1e6,abs(EP-SP)/3))]
	int.dat<-int.dat[int.dat[,1]==chr & int.dat[,2]>=(SP-min(1e6,abs(EP-SP)/3)) & int.dat[,2]<=(EP+min(1e6,abs(EP-SP)/3)) & int.dat[,3]>=(SP-min(1e6,abs(EP-SP)/3)) & int.dat[,3]<=(EP+min(1e6,abs(EP-SP)/3)),]
	if( nrow(int.dat) > 0 ){
		
		if(!is.null(gaps)){
			if(!is.null(gaps[[1]])){
				for(i in 1:nrow(gaps[[1]])){
					col.vect<-col.vect[(int.dat[,2]<=(gaps[[1]][i,1]+5e5) | int.dat[,2]>=(gaps[[1]][i,2]-5e5)) & (int.dat[,3]<=(gaps[[1]][i,1]+5e5) | int.dat[,3]>=(gaps[[1]][i,2]-5e5))]
					int.dat<-int.dat[(int.dat[,2]<=(gaps[[1]][i,1]+5e5) | int.dat[,2]>=(gaps[[1]][i,2]-5e5)) & (int.dat[,3]<=(gaps[[1]][i,1]+5e5) | int.dat[,3]>=(gaps[[1]][i,2]-5e5)),]
				}
			}
		}
		
		if(plot.axes=="x" | plot.axes=="xy"){plot.axes.tmp="x"}else{plot.axes.tmp="n"}
	
		# smooth the data if requested
		if(is.null(smooth.window)){smooth.window<-0}
		if(is.na(smooth.window)){smooth.window<-0}
		if(smooth.window>0){
			int.dat.tmp<-matrix(nrow=0,ncol=4)
			
			if(!is.null(gaps)){
				if(!is.null(gaps[[1]])){
					regions.tmp<-matrix(nrow=0,ncol=2)
					regions.tmp<-rbind(regions.tmp,c(SP,gaps[[1]][1,1]))
					if(nrow(gaps[[1]])==1){
						regions.tmp<-rbind(regions.tmp,c(gaps[[1]][1,2],EP))
					}else{
						for(i in 2:nrow(gaps[[1]])){
							regions.tmp<-rbind(regions.tmp,c(gaps[[1]][i-1,2],gaps[[1]][i,1]))
						}
						regions.tmp<-rbind(regions.tmp,c(gaps[[1]][nrow(gaps[[1]]),2],EP))
					}
					for(i in 1:nrow(regions.tmp)){
						dat.tmp<-int.dat[int.dat[,3]>=(regions.tmp[i,1]-abs(regions.tmp[i,2]-regions.tmp[i,1])/4) & int.dat[,2]<=(regions.tmp[i,2]+abs(regions.tmp[i,2]-regions.tmp[i,1])/4),] # limit to data within regions allowing for some buffer at edges
						#dat.tmp<-int.dat
						n.tmp<-nrow(dat.tmp)
						if(n.tmp>0){
							if(n.tmp>1){stepsize<-unique(dat.tmp[2:n.tmp,2]-dat.tmp[1:(n.tmp-1),2])}else{stepsize<-abs(dat.tmp[1,2]-dat.tmp[1,1])}
							if(length(stepsize)>1){
								stepsize.min=min(stepsize)
								if(length(unique(stepsize %% stepsize.min))==1 && unique(stepsize %% stepsize.min)==0){
									set.steps<-seq(from=min(dat.tmp[,2]),to=max(dat.tmp[,2]),by=stepsize.min)
									set.steps<-setdiff(set.steps,dat.tmp[,2])
									dat.tmp<-rbind(dat.tmp,cbind(chr,set.steps,set.steps+stepsize.min,0,deparse.level=0))
									stepsize<-stepsize.min
								}else{
									stop("smoothing of intensity tracks only supported for now for equally spaced position values in the input file")
								}
							}
							n.sum<-ceiling(smooth.window/stepsize)
							n.sum.left=floor(n.sum/2)
							n.sum.right=n.sum-n.sum.left
							if(smooth.window>stepsize){
								dat.tmp<-dat.tmp[sort(dat.tmp[,2],method="quick",index.return=TRUE)$ix,]
								if(smooth.method=="avg" | smooth.method=="sum"){
									tmp.val<-rep(0,n.tmp-n.sum+1)
									for(i in 1:n.sum){tmp.val<-tmp.val+dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4]}
									if(smooth.method=="avg"){tmp.val<-tmp.val/n.sum}
									int.dat.tmp<-rbind(int.dat.tmp,cbind(chr,dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),2],dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),3],tmp.val,deparse.level=0))
								}else if(smooth.method=="min"){
									tmp.val<-rep(max(int.dat[,4]),n.tmp-n.sum+1)
									for(i in 1:n.sum){tmp.val[tmp.val>dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4]]<-(dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4])[tmp.val>dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4]]}
									int.dat.tmp<-rbind(int.dat.tmp,cbind(chr,dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),2],dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),3],tmp.val,deparse.level=0))
								}else if(smooth.method=="max"){
									tmp.val<-rep(min(int.dat[,4]),n.tmp-n.sum+1)
									for(i in 1:n.sum){tmp.val[tmp.val<dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4]]<-(dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4])[dat.tmp<int.dat[(1:(n.tmp-n.sum+1))+(i-1),4]]}
									int.dat.tmp<-rbind(int.dat.tmp,cbind(chr,dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),2],dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),3],tmp.val,deparse.level=0))
								}else{
									int.dat.tmp<-rbind(int.dat.tmp,dat.tmp)
								}
							}else{
								int.dat.tmp<-rbind(int.dat.tmp,dat.tmp)
							}
						}
					}
				}
			}else{
				#dat.tmp<-int.dat[int.dat[,3]>=SP & int.dat[,2]<=EP,]
				dat.tmp<-int.dat
				n.tmp<-nrow(dat.tmp)
				if(n.tmp>0){
					if(n.tmp>1){stepsize<-unique(dat.tmp[2:n.tmp,2]-dat.tmp[1:(n.tmp-1),2])}else{stepsize<-abs(dat.tmp[1,2]-dat.tmp[1,1])}
					if(length(stepsize)>1){
						stepsize.min=min(stepsize)
						if(length(unique(stepsize %% stepsize.min))==1 && unique(stepsize %% stepsize.min)==0){
							set.steps<-seq(from=min(dat.tmp[,2]),to=max(dat.tmp[,2]),by=stepsize.min)
							set.steps<-setdiff(set.steps,dat.tmp[,2])
							dat.tmp<-rbind(dat.tmp,cbind(chr,set.steps,set.steps+stepsize.min,0,deparse.level=0))
							stepsize<-stepsize.min
						}else{
							stop("smoothing of intensity tracks only supported for now for equally spaced position values in the input file")
						}
					}
					n.sum<-ceiling(smooth.window/stepsize)
					n.sum.left=floor(n.sum/2)
					n.sum.right=n.sum-n.sum.left
					if(smooth.window>stepsize){
						dat.tmp<-dat.tmp[sort(dat.tmp[,2],method="quick",index.return=TRUE)$ix,]
						if(smooth.method=="avg" | smooth.method=="sum"){
							tmp.val<-rep(0,n.tmp-n.sum+1)
							for(i in 1:n.sum){tmp.val<-tmp.val+dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4]}
							if(smooth.method=="avg"){tmp.val<-tmp.val/n.sum}
							int.dat.tmp<-rbind(int.dat.tmp,cbind(chr,dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),2],dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),3],tmp.val,deparse.level=0))
						}else if(smooth.method=="min"){
							tmp.val<-rep(max(int.dat[,4]),n.tmp-n.sum+1)
							for(i in 1:n.sum){tmp.val[tmp.val>dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4]]<-(dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4])[tmp.val>dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4]]}
							int.dat.tmp<-rbind(int.dat.tmp,cbind(chr,dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),2],dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),3],tmp.val,deparse.level=0))
						}else if(smooth.method=="max"){
							tmp.val<-rep(min(int.dat[,4]),n.tmp-n.sum+1)
							for(i in 1:n.sum){tmp.val[tmp.val<dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4]]<-(dat.tmp[(1:(n.tmp-n.sum+1))+(i-1),4])[dat.tmp<int.dat[(1:(n.tmp-n.sum+1))+(i-1),4]]}
							int.dat.tmp<-rbind(int.dat.tmp,cbind(chr,dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),2],dat.tmp[(n.sum.left+1):(n.tmp-n.sum.right+1),3],tmp.val,deparse.level=0))
						}else{
							int.dat.tmp<-rbind(int.dat.tmp,dat.tmp)
						}
					}else{
						int.dat.tmp<-rbind(int.dat.tmp,dat.tmp)
					}
				}
			}
			int.dat<-int.dat.tmp
		}

		# plot the data
		if(is.null(int.ylim)){
			int.ylim<-round(c(min(c(0,int.dat[,4])),max(int.dat[,4])),digits=0)
		}
	
		breaks.info<-plot_with_breaks(type="n",x=int.dat[,2],y=int.dat[,3],xlab="",col=col.vect,xlim=c(SP,EP),ylim=int.ylim,plot.axes=plot.axes.tmp,lwd=lwd.line,gaps=gaps,ylab=ylab,...)
		if(!is.na(int.refbox[1])){
			par(xpd=TRUE)
			delta.ylim<-int.ylim[2]-int.ylim[1]
			for(j in 1:nrow(int.refbox)){
				lines_with_breaks(lwd=int.refbox.lines.lwd,x=c(int.refbox[j,1],int.refbox[j,1]),y=c(int.ylim+c(-delta.ylim,delta.ylim)),col=int.refbox.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				lines_with_breaks(lwd=int.refbox.lines.lwd,x=c(int.refbox[j,2],int.refbox[j,2]),y=c(int.ylim+c(-delta.ylim,delta.ylim)),col=int.refbox.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				delta.ylim<-1.2*(int.ylim[2]-int.ylim[1])
				for(i in 1:int.refbox.density){
					lines_with_breaks(lwd=int.refbox.lines.lwd,x=int.refbox[j,],y=c(int.ylim[1]-0.1*(int.ylim[2]-int.ylim[1])+(i-1)*delta.ylim/int.refbox.density,int.ylim[1]-0.1*(int.ylim[2]-int.ylim[1])+i*delta.ylim/int.refbox.density),col=int.refbox.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				}
				delta.ylim<-int.ylim[2]-int.ylim[1]
			}
			par(xpd=FALSE)
		}
		if(!is.na(int.ref2box[1])){
			par(xpd=TRUE)
			delta.ylim<-int.ylim[2]-int.ylim[1]
			for(j in 1:nrow(int.ref2box)){
				lines_with_breaks(lwd=int.ref2box.lines.lwd,x=c(int.ref2box[j,1],int.ref2box[j,1]),y=c(int.ylim+c(-delta.ylim,delta.ylim)),col=int.ref2box.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				lines_with_breaks(lwd=int.ref2box.lines.lwd,x=c(int.ref2box[j,2],int.ref2box[j,2]),y=c(int.ylim+c(-delta.ylim,delta.ylim)),col=int.ref2box.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				delta.ylim<-1.2*(int.ylim[2]-int.ylim[1])
				for(i in 1:int.ref2box.density){
					lines_with_breaks(lwd=int.ref2box.lines.lwd,x=int.ref2box[j,],y=c(int.ylim[1]-0.1*(int.ylim[2]-int.ylim[1])+(i-1)*delta.ylim/int.ref2box.density,int.ylim[1]-0.1*(int.ylim[2]-int.ylim[1])+i*delta.ylim/int.ref2box.density),col=int.ref2box.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				}
				delta.ylim<-int.ylim[2]-int.ylim[1]
			}
			par(xpd=FALSE)
		}
		if(int.style=="barplot"){
			x.poly<-cbind(int.dat[,c(2,2,3,3)],rep(NA,nrow(int.dat)))
			x.poly<-as.vector(t(x.poly))
			y.poly<-cbind(rep(0,nrow(int.dat)),int.dat[,c(4,4)],rep(0,nrow(int.dat)),rep(NA,nrow(int.dat)))
			y.poly[y.poly<min(int.ylim)]<-min(int.ylim)
			y.poly[y.poly>max(int.ylim)]<-max(int.ylim)
			y.poly<-as.vector(t(y.poly))
			polygon_with_breaks(x=x.poly,y=y.poly,col=col.vect,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,is.rectangle=TRUE)
		}else if(int.style=="lineplot"){
			x.line<-(int.dat[,2]+int.dat[,3])/2
			y.line<-int.dat[,4]
			lines_with_breaks(x=x.line,y=y.line,col=col.vect,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.line)
		}
		
		if(plot.axes=="y" | plot.axes=="xy"){
			axis.points<-convert2gaps(z=int.ylim,ranges_z=breaks.info$ranges_y,gap_width_z=breaks.info$gap_width_y)
			axis(side=2,at=axis.points,labels=int.ylim,cex.axis=int.cex.axis)
		}
	}else{
		plot_with_breaks(x=0,y=0,type="n",plot.axes='n',xlab="",ylab=ylab,main="",xlim=c(SP,EP),ylim=c(0,0),gaps=gaps,...)
	}
}

dix_track_with_breaks<-function(int.dat,chr,SP,EP,col1.vect=colorRampPalette(c(colors()[479],colors()[552],colors()[556]))(256),col2.vect=colorRampPalette(c(colors()[81],colors()[254],colors()[362]))(256),lwd.line=1,gaps=NULL,intpos.lim=NULL,intneg.lim=NULL,...){
	# plots directionality index data as a track
	# INPUT arguments:
	#	int.dat = association data file: column 1 is chr, columns 2 is the start position of each sample, column 3 the end position and column 4 is the intensity or score
	#	chr = chromosome to plot
	#	SP = starting position of the region to plot
	#	EP = end position of the region to plot
	#	col1.vect = vector with range of colours for positive value
	#	col2.vect = vector with range of colours for negative value
	#	lwd.line = width of lines
	#	gaps = list of gaps
	#	intpos.lim = vector giving the minimum and maximum values of the positive DI values; if set to NULL, these will be set from the data
	#	intneg.lim = vector giving the minimum and maximum values of the negative DI values; if set to NULL, these will be set from the data
	#	... = optional parameters for plot_with_breaks()

	# process data
	int.dat<-int.dat[int.dat[,1]==chr & int.dat[,2]>=SP & int.dat[,2]<=EP & int.dat[,3]>=SP & int.dat[,3]<=EP,]
	if( nrow(int.dat) > 0){
		
		if(!is.null(gaps)){
			if(!is.null(gaps[[1]])){
				for(i in 1:nrow(gaps[[1]])){
					int.dat<-int.dat[(int.dat[,2]<=gaps[[1]][i,1] | int.dat[,2]>=gaps[[1]][i,2]) & (int.dat[,3]<=gaps[[1]][i,1] | int.dat[,3]>=gaps[[1]][i,2]),]
				}
			}
		}
		
		# assign colours
		if(is.null(intpos.lim)){
			maxcolpos<-max(int.dat[int.dat[,4]>0,4])
			mincolpos<-min(int.dat[int.dat[,4]>0,4])
		}else{
			maxcolpos<-max(intpos.lim)
			maxcolpos<-min(intpos.lim)
		}
		if(is.null(intneg.lim)){
			maxcolneg<-max(int.dat[int.dat[,4]<0,4])
			mincolneg<-min(int.dat[int.dat[,4]<0,4])
		}else{
			maxcolneg<-max(intneg.lim)
			maxcolneg<-min(intneg.lim)
		}
		col.vect<-rep(NA,nrow(int.dat))
		idx.col.pos<-rep(NA,sum(int.dat[,4]>0))
		idx.col.pos<-trunc( 1 + 255*(int.dat[int.dat[,4]>0,4]-mincolpos)/(maxcolpos-mincolpos) )
		col.vect[int.dat[,4]>0]<-col1.vect[idx.col.pos]
		idx.col.neg<-rep(NA,sum(int.dat[,4]>0))
		idx.col.neg<-trunc( 1 + 255*(int.dat[int.dat[,4]<0,4]-mincolneg)/(maxcolneg-mincolneg) )
		col.vect[int.dat[,4]<0]<-col2.vect[idx.col.neg]
		rm(idx.col.pos,idx.col.neg,maxcolpos,mincolpos,maxcolneg,mincolneg)
		
		# plot the data	
		breaks.info<-plot_with_breaks(type="n",x=int.dat[,2],y=int.dat[,3],ylab="",xlab="",col=col.vect,xlim=c(SP,EP),ylim=c(0,1),lwd=lwd.line,gaps=gaps,...)
		x.poly<-cbind(int.dat[,c(2,2,3,3)],rep(NA,nrow(int.dat)))
		x.poly<-as.vector(t(x.poly))
		y.poly<-cbind(rep(0,nrow(int.dat)),rep(1,nrow(int.dat)),rep(1,nrow(int.dat)),rep(0,nrow(int.dat)),rep(NA,nrow(int.dat)))
		y.poly<-as.vector(t(y.poly))
		polygon_with_breaks(x=x.poly,y=y.poly,col=col.vect,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,is.rectangle=TRUE)
	}else{
		plot(0,0,type="n",xlab="",ylab="",main="")
	}
}
