interaction_track_with_breaks<-function(hic.dat,chr,SP,EP,compute_ntracks_only=FALSE,block.height=0.9,col.ref=colors()[30],col.nonref=colors()[53],hic.ylab="",hic.capt.SP=NA,hic.capt.EP=NA,hic.capt.col="green",hic.capt.density=50,hic.capt.lines.lwd=1,arches=FALSE,arches.type="round",arches.twist=TRUE,arches.lwd=1e-2,arches.nsegments=250,arches.neglog10=TRUE,arches.ylim=NULL,arches.col="darkred",arches.yaxis=TRUE,arches.varicol=TRUE,arches.dir=NULL,relplotheight=1,hic.plotThroughGaps=FALSE,col.throughGaps="grey",hic.plotThroughGapsMaxY=FALSE,...){
	# plots Hi-C interactions as a set of tracks with breaks on the x-axis
	# NB requires sourcing of the script 'axis_breaks.R' or else will not work
	# INPUT arguments:
	#	hic.dat = Hi-C data file in bed format: column 1 is chr, columns 2 & 3 are start and end position of each bin, column 4 indicates whether a bin is the reference bin in an interaction group (0) or not (1) and column 5 indicates which group the bin belongs to
	#	chr = chromosome to plot
	#	SP = starting position of the region to plot
	#	EP = end position of the region to plot
	#	compute_ntracks_only = logical; should only the number of required tracks be computed and no plot produced? (defaults to FALSE)
	#	block.height, col.ref, col.nonref = graphical parameters indicating how high each box should be and which colours should be used for reference and non-refence bins
	#	col.ref / col.nonref = colours for the reference / non-reference bin colours
	#	hic.ylab = y-axis label for the interaction plot
	#	hic.capt.SP/EP = SP & EP of the capture region to be indicated on the plot
	#	hic.capt.col = colour for the capture region box
	#	hic.capt.density = density of the shading lines for the capture region box
	#	arches = should the itneractions be drawn as arches, TRUE or FALSE?
	#	arches.type = specifies the type of arc used; options are "round" or "flat"
	#	arches.twist = if arches==TRUE, then this will indicate whether the arches should be a straight (FALSE) or twisted (TRUE; default) ribbon
	#	arches.lwd = line width used for plotting the arches
	#	arches.nsegments = number of segments an arc is made up of; higher numbers mean smoother arc, but also larger plot file size in bytes
	#	arches.neglog10 = whether or not the 7th column in an arches-format interaction data file should be recomputed as -log10 (TRUE) or not (FALSE)
	#	arches.ylim = upper and lower limits for the y-axis if arches are to be plotted
	#	arches.col = colour to be used for plotting arches
	#	arches.yaxis = should a y-axis scale be printed (TRUE) or not (FALSE)
	#	arches.varicol = should the colour strength of each arc reflect the strength of the signal (TRUE) or not (FALSE)?
	#	arches.dir = one of NULL, "upwards", "downwards" -- specifies the direction of the arches; if NULL, then direction is taken from arches.ylim
	#	relplotheight = parameter that gets passed through from plot_vispig.R
	#	hic.plotThroughGaps = logical; should ineractions be indicated crossing the gaps?
	#	col.throughGaps = colour for dashed lines inficating interactions that cross the gaps
	#	hic.plotThroughGapsMaxY = logical; should the maximum Y value achieved inside the gap be attained?
	#	... = further parameters for plot_with_breaks()

	if(!arches){	
		# reformat data
		if(typeof(hic.dat[,1])=="character"){hic.dat[,1]<-as.integer(sub(x=hic.dat[,1],pattern="chr",replacement=""))}
	
		# filter out only interactions for given chromosome and region
		hic.dat.tmp<-hic.dat[hic.dat[,1]==chr & ((hic.dat[,2]>=SP & hic.dat[,2]<=EP) | (hic.dat[,3]>=SP & hic.dat[,3]<=EP)),]
		if(nrow(hic.dat.tmp)>0){
			groups.tmp<-unique(hic.dat.tmp[,5])
			hic.dat<-hic.dat[is.element(el=hic.dat[,5],set=groups.tmp),]
			rm(hic.dat.tmp,groups.tmp)
		
			# plan tracks
			unique_groups<-unique(hic.dat[,5])
			ntracks<-length(unique_groups)
			minpos<-rep(NA,ntracks)
			maxpos<-rep(NA,ntracks)
			for(i in 1:ntracks){
				minpos[i]<-min(hic.dat[hic.dat[,5]==unique_groups[i],3]) # the minimum end position of the group
				maxpos[i]<-max(hic.dat[hic.dat[,5]==unique_groups[i],2]) # the maximum start position of the group
			}
	
			# plot
			if(!compute_ntracks_only){
				hic.ylim<-c(1-block.height/2,ntracks+block.height/2)
				breaks.info<-plot_with_breaks(x=c(SP,EP),y=c(1,ntracks),type="n",xlim=c(SP,EP),ylim=hic.ylim,ylab=hic.ylab,...)
				
				if(!is.na(hic.capt.SP) & !is.na(hic.capt.EP)){
					par(xpd=TRUE)
					delta.ylim<-hic.ylim[2]-hic.ylim[1]
					lines_with_breaks(lwd=hic.capt.lines.lwd,x=c(hic.capt.SP,hic.capt.SP),y=hic.ylim+c(-delta.ylim,delta.ylim),col=hic.capt.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
					lines_with_breaks(lwd=hic.capt.lines.lwd,x=c(hic.capt.EP,hic.capt.EP),y=hic.ylim+c(-delta.ylim,delta.ylim),col=hic.capt.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
					delta.ylim<-1.2*(hic.ylim[2]-hic.ylim[1])
					for(i in 1:hic.capt.density){
						lines_with_breaks(lwd=hic.capt.lines.lwd,x=c(hic.capt.SP,hic.capt.EP),y=c(hic.ylim[1]-0.1*(hic.ylim[2]-hic.ylim[1])+(i-1)*delta.ylim/hic.capt.density,hic.ylim[1]-0.1*(hic.ylim[2]-hic.ylim[1])+i*delta.ylim/hic.capt.density),col=hic.capt.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
					}
					par(xpd=FALSE)
				}
				
				for(i in 1:ntracks){
					idx.tmp<-(1:nrow(hic.dat))[hic.dat[,5]==unique_groups[i]]
					nblocks<-length(idx.tmp)
					x.poly<-as.vector(rbind(hic.dat[idx.tmp,2],hic.dat[idx.tmp,2],hic.dat[idx.tmp,3],hic.dat[idx.tmp,3],rep(NA,nblocks)))
					y.poly<-as.vector(rbind(rep(i-block.height/2,nblocks),rep(i+block.height/2,nblocks),rep(i+block.height/2,nblocks),rep(i-block.height/2,nblocks),rep(NA,nblocks)))
					segments_with_breaks(x0=minpos[i],y0=i,x1=maxpos[i],y1=i,col=col.nonref,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=1)
					if(hic.plotThroughGaps){
						segments_retain_breaks_xonly(x0=minpos[i],y0=i,x1=maxpos[i],y1=i,col=col.throughGaps,lty=2,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=1)
					}
					polygon_with_breaks(x=x.poly,y=y.poly,col=c(col.ref,col.nonref)[1+hic.dat[idx.tmp,4]],border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=1e-2,is.rectangle=TRUE)
				}
			}else{
				return(ntracks)
			}
		}else{
			if(!compute_ntracks_only){
				plot_with_breaks(x=0,y=0,type="n",xlab="",ylab=hic.ylab,main="",xlim=c(SP,EP),ylim=c(0,0),...)
			}else{
				ntracks<-0
				return(ntracks)
			}
		}
	}else{
		ntracks<-1
		
		if(!compute_ntracks_only){
			# filter out only interactions for given chromosome and region
			hic.dat<-hic.dat[(hic.dat[,1]==chr & ((hic.dat[,2]>=SP & hic.dat[,2]<=EP) | (hic.dat[,3]>=SP & hic.dat[,3]<=EP))) | (hic.dat[,4]==chr & ((hic.dat[,5]>=SP & hic.dat[,5]<=EP) | (hic.dat[,6]>=SP & hic.dat[,6]<=EP))),]
		
			# rescale numerical variable if requested
			if(arches.neglog10){hic.dat[,7]<-(-log10(hic.dat[,7]))}
		
			# set y-axis limits
			if(is.null(arches.ylim)){arches.ylim<-c(min(c(0,min(hic.dat[,7]))),max(hic.dat[,7]))}
			if(!is.null(arches.dir) & arches.dir=="upwards"){arches.ylim<-c(min(arches.ylim),max(arches.ylim))}else if(!is.null(arches.dir) & arches.dir=="downwards"){arches.ylim<-c(max(arches.ylim),min(arches.ylim))}
		
			# plot
			breaks.info<-plot_with_breaks(x=c(SP,EP),y=arches.ylim,type="n",xlim=c(SP,EP),ylim=arches.ylim,ylab=hic.ylab,...)
			arches.ylim<-sort(arches.ylim)
			
			if(!is.na(hic.capt.SP) & !is.na(hic.capt.EP)){
				par(xpd=TRUE)

				delta.ylim<-arches.ylim[2]-arches.ylim[1]
				lines_with_breaks(lwd=lines.lwd,x=c(hic.capt.SP,hic.capt.SP),y=arches.ylim+c(-delta.ylim,delta.ylim),col=hic.capt.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				lines_with_breaks(lwd=lines.lwd,x=c(hic.capt.EP,hic.capt.EP),y=arches.ylim+c(-delta.ylim,delta.ylim),col=hic.capt.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				delta.ylim<-1.2*(arches.ylim[2]-arches.ylim[1])
				for(i in 1:hic.capt.density){
					lines_with_breaks(lwd=lines.lwd,x=c(hic.capt.SP,hic.capt.EP),y=c(arches.ylim[1]-0.1*(arches.ylim[2]-arches.ylim[1])+(i-1)*delta.ylim/hic.capt.density,arches.ylim[1]-0.1*(arches.ylim[2]-arches.ylim[1])+i*delta.ylim/hic.capt.density),col=hic.capt.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				}
				par(xpd=FALSE)
			}
			
			col_palette<-colorRampPalette(c("white",arches.col))(250)
			for(i in 1:nrow(hic.dat)){
				if(arches.varicol){col.tmp<-col_palette[min(c(250,round( 250*hic.dat[i,7]/abs(arches.ylim[2]-arches.ylim[1]) )))]}else{col.tmp<-arches.col}
				if(arches.type=="round"){
					arc_ribbon_with_breaks(x0=unlist(hic.dat[i,2:3]),x1=unlist(hic.dat[i,5:6]),y0=arches.ylim[1],y1=hic.dat[i,7],xscale=sum(breaks.info$ranges_x[,2]-breaks.info$ranges_x[,1]),yscale=arches.ylim[2]-arches.ylim[1],yscale.fatten=1/relplotheight,lwd.line=arches.lwd,twist=arches.twist,n_segments=arches.nsegments,col=col.tmp,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,plot.throughGaps=hic.plotThroughGaps,col.throughGaps=col.throughGaps,plot.throughGapsMaxY=hic.plotThroughGapsMaxY)
				}else if(arches.type=="flat"){
					flat_arc_with_breaks(x0=unlist(hic.dat[i,2:3]),x1=unlist(hic.dat[i,5:6]),y0=arches.ylim[1],y1=hic.dat[i,7],xscale=sum(breaks.info$ranges_x[,2]-breaks.info$ranges_x[,1]),yscale=arches.ylim[2]-arches.ylim[1],yscale.fatten=1/relplotheight,lwd.line=arches.lwd,n_segments=arches.nsegments,col=col.tmp,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,plot.throughGaps=hic.plotThroughGaps,col.throughGaps=col.throughGaps)
				}
			}
		}
		
		return(ntracks)
	}
}

## required subroutines for the interaction track

arc_ribbon_with_breaks<-function(x0,x1,y0,y1,xscale=1,yscale=1,yscale.fatten=1,n_segments=250,col,twist=TRUE,ranges_x,ranges_y,gap_width_x,gap_width_y,lwd.line=1e-2,plot.throughGaps=FALSE,col.throughGaps="grey",plot.throughGapsMaxY=FALSE,...){
	# draws an arc on an existing plot
	# arguments
	#	x0 = start positions of the arc on the x-axis (this needs to be a vector of length 2, even for ribbons of zero width!)
	#	x1 = end positions of the arc on the x-axis (this needs to be a vector of length 2, even for ribbons of zero width!)
	#	y0 = start position of the arc on the y-axis (this is a scalar of length 1)
	#	y1 = end position of the arc on the y-axis (this is a scalar of length 1)
	#	xscale, yscale = [only used if twist==FALSE] gives the scale of the x-axis and the y-axis; e.g. if the x-ais stretches from 1 to 10, but the y-axis from 0 to 1, then xscale=10-1=9 and yscale=1-0=1
	#	yscale.fatten = will artificially increase the yscale by a factor of yscale.fatten should it be required
	#	n_segments = number of segments to discretise the arc into
	#	twist = should it be a twisted ribbon (TRUE; default) or a ribbon preserving its width throughout the entire arc (FALSE)?
	#	ranges_x, ranges_y, gap_width_x, gap_width_y = the usual parameters for *_with_breaks functions; cf. axis_breaks.R for further info
	#	lwd.line = line width
	#	plot.throughGaps = logical; should arcs been drawn across gaps?
	#	col.throughGaps = colour for dashed lines inficating interactions that cross the gaps
	#	plot.throughGapsMaxY = logical; should the maximum Y value achieved inside the gap be attained?
	#	... = additional parameters for the segments() routine
	# NB a plotting window has to be initialised before running this R routine!
	
	if(length(x0)!=2){stop("x0 needs to be a vector of length 2!")}
	if(length(x1)!=2){stop("x1 needs to be a vector of length 2!")}
	if(length(y0)!=1){stop("y0 needs to be a vector of length 1!")}
	if(length(y1)!=1){stop("y1 needs to be a vector of length 1!")}
	
	x0<-sort(x0)
	x1<-sort(x1)
	if(x0[1]>=x1[1] & x0[2]>=x1[2]){x0.tmp<-x0; x0<-x1; x1<-x0.tmp; rm(x0.tmp)}
	
	for(i in 1:nrow(ranges_y)){ranges_y[i,]<-sort(ranges_y[i,])}
	
	x_mid<-(sum(x0)/2+sum(x1)/2)/2
	if(twist){
		rad_x_1<-max(c(x0[1],x1[1]))-x_mid
		rad_x_2<-max(c(x0[2],x1[2]))-x_mid
		rad_y_1<-max(c(y0,y1))-min(c(y0,y1))
		rad_y_2<-max(c(y0,y1))-min(c(y0,y1))
	}else{
		rad_x_1<-max(c(x0[1],x1[2]))-x_mid
		rad_x_2<-max(c(x0[2],x1[1]))-x_mid
		delta_y<-yscale.fatten*yscale*( (x0[2]-x0[1]) + (x1[2]-x1[1]) )/(2*xscale)
		if(y1>=y0){y1alt<-y1-delta_y}else{y1alt<-y1+delta_y}
		rad_y_1<-max(c(y0,y1))-min(c(y0,y1))
		rad_y_2<-max(c(y0,y1alt))-min(c(y0,y1alt))
	}

	if(y0<=y1){
		angle1<-seq(0,pi,length=n_segments)
	}else{
		angle1<-seq(pi,2*pi,length=n_segments)
	}
	if(y0<=y1){
		angle2<-seq(0,pi,length=n_segments)
	}else{
		angle2<-seq(pi,2*pi,length=n_segments)
	}
	
	x_out<-x_mid+rad_x_1*cos(angle1)
	y_out<-y0+rad_y_1*sin(angle1)
	x_in<-x_mid+rad_x_2*cos(angle2)
	y_in<-y0+rad_y_2*sin(angle2)
	
	x.poly<-c(x_out,x_in[n_segments:1])
	y.poly<-c(y_out,y_in[n_segments:1])
		
	for(i in 1:nrow(ranges_x)){
		if(sum(x.poly>=ranges_x[i,1] & x.poly<=ranges_x[i,2])>0){
			y.tmp<-y.poly[x.poly>=ranges_x[i,1] & x.poly<=ranges_x[i,2]]
			x.tmp<-x.poly[x.poly>=ranges_x[i,1] & x.poly<=ranges_x[i,2]]
			for(j in 1:nrow(ranges_y)){
				idxTmp<-which(y.tmp>=ranges_y[j,1] & y.tmp<=ranges_y[j,2])
				if(length(idxTmp)>0){
					x.tmp.tmp<-x.tmp[idxTmp]
					y.tmp.tmp<-y.tmp[idxTmp]
					polygon_with_breaks(x=x.tmp.tmp,y=y.tmp.tmp,col=col,border=col,ranges_x=ranges_x,ranges_y=ranges_y,gap_width_x=gap_width_x,gap_width_y=gap_width_y,lwd=lwd.line)
				}
			}
		}
		if(plot.throughGaps){
			if(nrow(ranges_x)>1){
				gaps<-matrix(nrow=0,ncol=2)
				for(i in 1:(nrow(ranges_x)-1)){
					gaps<-rbind(gaps,c(ranges_x[i,2],ranges_x[i+1,1]))
				}
				for(i in 1:nrow(gaps)){
					idx.tmp<-which(x.poly>=gaps[i,1] & x.poly<=gaps[i,2])
					if(length(idx.tmp)>1){
						x.tmp<-x.poly[idx.tmp]
						y.tmp<-y.poly[idx.tmp]
						idx.minx<-which(x.tmp==min(x.tmp))
						idx.maxx<-which(x.tmp==max(x.tmp))
						idx.maxy<-which(y.tmp==max(y.tmp))
						xminx<-x.tmp[idx.minx[1]]
						yminx<-mean(y.tmp[idx.minx])
						xmaxx<-x.tmp[idx.maxx[1]]
						ymaxx<-mean(y.tmp[idx.maxx])
						xmaxy<-mean(x.tmp[idx.maxy])
						ymaxy<-y.tmp[idx.maxy[1]]
						if(plot.throughGapsMaxY){
							segments_retain_breaks_xonly(x0=xminx,y0=yminx,x1=xmaxy,y1=ymaxy,col=col.throughGaps,lty=2,ranges_x=ranges_x,ranges_y=ranges_y,gap_width_x=gap_width_x,gap_width_y=gap_width_y,lwd=1)
							segments_retain_breaks_xonly(x0=xmaxy,y0=ymaxy,x1=xmaxx,y1=ymaxx,col=col.throughGaps,lty=2,ranges_x=ranges_x,ranges_y=ranges_y,gap_width_x=gap_width_x,gap_width_y=gap_width_y,lwd=1)
						}else{
							segments_retain_breaks_xonly(x0=xminx,y0=yminx,x1=xmaxx,y1=ymaxx,col=col.throughGaps,lty=2,ranges_x=ranges_x,ranges_y=ranges_y,gap_width_x=gap_width_x,gap_width_y=gap_width_y,lwd=1)
						}
					}
				}
			}
		}
	}
}


flat_arc_with_breaks<-function(x0,x1,y0,y1,xscale=1,yscale=1,yscale.fatten=1,n_segments=250,col,ranges_x,ranges_y,gap_width_x,gap_width_y,lwd.line=1e-2,plot.throughGaps=FALSE,col.throughGaps="grey",...){
	# draws a flat arc on an existing plot
	# arguments
	#	x0 = start positions of the arc on the x-axis (this needs to be a vector of length 2, even for ribbons of zero width!)
	#	x1 = end positions of the arc on the x-axis (this needs to be a vector of length 2, even for ribbons of zero width!)
	#	y0 = start position of the arc on the y-axis (this is a scalar of length 1)
	#	y1 = end position of the arc on the y-axis (this is a scalar of length 1)
	#	xscale, yscale = [only used if twist==FALSE] gives the scale of the x-axis and the y-axis; e.g. if the x-ais stretches from 1 to 10, but the y-axis from 0 to 1, then xscale=10-1=9 and yscale=1-0=1
	#	yscale.fatten = will artificially increase the yscale by a factor of yscale.fatten should it be required
	#	n_segments = number of segments to discretise the arc into
	#	ranges_x, ranges_y, gap_width_x, gap_width_y = the usual parameters for *_with_breaks functions; cf. axis_breaks.R for further info
	#	lwd.line = line width
	#	plot.throughGaps = logical; should arcs been drawn across gaps?
	#	col.throughGaps = colour for dashed lines inficating interactions that cross the gaps
	#	... = additional parameters for the segments() routine
	# NB a plotting window has to be initialised before running this R routine!

	if(length(x0)!=2){stop("x0 needs to be a vector of length 2!")}
	if(length(x1)!=2){stop("x1 needs to be a vector of length 2!")}
	if(length(y0)!=1){stop("y0 needs to be a vector of length 1!")}
	if(length(y1)!=1){stop("y1 needs to be a vector of length 1!")}
	
	x0<-sort(x0)
	x1<-sort(x1)
	if(x0[1]>=x1[1] & x0[2]>=x1[2]){x0.tmp<-x0; x0<-x1; x1<-x0.tmp; rm(x0.tmp)}
	
	delta_y<-yscale.fatten*yscale*( (x0[2]-x0[1]) + (x1[2]-x1[1]) )/(2*xscale)
	if(y1>=y0){y1alt<-y1-delta_y}else{y1alt<-y1+delta_y}
	
	for(i in 1:nrow(ranges_y)){ranges_y[i,]<-sort(ranges_y[i,])}
	
	if(nrow(ranges_x)>1){
		points_out.tmp<-segments_with_breaks(plot.true=FALSE,x0=x0[1],y0=y1,x1=x1[2],y1=y1,ranges_x=ranges_x,ranges_y=ranges_y,gap_width_x=gap_width_x,gap_width_y=gap_width_y)
		points_in.tmp<-segments_with_breaks(plot.true=FALSE,x0=x0[2],y0=y1,x1=x1[1],y1=y1,ranges_x=ranges_x,ranges_y=ranges_y,gap_width_x=gap_width_x,gap_width_y=gap_width_y)
		x_out<-x0[1]
		if(nrow(points_out.tmp>=1)){for(i in 1:nrow(points_out.tmp)){x_out<-c(x_out,points_out.tmp[i,1:2])}}
		x_out<-c(x_out,x1[2])
		x_in<-x0[2]
		if(nrow(points_in.tmp>=1)){for(i in 1:nrow(points_in.tmp)){x_in<-c(x_in,points_in.tmp[i,1:2])}}
		x_in<-c(x_in,x1[1])
		rm(points_out.tmp,points_in.tmp)
		
	}else{
		x_out<-c(x0[1],x0[1],x1[2],x1[2])
		x_in<-c(x0[2],x0[2],x1[1],x1[1])
	}
	y_out<-c(y0,rep(y1,length=length(x_out)-2),y0)
	y_in<-c(y0,rep(y1alt,length=length(x_in)-2),y0)
	
	x.poly<-c(x_out,x_in[length(x_out):1])
	y.poly<-c(y_out,y_in[length(x_in):1])
		
	for(i in 1:nrow(ranges_x)){
		if(sum(x.poly>=ranges_x[i,1] & x.poly<=ranges_x[i,2])>0){
			y.tmp<-y.poly[x.poly>=ranges_x[i,1] & x.poly<=ranges_x[i,2]]
			x.tmp<-x.poly[x.poly>=ranges_x[i,1] & x.poly<=ranges_x[i,2]]
			for(j in 1:nrow(ranges_y)){
				if(sum(y.tmp>=ranges_y[j,1] & y.tmp<=ranges_y[j,2])>0){
					x.tmp.tmp<-x.tmp[y.tmp>=ranges_y[j,1] & y.tmp<=ranges_y[j,2]]
					y.tmp.tmp<-y.tmp[y.tmp>=ranges_y[j,1] & y.tmp<=ranges_y[j,2]]
					polygon_with_breaks(x=x.tmp.tmp,y=y.tmp.tmp,col=col,border=col,ranges_x=ranges_x,ranges_y=ranges_y,gap_width_x=gap_width_x,gap_width_y=gap_width_y,lwd=lwd.line)
				}
			}
		}
		if(plot.throughGaps){
			if(nrow(ranges_x)>1){
				gaps<-matrix(nrow=0,ncol=2)
				for(i in 1:(nrow(ranges_x)-1)){
					gaps<-rbind(gaps,c(ranges_x[i,2],ranges_x[i+1,1]))
				}
				for(i in 1:nrow(gaps)){
					idx.tmp<-which(x.poly>=gaps[i,1] & x.poly<=gaps[i,2])
					if(length(idx.tmp)>1){
						x.tmp<-x.poly[idx.tmp]
						y.tmp<-y.poly[idx.tmp]
						idx.minx<-which(x.tmp==min(x.tmp))
						idx.maxx<-which(x.tmp==max(x.tmp))
						idx.maxy<-which(y.tmp==max(y.tmp))
						xminx<-x.tmp[idx.minx[1]]
						yminx<-max(y.tmp[idx.minx])
						xmaxx<-x.tmp[idx.maxx[1]]
						ymaxx<-max(y.tmp[idx.maxx])
						segments_retain_breaks_xonly(x0=xminx,y0=yminx,x1=xmaxx,y1=ymaxx,col=col.throughGaps,lty=2,ranges_x=ranges_x,ranges_y=ranges_y,gap_width_x=gap_width_x,gap_width_y=gap_width_y,lwd=1)
					}
				}
			}
		}			
	}
}
