heat_track_with_breaks<-function(heat.dat,chr,SP,EP,heatSP=NA,heatEP=NA,lwd.line=1e-5,bw,symmetrise=TRUE,triangularise=TRUE,mai=c(0.3,0.3,0.1,0.1),get.size=FALSE,gaps,ylim=NULL,zlim=NULL,col.scale=NULL,col.zero=NULL,heat.ylab="",chop.data.gaps=FALSE,logscale=FALSE,heat.dir="upwards",heat.type="pos-pos",...){
	# plots interaction data as a barplot
	# INPUT arguments:
	#	heat.dat = heat map data file: col1=chr1, col2=pos1, col3=chr2, col4=pos2, col5=intensity
	#	chr = chromosome to plot
	#	SP = starting position of the region to plot
	#	EP = end position of the region to plot
	#	heatSP, heatEP = start and end positions of the region (included in ([SP,EP]) for which the heat map should be plotted
	#	col.vect = vector with colours to plot the data
	#	lwd.line = width of lines
	#	bw = width of each bin
	#	symmetrise = logical; is the heat map symmetrical or should it be first symmetrised?
	#	triangularise = logical; should the heat map (after symmetrisation) be triangularised by keeping only the upper diagonal part?
	#	mai = as in plot_with_breaks()
	#	get.size = logical; if TRUE no plot will be produced but instead the code will return the ratio of the maximum distance in the heat map to the length of the plotted region
	#	gaps = list with gaps
	#	ylim = range of values that the y-axis should span (defaults to NULL and gets determined from the input data)
	#	zlim = range of values that the colour range will span (defaults to NULL and gets determined from the input data)
	#	col.scale = a vector of colours used to plot the heat map; if not specified a red scale is used by default
	#	col.zero = special colour for zero values; if NULL, no special colour is used for zero values (default)
	#	heat.ylab = y-axis label
	#	chop.data.gaps = (logical) should the data be filtered so that only those data with both positions outside a gap are plotted (defaults to FALSE)
	#	logscale = (logical) if set to TRUE the data will be plotted on the log scale
	#	heat.dir = direction of the heat map triangle; either "upwards", pointing upwards, or "downwards", pointing downwards
	#	heat.type = type of the heatmap; one of "pos-pos" or "general"; the former is rotated 45 degrees ad shows only half of the diagonal matrix, the latter must have genomic position on x-axis, but can have numerical data on both y- and z-axes
	#	... = additional parameters for plot_with_breaks()

	# process data (filter out region of interest, symmetrise and compute distances)
	if(is.na(heatSP)){heatSP<-SP}
	if(is.na(heatEP)){heatEP<-EP}
		
	if(heat.type=="pos-pos"){
		heat.dat<-heat.dat[heat.dat[,1]==chr & heat.dat[,2]>=heatSP & heat.dat[,2]<=heatEP & heat.dat[,2]>=SP & heat.dat[,2]<=EP & heat.dat[,3]==chr & heat.dat[,4]>=heatSP & heat.dat[,4]<=heatEP & heat.dat[,4]>=SP & heat.dat[,4]<=EP,1:5]
		
		if( nrow(heat.dat) > 0){	
			
			if(chop.data.gaps){
				if(!is.null(gaps)){
					if(!is.null(gaps[[1]])){
						for(i in 1:nrow(gaps[[1]])){
							heat.dat<-heat.dat[!((heat.dat[,2]>=gaps[[1]][i,1] & heat.dat[,2]<=gaps[[1]][i,2]) | (heat.dat[,4]>=gaps[[1]][i,1] & heat.dat[,4]<=gaps[[1]][i,2])),]
						}
					}
				}
			}
			
			if(symmetrise){
				heat.dat<-rbind(heat.dat,heat.dat[,c(3,4,1,2,5)])
			}
			if(triangularise){
				heat.dat<-heat.dat[heat.dat[,4]>=heat.dat[,2],]
			}
			heat.dat<-cbind(heat.dat[,c(2,4,5)])
			
			if(nrow(heat.dat)>0){
				
				if(logscale){
					heat.dat<-heat.dat[heat.dat[,3]>0,]
					heat.dat[,3]<-log(heat.dat[,3])
					if(!is.null(zlim)){
						if(min(zlim)<=0){stop("the colour scale limits specified for the heatmap track need to be strictly positive when a log scale is used.")}
						zlim<-log(zlim)
					}
				}		
				
				# compute height of heat map
				if(!chop.data.gaps){
					ymax<-max(heat.dat[,2]-heat.dat[,1])
				}else{
					ymax<-0
					if(!is.null(gaps) & !is.null(gaps[[1]])){
						idx.tmp<-which(heat.dat[,1]<=gaps[[1]][1,1] & heat.dat[,2]<=gaps[[1]][1,1])
						if(length(idx.tmp)>0){ymax<-max(ymax,max(heat.dat[idx.tmp,2]-heat.dat[idx.tmp,1]))}
						if(nrow(gaps[[1]])>1){
							for(i in 2:nrow(gaps[[1]])){
								idx.tmp<-which(heat.dat[,1]>=gaps[[1]][i-1,2] & heat.dat[,1]<=gaps[[1]][i,1] & heat.dat[,2]>=gaps[[1]][i-1,2] & heat.dat[,2]<=gaps[[1]][i,1])
								if(length(idx.tmp)>0){ymax<-max(ymax,max(heat.dat[idx.tmp,2]-heat.dat[idx.tmp,1]))}
							}
						}
						idx.tmp<-which(heat.dat[,1]>=gaps[[1]][nrow(gaps[[1]]),2] & heat.dat[,2]>=gaps[[1]][nrow(gaps[[1]]),2])
						if(length(idx.tmp)>0){ymax<-max(ymax,max(heat.dat[idx.tmp,2]-heat.dat[idx.tmp,1]))}
					}else{
						idx.tmp<-which(heat.dat[,1]>=SP & heat.dat[,1]<=EP & heat.dat[,2]>=SP & heat.dat[,2]<=EP)
						ymax<-max(heat.dat[idx.tmp,2]-heat.dat[idx.tmp,1])
					}
				}
				ymax<-ymax+bw
				if(!is.null(ylim)){ymax<-max(c(0,min(c(ymax-min(ylim),max(ylim)-min(ylim)))))}else{ylim<-c(0,ymax)}
				if(heat.dir=="downwards"){ylim<-c(max(ylim),min(ylim))}else if(heat.dir=="upwards"){ylim<-c(min(ylim),max(ylim))}else{stop("heatmap direction (heat.dir) not clear; needs to be one of \"upwards\" or \"downwards\".")}
				
				# plot the data
				if(!get.size){
					breaks.info<-plot_with_breaks(type="n",x=c(SP,EP),y=c(0,ymax),mai=mai,gaps=gaps,ylab=heat.ylab,ylim=ylim,...)
					
					if(is.null(col.scale)){
						mid.point<-25
						col.scale<-c(rgb(maxColorValue=mid.point,red=mid.point,green=mid.point:0,blue=mid.point:0),rgb(maxColorValue=254-mid.point,red=(254-mid.point):0,green=0,blue=0))
					}
					heat.zlim<-heat.plot.rot45deg.track_with_breaks(X=heat.dat,SP=SP,EP=EP,width.rect=bw,col.scale=col.scale,chop.diagonal=TRUE,lwd.poly=lwd.line,mai=mai,add=TRUE,breaks.info=breaks.info,zlim=zlim,...)
				}
			
				# figure out size of plot (if required)
				if(get.size){
					div.tmp<-(EP-SP)
					length.x.tmp<-ymax
					if(!is.null(gaps)){
						if(!is.null(gaps[[1]])){
							for(i in 1:nrow(gaps[[1]])){
								div.tmp<-div.tmp-(gaps[[1]][i,2]-gaps[[1]][i,1])
							}
						}
					}
				}
			}else{
				length.x.tmp<-0
				heat.zlim<-c(0,0)
				if(!get.size){plot_with_breaks(x=0,y=0,type="n",xlab="",ylab=heat.ylab,main="",xlim=c(SP,EP),ylim=c(0,0),gaps=gaps,mai=mai,...)}
			}
			if(get.size){return(length.x.tmp/div.tmp)}else{return(heat.zlim)}
		}else{
			if(get.size){return(0)}else{plot_with_breaks(x=0,y=0,type="n",xlab="",ylab=heat.ylab,main="",xlim=c(SP,EP),ylim=c(0,0),gaps=gaps,mai=mai,...); return(0)}
		}
	}else if(heat.type=="general"){
		# filter data for quicker processing
		heat.dat<-heat.dat[heat.dat[,1]==chr & ((heat.dat[,2]>=heatSP & heat.dat[,2]<=heatEP) | (heat.dat[,3]>=heatSP & heat.dat[,3]<=heatEP)),1:6]
		if(!is.null(ylim)){
			heat.dat<-heat.dat[(heat.dat[,4]>=min(ylim) & heat.dat[,4]<=max(ylim)) | (heat.dat[,5]>=min(ylim) & heat.dat[,5]<=max(ylim)),]
		}else{ylim<-range(c(heat.dat[,4],heat.dat[,5]))}
		
		if( nrow(heat.dat) > 0){	
			# set various parameters
			if(logscale){
				heat.dat<-heat.dat[heat.dat[,6]>0,]
				heat.dat[,6]<-log(heat.dat[,6])

				if(!is.null(zlim)){
					if(min(zlim)<=0){stop("the colour scale limits specified for the heatmap track need to be strictly positive when a log scale is used.")}
					zlim<-log(zlim)
				}
			}
			
			if(is.null(col.scale)){
				ncol<-256
				col.scale<-heat.colours(ncol)
			}else{
				ncol<-length(col.scale)
			}
			
			if(is.null(zlim)){
				zlim<-c(min(heat.dat[,6]),max(heat.dat[,6]))
			}else{
				zlim<-sort(zlim)
			}
		
			# do the plot
			breaks.info<-plot_with_breaks(type="n",x=c(SP,EP),y=ylim,mai=mai,gaps=gaps,ylab=heat.ylab,ylim=ylim,...)
	
			x.poly<-as.vector(rbind(heat.dat[,2],heat.dat[,2],heat.dat[,3],heat.dat[,3],rep(NA,nrow(heat.dat))))
			y.poly<-as.vector(rbind(heat.dat[,4],heat.dat[,5],heat.dat[,5],heat.dat[,4],rep(NA,nrow(heat.dat))))
			z<-heat.dat[,6]
			z[!is.na(z) & z<zlim[1]]<-zlim[1]
			z[!is.na(z) & z>zlim[2]]<-zlim[2]
			colour.idx<-1+trunc(ncol*(z-zlim[1])/(zlim[2]-zlim[1])-1e-6)
			col.plot<-col.scale[colour.idx]
			if(!is.null(col.zero)){col.plot[z==0]<-col.zero}
			
			if(par("usr")[3]>par("usr")[4]){
				if(nrow(breaks.info$ranges_y)==1){breaks.info$ranges_y<-matrix(breaks.info$ranges_y[,2:1],ncol=2)}else{breaks.info$ranges_y<-breaks.info$ranges_y[,2:1]}
			}
			
			polygon_with_breaks(x=x.poly,y=y.poly,col=col.plot,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.line)
			return(zlim)
		}else{return(0)}
	}
}




##########################
## REQUIRED SUBROUTINES ##
##########################

heat.plot.rot45deg.track_with_breaks<-function(X,SP,EP,width.rect=1,rm.NA=TRUE,zlim=NULL,bg.true=FALSE,bg.col="black",col.scale=NULL,col.zero=NULL,pty.heat="m",chop.diagonal=FALSE,lwd.poly=1e-5,asp=0.5,mai=c(0.3,0.3,0.1,0.1),add=FALSE,breaks.info=NULL,...){
	# plots the heatmap of a three-column matrix; the matrix is rotated so that the y axis is the distance from the diagonal
	# assumes all coordinates are in integers
	#	X = matrix with x-axis and y-axis coordinates on the first two columns and the variable to be plotted on the colour scale on the third column
	#	SP, EP = start and end of the region to plot
	#	width.rect = the width of the rectangles plotted at each coordinate; defaults to 1 (assuming integer coordinates; if the data are binned, the bin width should be used here)
	#	rm.NA = logical; should NA be removed; defaults to TRUE
	#	triangularise = logical; should the input data be reduced to the upper triangular (including the diagonal) before processing
	#	zlim = optional; specify the range of values the colour scale should span
	#	bg.true = (only set this to TRUE if you are plotting triangular matrix with non-missing values for the bins at the three vertices) logical; should a background be drawn? defaults to FALSE
	#	bg.col = if bg.col==TRUE, then this specifies the colour of the background
	#	col.scale = a colour scale to be used; defaults to NULL in which case heat.colours(256)=colorRampPalette(colors=c("#320000","red","yellow","lightyellow"))(256) gets used
	#	col.zero = special colour for zero values; if NULL, no special colour is used for zero values (default)
	#	pty.heat = pty parameter (see ?par) for the heat map part of the plot; defaults to "m", i.e. will make use of the maximum drawing region
	#	chop.diagonal = should the diagonal elements be plotted as diamonds (FALSE; default) or triangles (TRUE)
	#	lwd.poly = line width of the polygons; if the plot is high-resolution, a large lwd parameter (e.g. lwd=1) will result in rounded edges of the polygons and they will appear to overlap, hence by default quite small (1e-5)
	#	asp = (see ?par); apect ratio y/x for the heat map; defaults to 0.5 so that the diamonds have sides of equal lengths
	#	add = logical; should the heat map be added to an existing plot (TRUE) or should a new plot be started (FALSE)
	#	breaks.info = if add=T, specifies the information about the potential gaps
	# 	... = further parameters for plot_with_breaks
	
	if(rm.NA){
		X<-X[!is.na(X[,1]) & !is.na(X[,2]) & !is.na(X[,3]),]
	}
	
	if(is.null(col.scale)){
		ncol<-256
		col.scale<-heat.colours(ncol)
	}else{
		ncol<-length(col.scale)
	}
	
	x<-X[,1]+(X[,2]-X[,1])/2
	y<-X[,2]-X[,1]
	z<-X[,3]
	
	if(is.null(zlim)){
		zlim<-c(min(z),max(z))
	}

	if(!add){
		breaks.info<-plot_with_breaks(type="n",x=c(SP,EP),y=c(0,max(y)),asp=asp,mai=mai,xlim=c(SP,EP),...)
	}
	if(bg.true){polygon_with_breaks(x=c(min(x)-width.rect/2,min(x)+(max(x)-min(x))/2,max(x)+width.rect/2),y=c(min(y),max(y)+width.rect,min(y)),col=bg.col,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.poly)}
	if(!chop.diagonal){
		x.poly<-rbind(x+width.rect/2,x,x+width.rect/2,x+width.rect,rep(NA,length(x)))
		x.poly<-as.vector(x.poly)
		y.poly<-rbind(y-width.rect,y,y+width.rect,y,rep(NA,length(y)))
		y.poly<-as.vector(y.poly)
		
		colour.idx<-round(ncol*(z-zlim[1])/(zlim[2]-zlim[1]))
		colour.idx[colour.idx==0]<-1
		col.plot<-col.scale[colour.idx]
		if(!is.null(col.zero)){col.plot[z==0]<-col.zero}
	}else{
		x.offdiag<-x[y!=0]; x.diag<-x[y==0]
		y.offdiag<-y[y!=0]; y.diag<-y[y==0]
		z.offdiag<-z[y!=0]; z.diag<-z[y==0]
		
		x.poly1<-rbind(x.offdiag+width.rect/2,x.offdiag,x.offdiag+width.rect/2,x.offdiag+width.rect,rep(NA,length(x.offdiag)))
		y.poly1<-rbind(y.offdiag-width.rect,y.offdiag,y.offdiag+width.rect,y.offdiag,rep(NA,length(y.offdiag)))
		x.poly2<-rbind(x.diag+width.rect/2,x.diag,x.diag+width.rect/2,x.diag+width.rect,rep(NA,length(x.diag)))
		y.poly2<-rbind(y.diag,y.diag,y.diag+width.rect,y.diag,rep(NA,length(y.diag)))
		x.poly<-cbind(x.poly1,x.poly2)
		y.poly<-cbind(y.poly1,y.poly2)
		rm(x.poly1,x.poly2,y.poly1,y.poly2,x.offdiag,x.diag,y.offdiag,y.diag)
		
		x.poly<-as.vector(x.poly)
		y.poly<-as.vector(y.poly)
		
		z<-c(z.offdiag,z.diag)
		rm(z.offdiag,z.diag)
		z[!is.na(z) & z<zlim[1]]<-zlim[1]
		z[!is.na(z) & z>zlim[2]]<-zlim[2]
		colour.idx<-1+trunc(ncol*(z-zlim[1])/(zlim[2]-zlim[1])-1e-6)
		col.plot<-col.scale[colour.idx]
		if(!is.null(col.zero)){col.plot[z==0]<-col.zero}
	}
	if(par("usr")[3]>par("usr")[4]){
		if(nrow(breaks.info$ranges_y)==1){breaks.info$ranges_y<-matrix(breaks.info$ranges_y[,2:1],ncol=2)}else{breaks.info$ranges_y<-breaks.info$ranges_y[,2:1]}
	}
	polygon_with_breaks(x=x.poly,y=y.poly,col=col.plot,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.poly)

        return(zlim)
}
