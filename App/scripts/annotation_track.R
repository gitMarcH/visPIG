annotation_track_with_breaks<-function(annot.dat,annot.classes,annot.col,chr,SP,EP,bg.col="white",gaps,annot.ylab="",annot.lwd=1e-2,annot.bgclass=NULL,bounding.box=FALSE,...){
	# plots an annotation track
	# INPUT arguments:
	#	annot.dat = annotation data in bed format; columns are chr, start_pos, end_pos, class, ...
	#	annot.classes = vector of classes that should be plotted
	#	annot.col = vector of the same length of annot.classes and giving the colours that should be used for each class
	#	chr = chromosome to plot (can be 0 of conversion to genome-wide coordinates is used; SP and EP should be gw-positions in that case)
	#	SP = starting position of the region to plot
	#	EP = end position of the region to plot
	#	bg.col = colour to use for the background (defaults to white)
	#	gaps = list with the gaps on the x-axis
	#	annot.ylab = y-axis label
	#	annot.lwd = line width to be used for drawing polygons
	#	annot.bgclass = name(s) of the "background" class(es), that get(s) plotted first, with all other classes plotted on top of this/these; if NULL, regions get plotted in the order they appear in the file
	#	bounding.box = logical; should a bounding box be drawn around the annotation track (defaults to FALSE)
	#	... = further parameters for plot_with_breaks()
	
	# filter out region of interest
	if(typeof(annot.dat[,1])=="character"){annot.dat[,1]<-as.integer(sub(annot.dat[,1],pattern="chr",replacement=""))}
	annot.dat<-annot.dat[annot.dat[,1]==chr & ( (annot.dat[,2]>=SP & annot.dat[,2]<=EP) | (annot.dat[,3]>=SP & annot.dat[,3]<=EP) | (annot.dat[,2]<=SP & annot.dat[,3]>=EP)),]
	
	if(!is.null(gaps)){
		if(!is.null(gaps[[1]])){
			for(i in 1:nrow(gaps[[1]])){
				annot.dat<-annot.dat[!(annot.dat[,2]>=gaps[[1]][i,1] & annot.dat[,2]<=gaps[[1]][i,2] & annot.dat[,3]>=gaps[[1]][i,1] & annot.dat[,3]<=gaps[[1]][i,2]),]
			}
		}
	}

	if( nrow(annot.dat) > 0 ){
		
		# reorder (if requested)
		if(!is.null(annot.bgclass)){
			annot.dat<-rbind(annot.dat[is.element(el=annot.dat[,4],set=annot.bgclass),],annot.dat[!is.element(el=annot.dat[,4],set=annot.bgclass),])
		}
		
		# set up polygons for each functional region
		x.poly<-cbind(annot.dat[,c(2,2,3,3)],rep(NA,nrow(annot.dat)))
		x.poly<-as.vector(t(x.poly))
		ymin=0
		ymax=1
		y.poly<-rep(c(ymin,ymax,ymax,ymin,NA),nrow(annot.dat))
		if(!is.null(gaps)){
			if(!is.null(gaps[[1]])){
				n_gaps<-nrow(gaps[[1]])
			}else{
				n_gaps<-0
			}
		}else{
			n_gaps<-0
		}
	
		#set up colours
		col.poly<-rep(bg.col,nrow(annot.dat))
		if(length(annot.classes)!=length(annot.col)){stop("The number of colours does not match the number of annotation classes.")}
		for(i in 1:length(annot.classes)){
			col.poly[annot.dat[,4]==annot.classes[i]]<-annot.col[i]
		}
	
		# do the plot
		breaks.info<-plot_with_breaks(x=c(SP,EP),y=c(0,1),plot.axes='n',ylab=annot.ylab,type="n",xlim=c(SP,EP),ylim=c(0,1),gaps=gaps,...)
		if(n_gaps==0){
			polygon_with_breaks(x=c(SP,SP,EP,EP),y=c(ymin,ymax,ymax,ymin),col=bg.col,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
		}else if(n_gaps==1){
			polygon_with_breaks(x=c(SP,SP,gaps[[1]][1,1],gaps[[1]][1,1]),y=c(ymin,ymax,ymax,ymin),col=bg.col,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
			polygon_with_breaks(x=c(gaps[[1]][1,2],gaps[[1]][1,2],EP,EP),y=c(ymin,ymax,ymax,ymin),col=bg.col,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
		}else if(n_gaps>1){
			polygon_with_breaks(x=c(SP,SP,gaps[[1]][1,1],gaps[[1]][1,1]),y=c(ymin,ymax,ymax,ymin),col=bg.col,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
			for(i in 2:n_gaps){
				polygon_with_breaks(x=c(gaps[[1]][i-1,2],gaps[[1]][i-1,2],gaps[[1]][i,1],gaps[[1]][i,1]),y=c(ymin,ymax,ymax,ymin),col=bg.col,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
			}
			polygon_with_breaks(x=c(gaps[[1]][n_gaps,2],gaps[[1]][n_gaps,2],EP,EP),y=c(ymin,ymax,ymax,ymin),col=bg.col,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
		}

		polygon_with_breaks(x=x.poly,y=y.poly,col=col.poly,border=NA,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)

		if(bounding.box){
			if(n_gaps==0){
				polygon_with_breaks(x=c(SP,SP,EP,EP),y=c(ymin,ymax,ymax,ymin),col=NA,border="black",ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
			}else if(n_gaps==1){
				polygon_with_breaks(x=c(SP,SP,gaps[[1]][1,1],gaps[[1]][1,1]),y=c(ymin,ymax,ymax,ymin),col=NA,border="black",ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
				polygon_with_breaks(x=c(gaps[[1]][1,2],gaps[[1]][1,2],EP,EP),y=c(ymin,ymax,ymax,ymin),col=NA,border="black",ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
			}else if(n_gaps>1){
				polygon_with_breaks(x=c(SP,SP,gaps[[1]][1,1],gaps[[1]][1,1]),y=c(ymin,ymax,ymax,ymin),col=NA,border="black",ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
				for(i in 2:n_gaps){
					polygon_with_breaks(x=c(gaps[[1]][i-1,2],gaps[[1]][i-1,2],gaps[[1]][i,1],gaps[[1]][i,1]),y=c(ymin,ymax,ymax,ymin),col=NA,border="black",ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
				}
				polygon_with_breaks(x=c(gaps[[1]][n_gaps,2],gaps[[1]][n_gaps,2],EP,EP),y=c(ymin,ymax,ymax,ymin),col=NA,border="black",ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=annot.lwd,is.rectangle=TRUE)
			}
		}
	}else{
		plot_with_breaks(x=0,y=0,type="n",plot.axes='n',xlab="",ylab=annot.ylab,main="",xlim=c(SP,EP),ylim=c(0,0),gaps=gaps,...)
	}
}
	