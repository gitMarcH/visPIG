plot_with_breaks<-function(x,y,side=NULL,gaps=NULL,gap_width=0.05,xlim=NULL,ylim=NULL,plot.axes="xy",plot.bars=FALSE,plot.bars.outsideplot=FALSE,break.style="doublebar",lty.bar=2,col.bar="gray",lwd.bar=0.5,gaps.sizes.x=NULL,gaps.sizes.y=NULL,gaps.sizes.offset.x=0,xlab="",ylab="",col="black",range_labels_x=NULL,range_labels_y=NULL,range_labels_fact_x=0.1,range_labels_fact_y=0.1,lwd=1,x.axis.format='Mb',mai=c(1.02,0.82,0.82,0.42),cex.annot=1,axis.labels.slanted=FALSE,axis.labels.angle=300,tickmark_labels_x=NULL,axis.pos.x=NA,axis.side.x=1,axis.side.y=2,tcl.axis=-0.5,lwd.axis=1,lwd.ticks.axis=1,line.axis=NA,line.axis.ticklabels=NA,line.axis.rangelabels=NA,xlab.line=1,ylab.line=1,lend=1,ylab.las=0,x.labels.show.unit=TRUE,...){
	# will call the R plot() function as specified with parameters by ..., but will add gaps in the axes on side specified by the vector side and at positions specified the list gaps
	# input arguments
	#	x,y = data vectors to be plotted againts each other
	#	side = which axis should have gaps (1 = x-axis (below), 2 = y-axis (left); anything else results in no gaps being included); this can be a vector if both axes should have gaps
	#	gaps = a list containing the intervals on each axes that should be left out; first element of the list refers to x-axis, second to y-axis; for each axis a two-column matrix is required with the gaps on rows and start and end positions on columns; if only one axis should have gaps, then this parameter can be a matrix
	#	gap_width = width of each gap relative to the (plotted) range of the data
	#	xlim, ylim = similar to the xlim and ylim in the standard R plot() function
	#	plot.axes = which of the axes should be plotted, none ("n"), x-axis only ("x"), y-axis only ("y") or both ("xy")? (defaults to "xy")
	#	plot.bars = logical; should bars indicating the gap be drawn on the plot? (defaults to FALSE)
	#	plot.bars.outsideplot = logical; should the bars (if drawn) be allowed to continue into the figure margins? (defaults to FALSE)
	#	break.style = style of how a break is indicated on an axis; options are "slanted" (a slanted line at both start and end of each gap) or "doublebar" (a double bar at both start and end of each gap; default); anything else will result in breaks not being indicated except for a gap in the axis
	#	lty.bar, col.bar = line type, width and colour arguments for the bars that indicated the gap limits
	#	gaps.sizes.x, gaps.sizes.y = if a character vector is specified, then within each gap on the corresponding axis, this will be indicated as the corresponding gap sizes; by default no gaps sizes are printed; the length of each vector must match the number of gaps on that axis
	#	gaps.sizes.offset.x = by how much should the gap size indication be printed above the x-axis (below if negative number specified); defaults to 0
	#	xlab, ylab = identical to their plot() counterparts
	#	col = identical to its plot() counterpart
	#	range_labels_x/y = labels for each range to annotate on the plot
	#	range_labels_fact_x/y = what proportion of the y/x total range should the x/y range labels be drawn below / left of the x/y axis?
	#	lwd = identical to its plot() counterpart
	#	x.axis.format = 'Mb' or 'Kb' or 'bp'... if 'Mb' then x-axis labels will be drawn as "x.xxxMb" rather than xxxxxxx
	#	mai = mai as in par()
	#	cex.annot = cex for labelling the axes
	#	axis.labels.slanted = logical; should the x-axis labels be slanted (TRUE) or horizontal (FALSE)
	#	axis.labels.angle = if angle.labels.slanted==TRUE, then this give the angle by which to rotate the label (default = 300)
	#	tickmark_labels_x = matrix with alternative ranges to use for annotating the x-axis
	#	axis.side.x/y = side that the x/y axis should be plotted; x-axis is either 1 (bottom; default) or 3 (top), y-axis is either 2 (left; default) or 4 (right)
  # axis.pos.x = the position of the x axis, i.e. where it intercepts the y-axis. Default NA, means use the automatic positioning.
	#	tcl.axis = same as tcl used in axis() & par()
	#	lwd.axis = same as lwd used in axis()
	#	lwd.ticks.axis = same as lwd.ticks used in axis()
	#	line.axis = how many lines should the axes be drawn into the margin (can be non-integer)
	#	line.axis.ticklabels = how many lines should the tickmark labels be drawn into the margin (can be non-integer)
	#	line.axis.rangelabels = how many lines should the range labels be drawn into the margin (can be non-integer)
	#	xlab.line = how many more lines (than line.axis) should the xlab label be drawn into the margin (can be non-integer)
	#	ylab.line = how many more lines (than line.axis) should the ylab label be drawn into the margin (can be non-integer)
	#	ylab.las = las parameter for ylab annotation (set ylab to 1 or 2 for ylab label to be horizontal / perpendicular to the y-axis)
	#	lend as in par()
	#	... = further input arguments for the plot() function; note that none of the parameters relating to any axis will have any effect; 

	# convert gaps argument into list if given as matrix
  
	if(is.null(gaps)){side<-NULL}
	if(is.null(side)){
		gaps<-NULL
	}else if(sum(side==1)==0 & sum(side==2)==0){
		gaps<-NULL
	}
	if(length(side)==1 & !is.list(gaps)){
		if(side==1){gaps_tmp<-list(); gaps_tmp[[1]]<-gaps; gaps<-gaps_tmp; rm(gaps_tmp)}
		if(side==2){gaps_tmp<-list(); gaps_tmp[[2]]<-gaps; gaps<-gaps_tmp; rm(gaps_tmp)}
	}else if(length(side)==1 & is.list(gaps)){
		if(side==1){gaps[[2]]<-numeric(0)}
		if(side==2){gaps[[1]]<-numeric(0)}
	}
	
	# set xlim and ylim if unspecified
	if(is.null(xlim)){xlim<-c(min(x[!is.na(x)]),max(x[!is.na(x)]))}
	if(is.null(ylim)){ylim<-c(min(y[!is.na(y)]),max(y[!is.na(y)]))}

	#count number of gaps	
	if(is.element(el=1,set=side)){n_gapsx<-nrow(gaps[[1]])}else{n_gapsx<-NULL}
	if(is.element(el=2,set=side)){n_gapsy<-nrow(gaps[[2]])}else{n_gapsy<-NULL}
	
	# define the ranges that will be plotted and assign x- and y-axis interval numbers for each data point to be plotted
	ranges_x<-matrix(nrow=0,ncol=2)
	ranges_y<-matrix(nrow=0,ncol=2)
	
	x_int<-rep(-9,length(x))
	y_int<-rep(-9,length(y))
	if(!is.null(n_gapsx)){
		ranges_x<-rbind(ranges_x,c(xlim[1],(gaps[[1]])[1,1]))
		x_int[x>=ranges_x[1,1] & x<=ranges_x[1,2]]<-1
		if(n_gapsx>1){
			for(i in 2:n_gapsx){
				ranges_x<-rbind(ranges_x,c((gaps[[1]])[i-1,2],(gaps[[1]])[i,1]))
				x_int[x>=ranges_x[i,1] & x<=ranges_x[i,2]]<-i
			}
			ranges_x<-rbind(ranges_x,c((gaps[[1]])[n_gapsx,2],xlim[2]))
			x_int[x>=ranges_x[n_gapsx+1,1] & x<=ranges_x[n_gapsx+1,2]]<-n_gapsx+1
		}else{
			ranges_x<-rbind(ranges_x,c((gaps[[1]])[1,2],xlim[2]))
			x_int[x>=ranges_x[2,1] & x<=ranges_x[2,2]]<-2
		}
	}else{
		ranges_x<-rbind(ranges_x,xlim)
		x_int<-rep(1,length(x))
	}
	if(!is.null(n_gapsy)){
		ranges_y<-rbind(ranges_y,c(ylim[1],(gaps[[2]])[1,1]))
		y_int[y>=ranges_y[1,1] & y<=ranges_y[1,2]]<-1
		if(n_gapsy>1){
			for(i in 2:n_gapsy){
				ranges_y<-rbind(ranges_y,c((gaps[[2]])[i-1,2],(gaps[[2]])[i,1]))
				y_int[y>=ranges_y[i,1] & y<=ranges_y[i,2]]<-i
			}
			ranges_y<-rbind(ranges_y,c((gaps[[2]])[n_gapsy,2],ylim[2]))
			y_int[y>=ranges_y[n_gapsy+1,1] & y<=ranges_y[n_gapsy+1,2]]<-n_gapsy+1
		}else{
			ranges_y<-rbind(ranges_y,c((gaps[[2]])[1,2],ylim[2]))
			y_int[y>=ranges_y[2,1] & y<=ranges_y[2,2]]<-2
		}
	}else{
		ranges_y<-rbind(ranges_y,ylim)
		y_int<-rep(1,length(y))
	}

	# compute the width of the gaps
	if(!is.null(n_gapsx)){gap_width_x<-sum(ranges_x[,2]-ranges_x[,1])*gap_width}else{gap_width_x<-0}
	if(!is.null(n_gapsy)){gap_width_y<-sum(ranges_y[,2]-ranges_y[,1])*gap_width}else{gap_width_y<-0}

	# convert the data and the ranges matrices into new coordinate system
	idx.retain<-(1:length(x))[x_int>0 & y_int>0]
	if(length(idx.retain)<length(x)){warning(paste("There were ",length(x)-length(idx.retain)," data points that were within the removed gaps and will now no longer be plotted.",sep=""))}#; print(cbind(x,y)[-idx.retain,])}

	if(!is.null(n_gapsx)){
		x_new<-convert2gaps(z=x,ranges_z=ranges_x,gap_width_z=gap_width_x)
		ranges_x_new<-convert2gaps(z=ranges_x,ranges_z=ranges_x,gap_width_z=gap_width_x)
		xlim_new<-convert2gaps(z=xlim,ranges_z=ranges_x,gap_width_z=gap_width_x)
	}else{
		x_new<-x
		ranges_x_new<-ranges_x
		xlim_new<-xlim
	}

	if(!is.null(n_gapsy)){
		y_new<-convert2gaps(z=y,ranges_z=ranges_y,gap_width_z=gap_width_y)
		ranges_y_new<-convert2gaps(z=ranges_y,ranges_z=ranges_y,gap_width_z=gap_width_y)
		ylim_new<-convert2gaps(z=ylim,ranges_z=ranges_y,gap_width_z=gap_width_y)
	}else{
		y_new<-y
		ranges_y_new<-ranges_y
		ylim_new<-ylim
	}
	
	# check direction of y-axis annotation
	if(ylab.las==1 | ylab.las==2){
		ylab.txt<-ylab
		ylab<-""
	}
	
	# plot the data without axes but with xlab/ylab labels
	par(xpd=TRUE,mai=mai,lend=lend)
   
	plot(x_new,y_new,axes=FALSE,xlim=xlim_new,ylim=ylim_new,xlab="",ylab="",col=col,lwd=lwd,cex.axis=cex.annot,cex.lab=cex.annot,...)
	axis_info<-par("usr")
	axis_info2<-par("mgp")
	if(is.na(line.axis)){
		axis_lines_x<-sum(axis_info2[2:3])+xlab.line
		axis_lines_y<-sum(axis_info2[2:3])+ylab.line
	}else{
		axis_lines_x<-sum(axis_info2[2:3])+line.axis+xlab.line
		axis_lines_y<-sum(axis_info2[2:3])+line.axis+ylab.line
	}
	if(!is.na(xlab)){axis(side=axis.side.x,at=sum(axis_info[1:2])/2,tick=FALSE,labels=xlab,line=axis_lines_x,cex.axis=cex.annot)}
	if(!is.na(ylab)){axis(side=axis.side.y,at=sum(axis_info[3:4])/2,tick=FALSE,labels=ylab,line=axis_lines_y,cex.axis=cex.annot)}

	# add ylab if ylab is desired to be horizontal
	if(ylab.las==1 | ylab.las==2){
		mtext(ylab.txt,las=ylab.las,side=2,line=3,adj=1,cex=cex.annot)
	}
	
	# add axes with tickmarks
	if(plot.axes!="n"){
		axis_info<-par("usr")
		if(plot.axes=="x" | plot.axes=="xy"){
			if(is.null(n_gapsx)){
				# draw the axis (without tickmarks or labels)
				axis(side=axis.side.x,pos=axis.pos.x,cex.axis=cex.annot,lwd=lwd.axis,lwd.ticks=0,at=xlim_new,labels=rep("",2),line=line.axis)
				
				#format the tickmark labels
				tmp.labels<-axis(side=axis.side.x,lwd=0,labels=FALSE)
				if(x.axis.format=='Mb'){
					tmp.labels.formatted<-paste(sprintf("%.2f",round(tmp.labels/1e6,digits=2)),rep(ifelse(x.labels.show.unit==TRUE,"Mb",''),length(tmp.labels)),sep="")
				}else if(x.axis.format=='Kb'){
					tmp.labels.formatted<-paste(format(as.numeric(sprintf("%.0f",round(tmp.labels/1e3,digits=0))),big.mark=',',scientific=FALSE),rep(ifelse(x.labels.show.unit==TRUE,"Kb",''),length(tmp.labels)),sep="")
				}else{
					tmp.labels.formatted<-format(tmp.labels,big.mark=',',scientific=FALSE)
				}
				
				# draw the tickmarks for the labels
				for(j in 1:length(tcl.axis)){axis(side=axis.side.x,pos=axis.pos.x,cex.axis=cex.annot,lwd=0,lwd.ticks=lwd.ticks.axis,tcl=tcl.axis[j],line=line.axis,at=tmp.labels,label=rep("",length(tmp.labels.formatted)))}
				
				# print the tickmark labels
				axis(side=axis.side.x,pos=axis.pos.x,cex.axis=cex.annot,lwd=0,lwd.ticks=0,tcl=0,line=line.axis.ticklabels,at=tmp.labels,label=tmp.labels.formatted)
			}else{
				for(i in 1:nrow(ranges_x_new)){
					if(is.null(tickmark_labels_x)){
						range.tmp<-ranges_x[i,]
					}else{
						range.tmp<-tickmark_labels_x[i,]
					}
					if(x.axis.format=='Mb'){
						tmp.labels<-paste(sprintf("%.2f",round(range.tmp/1e6,digits=2)),rep(ifelse(x.labels.show.unit==TRUE,"Mb",''),length(range.tmp)),sep="")
					}else if(x.axis.format=='Kb'){
						tmp.labels<-paste(format(as.numeric(sprintf("%.0f",round(range.tmp/1e3,digits=0))),big.mark=',',scientific=FALSE),rep(ifelse(x.labels.show.unit==TRUE,"Kb",''),length(range.tmp)),sep="")
					}else{
						tmp.labels<-format(range.tmp,big.mark=',',scientific=FALSE)
					}
					
					# draw the axis with the tick marks, but without the labels
					for(j in 1:length(tcl.axis)){axis(side=axis.side.x,at=ranges_x_new[i,],labels=rep("",2),cex.axis=cex.annot,lwd=lwd.axis,lwd.ticks=lwd.ticks.axis,tcl=tcl.axis[j],line=line.axis)}
					
					# print the tickmark labels
					if(axis.labels.slanted){
						if(axis.side.x==3){
							if(is.na(line.axis.ticklabels)){
								tmp.y <- axis_info[4]+(axis_info[4]-axis_info[3])*range_labels_fact_x*1.5
							}else{
								# number of lines for the entire plot device
								if(!is.na((par("mai")/par("mar"))[3])){
									l<-par("fin")[2]/((par("mai")/par("mar"))[3])
								}else{
									r_tmp<-(par("mai")/par("mar"))
									if(length(r_tmp[!is.na(r_tmp)>0])){r_tmp<-(r_tmp[!is.na(r_tmp)])[1]}else{r_tmp<-0.2}
									l<-par("fin")[2]/r_tmp
								}
								# number of lines just in the (vertical) plot region
								l_plot<-l-sum(par("mar")[c(1,3)])
								# height of the (vertical) plot region in units of the plot
								h_plot<-par("usr")[4]-par("usr")[3]
								# units of plot per line
								r_line<-h_plot/l_plot
								
								# compute label y-position
								tmp.y<-axis_info[4]+line.axis.ticklabels*r_line
							}
							tmp.adj <- 1                 
						}else{
							if(is.na(line.axis.ticklabels)){
								tmp.y <- axis_info[3]-(axis_info[4]-axis_info[3])*range_labels_fact_x
							}else{
								# number of lines for the entire plot device
								if(!is.na((par("mai")/par("mar"))[3])){
									l<-par("fin")[2]/((par("mai")/par("mar"))[3])
								}else{
									r_tmp<-(par("mai")/par("mar"))
									if(length(r_tmp[!is.na(r_tmp)>0])){r_tmp<-(r_tmp[!is.na(r_tmp)])[1]}else{r_tmp<-0.2}
									l<-par("fin")[2]/r_tmp
								}
								# number of lines just in the (vertical) plot region
								l_plot<-l-sum(par("mar")[c(1,3)])
								# height of the (vertical) plot region in units of the plot
								h_plot<-par("usr")[4]-par("usr")[3]
								# units of plot per line
								r_line<-h_plot/l_plot
								
								# compute label y-position
								tmp.y<-axis_info[3]-line.axis.ticklabels*r_line
							}
							tmp.adj<-0
						}
						text(x=ranges_x_new[i,],y=tmp.y,labels=tmp.labels,cex=cex.annot,srt=axis.labels.angle,adj=tmp.adj)
					}else{
						axis(side=axis.side.x,at=ranges_x_new[i,1],labels=tmp.labels[1],hadj=0,cex.axis=cex.annot,lwd=0,lwd.ticks=0,line=line.axis.ticklabels)
						axis(side=axis.side.x,at=ranges_x_new[i,2],labels=tmp.labels[2],hadj=1,cex.axis=cex.annot,lwd=0,lwd.ticks=0,line=line.axis.ticklabels)
					}
					
					# print the range labels
					if(!is.null(range_labels_x)){
						if(is.na(line.axis.rangelabels)){line.axis.rangelabels<-axis_lines_x/2}
						axis(side=axis.side.x,at=(ranges_x_new[i,1]+ranges_x_new[i,2])/2,tick=FALSE,labels=range_labels_x[i],cex.axis=cex.annot,line=line.axis.rangelabels)
					}
				}
				for(i in 1:n_gapsx){
					if(break.style=="doublebar"){
						if(axis.side.x==1){
							segments(x0=ranges_x_new[i,2],y0=axis_info[3]-gap_width_y/2,x1=ranges_x_new[i,2],y1=axis_info[3]+gap_width_y/2)
							segments(x0=ranges_x_new[i,2]+gap_width_x/15,y0=axis_info[3]-gap_width_y/2,x1=ranges_x_new[i,2]+gap_width_x/15,y1=axis_info[3]+gap_width_y/2)
							segments(x0=ranges_x_new[i+1,1],y0=axis_info[3]-gap_width_y/2,x1=ranges_x_new[i+1,1],y1=axis_info[3]+gap_width_y/2)
							segments(x0=ranges_x_new[i+1,1]-gap_width_x/15,y0=axis_info[3]-gap_width_y/2,x1=ranges_x_new[i+1,1]-gap_width_x/15,y1=axis_info[3]+gap_width_y/2)
						}else if(axis.side.x==3){
							segments(x0=ranges_x_new[i,2],y0=axis_info[4]+gap_width_y/2,x1=ranges_x_new[i,2],y1=axis_info[4]-gap_width_y/2)
							segments(x0=ranges_x_new[i,2]+gap_width_x/15,y0=axis_info[4]+gap_width_y/2,x1=ranges_x_new[i,2]+gap_width_x/15,y1=axis_info[4]-gap_width_y/2)
							segments(x0=ranges_x_new[i+1,1],y0=axis_info[4]+gap_width_y/2,x1=ranges_x_new[i+1,1],y1=axis_info[4]-gap_width_y/2)
							segments(x0=ranges_x_new[i+1,1]-gap_width_x/15,y0=axis_info[4]+gap_width_y/2,x1=ranges_x_new[i+1,1]-gap_width_x/15,y1=axis_info[4]-gap_width_y/2)
						}
					}else if(break.style=="slanted"){
						if(axis.side.x==1){
							segments(x0=ranges_x_new[i,2]-gap_width_x/8,y0=axis_info[3]-gap_width_y/2,x1=ranges_x_new[i,2]+gap_width_x/8,y1=axis_info[3]+gap_width_y/2)
							segments(x0=ranges_x_new[i+1,1]-gap_width_x/8,y0=axis_info[3]-gap_width_y/2,x1=ranges_x_new[i+1,1]+gap_width_x/8,y1=axis_info[3]+gap_width_y/2)
						}else if(axis.side.x==3){
							segments(x0=ranges_x_new[i,2]-gap_width_x/8,y0=axis_info[4]+gap_width_y/2,x1=ranges_x_new[i,2]+gap_width_x/8,y1=axis_info[4]-gap_width_y/2)
							segments(x0=ranges_x_new[i+1,1]-gap_width_x/8,y0=axis_info[4]+gap_width_y/2,x1=ranges_x_new[i+1,1]+gap_width_x/8,y1=axis_info[4]-gap_width_y/2)
						}
					}
					if(!is.null(gaps.sizes.x) & length(gaps.sizes.x)==n_gapsx){
						axis(side=axis.side.x,labels=gaps.sizes.x[i],lwd=0,at=(ranges_x_new[i,2]+ranges_x_new[i+1,1])/2,line=gaps.sizes.offset.x,cex.axis=cex.annot)
						#if(axis.side.x==1){text(x=(ranges_x_new[i,2]+ranges_x_new[i+1,1])/2,y=axis_info[3]+gaps.sizes.offset.x,labels=gaps.sizes.x[i],cex=cex.annot)}
						#if(axis.side.x==3){text(x=(ranges_x_new[i,2]+ranges_x_new[i+1,1])/2,y=axis_info[4]+gaps.sizes.offset.x,labels=gaps.sizes.x[i],cex=cex.annot)}
					}
				}
			}
		}
		if(plot.axes=="y" | plot.axes=="xy"){
			if(is.null(n_gapsy)){
				axis(side=axis.side.y,cex.axis=cex.annot,lwd=lwd.axis,lwd.ticks=0,at=ylim_new,labels=rep("",2),line=line.axis)
				for(j in 1:length(tcl.axis)){axis(side=axis.side.y,cex.axis=cex.annot,lwd=lwd.axis,lwd.ticks=lwd.ticks.axis,tcl=tcl.axis[j],line=line.axis)}
			}else{
				for(i in 1:nrow(ranges_y_new)){
					for(j in 1:length(tcl.axis)){axis(side=axis.side.y,at=ranges_y_new[i,],labels=ranges_y[i,],cex.axis=cex.annot,cex.axis=cex.annot,lwd=lwd.axis,lwd.ticks=lwd.ticks.axis,tcl=tcl.axis[j],line=line.axis)}
					if(!is.null(range_labels_y)){
						axis(side=axis.side.y,at=(ranges_y_new[i,1]+ranges_y_new[i,2])/2,tick=FALSE,labels=range_labels_y[i],cex.axis=cex.annot,line=axis_lines_y/2)
					}
					#segments(y0=ranges_y_new[i,1],x0=axis_info[1],y1=ranges_y_new[i,2],x1=axis_info[1])
				}
				for(i in 1:n_gapsy){
					if(break.style=="doublebar"){
						if(axis.side.y==2){
							segments(x0=axis_info[1]-gap_width_x/2,y0=ranges_y_new[i,2],x1=axis_info[1]+gap_width_x/2,y1=ranges_y_new[i,2])
							segments(x0=axis_info[1]-gap_width_x/2,y0=ranges_y_new[i,2]+gap_width_y/15,x1=axis_info[1]+gap_width_x/2,y1=ranges_y_new[i,2]+gap_width_y/15)
							segments(x0=axis_info[1]-gap_width_x/2,y0=ranges_y_new[i+1,1],x1=axis_info[1]+gap_width_x/2,y1=ranges_y_new[i+1,1])
							segments(x0=axis_info[1]-gap_width_x/2,y0=ranges_y_new[i+1,1]-gap_width_y/15,x1=axis_info[1]+gap_width_x/2,y1=ranges_y_new[i+1,1]-gap_width_y/15)
						}else if(axis.side.y==4){
							segments(x0=axis_info[2]+gap_width_x/2,y0=ranges_y_new[i,2],x1=axis_info[2]-gap_width_x/2,y1=ranges_y_new[i,2])
							segments(x0=axis_info[2]+gap_width_x/2,y0=ranges_y_new[i,2]+gap_width_y/15,x1=axis_info[2]-gap_width_x/2,y1=ranges_y_new[i,2]+gap_width_y/15)
							segments(x0=axis_info[2]+gap_width_x/2,y0=ranges_y_new[i+1,1],x1=axis_info[2]-gap_width_x/2,y1=ranges_y_new[i+1,1])
							segments(x0=axis_info[2]+gap_width_x/2,y0=ranges_y_new[i+1,1]-gap_width_y/15,x1=axis_info[2]-gap_width_x/2,y1=ranges_y_new[i+1,1]-gap_width_y/15)
						}
					}else if(break.style=="slanted"){
						if(axis.side.y==2){
							segments(x0=axis_info[1]+gap_width_x/2,y0=ranges_y_new[i,2]-gap_width_y/8,x1=axis_info[1]-gap_width_x/2,y1=ranges_y_new[i,2]+gap_width_y/8)
							segments(x0=axis_info[1]+gap_width_x/2,y0=ranges_y_new[i+1,1]-gap_width_y/8,x1=axis_info[1]-gap_width_x/2,y1=ranges_y_new[i+1,1]+gap_width_y/8)
						}else if(axis.side.y==4){
							segments(x0=axis_info[2]-gap_width_x/2,y0=ranges_y_new[i,2]-gap_width_y/8,x1=axis_info[2]+gap_width_x/2,y1=ranges_y_new[i,2]+gap_width_y/8)
							segments(x0=axis_info[2]-gap_width_x/2,y0=ranges_y_new[i+1,1]-gap_width_y/8,x1=axis_info[2]+gap_width_x/2,y1=ranges_y_new[i+1,1]+gap_width_y/8)
						}
					}
					if(!is.null(gaps.sizes.y) & length(gaps.sizes.y)==n_gapsy){
						if(axis.side.y==2){text(y=(ranges_y_new[i,2]+ranges_y_new[i+1,1])/2,x=axis_info[1],labels=gaps.sizes.y[i],cex=cex.annot)}
						if(axis.side.y==4){text(y=(ranges_y_new[i,2]+ranges_y_new[i+1,1])/2,x=axis_info[2],labels=gaps.sizes.y[i],cex=cex.annot)}
					}
				}
			}
		}
	}
	par(xpd=FALSE)
	
	# add bars at break points
	if(plot.bars){
		par(xpd=TRUE)
		
		if(!is.null(n_gapsx)){
			for(i in 1:n_gapsx){
				if(plot.bars.outsideplot){
					abline(v=ranges_x_new[i,2],lty=lty.bar,lwd=lwd.bar,col=col.bar)
					abline(v=ranges_x_new[i+1,1],lty=lty.bar,lwd=lwd.bar,col=col.bar)
				}else{
					segments(x0=ranges_x_new[i,2],y0=axis_info[3],x1=ranges_x_new[i,2],y1=axis_info[4],lty=lty.bar,lwd=lwd.bar,col=col.bar)
					segments(x0=ranges_x_new[i+1,1],y0=axis_info[3],x1=ranges_x_new[i+1,1],y1=axis_info[4],lty=lty.bar,lwd=lwd.bar,col=col.bar)
				}
			}
		}
		
		if(!is.null(n_gapsy)){
			for(i in 1:n_gapsy){
				if(plot.bars.outsideplot){
					abline(v=ranges_y_new[i,2],lty=lty.bar,lwd=lwd.bar,col=col.bar)
					abline(v=ranges_y_new[i+1,1],lty=lty.bar,lwd=lwd.bar,col=col.bar)
				}else{
					segments(y0=ranges_y_new[i,2],x0=axis_info[1],y1=ranges_y_new[i,2],x1=axis_info[2],lty=lty.bar,lwd=lwd.bar,col=col.bar)
					segments(y0=ranges_y_new[i+1,1],x0=axis_info[1],y1=ranges_y_new[i+1,1],x1=axis_info[2],lty=lty.bar,lwd=lwd.bar,col=col.bar)
				}
			}
		}
		
		par(xpd=FALSE)
	}
	
	# output ranges matrices for conversion of later data points or curves to be added to the plot
	res<-list(ranges_x,ranges_y,gap_width_x,gap_width_y)
	names(res)<-c("ranges_x","ranges_y","gap_width_x","gap_width_y")
	return(res)
}

text_with_breaks<-function(x,y,ranges_x,ranges_y,gap_width_x,gap_width_y,...){
	x_new<-convert2gaps(z=x,ranges_z=ranges_x,gap_width_z=gap_width_x)
	y_new<-convert2gaps(z=y,ranges_z=ranges_y,gap_width_z=gap_width_y)
	text(x=x_new,y=y_new,...)
}

lines_with_breaks<-function(x,y,ranges_x,ranges_y,gap_width_x,gap_width_y,...){
	x_new<-convert2gaps(z=x,ranges_z=ranges_x,gap_width_z=gap_width_x)
	y_new<-convert2gaps(z=y,ranges_z=ranges_y,gap_width_z=gap_width_y)
	if(nrow(ranges_x)>1){
		x_newnew<-x_new[x<=ranges_x[1,2]]
		y_newnew<-y_new[x<=ranges_x[1,2]]
		for(i in 2:nrow(ranges_x)){
			x_newnew<-c(x_newnew,NA,x_new[x>=ranges_x[i,1] & x<=ranges_x[i,2]])
			y_newnew<-c(y_newnew,NA,y_new[x>=ranges_x[i,1] & x<=ranges_x[i,2]])
		}
		x_new<-x_newnew; rm(x_newnew)
		y_new<-y_newnew; rm(y_newnew)
	}
	if(nrow(ranges_y)>1){
		x_newnew<-x_new[y<=ranges_y[1,2]]
		y_newnew<-y_new[y<=ranges_y[1,2]]
		for(i in 2:nrow(ranges_y)){
			x_newnew<-c(x_newnew,NA,x_new[y>=ranges_y[i,1] & y<=ranges_y[i,2]])
			y_newnew<-c(y_newnew,NA,y_new[y>=ranges_y[i,1] & y<=ranges_y[i,2]])
		}
		x_new<-x_newnew; rm(x_newnew)
		y_new<-y_newnew; rm(y_newnew)
	}
	lines(x_new,y_new,...)
}

points_with_breaks<-function(x,y,ranges_x,ranges_y,gap_width_x,gap_width_y,...){
	x_new<-convert2gaps(z=x,ranges_z=ranges_x,gap_width_z=gap_width_x)
	y_new<-convert2gaps(z=y,ranges_z=ranges_y,gap_width_z=gap_width_y)
	points(x_new,y_new,...)
}

split_segments_one_axis_only<-function(x0,y0,x1,y1,ranges_z,axis){
	segments_to_plot<-list()
	range_id_z<-c(0,0)
	check.noplot<-0
	
	# check that at least one end lies within the extremes of ranges_z
	if(axis=="x"){
		if((x0<ranges_z[1,1] & x1<ranges_z[1,1]) | (x0>ranges_z[nrow(ranges_z),2] & x1>ranges_z[nrow(ranges_z),2])){
			check.noplot<-1
		}else{
			if(x0<ranges_z[1,1]){x0<-ranges_z[1,1]}
			if(x1>ranges_z[nrow(ranges_z),2]){x1<-ranges_z[nrow(ranges_z),2]}
		}
	}
	if(axis=="y"){
		if((y0<ranges_z[1,1] & y1<ranges_z[1,1]) | (y0>ranges_z[nrow(ranges_z),2] & y1>ranges_z[nrow(ranges_z),2])){
			check.noplot<-1
		}else{
			if(y0<ranges_z[1,1]){y0<-ranges_z[1,1]}
			if(y1>ranges_z[nrow(ranges_z),2]){y1<-ranges_z[nrow(ranges_z),2]}
		}
	}

	if(check.noplot==0){
		if(axis=="x"){
			direction_z<-sign(x1-x0)
		}else if(axis=="y"){
			direction_z<-sign(y1-y0)
		}
	
		for(i in 1:nrow(ranges_z)){
			if(axis=="x"){
				if(x0>=ranges_z[i,1] & x0<=ranges_z[i,2]){range_id_z[1]<-i}
				if(x1>=ranges_z[i,1] & x1<=ranges_z[i,2]){range_id_z[2]<-i}
			}else if(axis=="y"){
				if(y0>=ranges_z[i,1] & y0<=ranges_z[i,2]){range_id_z[1]<-i}
				if(y1>=ranges_z[i,1] & y1<=ranges_z[i,2]){range_id_z[2]<-i}
			}
		}
	
		# case where both ends lie in a gap, possibly the same
		check<-1
		if(range_id_z[1]==0 & range_id_z[2]==0){
			for(i in 2:nrow(ranges_z)){
				if(axis=="x"){
					if(x0>ranges_z[i-1,2] & x0<ranges_z[i,1] & x1>ranges_z[i-1,2] & x1<ranges_z[i,1]){
						check<-0
						segments_to_plot<-NULL
					}
				}else if(axis=="y"){
					if(y0>ranges_z[i-1,2] & y0<ranges_z[i,1] & y1>ranges_z[i-1,2] & y1<ranges_z[i,1]){
						check<-0
						segments_to_plot<-NULL
					}
				}
			}
		}
	
		# case where (x0,y0) lies in a gap
		if(range_id_z[1]==0 & check==1){
			if(axis=="x"){
				a<-(y1-y0)/(x1-x0) # since we already made sure that x0 an x1 are not both in the same gap, x1!=x0
				b<-y0-a*x0
				if(direction_z>0){
					for(i in 2:nrow(ranges_z)){
						if(x0>ranges_z[i-1,2] & x0<ranges_z[i,1]){
							range_id_z[1]<-i
							x0<-ranges_z[i,1]
							y0<-a*x0+b
						}
					}
				}else{
					for(i in 2:nrow(ranges_z)){
						if(x0>ranges_z[i-1,2] & x0<ranges_z[i,1]){
							range_id_z[1]<-(i-1)
							x0<-ranges_z[i-1,2]
							y0<-a*x0+b
						}
					}
				}
			}else if(axis=="y"){
				a<-(x1-x0)/(y1-y0) # since we already made sure that y0 an y1 are not both in the same gap, y1!=y0
				b<-x0-a*y0
				if(direction_z>0){
					for(i in 2:nrow(ranges_z)){
						if(y0>ranges_z[i-1,2] & y0<ranges_z[i,1]){
							range_id_z[1]<-i
							y0<-ranges_z[i,1]
							x0<-a*y0+b
						}
					}
				}else{
					for(i in 2:nrow(ranges_z)){
						if(y0>ranges_z[i-1,2] & y0<ranges_z[i,1]){
							range_id_z[1]<-(i-1)
							y0<-ranges_z[i-1,2]
							x0<-a*y0+b
						}
					}
				}
			}	
		}
	
		# case where (x1,y1) lies in a gap
		if(range_id_z[2]==0 & check==1){
			if(axis=="x"){
				a<-(y1-y0)/(x1-x0) # since we already made sure that x0 an x1 are not both in the same gap, x1!=x0
				b<-y0-a*x0
				if(direction_z>0){
					for(i in 2:nrow(ranges_z)){
						if(x1>ranges_z[i-1,2] & x1<ranges_z[i,1]){
							range_id_z[2]<-(i-1)
							x1<-ranges_z[i-1,2]
							y1<-a*x1+b
						}
					}
				}else{
					for(i in 2:nrow(ranges_z)){
						if(x1>ranges_z[i-1,2] & x1<ranges_z[i,1]){
							range_id_z[2]<-i
							x1<-ranges_z[i,1]
							y1<-a*x1+b
						}
					}
				}
			}else if(axis=="y"){
				a<-(x1-x0)/(y1-y0) # since we already made sure that y0 an y1 are not both in the same gap, y1!=y0
				b<-x0-a*y0
				if(direction_z>0){
					for(i in 2:nrow(ranges_z)){
						if(y1>ranges_z[i-1,2] & y1<ranges_z[i,1]){
							range_id_z[2]<-(i-1)
							y1<-ranges_z[i-1,2]
							x1<-a*y1+b
						}
					}
				}else{
					for(i in 2:nrow(ranges_z)){
						if(y1>ranges_z[i,2] & y1<ranges_z[i,1]){
							range_id_z[2]<-i
							y1<-ranges_z[i,1]
							x1<-a*y1+b
						}
					}
				}
			}
		}
	
		# case where both ends lie outside gaps
		if(range_id_z[1]==range_id_z[2]){ # NB no else-if statement here so that we can also process the cases where one end of the segment was lying in a gap
			segments_to_plot[[1]]<-matrix(c(x0,y0,x1,y1),byrow=T,ncol=2)
		}else{
			if(axis=="x"){
				a<-(y1-y0)/(x1-x0) # since we already made sure that x0 an x1 are in different ranges, x1!=x0
				b<-y0-a*x0
				if(direction_z>0){
					x1_alt<-ranges_z[range_id_z[1],2]
				}else{
					x1_alt<-ranges_z[range_id_z[1],1]
				}
				y1_alt<-a*x1_alt+b
			}else if(axis=="y"){
				a<-(x1-x0)/(y1-y0) # since we already made sure that y0 an y1 are in different ranges, y1!=y0
				b<-x0-a*y0
				if(direction_z>0){
					y1_alt<-ranges_z[range_id_z[1],2]
				}else{
					y1_alt<-ranges_z[range_id_z[1],1]
				}
				x1_alt<-a*y1_alt+b
			}
			segments_to_plot[[1]]<-matrix(c(x0,y0,x1_alt,y1_alt),byrow=T,ncol=2)
		
			diff<-(range_id_z[2]-range_id_z[1])
			if(abs(diff)>1){
				#states<-seq(range_id_z[1],range_id_z[2],by=direction_z)
				states<-range_id_z[1]:range_id_z[2]
				states<-states[-c(1,length(states))]
				for(state in states){
					if(axis=="x"){
						if(direction_z>0){
							x0_alt<-ranges_z[state,1]
							x1_alt<-ranges_z[state,2]
						}else{
							x0_alt<-ranges_z[state,2]
							x1_alt<-ranges_z[state,1]
						}
						y0_alt<-a*x0_alt+b
						y1_alt<-a*x1_alt+b
					}else if(axis=="y"){
						if(direction_z>0){
							y0_alt<-ranges_z[state,1]
							y1_alt<-ranges_z[state,2]
						}else{
							y0_alt<-ranges_z[state,2]
							y1_alt<-ranges_z[state,1]
						}
						x0_alt<-a*y0_alt+b
						x1_alt<-a*y1_alt+b
					}
					segments_to_plot[[length(segments_to_plot)+1]]<-matrix(c(x0_alt,y0_alt,x1_alt,y1_alt),byrow=T,ncol=2)
				}
			}
		
			if(axis=="x"){
				if(direction_z>0){
					x0_alt<-ranges_z[range_id_z[2],1]
				}else{
					x0_alt<-ranges_z[range_id_z[2],2]
				}
				y0_alt<-a*x0_alt+b
			}else if(axis=="y"){
				if(direction_z>0){
					y0_alt<-ranges_z[range_id_z[2],1]
				}else{
					y0_alt<-ranges_z[range_id_z[2],2]
				}
				x0_alt<-a*y0_alt+b
			}
			segments_to_plot[[length(segments_to_plot)+1]]<-matrix(c(x0_alt,y0_alt,x1,y1),byrow=T,ncol=2)
		}
	}else{
		segments_to_plot<-NULL
	}
	return(segments_to_plot)
}

segments_with_breaks<-function(x0,y0,x1,y1,ranges_x,ranges_y,gap_width_x,gap_width_y,plot.true=TRUE,...){	
	## accommodating x-axis gaps
	segments_to_plot<-split_segments_one_axis_only(x0=x0,y0=y0,x1=x1,y1=y1,ranges_z=ranges_x,axis="x")
	if(!plot.true){res<-matrix(nrow=0,ncol=4)}
	
	## accommodating y-axis gaps and plotting the resulting segments
	if(!is.null(segments_to_plot)){
		for(i in 1:length(segments_to_plot)){
			segments_to_plot2<-split_segments_one_axis_only(x0=(segments_to_plot[[i]])[1,1],y0=(segments_to_plot[[i]])[1,2],x1=(segments_to_plot[[i]])[2,1],y1=(segments_to_plot[[i]])[2,2],ranges_z=ranges_y,axis="y")
		
			if(!is.null(segments_to_plot2)){
				for(j in 1:length(segments_to_plot2)){
					x<-(segments_to_plot2[[j]])[,1]
					x_new<-convert2gaps(z=x,ranges_z=ranges_x,gap_width_z=gap_width_x)
					y<-(segments_to_plot2[[j]])[,2]
					y_new<-convert2gaps(z=y,ranges_z=ranges_y,gap_width_z=gap_width_y)

					if(plot.true){
						segments(x0=x_new[1],y0=y_new[1],x1=x_new[2],y1=y_new[2],...)
					}else{
						res<-rbind(res,c(x,y))
					}
				}
			}
		}
	}
	
	if(!plot.true){return(res)}
	
}

segments_retain_breaks_xonly<-function(x0,y0,x1,y1,ranges_x,ranges_y,gap_width_x,gap_width_y,...){
	if(nrow(ranges_x)>1){
		#define x-axis gaps from ranges
		gaps<-matrix(nrow=0,ncol=2)
		if(nrow(ranges_x)>1){
			for(i in 1:(nrow(ranges_x)-1)){
				gaps<-rbind(gaps,c(ranges_x[i,2],ranges_x[i+1,1]))
			}
		}

		#identify segments to plot
		segments_to_plot<-list()
		count=1
		if(nrow(gaps)>0){
			for(i in 1:nrow(gaps)){
				#case where both end are in the gap: nothing to do here
				if(x0>=gaps[i,1] & x0<=gaps[i,2] & x1>gaps[i,2]){ #case where left end is in the gap
					a<-(y1-y0)/(x1-x0) # guaranteed that x0!=x1
					x1_alt<-gaps[i,2]
					y1_alt<-a*(gaps[i,2]-x0)+y0
					segments_to_plot[[count]]<-matrix(c(x0,y0,x1_alt,y1_alt),byrow=T,ncol=2)
					count=count+1
					x0<-x1_alt
					y0<-y1_alt
				}else if(x0<gaps[i,1] & x1>gaps[i,2]){ #case where the end points of the segment are either side of the gap
					a<-(y1-y0)/(x1-x0) # guaranteed that x0!=x1
					x0_alt<-gaps[i,1]
					y0_alt<-a*(gaps[i,1]-x0)+y0
					x1_alt<-gaps[i,2]
					y1_alt<-a*(gaps[i,2]-x0)+y0
					segments_to_plot[[count]]<-matrix(c(x0_alt,y0_alt,x1_alt,y1_alt),byrow=T,ncol=2)
					count=count+1
					x0<-x1_alt
					y0<-y1_alt
				}else if(x0<gaps[i,1] & x1>=gaps[i,1] & x1<=gaps[i,2]){#case where the right end if in the gap
					a<-(y1-y0)/(x1-x0) # guaranteed that x0!=x1
					x0_alt<-gaps[i,1]
					y0_alt<-a*(gaps[i,1]-x0)+y0
					segments_to_plot[[count]]<-matrix(c(x0_alt,y0_alt,x1,y1),byrow=T,ncol=2)
					count=count+1
				}else if(x0>=gaps[i,1] & x0<=gaps[i,2] & x1>=gaps[i,1] & x1<=gaps[i,2]){#case where both ends are in the gap
					segments_to_plot[[count]]<-matrix(c(x0,y0,x1,y1),byrow=T,ncol=2)
					count=count+1
				}
			}
			
			#convert positions of segments to plot and plot them
			if(length(segments_to_plot)>=1){
				for(j in 1:length(segments_to_plot)){
					#convert
					x<-(segments_to_plot[[j]])[,1]
					x_new<-convert2gaps_keepGaps(z=x,ranges_z=ranges_x,gap_width_z=gap_width_x)
					y_new<-(segments_to_plot[[j]])[,2]
					
					#plot
					segments(x0=x_new[1],y0=y_new[1],x1=x_new[2],y1=y_new[2],...)
				}
			}
		}	
	}
}

polygon_with_breaks<-function(x,y,ranges_x,ranges_y,gap_width_x,gap_width_y,col="black",is.rectangle=FALSE,...){
	# any polygon with at least one vertex in a gap, or which is spanning across a gap, is not plotted at all

	# remove double NAs and NAs at the start and end of the x and y vectors
	idx.na<-(1:length(x))[is.na(x) | is.na(y)]
	if(length(idx.na)>0){
		x<-c(NA,x,NA)
		y<-c(NA,y,NA)
		idx.na<-(1:length(x))[is.na(x) | is.na(y)]
		if(sum(diff(idx.na,1)==1)>0){
			x<-x[-idx.na[diff(idx.na,1)==1]]
			y<-y[-idx.na[diff(idx.na,1)==1]]
		}
		idx.na<-(1:length(x))[is.na(x) | is.na(y)]
		# x and y both start and end with NA if x or y had at least one NA within them
	}
		
	# check one polygon at a time whether it lies entirely within one region or not, then plot all polygons
	if(length(idx.na)==0){
		if(is.rectangle){
			x<-sort(x)
			y<-c(min(y),max(y),max(y),min(y))
		}
		check.x<-rep(0,length(x))
		check.y<-rep(0,length(y))
		for(i in 1:nrow(ranges_x)){check.x[x>=ranges_x[i,1] & x<=ranges_x[i,2]]<-i}
		for(i in 1:nrow(ranges_y)){check.y[y>=ranges_y[i,1] & y<=ranges_y[i,2]]<-i}
		if(is.rectangle){
			if(length(unique(check.x[check.x>0]))==1 & length(unique(x[check.x>0]))==1){
				if(sum(check.x[1:2]==0)==2){
					x[1:2]<-ranges_x[(ranges_x[,1]-x[1])>0 & (ranges_x[,1]-x[1])==min((ranges_x[,1]-x[1])[(ranges_x[,1]-x[1])>0]),1]
					check.x[1:2]<-which(ranges_x[,1]==x[1])
				}
				if(sum(check.x[3:4]==0)==2){
					x[3:4]<-ranges_x[(ranges_x[,2]-x[3])<0 & (ranges_x[,2]-x[3])==max((ranges_x[,2]-x[3])[(ranges_x[,2]-x[3])<0]),2]
					check.x[3:4]<-which(ranges_x[,2]==x[3])
				}
			}
			if(length(unique(check.y[check.y>0]))==1 & length(unique(y[check.y>0]))==1){
				if(sum(check.y[c(1,4)]==0)==2){
					y[c(1,4)]<-ranges_y[(ranges_y[,1]-y[1])>0 & (ranges_y[,1]-y[1])==min((ranges_y[,1]-y[1])[(ranges_y[,1]-y[1])>0]),1]
					check.y[c(1,4)]<-which(ranges_y[,1]==y[1])
				}
				if(sum(check.y[2:3]==0)==2){
					y[2:3]<-ranges_y[(ranges_y[,2]-y[2])<0 & (ranges_y[,2]-y[2])==max((ranges_y[,2]-y[2])[(ranges_y[,2]-y[2])<0]),2]
					check.y[2:3]<-which(ranges_y[,2]==y[2])
				}
			}
			if(sum(check.x==0)==4 & min(x)<=max(ranges_x[,2]) & max(x)>=min(ranges_x[,1])){
				check.gap.x<-rep(0,length(x))
				if(nrow(ranges_x)>1){for(k in 2:nrow(ranges_x)){check.gap.x[x>=ranges_x[k-1,2] & x<=ranges_x[k,1]]<-k-1}}
				if(length(unique(check.gap.x))>1 | sum(check.gap.x==0)==4){
					x[1:2]<-ranges_x[(ranges_x[,1]-x[1])>0 & (ranges_x[,1]-x[1])==min((ranges_x[,1]-x[1])[(ranges_x[,1]-x[1])>0]),1]
					x[3:4]<-ranges_x[(ranges_x[,2]-x[3])<0 & (ranges_x[,2]-x[3])==max((ranges_x[,2]-x[3])[(ranges_x[,2]-x[3])<0]),2]
					check.x[1:2]<-which(ranges_x[,1]==x[1])
					check.x[3:4]<-which(ranges_x[,2]==x[3])
				}
			}
			if(sum(check.y==0)==4 & min(y)<=max(ranges_y[,2]) & max(y)>=min(ranges_y[,1])){
				check.gap.y<-rep(0,length(y))
				if(nrow(ranges_y)>1){for(k in 2:nrow(ranges_y)){check.gap.y[y>=ranges_y[k-1,2] & y<=ranges_y[k,1]]<-k-1}}
				if(length(unique(check.gap.y))>1 | sum(check.gap.y==0)==4){
					y[c(1,4)]<-ranges_y[(ranges_y[,1]-y[1])>0 & (ranges_y[,1]-y[1])==min((ranges_y[,1]-y[1])[(ranges_y[,1]-y[1])>0]),1]
					y[2:3]<-ranges_y[(ranges_y[,2]-y[2])<0 & (ranges_y[,2]-y[2])==max((ranges_y[,2]-y[2])[(ranges_y[,2]-y[2])<0]),2]
					check.y[c(1,4)]<-which(ranges_y[,1]==y[1])
					check.y[2:3]<-which(ranges_y[,2]==y[2])
				}
			}
			if((check.x[3]-check.x[1])>0){
				for(i in check.x[1]:(check.x[3]-1)){
					x<-c(x[-c(length(x)-1,length(x))],ranges_x[i,2],ranges_x[i,2],NA,ranges_x[i+1,1],ranges_x[i+1,1],x[c(length(x)-1,length(x))])
					y<-c(y[-c(length(y)-1,length(y))],y[c(2,1)],NA,y[c(1,2)],y[c(length(y)-1,length(y))])
				}
			}
			if((check.y[2]-check.y[1])>0){
				for(i in check.y[1]:(check.y[2]-1)){
					y<-c(y[-c((length(y)-2):length(y))],ranges_y[i,2],ranges_y[i,2],y[length(y)],NA,ranges_y[i+1,1],y[c(length(y)-2,length(y)-1)],ranges_y[i+1,1])
					x<-c(x,NA,x[1:4])
				}
			}
			if(sum(check.x==0)==0 & sum(check.y==0)==0){
				x_new<-convert2gaps(z=x,ranges_z=ranges_x,gap_width_z=gap_width_x)
				y_new<-convert2gaps(z=y,ranges_z=ranges_y,gap_width_z=gap_width_y)
				polygon(x_new,y_new,col=col,...)
			}
		}else{
			if(sum(check.x==0)==0 & sum(check.y==0)==0){
				x_new<-convert2gaps(z=x,ranges_z=ranges_x,gap_width_z=gap_width_x)
				y_new<-convert2gaps(z=y,ranges_z=ranges_y,gap_width_z=gap_width_y)
				polygon(x_new,y_new,col=col,...)
			}
		}
	}else{
		for(i in 2:length(idx.na)){
			x.tmp<-x[(idx.na[i-1]+1):(idx.na[i]-1)]
			y.tmp<-y[(idx.na[i-1]+1):(idx.na[i]-1)]
			if(is.rectangle){
				x.tmp<-sort(x.tmp)
				y.tmp<-c(min(y.tmp),max(y.tmp),max(y.tmp),min(y.tmp))
			}
			check.x<-rep(0,length(x.tmp))
			check.y<-rep(0,length(y.tmp))
			for(k in 1:nrow(ranges_x)){check.x[x.tmp>=ranges_x[k,1] & x.tmp<=ranges_x[k,2]]<-k}
			for(k in 1:nrow(ranges_y)){check.y[y.tmp>=ranges_y[k,1] & y.tmp<=ranges_y[k,2]]<-k}
			
			if(is.rectangle){
				if(length(unique(check.x[check.x>0]))==1 & length(unique(x.tmp[check.x>0]))==1){
					if(sum(check.x[1:2]==0)==2){
						x.tmp[1:2]<-ranges_x[(ranges_x[,1]-x.tmp[1])>0 & (ranges_x[,1]-x.tmp[1])==min((ranges_x[,1]-x.tmp[1])[(ranges_x[,1]-x.tmp[1])>0]),1]
						check.x[1:2]<-which(ranges_x[,1]==x.tmp[1])
					}
					if(sum(check.x[3:4]==0)==2){
						x.tmp[3:4]<-ranges_x[(ranges_x[,2]-x.tmp[3])<0 & (ranges_x[,2]-x.tmp[3])==max((ranges_x[,2]-x.tmp[3])[(ranges_x[,2]-x.tmp[3])<0]),2]
						check.x[3:4]<-which(ranges_x[,2]==x.tmp[3])
					}
				}
				if(length(unique(check.y[check.y>0]))==1 & length(unique(y.tmp[check.y>0]))==1){
					if(sum(check.y[c(1,4)]==0)==2){
						y.tmp[c(1,4)]<-ranges_y[(ranges_y[,1]-y.tmp[1])>0 & (ranges_y[,1]-y.tmp[1])==min((ranges_y[,1]-y.tmp[1])[(ranges_y[,1]-y.tmp[1])>0]),1]
						check.y[c(1,4)]<-which(ranges_y[,1]==y.tmp[1])
					}
					if(sum(check.y[2:3]==0)==2){
						y.tmp[2:3]<-ranges_y[(ranges_y[,2]-y.tmp[2])<0 & (ranges_y[,2]-y.tmp[2])==max((ranges_y[,2]-y.tmp[2])[(ranges_y[,2]-y.tmp[2])<0]),2]
						check.y[2:3]<-which(ranges_y[,2]==y.tmp[2])
					}
				}
				if(sum(check.x==0)==4 & min(x.tmp)<=max(ranges_x[,2]) & max(x.tmp)>=min(ranges_x[,1])){
					check.gap.x<-rep(0,length(x.tmp))
					if(nrow(ranges_x)>1){for(k in 2:nrow(ranges_x)){check.gap.x[x.tmp>=ranges_x[k-1,2] & x.tmp<=ranges_x[k,1]]<-k-1}}
					if(length(unique(check.gap.x))>1 | sum(check.gap.x==0)==4){
						x.tmp[1:2]<-ranges_x[(ranges_x[,1]-x.tmp[1])>0 & (ranges_x[,1]-x.tmp[1])==min((ranges_x[,1]-x.tmp[1])[(ranges_x[,1]-x.tmp[1])>0]),1]
						x.tmp[3:4]<-ranges_x[(ranges_x[,2]-x.tmp[3])<0 & (ranges_x[,2]-x.tmp[3])==max((ranges_x[,2]-x.tmp[3])[(ranges_x[,2]-x.tmp[3])<0]),2]
						check.x[1:2]<-which(ranges_x[,1]==x.tmp[1])
						check.x[3:4]<-which(ranges_x[,2]==x.tmp[3])
					}
				}
				if(sum(check.y==0)==4 & min(y.tmp)<=max(ranges_y[,2]) & max(y.tmp)>=min(ranges_y[,1])){
					check.gap.y<-rep(0,length(y.tmp))
					if(nrow(ranges_y)>1){for(k in 2:nrow(ranges_y)){check.gap.y[y.tmp>=ranges_y[k-1,2] & y.tmp<=ranges_y[k,1]]<-k-1}}
					if(length(unique(check.gap.y))>1 | sum(check.gap.y==0)==4){
						y.tmp[c(1,4)]<-ranges_y[(ranges_y[,1]-y.tmp[1])>0 & (ranges_y[,1]-y.tmp[1])==min((ranges_y[,1]-y.tmp[1])[(ranges_y[,1]-y.tmp[1])>0]),1]
						y.tmp[2:3]<-ranges_y[(ranges_y[,2]-y.tmp[2])<0 & (ranges_y[,2]-y.tmp[2])==max((ranges_y[,2]-y.tmp[2])[(ranges_y[,2]-y.tmp[2])<0]),2]
						check.y[c(1,4)]<-which(ranges_y[,1]==y.tmp[1])
						check.y[2:3]<-which(ranges_y[,2]==y.tmp[2])
					}
				}
				if((check.x[3]-check.x[1])>0){
					for(i in check.x[1]:(check.x[3]-1)){
						x.tmp<-c(x.tmp[-c(length(x.tmp)-1,length(x.tmp))],ranges_x[i,2],ranges_x[i,2],NA,ranges_x[i+1,1],ranges_x[i+1,1],x.tmp[c(length(x.tmp)-1,length(x.tmp))])
						y.tmp<-c(y.tmp[-c(length(y.tmp)-1,length(y.tmp))],y.tmp[c(2,1)],NA,y.tmp[c(1,2)],y.tmp[c(length(y.tmp)-1,length(y.tmp))])
					}
				}
				if((check.y[2]-check.y[1])>0){
					for(i in check.y[1]:(check.y[2]-1)){
						y.tmp<-c(y.tmp[-c((length(y.tmp)-2):length(y.tmp))],ranges_y[i,2],ranges_y[i,2],y.tmp[length(y.tmp)],NA,ranges_y[i+1,1],y.tmp[c(length(y.tmp)-2,length(y.tmp)-1)],ranges_y[i+1,1])
						x.tmp<-c(x.tmp,NA,x.tmp[1:4])
					}
				}
				if(sum(check.x==0)==0 & sum(check.y==0)==0){
					x_new<-convert2gaps(z=x.tmp,ranges_z=ranges_x,gap_width_z=gap_width_x)
					y_new<-convert2gaps(z=y.tmp,ranges_z=ranges_y,gap_width_z=gap_width_y)
					polygon(x_new,y_new,col=col[i-1],...)
				}
			}else{
				if(length(unique(check.x))==1 & sum(check.x==0)==0 & length(unique(check.y))==1 & sum(check.y==0)==0){
					x_new<-convert2gaps(z=x.tmp,ranges_z=ranges_x,gap_width_z=gap_width_x)
					y_new<-convert2gaps(z=y.tmp,ranges_z=ranges_y,gap_width_z=gap_width_y)
					polygon(x_new,y_new,col=col[i-1],...)
				}
			}
		}
	}
}

# required sub-routines
convert2gaps<-function(z,ranges_z,gap_width_z){
	# converts input data z so that it can be plotted on a plot with gaps specified implicitly by ranges_z
	# input arguments:
	#	z = data to be converted (a vector or a matrix)
	#	ranges_z = a matrix with the range of values that will be plotted
	#	gap_width_z = the width of the gaps on the plot

	n_gapsz<-nrow(ranges_z)-1
	z_new<-z	

	if(is.vector(z)){
		z_int<-rep(-9,length(z))
	
		if(!is.null(n_gapsz) & n_gapsz>0){
			z_int[z>=ranges_z[1,1] & z<=ranges_z[1,2]]<-1
			for(i in 2:nrow(ranges_z)){
				z_int[z>=ranges_z[i,1] & z<=ranges_z[i,2]]<-i
				z_new[z_int==i]<-z_new[z_int==i]-sum((ranges_z[2:i,1]-ranges_z[(2:i)-1,2]))+(i-1)*gap_width_z
			}
			z_new[z_int==(-9)]<-NA
		}
	}else if(is.matrix(z)){
		for(j in 1:nrow(z)){
			z_int<-rep(-9,length(z[j,]))
	
			if(!is.null(n_gapsz) & n_gapsz>0){
				z_int[z[j,]>=ranges_z[1,1] & z[j,]<=ranges_z[1,2]]<-1
				for(i in 2:nrow(ranges_z)){
					z_int[z[j,]>=ranges_z[i,1] & z[j,]<=ranges_z[i,2]]<-i
					z_new[j,z_int==i]<-z_new[j,z_int==i]-sum((ranges_z[2:i,1]-ranges_z[(2:i)-1,2]))+(i-1)*gap_width_z
				}
				z_new[j,z_int==(-9)]<-NA
			}
			
		}
	}
	
	return(z_new)
}

convert2gaps_keepGaps<-function(z,ranges_z,gap_width_z){
	# converts input data z so that it can be plotted on a plot with gaps specified implicitly by ranges_z
	# this one will only keep the data that falls within the gaps
	# input arguments:
	#	z = data to be converted (a vector or a matrix)
	#	ranges_z = a matrix with the range of values that will be plotted
	#	gap_width_z = the width of the gaps on the plot

	n_gapsz<-nrow(ranges_z)-1
	z_new<-z	

	if(is.vector(z)){
		z_int<-rep(-9,length(z))
		
		if(!is.null(n_gapsz) & n_gapsz>0){
			z_int[z>=ranges_z[1,2] & z<=ranges_z[2,1]]<-1
			z_l<-ranges_z[1,2]
			z_u<-ranges_z[2,1]
			z_unew<-z_l+gap_width_z
			z_new[z_int==1]<-(z_new[z_int==1]-z_l)*(z_unew-z_l)/(z_u-z_l)+z_l
			if(nrow(ranges_z)>2){
				for(i in 2:(nrow(ranges_z)-1)){
					z_int[z>=ranges_z[i,2] & z<=ranges_z[i+1,1]]<-i
					z_l<-ranges_z[i,2]
					z_u<-ranges_z[i+1,1]
					z_unew<-z_l+gap_width_z
					z_new[z_int==1]<-(z_new[z_int==1]-z_l)*(z_unew-z_l)/(z_u-z_l)+z_l - sum((ranges_z[2:i,1]-ranges_z[(2:i)-1,2]))+(i-1)*gap_width_z
				}
			}
			z_new[z_int==(-9)]<-NA
		}

	}else if(is.matrix(z)){
		for(j in 1:nrow(z)){
			if(!is.null(n_gapsz) & n_gapsz>0){
				z_int[z[j,]>=ranges_z[1,2] & z[j,]<=ranges_z[2,1]]<-1
				z_l<-ranges_z[1,2]
				z_u<-ranges_z[2,1]
				z_unew<-z_l+gap_width_z
				z_new[j,z_int==1]<-(z_new[j,z_int==1]-z_l)*(z_unew-z_l)/(z_u-z_l)+z_l
				if(nrow(ranges_z)>2){
					for(i in 2:(nrow(ranges_z)-1)){
						z_int[z[j,]>=ranges_z[i,2] & z[j,]<=ranges_z[i+1,1]]<-i
						z_l<-ranges_z[i,2]
						z_u<-ranges_z[i+1,1]
						z_unew<-z_l+gap_width_z
						z_new[j,z_int==1]<-(z_new[j,z_int==1]-z_l)*(z_unew-z_l)/(z_u-z_l)+z_l - sum((ranges_z[2:i,1]-ranges_z[(2:i)-1,2]))+(i-1)*gap_width_z
					}
				}
				z_new[z_int==(-9)]<-NA
			
			}
		}
	}
	
	return(z_new)
}

convert2gaps_alt<-function(z,SP,EP,gaps,gap_width=0.05,side){
	# converts input data vector z so that it can be plotted on a plot with gaps specified by gaps
	# input arguments:
	#	z = data to be converted (a vector)
	#	SP / EP = start and end position of the interval of interest
	#	gaps = a list with gaps; gaps[[1]] list the gaps on the x-axis, gaps[[2]] those on the y-axis
	#	gap_width = width of each gap relative to the (plotted) range of the data
	#	side = which axis is z on? (1=x,2=y)
	
	# convert gaps argument into list if given as matrix
	if(is.null(gaps)){side<-NULL}
	if(is.null(side)){
		gaps<-NULL
	}else if(!is.element(el=side,set=c(1,2))){
		gaps<-NULL
	}
	if(is.list(gaps)){
		if(side==1){gaps<-gaps[[1]]}
		if(side==2){gaps<-gaps[[2]]}
	}
	
	# set zlim & n_gapsz
	zlim<-c(SP,EP)
	n_gapsz<-nrow(gaps)

	# define the ranges that will be plotted and assign x- and y-axis interval numbers for each data point to be plotted
	ranges_z<-matrix(nrow=0,ncol=2)
	z_int<-rep(-9,length(z))

	if(!is.null(n_gapsz)){
		ranges_z<-rbind(ranges_z,c(zlim[1],gaps[1,1]))
		z_int[z>=ranges_z[1,1] & z<=ranges_z[1,2]]<-1
		if(n_gapsz>1){
			for(i in 2:n_gapsz){
				ranges_z<-rbind(ranges_z,c(gaps[i-1,2],gaps[i,1]))
				z_int[z>=ranges_z[i,1] & z<=ranges_z[i,2]]<-i
			}
			ranges_z<-rbind(ranges_z,c(gaps[n_gapsz,2],zlim[2]))
			z_int[z>=ranges_z[n_gapsz+1,1] & z<=ranges_z[n_gapsz+1,2]]<-n_gapsz+1
		}else{
			ranges_z<-rbind(ranges_z,c(gaps[1,2],zlim[2]))
			z_int[z>=ranges_z[2,1] & z<=ranges_z[2,2]]<-2
		}
	}else{
		ranges_z<-rbind(ranges_z,zlim)
		z_int<-rep(1,length(z))
	}

	# compute the width of the gaps
	if(!is.null(n_gapsz)){gap_width_z<-sum(ranges_z[,2]-ranges_z[,1])*gap_width}else{gap_width_z<-0}

	# convert
	z_new<-convert2gaps(z,ranges_z=ranges_z,gap_width_z=gap_width_z)
	
	#export
	return(z_new)
}

add.lines<-function(filename,dat,SP,EP,gaps,gap_width,side,lines.lwd,lines.lty,lines.col,zoom.status,zoom.draw=TRUE,nonzoom.draw=TRUE){
	if(!is.na(filename) & ((!zoom.status & nonzoom.draw) | (zoom.status & zoom.draw))){
		par(xpd=TRUE)
		x_tmp<-convert2gaps_alt(z=dat[,2],SP=SP,EP=EP,gaps=gaps,gap_width=gap_width,side=side)
		for(j in 1:nrow(dat)){
			abline(v=x_tmp[j],lwd=lines.lwd[j],lty=lines.lty[j],col=lines.col[j])
		}
		par(xpd=FALSE)
	}
}
		
add.letter<-function(letter,width.plot,l.marg,r.marg,letter.cex){ 
	if(!is.na(letter)){
		par(xpd=TRUE)
		info.tmp<-par("usr")
		x_units<-info.tmp[2]-info.tmp[1]
		x_offset<-l.marg*(x_units/(width.plot-l.marg-r.marg))
		text(x=info.tmp[1]-x_offset,y=info.tmp[4],adj=c(0,1),bquote(bold(.(letter))),cex=letter.cex)
		par(xpd=FALSE)
	}  
}

enumEnd<-function(k){
	if(k==1){return("1st")}else if(k==2){return("2nd")}else if(k==3){return("3rd")}else{return(paste(sep="",k,"th"))}
}
