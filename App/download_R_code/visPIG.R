        ######################################        
    ##############################################    
######################################################
##                      visPIG                      ##
##      Visual Plotting Interface for Genetics      ##
######################################################
    ##############################################    
        ######################################        

     ######################
################################     
### COPYRIGHT & USER LICENSE ###
################################
     ######################

## Copyright (C) 2013 The Institute of Cancer Research (ICR).
## This Program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.
## Additional permissions under GNU GPL version 3 section 7:
## This Program is distributed as a service to the research community and is experimental in nature and may have hazardous properties.
## The Program is distributed WITHOUT ANY WARRANTY, express or implied.
## In particular all warranties as to SATISFACTORY QUALITY or FITNESS FOR A PARTICULAR PURPOSE are excluded.
## See the GNU General Public License for more details.
## You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses>.
## You shall not make any use of the name of The Institute of Cancer Research (ICR) in connection with this Program in any press or other public announcement without the prior written consent of the Institute of Cancer Research.

## SNAP track R code adapted from R code publicly available from:
##              Diabetes Genetics Initiative of Broad Institute of Harvard and MIT, Lund University and 
##                                  Novartis Institutes of BioMedical Research
##        Whole-genome association analysis identifies novel loci for type 2 diabetes and triglyceride levels
##                             Science 2007 Jun 1;316(5829):1331-6. Epub 2007 Apr 26 

## This code has been developed by the Institute of Cancer Research (ICR), UK.
## Authors: Marc Henrion & Matthew Scales
## Please send any correspondance to: marc [dot] henrion [at] icr.ac.uk

     ######################
################################     
#### MINIMAL  DOCUMENTATION ####
################################
     ######################
	#
	# Requires:
	#	R (any R version should be fine, but only versions > 2.10.1 have been tested; no additional packages are required)
	#	awk (if intensity, GERP or phastCons files are specified); users can specify to use gawk (or other awk versions) if desired
	#
	# Usage:
	#	R --vanilla < vispig.R --args para.file=XX outprefix=YY 
	#		where	-- vanilla can be replaced by other R running options (--silent, --slave etc)
	#				vispig.R can also be specified with a full path to the location of the downloaded R script
	#				XX = path to the downloaded or self-created parameters file
	#				YY = path and file prefix for output files
	#
	# For file formats, please check http://vispig.icr.ac.uk
	# NB The gene file should be in standard UCSC format and the SNAP files (data and rate) should be in standard format as required for the Broad Institute's SNAP plots.
	# NBB All files (except gene, SNAP and chrsizes files which do not require this) should be tab separated not simply whitespace separated.


     ######################
################################     
### COMMAND-LINE  PARAMETERS ###
################################
     ######################

# get command line parameters
	args<-commandArgs(TRUE)

# set the input file names & locations
	if(length(grep("pars.file=",args))>0){pars.file<-unlist(strsplit(args[grep("pars.file=",args)],split="pars.file="))[2]}else{stop("You need to specify a parameter file!")}
	if(length(grep("outprefix=",args))>0){outprefix<-unlist(strsplit(args[grep("outprefix=",args)],split="outprefix="))[2]}else{outprefix<-"./visPIG_out"}
	if(length(grep("plot.type=",args))>0){plot.type<-unlist(strsplit(args[grep("plot.type=",args)],split="plot.type="))[2]}else{plot.type<-"pdf"}
	if(length(grep("hic.files=",args))>0){hic.files<-unlist(strsplit(unlist(strsplit(args[grep("hic.files=",args)],split="hic.files="))[2],split=",")); n.hic<-length(hic.files)}else{hic.files<-NA; n.hic<-0}
	if(length(grep("annot.files=",args))>0){annot.files<-unlist(strsplit(unlist(strsplit(args[grep("annot.files=",args)],split="annot.files="))[2],split=",")); n.annot<-length(annot.files)}else{annot.files<-NA; n.annot<-0}
	if(length(grep("assoc.file=",args))>0){assoc.file<-unlist(strsplit(args[grep("assoc.file=",args)],split="assoc.file="))[2]}else{assoc.file<-NA}
	if(length(grep("dix.file=",args))>0){dix.file<-unlist(strsplit(args[grep("dix.file=",args)],split="dix.file="))[2]}else{dix.file<-NA}
	if(length(grep("heat.file=",args))>0){heat.file<-unlist(strsplit(args[grep("heat.file=",args)],split="heat.file="))[2]}else{heat.file<-NA}
	if(length(grep("gene.file=",args))>0){gene.file<-unlist(strsplit(args[grep("gene.file=",args)],split="gene.file="))[2]}else{gene.file<-NA}
	if(length(grep("gerp.files=",args))>0){gerp.files<-unlist(strsplit(unlist(strsplit(args[grep("gerp.files=",args)],split="gerp.files="))[2],split=","))}else{gerp.files<-NA}
	if(length(grep("phast.filter.file=",args))>0){phast.filter.file<-unlist(strsplit(args[grep("phast.filter.file=",args)],split="phast.filter.file="))[2]}else{phast.filter.file<-NA}
	if(length(grep("phast.files=",args))>0){phast.files<-unlist(strsplit(unlist(strsplit(args[grep("phast.files=",args)],split="phast.files="))[2],split=","))}else{phast.files<-NA}
	if(length(grep("gerp.filter.file=",args))>0){gerp.filter.file<-unlist(strsplit(args[grep("gerp.filter.file=",args)],split="gerp.filter.file="))[2]}else{gerp.filter.file<-NA}
	if(length(grep("bd.file=",args))>0){bd.file<-unlist(strsplit(args[grep("bd.file=",args)],split="bd.file="))[2]}else{bd.file<-NA}
	if(length(grep("int.files=",args))>0){int.files<-unlist(strsplit(unlist(strsplit(args[grep("int.files=",args)],split="int.files="))[2],split=",")); n.int<-length(int.files)}else{int.files<-NA; n.int<-0}
	if(length(grep("feat.files=",args))>0){feat.files<-unlist(strsplit(unlist(strsplit(args[grep("feat.files=",args)],split="feat.files="))[2],split=",")); n.feat<-length(feat.files)}else{feat.files<-NA; n.feat<-0}
	if(length(grep("snap.data.files=",args))>0){snap.data.files<-unlist(strsplit(unlist(strsplit(args[grep("snap.data.files=",args)],split="snap.data.files="))[2],split=",")); n.snap.data<-length(snap.data.files); n.snap<-n.snap.data}else{snap.data.files<-NA; n.snap<-0}
	if(length(grep("snap.rate.files=",args))>0){snap.rate.files<-unlist(strsplit(unlist(strsplit(args[grep("snap.rate.files=",args)],split="snap.rate.files="))[2],split=",")); n.snap.rate<-length(snap.rate.files); n.snap<-min(c(n.snap.data,n.snap.rate))}else{snap.rate.files<-NA; n.snap<-0}
	if(length(grep("snap.snp2.file=",args))>0){snap.snp2.file<-unlist(strsplit(args[grep("snap.snp2.file=",args)],split="snap.snp2.file="))[2]}else{snap.snp2.file<-NA}
	if(length(grep("lines.file=",args))>0){lines.file<-unlist(strsplit(args[grep("lines.file=",args)],split="lines.file="))[2]}else{lines.file<-NA}
	if(length(grep("regions.file=",args))>0){regions.file<-unlist(strsplit(args[grep("regions.file=",args)],split="regions.file="))[2]}else{stop("No regions.file specified; this is a required parameter.")}
	if(length(grep("chrsizes.file=",args))>0){chrsizes.file<-unlist(strsplit(args[grep("chrsizes.file=",args)],split="chrsizes.file="))[2]}else{chrsizes.file<-NA}
	if(length(grep("legend.annot.file=",args))>0){legend.annot.file<-unlist(strsplit(args[grep("legend.annot.file=",args)],split="legend.annot.file="))[2]}else{legend.annot.file<-NA}


     ######################
################################     
##### REQUIRED SUBROUTINES #####
################################
     ######################

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

feat_track_with_breaks<-function(dat,chr,SP,EP,feat.col="black",feat.height=0.4,feat.txt.vert.offset=0.05,feat.txt.hori.offset=0,compute_ntracks_only=FALSE,gaps,feat.lwd=1,feat.cex.txt=1.15,track.feat=NULL,feat.ylab="",feat.textpos="left",...){
	# plots a feature track with breaks on the x-axis
	# INPUT arguments:
	#	dat = feature data file; columns are chr, SP, EP, name
	#	chr = chromosome to plot
	#	SP = starting position of the region to plot
	#	EP = end position of the region to plot
	#	feat.col = colour used to plot the feature track
	#	compute_ntracks_only = logical; should only the number of required tracks be computed and no plot produced? (defaults to FALSE)
	#	gaps = a list specifying the gaps on the x-axis; see documentation of axis_breaks.R
	#	feat.lwd = width of line showing the extent of each feature
	#	feat.cex.txt = cex for printing the feature names
	#	track.feat = vector indicating on which track to plot which feat; NB this must be specified for the cleaned feature list (<= the features in dat); i.e. ideally you've got this info by running the function with compute_ntracks_only=TRUE first  
	#	feat.ylab = label for the y-axis of the feature plot
	#	feat.textpos = one of "left", "right", "top" indicating where to print the feature labels
	#	... = further parameters for plot_with_breaks()

	# filter data
	dat<-dat[dat[,1]==chr & ( (dat[,2]>=SP & dat[,2]<=EP) | (dat[,3]>=SP & dat[,3]<=EP) ),]

	if( nrow(dat) > 0 ){
		
		
		if(!is.null(gaps) & !is.null(gaps[[1]])){
			if(nrow(gaps[[1]])>=1){
					for(i in 1:nrow(gaps[[1]])){
						dat<-dat[!(dat[,2]>=gaps[[1]][i,1] & dat[,3]<=gaps[[1]][i,2]),]
					}
			}
		}
	
		# set up tracks on which to plot the different features 
		if(!is.null(gaps) & !is.null(gaps[[1]])){
			pos.tmp<-SP:((gaps[[1]])[1,1])
			if(nrow(gaps[[1]])>1){
				for(i in 2:nrow(gaps[[1]])){
					pos.tmp<-c(pos.tmp,(gaps[[1]])[i-1,2]:(gaps[[1]])[i,1])
				}
			}
			pos.tmp<-c(pos.tmp,((gaps[[1]])[nrow(gaps[[1]]),2]):EP)
		}else{
			pos.tmp<-SP:EP
		}
		
		minpos<-round(min(c(SP,dat[,2])-length(pos.tmp)/25))
		maxpos<-round(max(c(EP,dat[,3])+length(pos.tmp)/25))
		
		if(is.null(track.feat)){	
			tracks.list<-list()
			tracks.list[[1]]<-numeric(0)
			track.feat<-rep(NA,nrow(dat))
			
			for(i in 1:nrow(dat)){
				check.tmp<-0
				j<-1
				while(check.tmp==0){
					check.track<-0
					if(!is.null(nrow(tracks.list[[j]]))){
						check.vect.tmp<-rep(0,nrow(tracks.list[[j]]))
						check.vect.tmp[(dat[i,2]-length(pos.tmp)/10-minpos)>=tracks.list[[j]][,1] & (dat[i,2]-length(pos.tmp)/10-minpos)<=tracks.list[[j]][,2]]<-1
						check.vect.tmp[(dat[i,3]+length(pos.tmp)/10-minpos)>=tracks.list[[j]][,1] & (dat[i,3]+length(pos.tmp)/10-minpos)<=tracks.list[[j]][,2]]<-1
						check.vect.tmp[tracks.list[[j]][,1]>=(dat[i,2]-length(pos.tmp)/10-minpos) & tracks.list[[j]][,1]<=(dat[i,3]+length(pos.tmp)/10-minpos)]<-1
						check.vect.tmp[tracks.list[[j]][,2]>=(dat[i,2]-length(pos.tmp)/10-minpos) & tracks.list[[j]][,2]<=(dat[i,3]+length(pos.tmp)/10-minpos)]<-1
						if(sum(check.vect.tmp==1)>0){
							check.track<-1
						}
					}
					if(check.track==0){
						track.feat[i]<-j
						tracks.list[[j]]<-rbind(tracks.list[[j]],c(dat[i,2]-minpos,dat[i,3]-minpos))
						check.tmp<-1
					}else{
						j<-j+1
						if(length(tracks.list)<j){tracks.list[[j]]<-numeric(0)}
					}
				}
			}
			ntracks<-length(tracks.list)
			rm(tracks.list)
			track.feat<-max(track.feat)-(track.feat-1)
		}else{
			ntracks<-max(track.feat)
		}
		
		if(!compute_ntracks_only){
		# plot
			breaks.info<-plot_with_breaks(x=c(SP,EP),y=c(1,ntracks),type="n",plot.axes="n",xlim=c(SP,EP),ylim=c(0.5,ntracks+0.5),gaps=gaps,ylab=feat.ylab,...)
			for(i in 1:nrow(dat)){
				# plot the side bars and middle bar
				if(feat.textpos!="top"){
					lines_with_breaks(x=rep(dat[i,2],2),y=c(track.feat[i]-feat.height,track.feat[i]+feat.height),col=feat.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=feat.lwd)
					lines_with_breaks(x=rep(dat[i,3],2),y=c(track.feat[i]-feat.height,track.feat[i]+feat.height),col=feat.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=feat.lwd)
					lines_with_breaks(x=c(dat[i,2],dat[i,3]),y=rep(track.feat[i]+feat.height,2),col=feat.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=feat.lwd)
				}else{
					lines_with_breaks(x=rep(dat[i,2],2),y=c(track.feat[i]-feat.height,track.feat[i]),col=feat.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=feat.lwd)
					lines_with_breaks(x=rep(dat[i,3],2),y=c(track.feat[i]-feat.height,track.feat[i]),col=feat.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=feat.lwd)
					lines_with_breaks(x=c(dat[i,2],dat[i,3]),y=rep(track.feat[i],2),col=feat.col,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=feat.lwd)
				}
			# add text
				par(xpd=TRUE)                             
				if(feat.textpos=="left"){
					text_with_breaks(x=dat[i,2]-feat.cex.txt*length(pos.tmp)/200+feat.txt.hori.offset,y=track.feat[i]+feat.txt.vert.offset,label=dat[i,4],adj=c(1,0.5),cex=feat.cex.txt,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				}else if(feat.textpos=="right"){
					text_with_breaks(x=dat[i,3]+feat.cex.txt*length(pos.tmp)/200+feat.txt.hori.offset,y=track.feat[i]+feat.txt.vert.offset,label=dat[i,4],adj=c(0,0.5),cex=feat.cex.txt,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				}else if(feat.textpos=="top"){
					text_with_breaks(x=(dat[i,2]+dat[i,3])/2+feat.txt.hori.offset,y=track.feat[i]+feat.txt.vert.offset,label=dat[i,4],adj=c(0.5,0),cex=feat.cex.txt,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
				}
				par(xpd=FALSE)
			}
		}else{
			return(list(ntracks,track.feat))
		}
	}else{
		ntracks<-0
		track.feat<-0
print(compute_ntracks_only)
		if(compute_ntracks_only){
print("HERE I AM")
			return(list(ntracks,track.feat))	
		}else{
print("I SHOULD BE HERE")
			plot_with_breaks(x=0,y=0,type="n",plot.axes='n',xlab="",ylab=feat.ylab,main="",xlim=c(SP,EP),ylim=c(0,0),gaps=gaps,...)
		}
	}
}

gene_track_with_breaks<-function(dat,chr,SP,EP,col.genes="darkblue",utr.height=0.4,compute_ntracks_only=FALSE,convert.gw=FALSE,convert.chrstarts=NULL,gaps,lwd.line=1,cex.txt=1.15,cex.arrows=NULL,track.gene=NULL,gene.ylab="",axis.side.x=1,axis.side.y=2,gene.axis.tcl=-0.5,gene.axis.lwd=1,gene.axis.lwd.ticks=1,gene.axis.line=NA,gene.igs=0.1,...){
	# plots a gene track with breaks on the x-axis
	# INPUT arguments:
	#	dat = UCSC data file for genes; columns are chr, strand, txStart, tcEnd, cdsStart, cdsEnd, exonCount, exStarts, exonEnds, geneSymbol
	#	chr = chromosome to plot
	#	SP = starting position of the region to plot
	#	EP = end position of the region to plot
	#	col.genes = colour used to plot the genes
	#	compute_ntracks_only = logical; should only the number of required tracks be computed and no plot produced? (defaults to FALSE)
	#	convert.gw = logical; should the positions all be converted to genome-wide positions or not
	#	convert.chrstarts = (if convert.gw==TRUE) the ends of each chromosome in genome-wide position
	#	gaps = a list specifying the gaps on the x-axis; see documentation of axis_breaks.R
	#	lwd.line = width of line showing the extent of each gene
	#	cex.txt = cex for printing the gene names
	#	cex.arrows = cex for printing the arrows indicating the strands of the genes (defaults to NULL; in this case the code determines a 'good' value)
	#	track.gene = vector indicating on which track to plot which gene; NB this must be specified for the cleaned gene list (<= the genes in dat); i.e. ideally you've got this info by running the function with compute_ntracks_only=TRUE first  
	#	gene.ylab = label for the y-axis of the gene plot
	#	axis.side.x/y = side that the x/y axis should be plotted; x-axis is either 1 (bottom; default) or 3 (top), y-axis is either 2 (left; default) or 4 (right) 
	#	gene.axis.tcl = tcl to be used for axis tickmarks
	#	gene.axis.lwd = lwd to be used for plotting the axes
	#	gene.axis.lwd.ticks = lwd.ticks to be used for plotting the axes
	#	gene.axis.line = line as in axis()
	#	gene.igs = inter-gene space; i.e. what proportion of the extent of the x-axis should left free on each side of each gene (this is where the gene name will go, e.g.; so increase this if gene names overlap)
	#	... = further parameters for plot_with_breaks()

	# read in data
	if(!convert.gw){
		#dat<-dat[dat[,1]==paste("chr",chr,sep="") & ( (dat[,3]>=SP & dat[,3]<=EP) | (dat[,4]>=SP & dat[,4]<=EP) ),]
		dat<-dat[dat[,1]==chr & ( (dat[,3]>=SP & dat[,3]<=EP) | (dat[,4]>=SP & dat[,4]<=EP) ),]
	}else{
		#dat[,1]<-as.integer(matrix(unlist(strsplit(dat[,1],split="chr")),byrow=T,ncol=2)[,2])
		dat[,3]<-convert.chrstarts[dat[,1]]+dat[,3]
		dat[,4]<-convert.chrstarts[dat[,1]]+dat[,4]
		dat[,5]<-convert.chrstarts[dat[,1]]+dat[,5]
		dat[,6]<-convert.chrstarts[dat[,1]]+dat[,6]
		dat<-dat[(dat[,3]>=SP & dat[,3]<=EP) | (dat[,4]>=SP & dat[,4]<=EP),]
	}

	if(!is.null(gaps) & !is.null(gaps[[1]])){
		if(nrow(gaps[[1]])>=1){
				for(i in 1:nrow(gaps[[1]])){
					dat<-dat[!(dat[,3]>=gaps[[1]][i,1] & dat[,4]<=gaps[[1]][i,2]),]
				}
		}
	}

	# set up tracks on which to plot the different genes (and splicing variants of a given gene)
	if(!is.null(gaps) & !is.null(gaps[[1]])){
		pos.tmp<-SP:((gaps[[1]])[1,1])
		if(nrow(gaps[[1]])>1){
			for(i in 2:nrow(gaps[[1]])){
				pos.tmp<-c(pos.tmp,(gaps[[1]])[i-1,2]:(gaps[[1]])[i,1])
			}
		}
		pos.tmp<-c(pos.tmp,((gaps[[1]])[nrow(gaps[[1]]),2]):EP)
	}else{
		pos.tmp<-SP:EP
	}
	
	minpos<-round(min(c(SP,dat[,3])-length(pos.tmp)/25))
	maxpos<-round(max(c(EP,dat[,4])+length(pos.tmp)/25))

	if(is.null(track.gene)){	
		tracks.list<-list()
		tracks.list[[1]]<-numeric(0)
		track.gene<-rep(NA,nrow(dat))

		for(i in 1:nrow(dat)){
			check.tmp<-0
			j<-1
			while(check.tmp==0){
				check.track<-0
				if(!is.null(nrow(tracks.list[[j]]))){
					check.vect.tmp<-rep(0,nrow(tracks.list[[j]]))
					check.vect.tmp[(dat[i,3]-length(pos.tmp)*gene.igs-minpos)>=tracks.list[[j]][,1] & (dat[i,3]-length(pos.tmp)*gene.igs-minpos)<=tracks.list[[j]][,2]]<-1
					check.vect.tmp[(dat[i,4]+length(pos.tmp)*gene.igs-minpos)>=tracks.list[[j]][,1] & (dat[i,4]+length(pos.tmp)*gene.igs-minpos)<=tracks.list[[j]][,2]]<-1
					check.vect.tmp[tracks.list[[j]][,1]>=(dat[i,3]-length(pos.tmp)*gene.igs-minpos) & tracks.list[[j]][,1]<=(dat[i,4]+length(pos.tmp)*gene.igs-minpos)]<-1
					check.vect.tmp[tracks.list[[j]][,2]>=(dat[i,3]-length(pos.tmp)*gene.igs-minpos) & tracks.list[[j]][,2]<=(dat[i,4]+length(pos.tmp)*gene.igs-minpos)]<-1
					if(sum(check.vect.tmp==1)>0){
						check.track<-1
					}
				}
				if(check.track==0){
					track.gene[i]<-j
					tracks.list[[j]]<-rbind(tracks.list[[j]],c(dat[i,3]-minpos,dat[i,4]-minpos))
					check.tmp<-1
				}else{
					j<-j+1
					if(length(tracks.list)<j){tracks.list[[j]]<-numeric(0)}
				}
			}
		}
		ntracks<-length(tracks.list)
		rm(tracks.list)
		track.gene<-max(track.gene)-(track.gene-1)
	}else{
		ntracks<-max(track.gene)
	}

	if(!compute_ntracks_only){
	# plot
		breaks.info<-plot_with_breaks(x=c(SP,EP),y=c(1,ntracks),type="n",yaxt="n",xlim=c(SP,EP),ylim=c(0.5,ntracks+0.5),gaps=gaps,ylab=gene.ylab,axis.side.x=axis.side.x,axis.side.y=axis.side.y,tcl.axis=gene.axis.tcl,lwd.axis=gene.axis.lwd,lwd.ticks.axis=gene.axis.lwd.ticks,line.axis=gene.axis.line,...)
		if(nrow(dat)>0){
			for(i in 1:nrow(dat)){
				exon_start<-as.integer(unlist(strsplit(dat[i,8],split=",")))
				exon_end<-as.integer(unlist(strsplit(dat[i,9],split=",")))
				utr_start<-integer(0); utr_end<-integer(0)
				cexon_start<-integer(0); cexon_end<-integer(0)
				processed_exons<-rep(0,dat[i,7])
				if(convert.gw){
					exon_start<-convert.chrstarts[dat[i,1]]+exon_start
					exon_end<-convert.chrstarts[dat[i,1]]+exon_end
				}
				for(j in 1:length(exon_start)){
					if(exon_start[j]<dat[i,5] & exon_end[j]<dat[i,5]){ # exon starts and finishes to the left of the coding sequence (CS)
						utr_start<-c(utr_start,exon_start[j])
						utr_end<-c(utr_end,exon_end[j])
						processed_exons[j]<-1
					}else if(exon_start[j]<dat[i,5] & exon_end[j]>=dat[i,5] & exon_end[j]<=dat[i,6]){ # exon starts to the left of CS, finishes within it
						utr_start<-c(utr_start,exon_start[j])
						utr_end<-c(utr_end,dat[i,5])
						cexon_start<-c(cexon_start,dat[i,5])
						cexon_end<-c(cexon_end,exon_end[j])
						processed_exons[j]<-1
					}else if(exon_start[j]>=dat[i,5] & exon_end[j]<=dat[i,6]){ # exon starts within CS, finishes within it
						cexon_start<-c(cexon_start,exon_start[j])
						cexon_end<-c(cexon_end,exon_end[j])
						processed_exons[j]<-1
					}else if(exon_start[j]>=dat[i,5] & exon_start[j]<=dat[i,6] & exon_end[j]>dat[i,6]){ # exon starts within CS, finishes to the right of CS
						utr_start<-c(utr_start,dat[i,6])
						utr_end<-c(utr_end,exon_end[j])
						cexon_start<-c(cexon_start,exon_start[j])
						cexon_end<-c(cexon_end,dat[i,6])
						processed_exons[j]<-1
					}else if(exon_start[j]>dat[i,6] & exon_end[j]>dat[i,6]){ # exon starts & finishes to the right of CS
						utr_start<-c(utr_start,exon_start[j])
						utr_end<-c(utr_end,exon_end[j])
						processed_exons[j]<-1
					}else if(exon_start[j]<dat[i,5] & exon_end[j]>dat[i,6]){ # exon starts to the left of CS, finishes to the right of CS
						utr_start<-c(utr_start,exon_start[j],dat[i,6])
						utr_end<-c(utr_end,dat[i,5],exon_end[j])
						cexon_start<-c(cexon_start,dat[i,5])
						cexon_end<-c(cexon_end,dat[i,6])
						processed_exons[j]<-1
					}
				}
				if(sum(processed_exons==0)>0){warning("Not all exons processed!")}
			
				# extent of the gene & print gene name
				if(dat[i,3]>=SP & dat[i,4]<=EP){
					lines_with_breaks(x=c(dat[i,3],dat[i,4]),y=rep(track.gene[i],2),col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.line)
				}else if(dat[i,3]<SP & dat[i,4]<=EP){
					lines_with_breaks(x=c(SP,dat[i,4]),y=rep(track.gene[i],2),col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.line)
				}else if(dat[i,3]>=SP & dat[i,4]>EP){
					lines_with_breaks(x=c(dat[i,3],EP),y=rep(track.gene[i],2),col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.line)
				}else if(dat[i,3]<SP & dat[i,4]>EP){
					lines_with_breaks(x=c(SP,EP),y=rep(track.gene[i],2),col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.line)
				}
				if(!is.null(gaps) & !is.null(gaps[[1]])){
					if(nrow(gaps[[1]]>=1)){
						for(k in 1:nrow(gaps[[1]])){
							if(dat[i,3]>=gaps[[1]][k,1] & dat[i,3]<gaps[[1]][k,2] & dat[i,4]>gaps[[1]][k,2]){
								lines_with_breaks(x=c(gaps[[1]][k,2],dat[i,4]),y=rep(track.gene[i],2),col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.line)
							}else if(dat[i,3]<gaps[[1]][k,1] & dat[i,4]<=gaps[[1]][k,2] & dat[i,4]>gaps[[1]][k,1]){
								lines_with_breaks(x=c(dat[i,3],gaps[[1]][k,1]),y=rep(track.gene[i],2),col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,lwd=lwd.line)
							}
						}
					}
				}
				
				check.text<-0
				if(dat[i,3]<=SP+length(pos.tmp)/50){check.text<-1}
				for(k in 1:nrow(breaks.info$ranges_x)){
					#if(abs(dat[i,3]-breaks.info$ranges_x[k,1])<length(pos.tmp)/50){check.text<-1}
					if(k<nrow(breaks.info$ranges_x)){
						if(dat[i,3]>breaks.info$ranges_x[k,2] & dat[i,3]<breaks.info$ranges_x[k+1,1]+length(pos.tmp)/50){check.text<-1}
					}
				}
				par(xpd=TRUE)
				if(check.text==0){
					text_with_breaks(x=dat[i,3]-cex.txt*length(pos.tmp)/200,y=track.gene[i],label=paste(dat[i,10],sep=""),cex=cex.txt,col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,adj=1)
				}else{
					text_with_breaks(x=dat[i,4]+cex.txt*length(pos.tmp)/200,y=track.gene[i],label=paste(dat[i,10],sep=""),cex=cex.txt,col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,adj=0)
				}
				par(xpd=FALSE)
			
				# add arrows in direction of strand (+ to the right, - to the left) to mark introns
				if(is.null(cex.arrows)){cex.arrows<-min(c(1,0.6+0.02*ntracks))}
				space_between_intron_arrows<-round(length(pos.tmp)/1e4)*1e2
				if((dat[i,4]-dat[i,3])>0.2*space_between_intron_arrows){
					if(dat[i,2]=="+"){pch.arrow<-">"}else if(dat[i,2]=="-"){pch.arrow<-"<"}
					if((dat[i,4]-dat[i,3])>2*space_between_intron_arrows){
						if(dat[i,3]>=SP & dat[i,4]<=EP){
							arrow.pos.tmp<-(dat[i,3]+space_between_intron_arrows/2):(dat[i,4]-space_between_intron_arrows/2)
						}else if(dat[i,3]<SP & dat[i,4]<=EP){
							arrow.pos.tmp<-SP:(dat[i,4]-space_between_intron_arrows/2)
						}else if(dat[i,3]>=SP & dat[i,4]>EP){
							arrow.pos.tmp<-(dat[i,3]+space_between_intron_arrows/2):EP
						}else if(dat[i,3]<SP & dat[i,4]>EP){
							arrow.pos.tmp<-SP:EP
						}
						points_with_breaks(arrow.pos.tmp[(arrow.pos.tmp %% space_between_intron_arrows) == 0],rep(track.gene[i],sum((arrow.pos.tmp %% space_between_intron_arrows) == 0)),pch=pch.arrow,cex=cex.arrows,col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
					}else{
						points_with_breaks((dat[i,3]+dat[i,4])/2,track.gene[i],pch=pch.arrow,cex=cex.arrows,col=col.genes,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y)
					}
				}
			
				# exons (or part of exons) within coding sequence as high boxes
				if(length(cexon_start)>0){
					poly.x<-rbind(cexon_start,cexon_start,cexon_end,cexon_end,rep(NA,length(cexon_start)))
					poly.y<-rbind(track.gene[i]-utr.height,track.gene[i]+utr.height,track.gene[i]+utr.height,track.gene[i]-utr.height,rep(NA,length(cexon_start)))
					polygon_with_breaks(x=as.vector(poly.x),y=as.vector(poly.y),col=rep(col.genes,length(poly.x)),border=col.genes,lwd=0.5,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,is.rectangle=TRUE)
				}
			
				# exons (or part of exons) outside coding sequence (UTRs) as not so high boxes
				if(length(utr_start)>0){
					poly.x<-rbind(utr_start,utr_start,utr_end,utr_end,rep(NA,length(utr_start)))
					poly.y<-rbind(track.gene[i]-utr.height/2,track.gene[i]+utr.height/2,track.gene[i]+utr.height/2,track.gene[i]-utr.height/2,rep(NA,length(utr_start)))
					polygon_with_breaks(x=as.vector(poly.x),y=as.vector(poly.y),col=rep(col.genes,length(poly.x)),border=col.genes,lwd=0.5,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,is.rectangle=TRUE)
				}
			}
		}
	}else{
		return(list(ntracks,track.gene))
	}
}

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
	

     ######################
################################
####### EXTRA PARAMETERS #######
################################
     ######################

dynamicHelp <<- "Advice & troubleshooting:
"


     ######################
################################     
####### LOAD  PARAMETERS #######
################################
     ######################

# read in parameters file, treating everything as.character()
	pars<-read.table(pars.file,sep="\n",header=F,colClasses="character")

# process line-by-line
	for(i in 1:nrow(pars)){

# split on "=" and the parameters value on ","
		var_name<-unlist(strsplit(pars[i,],split="="))[1]
		var_value<-unlist(strsplit(unlist(strsplit(pars[i,],split="="))[2],split=","))
		if(!is.na(var_value[1]) & !is.null(var_value[1])){
			var_value[var_value=="" | var_value=="NA"]<-NA
		}
		if(!is.na(var_value[1]) & !is.null(var_value[1])){
			var_value[var_value=="NULL"]<-NULL
		}
	
# assign value to variable name using do.call("<-",list(var_name,var_value)) or assign(var_name,var_value)
		do.call("<-",list(var_name,var_value))

# close loop on line
	}

# assign correct object type
	zoom<-as.logical(zoom)
	zoom.chr<-as.integer(zoom.chr)
	zoom.SP<-as.numeric(zoom.SP)
	zoom.EP<-as.numeric(zoom.EP)
	zoom.vertical.spacing<-as.numeric(zoom.vertical.spacing)
	zoom.lines.alpha<-as.numeric(zoom.lines.alpha)
	zoom.lines.lwd<-as.numeric(zoom.lines.lwd)
	zoom.lines.lty<-as.numeric(zoom.lines.lty)
	zoom.labels.show.unit<-as.logical(zoom.labels.show.unit)
	zoom.axis.tcl<-as.numeric(zoom.axis.tcl)
	zoom.axis.lend<-as.integer(zoom.axis.lend)
	zoom.x.lwd<-as.numeric(zoom.x.lwd)
	zoom.x.labels.up<-as.logical(zoom.x.labels.up)
	width.plot<-as.numeric(width.plot)
	height.plot<-as.numeric(height.plot)
	left.margin<-as.numeric(left.margin)
	right.margin<-as.numeric(right.margin)
	title.top.margin<-as.numeric(title.top.margin)
	top.margin<-as.numeric(top.margin)
	bottom.margin<-as.numeric(bottom.margin)
	sep.margin<-as.numeric(sep.margin)
	ylab.las<-as.integer(ylab.las)
	letter.cex<-as.numeric(letter.cex)
	xlab.cex<-as.numeric(xlab.cex)
	axis.relplotheight<-as.numeric(axis.relplotheight)
	axis.line<-as.numeric(axis.line)
	xlab.line<-as.numeric(xlab.line)
	axis.range.line<-as.numeric(axis.range.line)
	axis.tickmarks.line<-as.numeric(axis.tickmarks.line)
	axis.addlines<-as.logical(axis.addlines)
	x.lwd<-as.numeric(x.lwd)
	x.labels.show.unit<-as.logical(x.labels.show.unit)
	axis.tcl<-as.numeric(axis.tcl)
	axis.lend<-as.integer(axis.lend)
	axis.labels.slanted<-as.logical(axis.labels.slanted)
	axis.labels.angle<-as.numeric(axis.labels.angle)
	gap_width<-as.numeric(gap_width)
	gaps.sizes.offset.x<-as.numeric(gaps.sizes.offset.x)
	title.relplotheight<-as.numeric(title.relplotheight)
	title.cex.txt<-as.numeric(title.cex.txt)
	title.pos<-as.numeric(title.pos)
	title.chr<-as.numeric(title.chr)
	title.showgaps<-as.logical(title.showgaps)
	buffer.relplotheight<-as.numeric(buffer.relplotheight)
	max_chr<-as.integer(max_chr)
	legend.width<-as.numeric(legend.width)
	legend.height<-as.numeric(legend.height)
	legend.rows<-as.integer(legend.rows)
	legend.cols<-as.integer(legend.cols)
	legend.assoc<-as.logical(legend.assoc)
	legend.hic<-as.logical(legend.hic)
	legend.heat<-as.logical(legend.heat)
	legend.annot<-as.logical(legend.annot)
	legend.dix<-as.logical(legend.dix)
	legend.mai<-as.numeric(legend.mai)
	legend.cex.lab<-as.numeric(legend.cex.lab)
	legend.cex.txt<-as.numeric(legend.cex.txt)
	legend.adj<-as.numeric(legend.adj)
	legend.annot.nrow<-as.integer(legend.annot.nrow)
	legend.annot.rgb<-as.logical(legend.annot.rgb)
	legend.annot.axline<-as.numeric(legend.annot.axline)
	legend.hic.capt<-as.logical(legend.hic.capt)
	legend.hic.xlim<-as.numeric(legend.hic.xlim)
	legend.hic.hori<-as.logical(legend.hic.hori)
	legend.hic.axline<-as.numeric(legend.hic.axline)
	legend.heat.hori<-as.logical(legend.heat.hori)
	legend.heat.axline<-as.numeric(legend.heat.axline)
	legend.assoc.pvalticksround<-as.integer(legend.assoc.pvalticksround)
	legend.assoc.hori<-as.logical(legend.assoc.hori)
	legend.assoc.axline<-as.numeric(legend.assoc.axline)
	legend.dix.hori<-as.logical(legend.dix.hori)
	legend.dix.axline<-as.numeric(legend.dix.axline)
	gene.relplotheight<-as.numeric(gene.relplotheight)
	gene.ylab.cex.txt<-as.numeric(gene.ylab.cex.txt)
	gene.igs<-as.numeric(gene.igs)
	gene.cex.txt<-as.numeric(gene.cex.txt)
	gene.cex.arrows<-as.numeric(gene.cex.arrows)
	gene.lwd<-as.numeric(gene.lwd)
	gerp.relplotheight<-as.numeric(gerp.relplotheight)
	gerp.cex.txt<-as.numeric(gerp.cex.txt)
	gerp.lwd<-as.numeric(gerp.lwd)
	gerp.ylim<-as.numeric(gerp.ylim)
	gerp.noblocks<-as.logical(gerp.noblocks)
	gerp.mai.top<-as.numeric(gerp.mai.top)
	gerp.mai.bot<-as.numeric(gerp.mai.bot)
	phast.relplotheight<-as.numeric(phast.relplotheight)
	phast.cex.txt<-as.numeric(phast.cex.txt)
	phast.lwd<-as.numeric(phast.lwd)
	phast.ylim<-as.numeric(phast.ylim)
	phast.noblocks<-as.logical(phast.noblocks)
	annot.relplotheight<-as.numeric(annot.relplotheight)
	annot.cex.txt<-as.numeric(annot.cex.txt)
	annot.lwd<-as.numeric(annot.lwd)
	annot.mai.top<-as.numeric(annot.mai.top)
	annot.mai.bot<-as.numeric(annot.mai.bot)
	hic.relplotheight<-as.numeric(hic.relplotheight)
	hic.cex.txt<-as.numeric(hic.cex.txt)
	hic.plotThroughGaps<-as.logical(hic.plotThroughGaps)
	hic.stack.ylim.bot<-as.numeric(hic.stack.ylim.bot)
	hic.stack.ylim.top<-as.numeric(hic.stack.ylim.top)
	hic.stack.lwd<-as.numeric(hic.stack.lwd)
	hic.arches.twist<-as.logical(hic.arches.twist)
	hic.arches.lwd<-as.numeric(hic.arches.lwd)
	hic.arches.nsegments<-as.integer(hic.arches.nsegments)
	hic.arches.neglog10<-as.logical(hic.arches.neglog10)
	hic.arches.ylim.lower<-as.numeric(hic.arches.ylim.lower)
	hic.arches.ylim.upper<-as.numeric(hic.arches.ylim.upper)
	hic.arches.yaxis<-as.logical(hic.arches.yaxis)
	hic.arches.varicol<-as.logical(hic.arches.varicol)
	hic.plotThroughGapsMaxY<-as.logical(hic.plotThroughGapsMaxY)
	hic.capt.density<-as.numeric(hic.capt.density)
	hic.capt.lines.lwd<-as.numeric(hic.capt.lines.lwd)
	int.relplotheight<-as.numeric(int.relplotheight)
	int.cex.txt<-as.numeric(int.cex.txt)
	int.lwd<-as.numeric(int.lwd)
	int.ylim.lower<-as.numeric(int.ylim.lower)
	int.ylim.upper<-as.numeric(int.ylim.upper)
	int.smooth.window<-as.integer(int.smooth.window)
	int.refbox.density<-as.numeric(int.refbox.density)
	int.refbox.lines.lwd<-as.numeric(int.refbox.lines.lwd)
	int.ref2box.density<-as.numeric(int.ref2box.density)
	int.ref2box.lines.lwd<-as.numeric(int.ref2box.lines.lwd)
	assoc.relplotheight<-as.numeric(assoc.relplotheight)
	assoc.cex.txt<-as.numeric(assoc.cex.txt)
	assoc.pvalscale<-as.numeric(assoc.pvalscale)
	assoc.lwd.bars<-as.numeric(assoc.lwd.bars)
	dix.relplotheight<-as.numeric(dix.relplotheight)
	dix.cex.txt<-as.numeric(dix.cex.txt)
	dix.col.rgb<-as.logical(dix.col.rgb)
	dix.cols.toplot<-as.integer(dix.cols.toplot)
	dix.ylim.lower<-as.numeric(dix.ylim.lower)
	dix.ylim.upper<-as.numeric(dix.ylim.upper)
	feat.relplotheight<-as.numeric(feat.relplotheight)
	feat.cex.txt<-as.numeric(feat.cex.txt)
	feat.cex.lab<-as.numeric(feat.cex.lab)
	feat.lwd<-as.numeric(feat.lwd)
	feat.height<-as.numeric(feat.height)
	feat.txt.vert.offset<-as.numeric(feat.txt.vert.offset)
	feat.txt.hori.offset<-as.numeric(feat.txt.hori.offset)
	heat.relplotheight<-as.numeric(heat.relplotheight)
	heat.cex.txt<-as.numeric(heat.cex.txt)
	heat.chr<-as.integer(heat.chr)
	heat.SP<-as.integer(heat.SP)
	heat.EP<-as.integer(heat.EP)
	heat.ylim<-as.numeric(heat.ylim)
	heat.zlim<-as.numeric(heat.zlim)
	heat.logscale<-as.logical(heat.logscale)
	heat.bw<-as.numeric(heat.bw)
	heat.chop.data.gaps<-as.logical(heat.chop.data.gaps)
	heat.general.yaxis<-as.logical(heat.general.yaxis)
	snap.relplotheight<-as.numeric(snap.relplotheight)
	snap.cex.txt<-as.numeric(snap.cex.txt)
	snap.cex.axis<-as.numeric(snap.cex.axis)
	snap.snp.label.cex<-as.numeric(snap.snp.label.cex)
	snap.snp.offset.x<-as.numeric(snap.snp.offset.x)
	snap.snp.offset.y<-as.numeric(snap.snp.offset.y)
	snap.r2.legend.x.offset<-as.numeric(snap.r2.legend.x.offset)
	snap.ylim<-as.numeric(snap.ylim)
	snap.snp2.offset.x<-as.numeric(snap.snp2.offset.x)
	snap.snp2.offset.y<-as.numeric(snap.snp2.offset.y)
	lines.lwd<-as.numeric(lines.lwd)
	lines.lty<-as.integer(lines.lty)
	lines.nonzoom<-as.logical(lines.nonzoom)
	lines.zoom<-as.logical(lines.zoom)


# set specific parameters to NULL, rather than NA if they have not been specified explicitly
	if(!is.null(annot.bgclass)){if(sum(is.na(annot.bgclass))>0){annot.bgclass<-NULL}}
	if(!is.null(assoc.pvalscale)){if(sum(is.na(assoc.pvalscale))>0){assoc.pvalscale<-NULL}}
	if(!is.null(heat.ylim)){if(sum(is.na(heat.ylim))>0){heat.ylim<-NULL}}
	if(!is.null(heat.zlim)){if(sum(is.na(heat.zlim))>0){heat.zlim<-NULL}}
	if(!is.null(gene.cex.arrows)){if(sum(is.na(gene.cex.arrows))>0){gene.cex.arrows<-NULL}}
	if(!is.null(gerp.ylim)){if(sum(is.na(gerp.ylim))>0){gerp.ylim<-NULL}}
	if(!is.null(phast.ylim)){if(sum(is.na(phast.ylim))>0){phast.ylim<-NULL}}


# set the output file for the plot
	file<-paste(outprefix,".",plot.type,sep="")


	outfile<-paste(outprefix,".",plot.type,sep="")


     ######################
################################
####### MAIN SCRIPT BODY #######
################################
     ######################

## STARTING THE LOG FILE ##
###########################

cat("This is visPIG.\nR code developed by the Institute of Cancer Research (ICR), UK.\nR code for creating SNAP tracks is adapted from code from the Broad Institute (see in-code comments for more detailed credits).\n\n",file=paste(outprefix,"_R.log",sep=""))


######################################
## A LITTLE TRICK FOR y-AXIS LABELS ##
######################################

annot.ylab<-sub(annot.ylab,pattern="\\n",replacement="\n",fixed=T)
assoc.ylab<-sub(assoc.ylab,pattern="\\n",replacement="\n",fixed=T)
dix.ylab<-sub(dix.ylab,pattern="\\n",replacement="\n",fixed=T)
feat.ylab<-sub(feat.ylab,pattern="\\n",replacement="\n",fixed=T)
snap.ylab<-sub(snap.ylab,pattern="\\n",replacement="\n",fixed=T)
gene.ylab<-sub(gene.ylab,pattern="\\n",replacement="\n",fixed=T)
gerp.ylab<-sub(gerp.ylab,pattern="\\n",replacement="\n",fixed=T)
heat.ylab<-sub(heat.ylab,pattern="\\n",replacement="\n",fixed=T)
hic.ylab<-sub(hic.ylab,pattern="\\n",replacement="\n",fixed=T)
int.ylab<-sub(int.ylab,pattern="\\n",replacement="\n",fixed=T)
phast.ylab<-sub(phast.ylab,pattern="\\n",replacement="\n",fixed=T)


#########################
## READ THE INPUT DATA ##
#########################

cat("Reading the data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)

chr.set<-paste("chr",1:max_chr,sep="")

# regions file
	cat("...regions file...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
	if(is.na(regions.file)){
		stop("To get started, please upload a region file specifying the genetic positions you would like to plot.")
	}
	regions.dat<-try(read.table(regions.file,sep="",header=F,colClasses=c("character",rep("numeric",2))),silent=TRUE)
	if(length(grep(regions.dat[1],pattern="Error"))>0){stop("Cannot read the regions file. Please check the input file format is correct.")}
	regions.dat[,1]<-as.integer(sub(regions.dat[,1],pattern="chr",replacement=""))
	regions.dat<-regions.dat[order(regions.dat[,1],regions.dat[,2],regions.dat[,3]),]

# gene file
	if(!is.na(gene.file)){
		cat("...gene file...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		gene.dat<-try(read.table(gene.file,sep="",header=T,colClasses=c(rep("character",2),rep("integer",5),rep("character",3))),silent=TRUE)
		if(length(grep(gene.dat[1],pattern="Error"))>0){stop("Cannot read the gene file. Please check the input file format is correct.")}
		gene.dat<-gene.dat[is.element(el=gene.dat[,1],set=chr.set),]
		gene.dat[,1]<-as.integer(sub(gene.dat[,1],pattern="chr",replacement=""))
	}
	
# chromosome sizes file
	if(!is.na(chrsizes.file)){
		cat("...chromosome sizes file...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		chrsizes.dat<-try(as.numeric(read.table(chrsizes.file,sep="",header=F,colClasses="numeric")[1,]),silent=TRUE)
		if(length(grep(chrsizes.dat[1],pattern="Error"))>0){stop("Cannot read the chromosome sizes file. Please check the input file format is correct.")}
		max_chr<-max(chrsizes.dat)
	}else{
		chrsizes.dat<-(-9)
	}

# snap data
	if((!is.na(snap.data.files[1]) & is.na(snap.rate.files[1])) | (is.na(snap.data.files[1]) & !is.na(snap.rate.files[1]))){stop("You provided only one of the data and rate files required for the SNAP track. Please provide both or none.")}

	if(!is.na(snap.data.files[1]) || !is.na(snap.rate.files[1])){
		cat("...snap data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		if(is.na(snap.ylab)){snap.ylab<-expression(paste(-log[10],"P"))}
		snap.rate.dat<-list()
		snap.dat<-list() 
		for(i in 1:n.snap){
			snap.rate.dat[[i]] <- try(read.table(snap.rate.files[i], header=T,colClasses=c("numeric","numeric","character"), na.strings=c("N/A"),fill=TRUE),silent=TRUE)
			if(length(grep(snap.rate.dat[[i]][1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," SNAP rate file. Please check the input file format is correct."))}
			snap.dat[[i]] <- try(read.table(snap.data.files[i], header=T, row.names=1, na.strings=c("N/A")),silent=TRUE)
			if(length(grep(snap.dat[[i]][1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," SNAP data file. Please check the input file format is correct."))}
		}
		if(!is.na(snap.snp2.file)){
			snap.snp2.dat <- try(read.table(snap.snp2.file, header=TRUE, row.names=1,na.strings=c("N/A")),silent=TRUE)
			if(length(grep(snap.snp2.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the SNAP data file for the second SNP. Please check the input file format is correct."))}
		}  
	
		if(is.na(snap.snp)) { dynamicHelp <<- c(dynamicHelp, paste(length(dynamicHelp),')...\tSNAP plot: You will need to specify the reference SNP for the SNAP plot, \n\tit can be specified by editing the relevant parameter in the SNAP track parameters tab.',sep='')) }
		if(is.na(snap.chr) && sum(is.na(snap.rate.dat[[1]][,3]))==nrow(snap.rate.dat[[1]])) { dynamicHelp <<- c(dynamicHelp, paste(length(dynamicHelp),')...\tSNAP plot: You need to either specify the chromosome for the snap plot, \n\t(this can be specified by editing the relevant parameter in the SNAP track parameters tab)\n\tor you need to add a chromosome column to the SNAP rate file.',sep='')) }
	}

# interaction data
	if(n.hic>0){
		hic.dat<-list()
		if(length(hic.style)!=n.hic){hic.style<-rep(hic.style[1],n.hic)}
		for(i in 1:n.hic){
			cat(paste("...",enumEnd(i)," interaction data...\n"),file=paste(outprefix,"_R.log",sep=""),append=T)
			if(hic.style[i]!="arches" & hic.style[i]!="flatarches"){
				test<-try(read.table(hic.files[i],header=F,sep="",nrows=5),silent=TRUE)
				if(length(grep(test[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," interaction data file. Please check the input file format is correct."))}
				if(ncol(test)!=5){stop("Interaction data is in wrong format. NB the interaction data format is different depending on whether you wish to plot arches or not.")}else{rm(test)}
				hic.dat[[i]]<-try(read.table(hic.files[i],header=F,sep="",colClasses=c("character",rep("integer",4))),silent=TRUE)
				if(length(grep(hic.dat[[i]][1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," interaction data file. Please check the input file format is correct. NB: different format for interaction data depending on whether you wish to plot arches or not."))}
				hic.dat[[i]][,1]<-as.integer(sub(x=hic.dat[[i]][,1],pattern="chr",replacement=""))
			}else{
				test<-try(read.table(hic.files[i],header=F,sep="",nrows=5),silent=TRUE)
				if(length(grep(test[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," interaction data file. Please check the input file format is correct."))}
				if(ncol(test)!=7){stop("Interaction data is in wrong format. NB the interaction data format is different depending on whether you wish to plot arches or not.")}else{rm(test)}
				hic.dat[[i]]<-try(read.table(hic.files[i],header=F,sep="",colClasses=c("character",rep("numeric",2),"character",rep("integer",2),"numeric")),silent=TRUE)
				if(length(grep(hic.dat[[i]][1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," interaction data file. Please check the input file format is correct. NB: different format for interaction data depending on whether you wish to plot arches or not."))}
				hic.dat[[i]][,1]<-as.integer(sub(x=hic.dat[[i]][,1],pattern="chr",replacement=""))
				hic.dat[[i]][,4]<-as.integer(sub(x=hic.dat[[i]][,4],pattern="chr",replacement=""))
			}
		}
		if(length(hic.relplotheight)!=n.hic){hic.relplotheight<-rep(hic.relplotheight[1],n.hic)}
		if(length(hic.colref)!=n.hic){hic.colref<-rep(hic.colref[1],n.hic)}
		if(length(hic.colnonref)!=n.hic){hic.colnonref<-rep(hic.colnonref[1],n.hic)}
		if(length(hic.ylab)!=n.hic){hic.ylab<-rep(hic.ylab[1],n.hic)}
		if(length(hic.cex.txt)!=n.hic){hic.cex.txt<-rep(hic.cex.txt[1],n.hic)}
		if(length(hic.stack.style)!=n.hic){hic.stack.style<-rep(hic.stack.style[1],n.hic)}
		if(length(hic.stack.ylim.bot)!=n.hic){hic.stack.ylim.bot<-rep(hic.stack.ylim.bot[1],n.hic)}	
		if(length(hic.stack.ylim.top)!=n.hic){hic.stack.ylim.top<-rep(hic.stack.ylim.top[1],n.hic)}
		if(length(hic.capt.region)!=n.hic){hic.capt.region<-rep(hic.capt.region[1],n.hic)}
		if(length(hic.capt.col)!=n.hic){hic.capt.col<-rep(hic.capt.col[1],n.hic)}
		if(length(hic.capt.density)!=n.hic){hic.capt.density<-rep(hic.capt.density[1],n.hic)}
		if(length(hic.arches.twist)!=n.hic){hic.arches.twist<-rep(hic.arches.twist[1],n.hic)}
		if(length(hic.arches.neglog10)!=n.hic){hic.arches.neglog10<-rep(hic.arches.neglog10[1],n.hic)}
		if(length(hic.arches.lwd)!=n.hic){hic.arches.lwd<-rep(hic.arches.lwd[1],n.hic)}
		if(length(hic.arches.nsegments)!=n.hic){hic.arches.nsegments<-rep(hic.arches.nsegments[1],n.hic)}
		if(length(hic.arches.ylim.upper)!=n.hic){hic.arches.ylim.upper<-rep(hic.arches.ylim.upper[1],n.hic)}
		if(length(hic.arches.ylim.lower)!=n.hic){hic.arches.ylim.lower<-rep(hic.arches.ylim.lower[1],n.hic)}
		if(length(hic.arches.yaxis)!=n.hic){hic.arches.yaxis<-rep(hic.arches.yaxis[1],n.hic)}
		if(length(hic.arches.col)!=n.hic){hic.arches.col<-rep(hic.arches.col[1],n.hic)}
		if(length(hic.arches.varicol)!=n.hic){hic.arches.varicol<-rep(hic.arches.varicol[1],n.hic)}
		if(length(hic.arches.dir)!=n.hic){hic.arches.dir<-rep(hic.arches.dir[1],n.hic)}
		
		if(!is.na(hic.capt.region[1])){
			hic.capt.region<-matrix(byrow=T,nrow=n.hic,as.numeric(unlist(strsplit(hic.capt.region,split=":"))))
		}else{
			hic.capt.region<-matrix(rep(NA,3*n.hic),ncol=3)
		}		
	}
	
# multi-state annotation data
	if(n.annot>0){
		annot.dat<-list()
		for(i in 1:n.annot){
			cat(paste("...",enumEnd(i)," multi-state annotation data...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
			annot.tmp.dat<-try(read.table(annot.files[i],header=F,sep="\t",colClasses=c("character",rep("numeric",2),"character","numeric","character",rep("numeric",2),"character")),silent=TRUE)
			if(length(grep(annot.tmp.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," multi-state annotation data file. Please check the input file format is correct. NB. for larger annotation files you might have to wait a bit until visPIG has had a chance to fully upload your data. NBB. Multi-state annotation file needs to be tab-separated, not whitespace separated."))}
			annot.tmp.dat[is.element(el=annot.tmp.dat[,1],set=chr.set),]
			annot.dat[[i]]<-annot.tmp.dat[annot.tmp.dat[,1]==paste("chr",regions.dat[1,1],sep="") & annot.tmp.dat[,2]<=regions.dat[1,3] & annot.tmp.dat[,3]>=regions.dat[1,2],]
			if(nrow(regions.dat)>1){
				for(j in 2:nrow(regions.dat)){
					annot.dat[[i]]<-rbind(annot.dat[[i]],annot.tmp.dat[annot.tmp.dat[,1]==paste("chr",regions.dat[j,1],sep="") & annot.tmp.dat[,2]<=regions.dat[j,3] & annot.tmp.dat[,3]>=regions.dat[j,2],])
				}
			}
		}
		if(n.annot>1){
			if(length(annot.mai.top)==1){annot.mai.top<-rep(annot.mai.top,n.annot)}else if(length(annot.mai.top)!=n.annot){stop("Wrong length for annot.mai.top; needs to match the number of annotation plots!")}
			if(length(annot.mai.bot)==1){annot.mai.bot<-rep(annot.mai.bot,n.annot)}else if(length(annot.mai.bot)!=n.annot){stop("Wrong length for annot.mai.bot; needs to match the number of annotation plots!")}
			if(length(annot.bgclass)==1){annot.bgclass<-rep(annot.bgclass,n.annot)}else if(length(annot.bgclass)!=n.annot & !is.null(annot.bgclass)){stop("Wrong length for annot.bgclass; needs to match the number of annotation plots!")}
			if(length(annot.lwd)==1){annot.lwd<-rep(annot.lwd,n.annot)}else if(length(annot.lwd)!=n.annot){stop("Wrong length for annot.lwd; needs to match the number of annotation plots!")}
			if(length(annot.ylab)==1){annot.ylab<-rep(annot.ylab,n.annot)}else if(length(annot.ylab)!=n.annot){stop("Wrong length for annot.ylab; needs to match the number of annotation plots!")}
			if(length(annot.cex.txt)==1){annot.cex.txt<-rep(annot.cex.txt,n.annot)}else if(length(annot.cex.txt)!=n.annot){stop("Wrong length for annot.cex.txt; needs to match the number of annotation plots!")}
			if(length(annot.relplotheight)==1){annot.relplotheight<-rep(annot.relplotheight,n.annot)}else if(length(annot.relplotheight)!=n.annot){stop("Wrong length for annot.relplotheight; needs to match the number of annotation plots!")}
		}	
	}
	
# association data
	if(!is.na(assoc.file)){
		cat("...association data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		assoc.dat<-try(read.table(assoc.file,header=F,sep="",colClasses=c("character",rep("numeric",2))),silent=TRUE)
		if(length(grep(assoc.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," association data file. Please check the input file format is correct."))}
		assoc.dat[,1]<-as.integer(sub(assoc.dat[,1],pattern="chr",replacement=""))
	}

# directionality index data
	if(!is.na(dix.file)){
		cat("...directionality index data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		dix.dat<-try(read.table(dix.file,sep="",header=F,nrows=5),silent=TRUE)
		if(length(grep(dix.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the directionality index data file. Please check the input file format is correct."))}
		dix.ncol<-ncol(dix.dat)
		rm(dix.dat)
		dix.dat<-read.table(dix.file,sep="\t",header=F,colClasses=c("character",rep("numeric",dix.ncol-1)))
		dix.dat[,1]<-as.integer(sub(dix.dat[,1],pattern="chr",replacement=""))
		if(n.dix>1){
			if(length(dix.col.pos)==1){dix.col.pos<-rep(dix.col.pos,n.dix)}else if(length(dix.col.pos)!=n.dix){stop("Wrong length for dix.col.pos; needs to match the number of dix plots!")}
			if(length(dix.col.neg)==1){dix.col.neg<-rep(dix.col.neg,n.dix)}else if(length(dix.col.neg)!=n.dix){stop("Wrong length for dix.col.neg; needs to match the number of dix plots!")}
			if(length(dix.col.rgb)==1){dix.col.rgb<-rep(dix.col.rgb,n.dix)}else if(length(dix.col.rgb)!=n.dix){stop("Wrong length for dix.col.rgb; needs to match the number of dix plots!")}
			if(length(dix.ylab)==1){dix.ylab<-rep(dix.ylab,n.dix)}else if(length(dix.ylab)!=n.dix){stop("Wrong length for dix.ylab; needs to match the number of dix plots!")}
			if(length(dix.cex.txt)==1){dix.cex.txt<-rep(dix.cex.txt,n.dix)}else if(length(dix.cex.txt)!=n.dix){stop("Wrong length for dix.cex.txt; needs to match the number of dix plots!")}
			if(length(dix.relplotheight)==1){dix.relplotheight<-rep(dix.relplotheight,n.dix)}else if(length(dix.relplotheight)!=n.dix){stop("Wrong length for dix.relplotheight; needs to match the number of dix plots!")}
			if(length(dix.ylim.upper)==1){dix.ylim.upper<-rep(dix.ylim.upper,n.int)}else if(length(dix.ylim.upper)!=n.dix){stop("Wrong length for dix.ylim.upper; needs to match the number of dix plots!")}
			if(length(dix.ylim.lower)==1){dix.ylim.lower<-rep(dix.ylim.lower,n.int)}else if(length(dix.ylim.lower)!=n.dix){stop("Wrong length for dix.ylim.lower; needs to match the number of dix plots!")}
		}
		if(sum(dix.col.rgb)>0){
			for(i in 1:n.dix){
				if(dix.col.rgb[i]){
					col.tmp<-as.integer(unlist(strsplit(dix.col.pos[i],split="_")))
					dix.col.pos[i]<-rgb(red=col.tmp[1],green=col.tmp[2],blue=col.tmp[3],maxColorValue=255)
					col.tmp<-as.integer(unlist(strsplit(dix.col.neg[i],split="_")))
					dix.col.neg[i]<-rgb(red=col.tmp[1],green=col.tmp[2],blue=col.tmp[3],maxColorValue=255)
				}
			}
		}
	}
	
# heat map data
	if(!is.na(heat.file)){
		cat("...heat map data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		if(heat.type=="pos-pos"){
			heat.dat<-try(read.table(heat.file,sep="",header=F,colClasses=c("character","numeric","character","numeric","numeric")),silent=TRUE)
			if(length(grep(heat.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the heat map data file. Please check the input file format is correct. Note that there are two different heat map file formats dependent on the type of heat map requested."))}
			if(ncol(heat.dat)!=5){stop(paste(sep="","Wrong number of columns in the heat map data file. Please check the input file format is correct. Note that there are two different heat map file formats dependent on the type of heat map requested."))}
			heat.dat[,1]<-as.integer(sub(heat.dat[,1],pattern="chr",replacement=""))
			heat.dat[,3]<-as.integer(sub(heat.dat[,3],pattern="chr",replacement=""))
			
			if(is.na(heat.bw)){
				heat.bw<-1
				dynamicHelp <<- c(dynamicHelp, paste(length(dynamicHelp),')...\tHeat map : Be aware that you have not specified the bin size parameter for the chr-chr heat map.\n\tBy default this will equal 1. If your bins are wider, please set the correct bin size in the \"Heatmap Track\" panel',sep=''))
			}
		}else if(heat.type=="general"){
			heat.dat<-try(read.table(heat.file,sep="",header=F,colClasses=c("character",rep("numeric",5))),silent=TRUE)
			if(length(grep(heat.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the heat map data file. Please check the input file format is correct. Note that there are two different heat map file formats dependent on the type of heat map requested."))}
			if(ncol(heat.dat)!=6){stop(paste(sep="","Wrong number of columns in the heat map data file. Please check the input file format is correct. Note that there are two different heat map file formats dependent on the type of heat map requested."))}
			heat.dat[,1]<-as.integer(sub(heat.dat[,1],pattern="chr",replacement=""))
			heat.dat[,3]<-as.integer(sub(heat.dat[,3],pattern="chr",replacement=""))

		}
		
		if(is.na(heat.logscale)){heat.logscale<-FALSE}
	}
	
# intensity data
	if(n.int>0){
		int.dat<-list()
		if(n.int>1){
			if(length(int.lwd)==1){int.lwd<-rep(int.lwd,n.int)}else if(length(int.lwd)!=n.int){stop("Wrong length for int.lwd; needs to match the number of intensity plots!")}
			if(length(int.ylab)==1){int.ylab<-rep(int.ylab,n.int)}else if(length(int.ylab)!=n.int){stop("Wrong length for int.ylab; needs to match the number of intensity plots!")}
			if(length(int.ylim.upper)==1){int.ylim.upper<-rep(int.ylim.upper,n.int)}else if(length(int.ylim.upper)!=n.int){stop("Wrong length for int.ylim.upper; needs to match the number of intensity plots!")}
			if(length(int.ylim.lower)==1){int.ylim.lower<-rep(int.ylim.lower,n.int)}else if(length(int.ylim.lower)!=n.int){stop("Wrong length for int.ylim.lower; needs to match the number of intensity plots!")}
			if(length(int.cex.txt)==1){int.cex.txt<-rep(int.cex.txt,n.int)}else if(length(int.cex.txt)!=n.int){stop("Wrong length for int.cex.txt; needs to match the number of intensity plots!")}
			if(length(int.relplotheight)==1){int.relplotheight<-rep(int.relplotheight,n.int)}else if(length(int.relplotheight)!=n.int){stop("Wrong length for int.relplotheight; needs to match the number of intensity plots!")}
			if(length(int.style)==1){int.style<-rep(int.style,n.int)}else if(length(int.style)!=n.int){stop("Wrong length for int.style; needs to match the number of intensity plots!")}		
		}
		
		for(i in 1:n.int){
			cat(paste("...",enumEnd(i)," intensity data file...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
			int.dat[[i]]<-try(read.table(int.files[i],sep="",header=F,colClasses=c("character",rep("integer",2),"numeric")),silent=TRUE)
			if(length(grep(int.dat[[i]][1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," intensity data file. Please check the input file format is correct."))}
			int.dat[[i]][,1]<-as.integer(sub(int.dat[[i]][,1],pattern="chr",replacement=""))
		}
		if(!is.na(int.refbox)){
			int.ref.list<-vector("list",n.int)
			for(i in 1:n.int){
				refbox<-unlist(strsplit(int.refbox[i],split=";"))
				int.ref.list[[i]]<-matrix(ncol=3,nrow=length(refbox))
				for(j in 1:length(refbox)){
					refbox.tmp<-unlist(strsplit(refbox[j],split=":"))
					int.ref.list[[i]][j,1]<-as.integer(sub(refbox.tmp[1],pattern="chr",replacement=""))
					int.ref.list[[i]][j,2:3]<-as.numeric(refbox.tmp[2:3])
				}
			}
		}
		if(!is.na(int.ref2box)){
			int.ref2.list<-vector("list",n.int)
			for(i in 1:n.int){
				ref2box<-unlist(strsplit(int.ref2box[i],split=";"))
				int.ref2.list[[i]]<-matrix(ncol=3,nrow=length(ref2box))
				for(j in 1:length(ref2box)){
					ref2box.tmp<-unlist(strsplit(ref2box[j],split=":"))
					int.ref2.list[[i]][j,1]<-as.integer(sub(ref2box.tmp[1],pattern="chr",replacement=""))
					int.ref2.list[[i]][j,2:3]<-as.numeric(ref2box.tmp[2:3])
				}
			}
		}
	}
	
# feature annotation data
	if(n.feat>0){
		if(length(feat.relplotheight)!=n.feat){feat.relplotheight<-rep(feat.relplotheight[1],n.feat)}
		feat.dat<-list()
		if(n.feat>1){
			if(length(feat.lwd)==1){feat.lwd<-rep(feat.lwd,n.feat)}else if(length(feat.lwd)!=n.feat){stop("Wrong length for feat.lwd; needs to match the number of feature plots!")}
			if(length(feat.textpos)==1){feat.textpos<-rep(feat.textpos,n.feat)}else if(length(feat.txtpos)!=n.feat){stop("Wrong length for feat.textpos; needs to match the number of feature plots!")}
			if(length(feat.cex.txt)==1){feat.cex.txt<-rep(feat.cex.txt,n.feat)}else if(length(feat.cex.txt)!=n.feat){stop("Wrong length for feat.cex.txt; needs to match the number of feature plots!")}
			if(length(feat.ylab)==1){feat.ylab<-rep(feat.ylab,n.feat)}else if(length(feat.ylab)!=n.feat){stop("Wrong length for feat.ylab; needs to match the number of feature plots!")}
			if(length(feat.col)==1){feat.col<-rep(feat.col,n.feat)}else if(length(feat.col)!=n.feat){stop("Wrong length for feat.col; needs to match the number of feature plots!")}
			if(length(feat.txt.hori.offset)==1){feat.txt.hori.offset<-rep(feat.txt.hori.offset,n.feat)}else if(length(feat.txt.hori.offset)!=n.feat){stop("Wrong length for feat.txt.hori.offset; needs to match the number of feature plots!")}
			if(length(feat.txt.vert.offset)==1){feat.txt.vert.offset<-rep(feat.txt.vert.offset,n.feat)}else if(length(feat.txt.vert.offset)!=n.feat){stop("Wrong length for feat.txt.vert.offset; needs to match the number of feature plots!")}
		}
		
		for(i in 1:n.feat){
			cat(paste("...",enumEnd(i)," feature data file...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
			feat.dat[[i]]<-try(read.table(feat.files[i],sep="\t",header=F,colClasses=c("character",rep("integer",2),"character")),silent=TRUE)
			if(length(grep(feat.dat[[i]][1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," feature annotation file. Please check the input file format is correct. Note that feature annotation files need to be tab-separated, not whitespace-separated."))}
			feat.dat[[i]][,1]<-as.integer(sub(feat.dat[[i]][,1],pattern="chr",replacement=""))
		}
	}
	
# GERP conservation data
	if(!is.na(gerp.files[1])){
		cat("...GERP data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		if(!gerp.noblocks){
			gerp.dat<-try(read.table(gerp.files[1],sep="",header=F,colClasses=c("character",rep("numeric",3))),silent=TRUE)
			if(length(grep(gerp.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the GERP data file (block format). Please check the input file format is correct."))}
		}else{
			gerp.dat<-try(read.table(gerp.files[1],sep="",header=F,colClasses=c("character",rep("numeric",2))),silent=TRUE)
			if(length(grep(gerp.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the GERP data file (no block format). Please check the input file format is correct."))}
		}
		gerp.dat[,1]<-as.integer(sub(gerp.dat[,1],pattern="chr",replacement=""))
		
		if(!is.na(gerp.filter.file)){
			cat("...GERP filter data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			gerp.filter.dat<-try(read.table(gerp.filter.file,colClasses=c("character","integer"),sep=""),silent=TRUE)
			if(length(grep(gerp.filter.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the GERP filter data file. Please check the input file format is correct."))}
			gerp.filter.dat[,1]<-as.integer(sub(gerp.filter.dat[,1],pattern="chr",replacement=""))
		}
	}
	
# phastCons conservation data
	if(!is.na(phast.files[1])){
		cat("...phastCons data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		if(!phast.noblocks){
			phast.dat<-try(read.table(phast.files[1],sep="",header=F,colClasses=c("character",rep("numeric",3))),silent=TRUE)
			if(length(grep(phast.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the phastCons data file (block format). Please check the input file format is correct."))}
		}else{
			phast.dat<-try(read.table(phast.files[1],sep="",header=F,colClasses=c("character",rep("numeric",2))),silent=TRUE)
			if(length(grep(phast.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the phastCons data file (no block format). Please check the input file format is correct."))}
		}
		phast.dat[,1]<-as.integer(sub(phast.dat[,1],pattern="chr",replacement=""))
		
		if(!is.na(phast.filter.file)){
			cat("...phastCons filter data...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			phast.filter.dat<-try(read.table(phast.filter.file,colClasses=c("character","integer"),sep=""),silent=TRUE)
			if(length(grep(phast.filter.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the phastCons filter data file. Please check the input file format is correct."))}
			phast.filter.dat[,1]<-as.integer(sub(phast.filter.dat[,1],pattern="chr",replacement=""))
		}
	}

# vertical line position data
	if(!is.na(lines.file)){
		lines.dat<-try(read.table(lines.file,sep="",header=F,colClasses=c("character","integer")),silent=TRUE)
		if(length(grep(lines.dat[1],pattern="Error"))>0){stop(paste(sep="","Cannot read the vertical lines file. Please check the input file format is correct."))}
		lines.dat[,1]<-as.integer(sub(lines.dat[,1],pattern="chr",replacement=""))
		if(nrow(lines.dat)>1){
			if(length(lines.lwd)==1){lines.lwd<-rep(lines.lwd,nrow(lines.dat))}
			if(length(lines.lty)==1){lines.lty<-rep(lines.lty,nrow(lines.dat))}
			if(length(lines.col)==1){lines.col<-rep(lines.col,nrow(lines.dat))}
		}
	}

cat("Done.\n\n",file=paste(outprefix,"_R.log",sep=""),append=T)


######################################
## SET UP PLOTTING RANGE PARAMETERS ##
######################################

# legend set-up
	legend.plot<-(!is.na(legend.where) & is.element(el=legend.where,set=c("top","bottom","left","left_top","left_bottom","right","right_top","right_bottom","separate")) & sum(c(legend.annot,legend.hic,legend.heat,legend.dix,legend.assoc)==TRUE)>0 & sum(legend.width==0)==0)
	if(legend.plot){
		
		# compute the number of legends
		n.leg<-0
		n.legend.dix<-0
		n.legend.hic<-0
        for(leg in legend.order){
			if(legend.assoc & leg=="assoc"){n.leg<-n.leg+1}
			if(legend.hic & length(grep(leg,pattern="^hic",perl=T))>0){
				if(leg=="hic"){
					if(length(hic.colref)==length(hic.colnonref)){
						n.legend.hic<-length(unique(paste(hic.colref,hic.colnonref,sep=":")))
						n.leg<-n.leg+n.legend.hic
					}else{
						n.legend.hic<-1
						n.leg<-n.leg+1
						hic.colref<-dix.colref[1]
						hic.colnonref<-dix.colnonref[1]
					}
				}else if(as.integer(sub(leg,pattern="hic",replacement=""))<=min(c(length(hic.colref),length(hic.colnonref)))){
					n.legend.hic<-n.legend.hic+1
					n.leg<-n.leg+1
				}
			}
			if(legend.heat & leg=="heat"){n.leg<-n.leg+1}
			if(legend.annot & length(grep(leg,pattern="^annot",perl=T))>=1){
				if(leg=="annot"){
					n.leg<-n.leg+length(legend.annot.file)
				}else if(as.integer(sub(leg,pattern="annot",replacement=""))<=length(legend.annot.file)){
					n.leg<-n.leg+1
				}
			}
			if(legend.dix & length(grep(leg,pattern="^dix",perl=T))>0){
				if(leg=="dix"){
					if(length(dix.col.pos)==length(dix.col.neg)){
						n.legend.dix<-length(unique(paste(dix.col.pos,dix.col.neg,sep=":")))
						n.leg<-n.leg+n.legend.dix
					}else{
						n.legend.dix<-1
						n.leg<-n.leg+1
						dix.col.pos<-dix.col.pos[1]
						dix.col.neg<-dix.col.neg[1]
					}
				}else if(as.integer(sub(leg,pattern="dix",replacement=""))<=min(c(length(dix.col.pos),length(dix.col.neg)))){
					n.legend.dix<-n.legend.dix+1
					n.leg<-n.leg+1
				}
			}
		}
		
		# figure out how and where to put the legend
		if(sum(is.na(legend.width))>0){legend.width<-0.25}
		if(sum(is.na(legend.height))>0){legend.height<-0.25}
		if(is.na(legend.rows) & is.na(legend.cols)){
			if(legend.where=="top" | legend.where=="bottom"){
				legend.rows<-1
				legend.cols<-n.leg
			}else{
				legend.cols<-1
				legend.rows<-n.leg
			}
		}else{
			if(is.na(legend.rows)){legend.rows<-ceiling(n.leg/legend.cols)}
			if(is.na(legend.cols)){legend.cols<-ceiling(n.leg/legend.rows)}
		}
		if(sum(legend.width<0)>0){
			stop("legend widths vector needs to be positive.")
		}else if(sum(legend.width)>1){
			legend.width<-legend.width/sum(legend.width)
		}
		
		if(sum(legend.height<0)>0){
			stop("legend heights vector needs to be positive.")
		}else if(sum(legend.height)>1){
			legend.height<-legend.height/sum(legend.height)
		}
		if(legend.where=="left"){legend.where<-"left_bottom"}
		if(legend.where=="right"){legend.where<-"right_bottom"}
		if(length(legend.width)!=legend.cols){legend.width<-rep(legend.width[1]/legend.cols,legend.cols)}
		if(length(legend.height)!=legend.rows){legend.height<-rep(legend.height[1]/legend.rows,legend.rows)}
		if(!is.element(el=legend.where,set=c("left_top","left_bottom","right_top","right_bottom"))){
			if(sum(legend.width)!=1){legend.width<-legend.width/sum(legend.width)}
			if(sum(legend.height)==1){stop("You need to leave space for the main plot when specifying the legend height vector and opting to have the legend at the top, bottom or middle (i.e. make sure that the sum of the legend heights does not equal 1 or the total height of the plot).")}
		}else{
			if(sum(legend.width)==1){stop("You need to leave space for the main plot when specifying the legend width vector and opting to have the legend on the side (i.e. make sure that the sum of the legend widths does not equal 1 or the total width of the plot).")}
                        if(sum(legend.height)!=1){legend.height<-legend.height/sum(legend.height)}
		}
		
		# specific parameters for the multi-state legend
		if(legend.annot){
			if(!is.null(legend.annot.file[1]) & !is.na(legend.annot.file[1])){
				legend.annot.dat<-list()
				n.legend.annot<-length(legend.annot.file)
				for(i in 1:n.legend.annot){
					legend.annot.dat[[i]]<-try(read.table(legend.annot.file[i],sep="\t",header=F,colClasses=rep("character",2)),silent=TRUE)
					if(length(grep(legend.annot.dat[[i]][1],pattern="Error"))>0){stop(paste(sep="","Cannot read the ",enumEnd(i)," multi-state annotation colour scale legend file. Please check the input file format is correct."))}
				}
			}else{
				if(n.leg==0){legend.plot<-FALSE}else{stop("No multi-state legend file provided.")}
			}
		}
		
		# specific parameters for the interaction track legend
		if(legend.hic){
			if(length(legend.hic.xlab)!=n.legend.hic){legend.hic.xlab<-rep(legend.hic.xlab[1],n.legend.hic)}
			if(length(legend.hic.xlim)!=n.legend.hic){legend.hic.xlim<-rep(legend.hic.xlim[1],n.legend.hic)}
			if(length(legend.hic.labref)!=n.legend.hic){legend.hic.labref<-rep(legend.hic.labref[1],n.legend.hic)}
			if(length(legend.hic.labnonref)!=n.legend.hic){legend.hic.labnonref<-rep(legend.hic.labnonref[1],n.legend.hic)}
			if(length(legend.hic.lablink)!=n.legend.hic){legend.hic.lablink<-rep(legend.hic.lablink[1],n.legend.hic)}
			if(length(legend.hic.axline)!=n.legend.hic){legend.hic.axline<-rep(legend.hic.axline[1],n.legend.hic)}
		}

		# specific parameters for the dix legend
		if(legend.dix){
			if(length(legend.dix.xlab)!=n.legend.dix){legend.dix.xlab<-rep(legend.dix.xlab[1],n.legend.dix)}
			if(length(legend.dix.labpos)!=n.legend.dix){legend.dix.labpos<-rep(legend.dix.labpos[1],n.legend.dix)}
			if(length(legend.dix.labneg)!=n.legend.dix){legend.dix.labneg<-rep(legend.dix.labneg[1],n.legend.dix)}
			if(length(legend.dix.axline)!=n.legend.dix){legend.dix.axline<-rep(legend.dix.axline[1],n.legend.dix)}
		}
		
		# legend margins, and make sure some general legend parameters are encoded as vectors	
		if(length(legend.mai)!=4){legend.mai<-rep(legend.mai[1],4)}
		if(length(legend.cex.lab)<n.leg){legend.cex.lab<-rep(legend.cex.lab[1],n.leg)}
		if(length(legend.cex.txt)<n.leg){legend.cex.txt<-rep(legend.cex.txt[1],n.leg)}
		if(length(legend.adj)<n.leg){legend.adj<-rep(legend.adj[1],n.leg)}
		legend.adj[legend.adj<0 | legend.adj>1 | is.na(legend.adj)]<-0.5

	}

# compute SP (start position of plotted region), EP (end position) and gaps (intervals between SP and EP that should be left out)
	cat("Computing SP, EP and gaps...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
	
	if(nrow(regions.dat)==1){
		chr1<-regions.dat[1,1]
		chr2<-regions.dat[1,1]
		SP<-regions.dat[1,2]
		EP<-regions.dat[1,3]
		gaps<-NA
		n_gaps<-0
	}else{
		chr1<-regions.dat[1,1]
		SP<-regions.dat[1,2]
		chr2<-regions.dat[nrow(regions.dat),1]
		EP<-regions.dat[nrow(regions.dat),3]
		n_gaps<-nrow(regions.dat)-1
		gaps<-integer(4*n_gaps)
		if(n_gaps>0){
			for(i in 1:n_gaps){
				gaps[(4*(i-1)+1)]<-regions.dat[i,1]
				gaps[(4*(i-1)+2)]<-regions.dat[i,3]
				gaps[(4*(i-1)+3)]<-regions.dat[i+1,1]
				gaps[(4*i)]<-regions.dat[i+1,2]
			}
		}
	}

	if(sum(is.na(gaps))==0 & n_gaps>0){
		if(length(gaps) %% 4 != 0){stop("Bad gaps arguments (length not divisible by 4).")}
		
		gaps.list<-list()
		gaps.list[[1]]<-matrix(as.integer(gaps),ncol=4,byrow=T)
		gaps.chr<-(gaps.list[[1]])[,c(1,3)]
		gaps.list[[1]]<-(gaps.list[[1]])[,c(2,4)]
		if(n_gaps==1){
			gaps.chr<-matrix(gaps.chr,ncol=2,byrow=T)
			gaps.list[[1]]<-matrix(gaps.list[[1]],ncol=2,byrow=T)
		}
		
		range_labels_x<-rep("",n_gaps)
		
		gaps.sizes<-(gaps.list[[1]])[,2]-(gaps.list[[1]])[,1]
		ext.tmp<-rep("Mb",n_gaps)
		gaps.sizes<-round(gaps.sizes/1e6,digits=2)
		gaps.sizes<-paste(gaps.sizes,ext.tmp,sep="")
		rm(ext.tmp)
		gaps.sizes[gaps.chr[,1]!=gaps.chr[,2]]<-"trans"
		if(chr1!=chr2){range_labels_x<-paste("chr",gaps.chr[,1],sep="")}
		if(chr1!=chr2){
			range_labels_x[n_gaps+1]<-paste("chr",gaps.chr[n_gaps,2],sep="")
			if(is.na(xlab)){xlab<-""}
		}else{
			if(is.na(xlab) && x.axis.format=='Mb'){
				xlab<-paste("chr ",chr1," position (Mb)",sep="")      
			}else if(is.na(xlab) && x.axis.format=='Kb'){
				xlab<-paste("chr ",chr1," position (Kb)",sep="")        
			}else if(is.na(xlab) && x.axis.format=='bp'){
				xlab<-paste("chr ",chr1," position (bp)",sep="")
			}
		}
		if(!is.null(n_gaps)){gap_width_x<-sum(regions.dat[,3]-regions.dat[,2])*gap_width}else{gap_width_x<-0}
	}else{
		gaps.list<-NULL
		gaps.sizes<-NULL
		if(is.na(xlab) && x.axis.format=='Mb'){
		  xlab<-paste("chr ",chr1," position (Mb)",sep="")      
		}else if(is.na(xlab) && x.axis.format=='Kb'){
	 	 xlab<-paste("chr ",chr1," position (Kb)",sep="")        
		}else if(is.na(xlab) && x.axis.format=='bp'){
		  xlab<-paste("chr ",chr1," position (bp)",sep="")
		}
	}
	
	cat(paste("Done: SP = ",SP,", EP = ",EP,".\n\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)

# set up the zoom parameters
	zoom.SP.local <- zoom.SP
	zoom.EP.local <- zoom.EP
	zoom.chr.local <- zoom.chr
	if(zoom==TRUE && !is.na(zoom.SP) && !is.na(zoom.EP) && (abs(zoom.EP-zoom.SP)<=max_chr)){
		do.zoom<-TRUE
	}else{
		do.zoom<-FALSE
	}
	if(do.zoom){ # NB only plot axis at the bottom of the normal scale plots if it is requested explicitly
 		if(track.order.top.down[length(track.order.top.down)]=='axis'){track.order.top.down<-track.order.top.down[1:(length(track.order.top.down)-1)]}	  
		if(zoom.track.order.top.down[1]==''){
			track.order.top.down<-c(track.order.top.down,'change.scale',track.order.top.down)
		}else{
			track.order.top.down<-c(track.order.top.down,'change.scale',zoom.track.order.top.down)
		}
	}

# compute chromosomal position x-axis labels
	if(chr1!=chr2){
		ranges_x<-matrix(nrow=0,ncol=2)
		n_gapsx<-nrow(gaps.list[[1]])
		if(!is.null(n_gapsx)){
			ranges_x<-rbind(ranges_x,c(SP,(gaps.list[[1]])[1,1]))
			if(n_gapsx>1){
				for(i in 2:n_gapsx){
					ranges_x<-rbind(ranges_x,c((gaps.list[[1]])[i-1,2],(gaps.list[[1]])[i,1]))
				}
			}
			ranges_x<-rbind(ranges_x,c((gaps.list[[1]])[n_gapsx,2],EP))
		}else{
			ranges_x<-NULL
		}
	}else{
		ranges_x<-NULL
		if(sum(!is.na(snap.chr) & snap.chr!=chr1)>0) stop(paste('The chromosome you specified for the SNAP plot, ',snap.chr,', is not the same as that in your regions file, ',chr1,'. ',sep=''))
		snap.chr[1:n.snap]<-chr1
	}
	

#################################################################
## CONVERT EVERYTHING TO GENOME-WIDE COORDINATES IF chr1!=chr2 ##
#################################################################

# check if conversion required
	if(chr1!=chr2){
		if(is.na(chrsizes.file)){stop("Regions file contains more than one chromosome, yet no chrsizes.file specified!")}
		cat("Converting to genome-wide coordinates...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		
		for(i in 2:length(chrsizes.dat)){chrsizes.dat[i]<-chrsizes.dat[i-1]+chrsizes.dat[i]}
		chrsizes.dat<-c(0,chrsizes.dat)
		chr<-0
		SP<-chrsizes.dat[chr1]+SP
		EP<-chrsizes.dat[chr2]+EP
  
		if(do.zoom & is.na(zoom.chr)){stop("Please specify a chromosome for the zoomed region.")}
		zoom.SP<-chrsizes.dat[zoom.chr]+zoom.SP
		zoom.EP<-chrsizes.dat[zoom.chr]+zoom.EP
		zoom.chr<-chr
	
		gaps.list[[1]][,1]<-chrsizes.dat[gaps.chr[,1]]+(gaps.list[[1]])[,1]
		gaps.list[[1]][,2]<-chrsizes.dat[gaps.chr[,2]]+(gaps.list[[1]])[,2]
  
# convert the SNAP data to gw coordinates
		if(!is.na(snap.data.files) && !is.na(snap.rate.files)){
			for (i in 1:n.snap){
				# Rate file
				if(sum(is.na(snap.rate.dat[[i]][,3]))==nrow(snap.rate.dat[[i]])){
					if(is.na(snap.chr[i])){stop("You need to specify a chromosome for each of your SNAP tracks (or specify a chromosome column in the SNAP rate file).")}
					snap.rate.dat[[i]]$POSITION<-snap.rate.dat[[i]]$POSITION+chrsizes.dat[as.integer(snap.chr[i])]
				}else{
					snap.rate.dat[[i]][,3]<-as.integer(sub(snap.rate.dat[[i]][,3],pattern="chr",replacement=""))
					snap.rate.dat[[i]]$POSITION<-snap.rate.dat[[i]]$POSITION+chrsizes.dat[as.integer(snap.rate.dat[[i]][,3])]
				}
      	
				# Data file (for first SNP)
				temp.chr <- as.character(snap.dat[[i]][,"Chromosome"])
				temp.chr <- sub("^chr", "", temp.chr)
				coordname <- names(snap.dat[[i]])[grep("^Coordinate_", names(snap.dat[[i]]))]
				names(snap.dat[[i]])[names(snap.dat[[i]])==coordname] = "Coordinate"
				snap.dat[[i]]$Coordinate<-snap.dat[[i]]$Coordinate+chrsizes.dat[as.integer(temp.chr)]
				snap.dat[[i]]$Position<-snap.dat[[i]]$Position+chrsizes.dat[as.integer(temp.chr)]
			}
    
			# Data file (for second SNP)
			if(!is.na(snap.snp2.file)){
				temp.chr <- as.character(snap.snp2.dat[,"Chromosome"])
				temp.chr <- sub("^chr", "", temp.chr)
				coordname <- names(snap.snp2.dat)[grep("^Coordinate_", names(snap.snp2.dat))]
				names(snap.snp2.dat)[names(snap.snp2.dat)==coordname] = "Coordinate"
				snap.snp2.dat$Coordinate<-snap.snp2.dat$Coordinate+chrsizes.dat[as.integer(snap.chr[1])]
				snap.snp2.dat$Position<-snap.snp2.dat$Position+chrsizes.dat[as.integer(snap.chr[1])]     
			}
		}
		
# convert the interaction data to gw coordinates
		if(n.hic>0){
			for(i in 1:n.hic){
				hic.dat[[i]][,2]<-chrsizes.dat[hic.dat[[i]][,1]]+hic.dat[[i]][,2]
				hic.dat[[i]][,3]<-chrsizes.dat[hic.dat[[i]][,1]]+hic.dat[[i]][,3]
				hic.dat[[i]][,1]<-0
				if(hic.style[i]=="arches" | hic.style[i]=="flatarches"){
					hic.dat[[i]][,5]<-chrsizes.dat[hic.dat[[i]][,4]]+hic.dat[[i]][,5]
					hic.dat[[i]][,6]<-chrsizes.dat[hic.dat[[i]][,4]]+hic.dat[[i]][,6]
					hic.dat[[i]][,4]<-0
				}
			}
			
			if(!is.na(hic.capt.region[1])){
				hic.capt.region[,2]<-chrsizes.dat[hic.capt.region[,1]]+hic.capt.region[,2]
				hic.capt.region[,3]<-chrsizes.dat[hic.capt.region[,1]]+hic.capt.region[,3]
				hic.capt.region[,1]<-0
			}
		}
		
# convert the multi-state annotation data to gw coordinates
		if(n.annot>0){
			for(i in 1:n.annot){
				annot.dat[[i]][,1]<-as.integer(sub(annot.dat[[i]][,1],pattern="chr",replacement=""))
				annot.dat[[i]][,2]<-chrsizes.dat[annot.dat[[i]][,1]]+annot.dat[[i]][,2]
				annot.dat[[i]][,3]<-chrsizes.dat[annot.dat[[i]][,1]]+annot.dat[[i]][,3]
				annot.dat[[i]][,1]<-0
			}
		}
		
# convert the association data to gw coordinates		
		if(!is.na(assoc.file)){
			assoc.dat[,2]<-chrsizes.dat[assoc.dat[,1]]+assoc.dat[,2]
			assoc.dat[,1]<-0
		}
		
# convert the directionality index data to gw coordinates
		if(!is.na(dix.file)){
			dix.dat[,2]<-chrsizes.dat[dix.dat[,1]]+dix.dat[,2]
			dix.dat[,3]<-chrsizes.dat[dix.dat[,1]]+dix.dat[,3]
			dix.dat[,1]<-0
		}
		
# convert the heatmap data to gw coordinates
		if(!is.na(heat.file)){
			if(heat.type=="pos-pos"){
				heat.dat[,2]<-chrsizes.dat[heat.dat[,1]]+heat.dat[,2]
				heat.dat[,4]<-chrsizes.dat[heat.dat[,3]]+heat.dat[,4]
				heat.dat[,1]<-0
				heat.dat[,3]<-0
			}else if(heat.type=="general"){
				heat.dat[,2]<-chrsizes.dat[heat.dat[,1]]+heat.dat[,2]
				heat.dat[,3]<-chrsizes.dat[heat.dat[,1]]+heat.dat[,3]
				heat.dat[,1]<-0
			}
			if(!is.na(heat.SP)){heat.SP<-chrsizes.dat[heat.chr]+heat.SP}else{heat.SP<-SP}
			if(!is.na(heat.EP)){heat.EP<-chrsizes.dat[heat.chr]+heat.EP}else{heat.EP<-EP}
		}
		
# convert the intensity data to gw coordinates
		if(n.int>0){
			for(i in 1:n.int){
				int.dat[[i]][,2]<-chrsizes.dat[int.dat[[i]][,1]]+int.dat[[i]][,2]
				int.dat[[i]][,3]<-chrsizes.dat[int.dat[[i]][,1]]+int.dat[[i]][,3]
				int.dat[[i]][,1]<-0
			}
			if(!is.na(int.refbox[1])){
				for(j in 1:length(int.ref.list)){
					int.ref.list[[j]][,2]<-chrsizes.dat[int.ref.list[[j]][,1]]+int.ref.list[[j]][,2]
					int.ref.list[[j]][,3]<-chrsizes.dat[int.ref.list[[j]][,1]]+int.ref.list[[j]][,3]
					int.ref.list[[j]][,1]<-0
				}
			}
			if(!is.na(int.ref2box[1])){
				for(j in 1:length(int.ref2.list)){
					int.ref2.list[[j]][,2]<-chrsizes.dat[int.ref2.list[[j]][,1]]+int.ref2.list[[j]][,2]
					int.ref2.list[[j]][,3]<-chrsizes.dat[int.ref2.list[[j]][,1]]+int.ref2.list[[j]][,3]
					int.ref2.list[[j]][,1]<-0
				}
			}
		}
		
# convert the feature annotation data to gw coordinates
		if(n.feat>0){
			for(i in 1:n.feat){
				feat.dat[[i]][,2]<-chrsizes.dat[feat.dat[[i]][,1]]+feat.dat[[i]][,2]
				feat.dat[[i]][,3]<-chrsizes.dat[feat.dat[[i]][,1]]+feat.dat[[i]][,3]
				feat.dat[[i]][,1]<-0
			}
		}
		
# convert the GERP conservation data to gw coordinates		
		if(!is.na(gerp.files[1])){
			gerp.dat[,2]<-chrsizes.dat[gerp.dat[,1]]+gerp.dat[,2]
			if(!gerp.noblocks){gerp.dat[,3]<-chrsizes.dat[gerp.dat[,1]]+gerp.dat[,3]}
			gerp.dat[,1]<-0
			if(!is.na(gerp.filter.file)){
				gerp.filter.dat[,2]<-chrsizes.dat[gerp.filter.dat[,1]]+gerp.filter.dat[,2]
				gerp.filter.dat[,1]<-0
			}
		}
		
# convert the phastCons conservation data to gw coordinates		
		if(!is.na(phast.files[1])){
			phast.dat[,2]<-chrsizes.dat[phast.dat[,1]]+phast.dat[,2]
			if(!phast.noblocks){phast.dat[,3]<-chrsizes.dat[phast.dat[,1]]+phast.dat[,3]}
			phast.dat[,1]<-0
			if(!is.na(phast.filter.file)){
				phast.filter.dat[,2]<-chrsizes.dat[phast.filter.dat[,1]]+phast.filter.dat[,2]
				phast.filter.dat[,1]<-0
			}
		}
		
# convert vertical line data to gw coordinates
		if(!is.na(lines.file)){
			lines.dat[,2]<-chrsizes.dat[lines.dat[,1]]+lines.dat[,2]
			lines.dat[,1]<-0
		}
		
		
		if(!is.na(title.pos) & !is.na(title.chr)){
			title.pos<-chrsizes.dat[title.chr]+title.pos
		}else{
			title.pos<-NA
		}  
	
		cat("Done.\n\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		
# if no conversion is required, set a few parameters	
	}else{	
		chr<-chr1
		if(!is.na(lines.file)){lines.dat <- lines.dat[as.integer(gsub('^chr','',lines.dat[,1]))==chr,]}
		if(!is.na(title.chr) & title.chr!=chr){title.pos<-NA}  
		if(do.zoom && zoom.chr!=chr){stop('The chromosome specified in the zoom parameters is not the same as that specified in the regions file. Change either the zoom chromosome parameter or the regions file')}
	}


#################################################################
## COMPUTE THE AMOUNT OF VERTICAL SPACE REQUIRED BY SOME PLOTS ##
#################################################################

# gene plot
	if(!is.na(gene.file)){
		cat("Computing number of tracks required for gene data in specified interval...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		if(chr1==chr2){
			gene.info<-gene_track_with_breaks(dat=gene.dat,chr=chr,SP=SP,EP=EP,compute_ntracks_only=TRUE,gaps=gaps.list,gaps.sizes.x=gaps.sizes,plot.bars=F,plot.bars.outsideplot=F,gene.igs=gene.igs)
		}else{
			gene.info<-gene_track_with_breaks(dat=gene.dat,chr=0,SP=SP,EP=EP,compute_ntracks_only=TRUE,gaps=gaps.list,gaps.sizes.x=gaps.sizes,plot.bars=F,plot.bars.outsideplot=F,convert.gw=TRUE,convert.chrstarts=chrsizes.dat,gene.igs=gene.igs)
		}
		ntracks.gene<-gene.info[[1]]
		track.gene<-gene.info[[2]]
		cat(paste("Done: ",ntracks.gene," tracks.\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
  
		if(do.zoom){ 
			if(chr1==chr2){
				gene.info.zoom<-gene_track_with_breaks(dat=gene.dat,chr=zoom.chr,SP=zoom.SP,EP=zoom.EP,compute_ntracks_only=TRUE,gaps=NULL,gaps.sizes.x=NULL,plot.bars=F,plot.bars.outsideplot=F,gene.igs=gene.igs)
			}else{
				gene.info.zoom<-gene_track_with_breaks(dat=gene.dat,chr=0,SP=zoom.SP,EP=zoom.EP,compute_ntracks_only=TRUE,gaps=NULL,gaps.sizes.x=NULL,plot.bars=F,plot.bars.outsideplot=F,convert.gw=TRUE,convert.chrstarts=chrsizes.dat,gene.igs=gene.igs)
			}
			ntracks.gene.zoom<-gene.info.zoom[[1]]
			track.gene.zoom<-gene.info.zoom[[2]] 
			cat(paste("Done: ",ntracks.gene.zoom," tracks (on the zoomed region).\n\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
		}else{
			cat("\n",file=paste(outprefix,"_R.log",sep=""),append=T)
		}
  
	}

# interaction plot
	if(n.hic>0){
		ntracks.hic<-rep(NA,n.hic)
		if(do.zoom){ntracks.hic.zoom<-rep(NA,n.hic)}
		for(i in 1:n.hic){
			cat(paste("Computing the number of tracks required for the ",enumEnd(i)," interaction track...\n"),file=paste(outprefix,"_R.log",sep=""),append=T)
			if(hic.style[i]=="bins"){
				ntracks.hic[i]<-interaction_track_with_breaks(hic.dat=hic.dat[[i]],chr=chr,SP=SP,EP=EP,compute_ntracks_only=TRUE,gaps=gaps.list,gaps.sizes.x=gaps.sizes,plot.bars=F,plot.bars.outsideplot=F)
				cat(paste("Done: ",ntracks.hic[i]," tracks.\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
				
				if(do.zoom){   
					ntracks.hic.zoom[i]<-interaction_track_with_breaks(hic.dat=hic.dat[[i]],chr=chr,SP=zoom.SP,EP=zoom.EP,compute_ntracks_only=TRUE,gaps=NULL,gaps.sizes.x=NULL,plot.bars=F,plot.bars.outsideplot=F)
					cat(paste("Done: ",ntracks.hic.zoom[i]," tracks (on the zoomed region).\n\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
				}else{
					cat("\n",file=paste(outprefix,"_R.log",sep=""),append=T)
				}
			}else{
				ntracks.hic[i]<-1
				cat(paste("Done: ",ntracks.hic[i]," tracks.\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
				if(do.zoom){
					ntracks.hic.zoom[i]<-1
					cat(paste("Done: ",ntracks.hic.zoom[i]," tracks (on the zoomed region).\n\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
				}else{
					cat("\n",file=paste(outprefix,"_R.log",sep=""),append=T)
				}
			}
		}
	}

# heat map
	if(!is.na(heat.file)){
		if(heat.type=="pos-pos"){
			cat("Computing height of heat map...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			prop.heat<-heat_track_with_breaks(heat.dat=heat.dat,chr=chr,SP=SP,EP=EP,heatSP=heat.SP,heatEP=heat.EP,bw=heat.bw,ylim=heat.ylim,plot.axes="n",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,lwd.line=1e-6,get.size=TRUE,chop.data.gaps=heat.chop.data.gaps,logscale=heat.logscale,heat.dir=heat.dir)
			cat(paste("Done: prop.heat = ",prop.heat,".\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
		
			if(do.zoom){   
				prop.heat.zoom<-heat_track_with_breaks(heat.dat=heat.dat,chr=chr,SP=zoom.SP,EP=zoom.EP,bw=heat.bw,plot.axes="n",side=1,gaps=NULL,gaps.sizes.x=NULL,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,lwd.line=1e-6,get.size=TRUE,chop.data.gaps=heat.chop.data.gaps,logscale=heat.logscale,heat.dir=heat.dir)
				cat(paste("Done: prop.heat.zoom = ",prop.heat.zoom,".\n\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
			}else{
				cat("\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			}
		}else{
			prop.heat<-1/3
			if(do.zoom){prop.heat.zoom<-1/3}
		}
	}

# feature annotation plot
	if(n.feat>0){
		ntracks.feat<-list()
		track.feat<-list()
		if(do.zoom==TRUE){ntracks.feat.zoom<-list(); track.feat.zoom<-list()}
		for(i in 1:n.feat){
			cat(paste("Computing number of tracks required for the ",enumEnd(i)," feature annotation plot...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
			feat.info<-feat_track_with_breaks(dat=feat.dat[[i]],chr=chr,SP=SP,EP=EP,compute_ntracks_only=TRUE,gaps=gaps.list,gaps.sizes.x=gaps.sizes,plot.bars=F,plot.bars.outsideplot=F,feat.cex.txt=feat.cex.txt[i],feat.textpos=feat.textpos[i],feat.lwd=feat.lwd[i],feat.ylab=feat.ylab[i],feat.col=feat.col[i])
			ntracks.feat[[i]]<-feat.info[[1]]
			track.feat[[i]]<-feat.info[[2]]
			cat(paste("Done: ",ntracks.feat[[i]]," tracks.\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
			
			if(do.zoom){
				feat.info.zoom<-feat_track_with_breaks(dat=feat.dat[[i]],chr=chr,SP=zoom.SP,EP=zoom.EP,compute_ntracks_only=TRUE,gaps=NULL,gaps.sizes.x=NULL,plot.bars=F,plot.bars.outsideplot=F,feat.cex.txt=feat.cex.txt[i],feat.textpos=feat.textpos[i],feat.lwd=feat.lwd[i],feat.ylab=feat.ylab[i],feat.col=feat.col[i])
				ntracks.feat.zoom[[i]]<-feat.info.zoom[[1]]
				track.feat.zoom[[i]]<-feat.info.zoom[[2]]
				cat(paste("Done: ",ntracks.feat.zoom[[i]]," tracks (on the zoomed region).\n\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
			}else{
				cat("\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			}
		}
	}


########################
## SET-UP PLOT LAYOUT ##
########################

# start the plot set-up
	cat("Starting the plot...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
	
	height.plot.auto<-0
	layout.height<-numeric(0)
	
	current.index <- 1 # used to keep track where we are in the layout.height vector
	tmp.layout.change.scale <- FALSE
	
# iterate on the track order
	for(i in 1:length(track.order.top.down)){
		
# empty / buffer track
		if(track.order.top.down[i]=='buffer'){
			height.plot.auto <- height.plot.auto+buffer.relplotheight
			layout.height[current.index]<-buffer.relplotheight
			current.index <- current.index + 1
		}

# change of scale track (which takes up two tracks on the layout)
		if(track.order.top.down[i]=='change.scale'){
			if(do.zoom){
				layout.height[current.index] <- 0.2 + zoom.vertical.spacing # for the axis that's modified to have the zoom lines on.
				layout.height[current.index + 1] <- 0.2 # For the axis with the new scale
				height.plot.auto <- height.plot.auto + 0.4 + zoom.vertical.spacing
        	
				if(zoom.x.labels.up!=TRUE && zoom.xlab!=''){ # Give the lower axis a little bit of extra space if the labels are to be drawn underneath the axis.
					layout.height[current.index+1]<-layout.height[current.index+1]+0.8
					height.plot.auto <- height.plot.auto + 0.8
				}
				
				tmp.layout.change.scale<-TRUE
				current.index <- current.index + 2
			}
		}
	
# title track
		if(!is.na(title) && track.order.top.down[i]=='title'){
			height.plot.auto<-height.plot.auto+title.relplotheight
			layout.height[current.index]<-title.relplotheight
			current.index <- current.index + 1 
		}

# snap track  
		if(!is.na(snap.data.files[1]) && !is.na(snap.rate.files[1]) && track.order.top.down[i]=='snap'){
			height.plot.auto <- height.plot.auto+(snap.relplotheight*n.snap)
			layout.height[current.index:(current.index+n.snap-1)]<-snap.relplotheight
			current.index <- current.index + n.snap
		}
		
# heat track
		if(!is.na(heat.file) && track.order.top.down[i]=='heat'){
			if(tmp.layout.change.scale && exists('prop.heat.zoom')){
				if(prop.heat.zoom>0){
					height.plot.auto<-height.plot.auto+6*prop.heat.zoom*heat.relplotheight
					layout.height[current.index]<-6*prop.heat.zoom*heat.relplotheight
				}else{
					height.plot.auto<-height.plot.auto+heat.relplotheight
					layout.height[current.index]<-heat.relplotheight
				}
				current.index <- current.index + 1
			}else{
				if(prop.heat>0){
					height.plot.auto<-height.plot.auto+6*prop.heat*heat.relplotheight
					layout.height[current.index]<-6*prop.heat*heat.relplotheight
				}else{
					height.plot.auto<-height.plot.auto+heat.relplotheight
					layout.height[current.index]<-heat.relplotheight
				}
				current.index <- current.index + 1
			}
		}
		
# feature track
	if(n.feat>0 && length(grep(x=track.order.top.down[i],pattern="^feat",perl=T))>0){
			if(tmp.layout.change.scale && exists('ntracks.feat.zoom')){
				ntracks.feat.vect<-unlist(ntracks.feat.zoom)
			}else{
				ntracks.feat.vect<-unlist(ntracks.feat)
			}
			ntracks.feat.vect[ntracks.feat.vect<1]<-1
			if(track.order.top.down[i]=="feat"){
				height.plot.auto<-height.plot.auto+sum(feat.relplotheight*ntracks.feat.vect/4)
				layout.height[current.index:(current.index+n.feat-1)]<-feat.relplotheight[1:n.feat]*ntracks.feat.vect[1:n.feat]/4
				current.index <- current.index + n.feat 
			}else{
				for(j in 1:n.feat){
					if(track.order.top.down[i]==paste("feat",j,sep="")){
						height.plot.auto<-height.plot.auto+(feat.relplotheight*ntracks.feat.vect/4)[j]
						layout.height[current.index]<-feat.relplotheight[j]*ntracks.feat.vect[j]/4
						current.index <- current.index + 1
					}
				} 
			}
		}

# dix track
		if(!is.na(dix.file) && length(grep(x=track.order.top.down[i],pattern="^dix",perl=T))>0){
			if(track.order.top.down[i]=="dix"){
				height.plot.auto<-height.plot.auto+sum(dix.relplotheight)
				layout.height[(current.index):(current.index+n.dix-1)]<-dix.relplotheight[1:n.dix]
				current.index <- current.index + n.dix
			}else{
				for(j in 1:n.dix){
					if(track.order.top.down[i]==paste("dix",j,sep="")){
						height.plot.auto<-height.plot.auto+dix.relplotheight[j]
						layout.height[current.index]<-dix.relplotheight[j]
						current.index <- current.index + 1
					}
				}
			}
		}
		
# association track
		if(!is.na(assoc.file) && track.order.top.down[i]=='assoc'){
			height.plot.auto<-height.plot.auto+assoc.relplotheight
			layout.height[current.index]<-assoc.relplotheight
			current.index <- current.index + 1
		}
		
# intensity track
		if(n.int>0 && length(grep(x=track.order.top.down[i],pattern="^int",perl=T))>0){
			if(track.order.top.down[i]=="int"){
				height.plot.auto<-height.plot.auto+sum(int.relplotheight)
				layout.height[(current.index):(current.index+n.int-1)]<-int.relplotheight[1:n.int]
				current.index <- current.index + n.int
			}else{
				for(j in 1:n.int){
					if(track.order.top.down[i]==paste("int",j,sep="")){
						height.plot.auto<-height.plot.auto+int.relplotheight[j]
						layout.height[current.index]<-int.relplotheight[j]
						current.index <- current.index + 1
					}
				}
			}
		}
		
# hic track
		if(n.hic>0 && length(grep(x=track.order.top.down[i],pattern="^hic",perl=T))>0){
			for(j in 1:n.hic){
				if(track.order.top.down[i]=="hic" | track.order.top.down[i]==paste("hic",j,sep="")){
					if(tmp.layout.change.scale && exists('ntracks.hic.zoom')){
						if(ntracks.hic.zoom[j]>0){
							height.plot.auto<-height.plot.auto+ntracks.hic.zoom[j]*hic.relplotheight[j]
							layout.height[current.index]<-ntracks.hic.zoom[j]*hic.relplotheight[j]
						}else{
							height.plot.auto<-height.plot.auto+hic.relplotheight[j]
							layout.height[current.index]<-hic.relplotheight[j]
						}
					}else{
						if(ntracks.hic[j]>0){
							height.plot.auto<-height.plot.auto+ntracks.hic[j]*hic.relplotheight[j]
							layout.height[current.index]<-ntracks.hic[j]*hic.relplotheight[j]
						}else{
							height.plot.auto<-height.plot.auto+hic.relplotheight[j]
							layout.height[current.index]<-hic.relplotheight[j]
						}
					}
					current.index <- current.index + 1	
				}
			}
		}
		
# annotation track
		if(n.annot>0 && length(grep(x=track.order.top.down[i],pattern="^annot",perl=T))>0){
			if(track.order.top.down[i]=="annot"){
				height.plot.auto<-height.plot.auto+sum(annot.relplotheight)
				layout.height[(current.index):(current.index+n.annot-1)]<-annot.relplotheight[1:n.annot]
				current.index <- current.index + n.annot
			}else{
				for(j in 1:n.annot){
					if(track.order.top.down[i]==paste("annot",j,sep="")){
						height.plot.auto<-height.plot.auto+annot.relplotheight[j]
						layout.height[current.index]<-annot.relplotheight[j]
						current.index <- current.index + 1
					}
				}
			}
		}
		
# phastCons track
		if(!is.na(phast.files[1]) && track.order.top.down[i]=='phast'){
			height.plot.auto<-height.plot.auto+phast.relplotheight
			layout.height[current.index]<-phast.relplotheight
			current.index <- current.index + 1
		}
		
# GERP track
		if(!is.na(gerp.files[1]) && track.order.top.down[i]=='gerp'){
			height.plot.auto<-height.plot.auto+gerp.relplotheight
			height.plot.auto<-height.plot.auto+gerp.mai.top+gerp.mai.bot # Margins to the total plot height
			layout.height[current.index]<-gerp.relplotheight + gerp.mai.top + gerp.mai.bot # Margins to the current plot
			current.index <- current.index + 1
		}
		
# gene track
		if(!is.na(gene.file) && track.order.top.down[i]=='gene'){
			if(tmp.layout.change.scale && exists('gene.info.zoom')){
				if(ntracks.gene.zoom>0){
					height.plot.auto<-height.plot.auto+gene.relplotheight*ntracks.gene.zoom/2
					layout.height[current.index]<-gene.relplotheight*ntracks.gene.zoom/2
				}else{
					height.plot.auto<-height.plot.auto+gene.relplotheight/2
					layout.height[current.index]<-gene.relplotheight/2
				}
			}else{
				if(ntracks.gene>0){
					height.plot.auto<-height.plot.auto+gene.relplotheight*ntracks.gene/2
					layout.height[current.index]<-gene.relplotheight*ntracks.gene/2
				}else{
					height.plot.auto<-height.plot.auto+gene.relplotheight/2
					layout.height[current.index]<-gene.relplotheight/2
				}
			}
			current.index <- current.index + 1
		}
		
# axis track
		if(!is.na(regions.file) && track.order.top.down[i]=='axis'){
			if(axis.labels.slanted){
				#layout.height[current.index] <- 1.25*max(c(1,1.25*(xlab.line-1)))*axis.relplotheight
				#height.plot.auto<-height.plot.auto+1.25*max(c(1,1.25*(xlab.line-1)))*axis.relplotheight
				layout.height[current.index] <- 1.25*axis.relplotheight
				height.plot.auto<-height.plot.auto+1.25*axis.relplotheight
			}else{
				#layout.height[current.index] <- 0.9*max(c(1,1.25*(xlab.line-1)))*axis.relplotheight
				#height.plot.auto<-height.plot.auto+0.9*max(c(1,1.25*(xlab.line-1)))*axis.relplotheight
				layout.height[current.index] <- axis.relplotheight
				height.plot.auto<-height.plot.auto+axis.relplotheight
			}
			current.index <- current.index + 1
		}
		
# legend(s)
	# only required when legend.where=="separate"; not implemented for now
		
# close the iteration on the track order
	}
	
# clean out track spaces which have not been requested and compute final number of plots
	layout.height<-layout.height[!is.na(layout.height)]
	n.plots<-length(layout.height)
	cat(paste("There will be ",n.plots," plots. (possibly including one 'plot' for the axis)\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)

# if total plot height is not specified explicitly, set it, based on above calculations
	if(is.na(height.plot)){
            if(legend.plot & is.element(el=legend.where,set=c("top","bottom"))){height.plot.auto<-height.plot.auto/(1-sum(legend.height))}
            height.plot<-height.plot.auto
        }
	
# check that no layouts are ridiculously small; and if give a troubleshooting warning as it is extremely likely to halt R execution
	if(sum(layout.height<0.15)>0){dynamicHelp <<- c(dynamicHelp, paste(length(dynamicHelp),')...\tGeneral warning: Some plot(s) have quite low plot window heights associated with them.\n\tIf you get an error message saying that the plot region is too small or the margins too large,\n\tconsider increasing some of the \"relative height\" parameters.',sep=''))}


####################
## START PLOTTING ##
####################

# fire up the PDF or PNG device
	if(plot.type=="pdf"){
		pdf(file,width=width.plot,height=height.plot)
	}else if (plot.type=="png"){
		shiny.image.preview <- 1 #This is used in the plotting script to make sure that e.g. GERP has a big enough line width.
		if(!is.na(assoc.lwd.bars) & assoc.lwd.bars<1){assoc.lwd.bars<-1}
		if(!is.na(gerp.lwd) & gerp.lwd<1){gerp.lwd<-1}
		if(!is.na(phast.lwd) & phast.lwd<1){phast.lwd<1}
		png(outfile,width=width.plot,height=height.plot,units='in',res=72)
	}

# set the plot layout
	if(!legend.plot){
		layout(matrix(1:n.plots,nrow=n.plots,ncol=1),width=c(1),height=layout.height)
	}else if(is.element(el=legend.where,set=c("left_top","left_bottom","right_top","right_bottom"))){
		if(legend.rows>n.plots | legend.cols>n.plots){stop("The number of legend rows/columns cannot exceed the number of plots.")}
                if(length(legend.height)!=legend.rows | length(legend.height)==1){
                    leg.rep<-rep(floor(n.plots/legend.rows),legend.rows)
                }else{
                    leg.rep<-rep(0,legend.rows)
                    for(i in 1:legend.rows){
                        leg.rep[i]<-floor(legend.height[i]*n.plots)
                    }
                }
                leg.leftover<-n.plots-sum(leg.rep)
		layout.mat<-matrix(1:n.plots,ncol=1)
		leg.counter<-n.plots
		for(i in 1:legend.cols){
			leg.column<-integer(0)
			if(is.element(el=legend.where,set=c("left_bottom","right_bottom")) & leg.leftover>0){leg.column<-c(rep(0,leg.leftover))}
			for(j in 1:legend.rows){
				leg.counter<-leg.counter+1
				leg.column<-c(leg.column,rep(leg.counter,leg.rep[j]))
			}
			if(is.element(el=legend.where,set=c("left_top","right_top")) & leg.leftover>0){leg.column<-c(leg.column,rep(0,leg.leftover))}
			if(legend.where=="right_bottom" | legend.where=="right_top"){layout.mat<-cbind(layout.mat,leg.column)}
			if(legend.where=="left_bottom" | legend.where=="left_top"){layout.mat<-cbind(leg.column,layout.mat)}
		}
		if(legend.where=="right_bottom" | legend.where=="right_top"){layout(layout.mat,width=c(1-sum(legend.width),legend.width),height=layout.height)}
		if(legend.where=="left_bottom" | legend.where=="left_top"){layout(layout.mat,width=c(legend.width,1-sum(legend.width)),height=layout.height)}
	}else if(legend.where=="bottom"){
		layout.mat<-rbind(matrix(rep(1:n.plots,legend.cols),byrow=F,nrow=n.plots),matrix(n.plots+1:(legend.cols*legend.rows),nrow=legend.rows,byrow=T))
		layout(layout.mat,width=legend.width,height=c((1-sum(legend.height))*layout.height/sum(layout.height),legend.height))
	}else if(legend.where=="top"){
		layout.mat<-rbind(matrix(n.plots+(1:(legend.cols*legend.rows)),nrow=legend.rows,byrow=T),matrix(rep(1:n.plots,legend.cols),byrow=F,nrow=n.plots))
		layout(layout.mat,width=legend.width,height=c(legend.height,(1-sum(legend.height))*layout.height/sum(layout.height)))
	}else if(legend.where=="separate"){
		stop("separate in-plot legends is not implemented yet. sorry.")
	}

# set the various margins
	if(n.int==0 & is.na(gerp.files[1]) & is.na(phast.files[1]) & is.na(dix.file) & is.na(gene.ylab) & is.na(annot.ylab[1]) & is.na(assoc.ylab) & is.na(hic.ylab[1]) & is.na(heat.ylab) & is.na(snap.ylab) & (sum(hic.style=="arches" | hic.style=="flatarches")==0 | sum(hic.arches.yaxis==TRUE)==0) & (heat.type=="pos-pos" | heat.general.yaxis==FALSE)){
		l.marg<-0.05
		r.marg<-0.05
	}else{
		l.marg<-0.75
		r.marg<-0.05
		if(!is.na(snap.ylab)){r.marg<-0.75}
	}
	
	if(ylab.las==1 | ylab.las==2){
		l.marg<-l.marg+1
		if((!is.na(snap.data.files[1]) && !is.na(snap.rate.files[1]))){ r.marg<-r.marg+1 }
	}
	
	if(!is.na(left.margin)){l.marg<-left.margin}
	if(!is.na(right.margin)){r.marg<-right.margin}
	
	if(axis.labels.slanted){
		b.marg<-0.9#1.15
	}else{
		b.marg<-0.5#0.8  
	}
	
	t.marg<-sep.margin
	
	#if(xlab.line>2){b.marg<-b.marg+(xlab.line-1)*0.25}
	
	if(!is.na(bottom.margin)){b.marg<-bottom.margin}
	if(!is.na(top.margin)){t.marg<-top.margin}
	
	mai.axis<-c(b.marg,l.marg,t.marg,r.marg)
	mai.alt<-c(sep.margin,l.marg,sep.margin,r.marg)
	if(!is.na(title.top.margin)){mai.title<-c(sep.margin,l.marg,title.top.margin,r.marg)}else{mai.title<-mai.alt}
	mai.buffer<-c(0,l.marg,0,r.marg)
	annot.mai.bot[is.na(annot.mai.bot)]<-mai.alt[1]
	annot.mai.top[is.na(annot.mai.top)]<-mai.alt[3]


#############################################
## ADD AXIS AND ZOOM LINES FOR ZOOMED PLOT ##
#############################################

# set the initial zoom status
	zoomed<-FALSE #This variable keeps track of whether the "change.scale" track has been plotted yet.

# iterate over the track order
	for(index in 1:length(track.order.top.down)){ # This for loop is used to do the plots in the order specified in the vector track.order.top.down

# keep track which track is being plotted (for debugging)		
		print(paste("index = ",index,sep=""))
		print(track.order.top.down[index])
		
# if a zoomed plot is requested, and all non-zoom tracks have been plotted, plot the change of scale track and update SP, EP and other parameters to match the zoomed region and -- crucially -- set the zoom status to 'zoomed'
		if(do.zoom==TRUE & track.order.top.down[index]=="change.scale"){

			# plot the change of scale track [including the axis for the non-zoomed plot]
			plot_with_breaks(mai=c(zoom.vertical.spacing,mai.axis[2],0,mai.axis[4]),x=c(SP),y=c(0),yaxt="n",type="n",xlim=c(SP,EP),ylim=c(0,0),gaps=gaps.list,axis.side.x=1,axis.side.y=2,lwd.axis=x.lwd,lwd.ticks.axis=x.lwd,line.axis=NA,side=1,gaps.sizes.x=gaps.sizes,gaps.sizes.offset.x=gaps.sizes.offset.x,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=F,plot.axes="x",range_labels_x=range_labels_x,range_labels_fact_x=1,xlab=xlab,cex.annot=xlab.cex,axis.labels.slanted=axis.labels.slanted,axis.labels.angle=axis.labels.angle,tcl.axis=axis.tcl,lend=axis.lend,tickmark_labels_x=ranges_x,x.axis.format=x.axis.format,x.labels.show.unit=x.labels.show.unit)
			
			tmp.usr <- par('usr') # info about the extent of the 'plot' region in user co-ordinates.
			tmp.with.gaps<-convert2gaps_alt(z=c(zoom.SP,zoom.EP,SP,EP),SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1)
			top <- tmp.usr[4]+(par('mai')[3]*par('cxy')[2]/par('cin')[2])
			bottom <- tmp.usr[3]-(par('mai')[1]*par('cxy')[2]/par('cin')[2])
			par(xpd=TRUE) # let the lines extend into the figure margins   
			
			tmp.col <- rgb(col2rgb(zoom.lines.col)['red',],col2rgb(zoom.lines.col)['green',],col2rgb(zoom.lines.col)['blue',],alpha=floor(zoom.lines.alpha*255),maxColorValue=255)
			lines(c(tmp.with.gaps[1],tmp.with.gaps[1]),c(tmp.usr[3],tmp.usr[4]/3),col=tmp.col,lwd=zoom.lines.lwd,lty=zoom.lines.lty)
			lines(c(tmp.with.gaps[2],tmp.with.gaps[2]),c(tmp.usr[3],tmp.usr[4]/3),col=tmp.col,lwd=zoom.lines.lwd,lty=zoom.lines.lty)
			lines(c(tmp.with.gaps[1],tmp.with.gaps[3]),c(tmp.usr[3],bottom),col=tmp.col,lwd=zoom.lines.lwd,lty=zoom.lines.lty)
			lines(c(tmp.with.gaps[2],tmp.with.gaps[4]),c(tmp.usr[3],bottom),col=tmp.col,lwd=zoom.lines.lwd,lty=zoom.lines.lty)

			par(xpd=FALSE)

			# add vertical lines only up to the non-zoomed axis; no further below it
			if(!is.na(lines.file)){
				par(xpd=TRUE)
					x_tmp<-convert2gaps_alt(z=lines.dat[,2],SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1)
					for(j in 1:nrow(lines.dat)){
						segments(x0=x_tmp[j],x1=x_tmp[j],y0=tmp.usr[3],y1=top,lwd=lines.lwd[j],lty=lines.lty[j],col=lines.col[j])
					}
					par(xpd=FALSE)
			}
			   
			# Update SP, EP and other parameters
			regions.dat<-matrix(c(zoom.chr,zoom.SP,zoom.EP),nrow=1)
			SP<-zoom.SP
			EP<-zoom.EP
			gaps<-NA
			n_gaps<-0
			gaps.list<-NULL
			gaps.sizes<-NULL
			gap_width<-0
			if(is.na(zoom.xlab) && zoom.axis.format=='Mb'){
				zoom.xlab<-paste("chr ",zoom.chr.local," position (Mb)",sep="")      
			}else if(is.na(zoom.xlab) && zoom.axis.format=='Kb'){
				zoom.xlab<-paste("chr ",zoom.chr.local," position (Kb)",sep="")
			}else if(is.na(zoom.xlab) && zoom.axis.format=='bp'){
				zoom.xlab<-paste("chr ",zoom.chr.local," position (bp)",sep="")
			}
		
			# update various track-specific objects
			if(!is.na(gene.file)){track.gene<-track.gene.zoom; ntracks.gene<-ntracks.gene.zoom}
			if(n.hic>0){ntracks.hic<-ntracks.hic.zoom}
			if(!is.na(heat.file)){prop.heat<-prop.heat.zoom}
			if(n.feat>0){track.feat<-track.feat.zoom; ntracks.feat<-ntracks.feat.zoom}

			# plot the zoomed axis:
			plot_with_breaks(mai=c(ifelse(zoom.x.labels.up==TRUE || zoom.xlab=='',0,mai.axis[1]),mai.axis[2],0,mai.axis[4]),x=c(zoom.SP.local),y=c(0),yaxt="n",type="n",xlim=c(zoom.SP.local,zoom.EP.local),ylim=c(0,0.1),gaps=gaps.list,axis.side.x=ifelse(zoom.x.labels.up==TRUE,3,1),axis.side.y=2,lwd.axis=zoom.x.lwd,lwd.ticks.axis=zoom.x.lwd,line.axis=NA,axis.pos.x=ifelse(zoom.x.labels.up==TRUE,NA,0.1),side=1,gaps.sizes.x=gaps.sizes,gaps.sizes.offset.x=gaps.sizes.offset.x,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=F,range_labels_x=range_labels_x,range_labels_fact_x=0.08,plot.axes="x",xlab=zoom.xlab,cex.annot=xlab.cex,axis.labels.slanted=axis.labels.slanted,axis.labels.angle=axis.labels.angle,tcl.axis=zoom.axis.tcl,lend=zoom.axis.lend,tickmark_labels_x=ranges_x,x.axis.format=zoom.axis.format,x.labels.show.unit=zoom.labels.show.unit)

 
			# add vertical lines to the zoomed axis; NB we first must convert the line positions back to chromosomal positions if they have been converted to genome-wide coordinates previously
			if(lines.zoom){
				if(!is.na(lines.file)){
					lines.local.dat<-lines.dat
					
					if(chrsizes.dat[1]==0){ # i.e. positions have been converted genome-wide
						for(tmp.row in 1:nrow(lines.dat)){
							for(tmp.i in length(chrsizes.dat):1){
								if(lines.dat[tmp.row,2]-chrsizes.dat[tmp.i]>=0){
									lines.local.dat[tmp.row,1]<-tmp.i
									lines.local.dat[tmp.row,2]<-lines.dat[tmp.row,2]-chrsizes.dat[tmp.i]
									break()
								}
							}
						}
					}
					
					# Get rid of the lines that aren't on the zoomed chromosome
					lines.local.dat<-lines.local.dat[lines.local.dat[,1]==zoom.chr.local,]
					
					x_tmp<-convert2gaps_alt(z=lines.local.dat[,2],SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1)
					for(j in 1:nrow(lines.local.dat)){
						par(xpd=TRUE)
						lines(c(x_tmp[j],x_tmp[j]),c(par('usr')[4],-10),lwd=lines.lwd[j],lty=lines.lty[j],col=lines.col[j])
						par(xpd=FALSE)
					}
				}
			}
			
			# set the zoomed status to 'zoomed'
			zoomed<-TRUE
		}
	
	
###############
## MAIN PLOT ##
###############


# plot the axis (if requested)
		if(!is.na(regions.file) && track.order.top.down[index]=='axis'){
			if(zoomed==TRUE){
				tmp.xlim<-c(zoom.SP.local,zoom.EP.local)
			}else{
				tmp.xlim<-c(SP,EP)
			}
			
			plot_with_breaks(mai=mai.axis[ifelse(rep(x=index,4)==rep(x=1,4),c(3,2,1,4),c(1,2,3,4))],x=c(SP),y=c(0),yaxt="n",type="n",xlim=tmp.xlim,ylim=c(0,0),gaps=gaps.list,axis.side.x=ifelse(index==1,3,1),axis.side.y=2,tcl.axis=axis.tcl,lend=axis.lend,lwd.axis=x.lwd,lwd.ticks.axis=x.lwd,xlab.line=xlab.line,line.axis=axis.line,line.axis.ticklabels=axis.tickmarks.line,line.axis.rangelabels=axis.range.line,side=1,gaps.sizes.x=gaps.sizes,gaps.sizes.offset.x=gaps.sizes.offset.x,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,plot.axes="x",range_labels_x=range_labels_x,range_labels_fact_x=2,xlab=xlab,cex.annot=xlab.cex,axis.labels.slanted=axis.labels.slanted,axis.labels.angle=axis.labels.angle,tickmark_labels_x=ranges_x,x.axis.format=x.axis.format,x.labels.show.unit=x.labels.show.unit)
			
			if(axis.addlines){
				add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
			}
		}  

# empty / buffer track
		if(track.order.top.down[index]=="buffer"){
			cat("...adding an empty / buffer track...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			plot_with_breaks(type="n",x=c(SP,EP),y=c(0,1),xlim=c(SP,EP),ylim=c(0,1),plot.axes="n",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=title.showgaps,plot.bars.outsideplot=T,mai=mai.buffer,ylab.las=ylab.las)
			add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
		}
 
# add title if requested
		if(!is.na(title) && track.order.top.down[index]=='title'){
			cat("...adding title...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			breaks.info<-plot_with_breaks(type="n",x=c(SP,EP),y=c(0,1),xlim=c(SP,EP),ylim=c(0,1),plot.axes="n",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=title.showgaps,plot.bars.outsideplot=T,mai=mai.title,ylab.las=ylab.las)
			usr.info<-par("usr")
			x_units<-(usr.info[2]-usr.info[1])/(width.plot-l.marg-r.marg)
			x_pos<-( (usr.info[1]-l.marg*x_units) + (usr.info[2]+r.marg*x_units) )/2
			par(xpd=TRUE)
			if(is.na(title.pos)){
				text(bquote(bold(.(title))),x=x_pos,y=0.5,cex=title.cex.txt,adj=c(0.5,0.5))
			}else{
				text_with_breaks(bquote(bold(.(title))),x=title.pos,y=0.5,cex=title.cex.txt,ranges_x=breaks.info$ranges_x,ranges_y=breaks.info$ranges_y,gap_width_x=breaks.info$gap_width_x,gap_width_y=breaks.info$gap_width_y,adj=c(0.5,0.5))
			}
			par(xpd=FALSE)
			
			add.letter(letter=title.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
		}  
  
# plot SNAP track (if requested)
		if(track.order.top.down[index]=='snap' && (!is.na(snap.data.files[1]) && !is.na(snap.rate.files[1]))){  
			for(i in 1:n.snap){ # n.snap
			
				if(!is.na(snap.snp2.file)){
					snp2 <- list(data=snap.snp2.dat,snp=snap.snp2.snp,'offset.x'=snap.snp2.offset.x,'offset.y'=snap.snp2.offset.y)
					names(snp2) <- c('data','snp','offset.x','offset.y')
				}else{
					snp2 <- NA
				}
				if(is.na(snap.snp)){snap.snp<-""}
				snap_track_with_breaks(snp=snap.snp,data=snap.dat[[i]], recomb=snap.rate.dat[[i]],snp2=snp2,mai=mai.alt,ylab=snap.ylab,cex.annot=snap.cex.txt,ylab.las=ylab.las,gaps.list=gaps.list,gaps.sizes=gaps.sizes,gaps.sizes.offset.x=gaps.sizes.offset.x,gap_width=gap_width,SP=SP,EP=EP,ylim=snap.ylim,snap.snp.offset.x=snap.snp.offset.x,snap.snp.offset.y=snap.snp.offset.y,snap.cex.axis=snap.cex.axis,snap.r2.legend.x.offset=snap.r2.legend.x.offset,snap.snp.label.cex=snap.snp.label.cex)
      			add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
      			add.letter(letter=snap.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)     
			}
		}
	
# plot gene track (if requested)
		if(track.order.top.down[index]=='gene' && !is.na(gene.file)){
			cat("...plotting gene track...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			
			if(is.na(gene.lwd)){gene.lwd<-0.25}
			
			if(chr1==chr2){
			gene_track_with_breaks(dat=gene.dat,chr=chr,SP=SP,EP=EP,compute_ntracks_only=FALSE,side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gaps.sizes.offset.x=gaps.sizes.offset.x,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,plot.axes="n",lwd.line=gene.lwd,mai=mai.alt,track.gene=track.gene,col.genes=gene.col,gene.ylab=gene.ylab,ylab.las=ylab.las,cex.txt=gene.cex.txt,cex.arrows=gene.cex.arrows,gene.igs=gene.igs,cex.annot=gene.ylab.cex.txt)      
			}else{
				gene_track_with_breaks(dat=gene.dat,chr=chr,SP=SP,EP=EP,compute_ntracks_only=FALSE,side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gaps.sizes.offset.x=gaps.sizes.offset.x,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,plot.axes="n",lwd.line=gene.lwd,mai=mai.alt,track.gene=track.gene,col.genes=gene.col,gene.ylab=gene.ylab,ylab.las=ylab.las,cex.txt=gene.cex.txt,cex.arrows=gene.cex.arrows,gene.igs=gene.igs,convert.gw=TRUE,convert.chrstarts=chrsizes.dat,cex.annot=gene.ylab.cex.txt) 
			}
			
      		add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
      		add.letter(letter=gene.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)   
		}
	
# plot GERP track (if requested)
		if(track.order.top.down[index]=='gerp' && !is.na(gerp.files[1])){
			cat("...plotting GERP score track...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			col.vect<-rep("black",nrow(gerp.dat))
			if(is.null(gerp.ylim)){
				gerp.ylim<-c(min(0,min(gerp.dat[,3])),max(gerp.dat[,3]))
			}
			if(!gerp.noblocks){
				int_track_with_breaks(int.dat=gerp.dat,chr=chr,SP=SP,EP=EP,col.vect=col.vect,plot.axes="y",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=c(gerp.mai.bot,mai.alt[2],gerp.mai.top,mai.alt[4]),ylab=gerp.ylab,lwd.line=gerp.lwd,int.ylim=gerp.ylim,ylab.las=ylab.las,cex.annot=gerp.cex.txt)
			}else{
				gerp.dat<-gerp.dat[gerp.dat[,1]==chr & gerp.dat[,2]>=SP & gerp.dat[,2]<=EP,]
				if(!is.na(gerp.filter.file)){
					gerp.filter.dat<-gerp.filter.dat[gerp.filter.dat[,1]==chr & gerp.filter.dat[,2]>=SP & gerp.filter.dat[,2]<=EP,]
					gerp.dat<-gerp.dat[is.element(el=gerp.dat[,2],set=gerp.filter.dat[,2]),]
				}
				if(nrow(gerp.dat)>0){
					breaks.info<-plot_with_breaks(type="h",x=gerp.dat[,2],y=gerp.dat[,3],xlim=c(SP,EP),col=col.vect,plot.axes="n",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=c(gerp.mai.bot,mai.alt[2],gerp.mai.top,mai.alt[4]),ylab=gerp.ylab,lwd=gerp.lwd,ylim=gerp.ylim,ylab.las=ylab.las,cex.annot=gerp.cex.txt)
					axis.points<-convert2gaps(z=gerp.ylim,ranges_z=breaks.info$ranges_y,gap_width_z=breaks.info$gap_width_y)
					axis(side=2,at=axis.points,labels=gerp.ylim,cex.axis=1)
				}else{
					plot(0,0,type="n",axes=F,xlab="",ylab="",main="")
				}
			}
			
      		add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
      		add.letter(letter=gerp.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)   
		}
	
# plot phastCons track (if requested) 
		if(track.order.top.down[index]=='phast' && !is.na(phast.files[1])){
			cat("...plotting phastCons score track...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			
			col.vect<-rep("black",nrow(phast.dat))
			if(is.null(phast.ylim)){
				phast.ylim<-c(min(0,min(phast.dat[,3])),max(phast.dat[,3]))
			}
			if(!phast.noblocks){
				int_track_with_breaks(int.dat=phast.dat,chr=chr,SP=SP,EP=EP,col.vect=col.vect,plot.axes="y",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=mai.alt,ylab=phast.ylab,lwd.line=phast.lwd,int.ylim=phast.ylim,ylab.las=ylab.las,cex.annot=phast.cex.txt,)
			}else{
				phast.dat<-phast.dat[phast.dat[,1]==chr & phast.dat[,2]>=SP & phast.dat[,2]<=EP,]
				if(!is.na(phast.filter.file)){
					phast.filter.dat<-phast.filter.dat[phast.filter.dat[,1]==chr & phast.filter.dat[,2]>=SP & phast.filter.dat[,2]<=EP,]
					phast.dat<-phast.dat[is.element(el=phast.dat[,2],set=phast.filter.dat[,2]),]
				}
      			if(nrow(phast.dat)>0){
					breaks.info<-plot_with_breaks(type="h",x=phast.dat[,2],y=phast.dat[,3],xlim=c(SP,EP),col=col.vect,plot.axes="n",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=mai.alt,ylab=phast.ylab,lwd=phast.lwd,ylim=phast.ylim,ylab.las=ylab.las,cex.annot=phast.cex.txt)
					axis.points<-convert2gaps(z=phast.ylim,ranges_z=breaks.info$ranges_y,gap_width_z=breaks.info$gap_width_y)
					axis(side=2,at=axis.points,labels=phast.ylim,cex.axis=1)
				}else{
					plot(0,0,type="n",axes=F,xlab="",ylab="",main="")
				}
			}
			
      		add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
      		add.letter(letter=phast.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)     
		}
	
# plot multi-stage annotation track (if requested)
		if(length(grep(x=track.order.top.down[index],pattern="^annot",perl=T))>0 && n.annot>0){
			for(i in 1:n.annot){
				if(track.order.top.down[index]=="annot" | track.order.top.down[index]==paste("annot",i,sep="")){
					cat(paste("...plotting annotation track number ",i,"...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
					
					mai.annot<-c(annot.mai.bot[i],l.marg,annot.mai.top[i],r.marg)
					annot.classcol<-paste(annot.dat[[i]][,4],annot.dat[[i]][,9],sep="::")
					annot.classcol<-unique(annot.classcol)
					annot.classcol<-matrix(unlist(strsplit(annot.classcol,split="::")),ncol=2,byrow=T)
					annot.class<-annot.classcol[,1]
					annot.classcol<-matrix(as.integer(unlist(strsplit(annot.classcol[,2],split=","))),ncol=3,byrow=T)
					annot.col<-rgb(red=annot.classcol[,1],green=annot.classcol[,2],blue=annot.classcol[,3],maxColorValue=255)
					rm(annot.classcol)
					if(annot.mai.bot[i]==0 | annot.mai.top[i]==0){par(yaxs="i")}
					if(is.null(annot.bgclass[i])){annot.bgclass.tmp<-NULL}else{if(is.na(annot.bgclass[i])){annot.bgclass.tmp<-NULL}else{annot.bgclass.tmp<-unlist(strsplit(annot.bgclass[i],split=":"))}}
					annotation_track_with_breaks(annot.dat=annot.dat[[i]],annot.classes=annot.class,annot.col=annot.col,chr=chr,SP=SP,EP=EP,bg.col="white",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=mai.annot,annot.ylab=annot.ylab[i],annot.lwd=annot.lwd[i],annot.bgclass=annot.bgclass.tmp,ylab.las=ylab.las,cex.annot=annot.cex.txt[i])
     				
					add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
					if(length(annot.letter)>1){
      					add.letter(letter=annot.letter[i],width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
      				}else if(length(annot.letter)==1 && i==1){
      					add.letter(letter=annot.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
      				}
				}
			}
		}
	
# plot interaction track (if requested)
		if(length(grep(x=track.order.top.down[index],pattern="^hic",perl=T))>0 && n.hic>0){
			for(j in 1:n.hic){
				if(track.order.top.down[index]=="hic" | track.order.top.down[index]==paste("hic",j,sep="")){
					cat(paste("...plotting the ",j,"th interaction track...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
					
					if(hic.style[j]=="bins"){
						interaction_track_with_breaks(hic.dat=hic.dat[[j]],chr=chr,SP=SP,EP=EP,compute_ntracks_only=FALSE,side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,plot.axes="n",col.nonref=hic.colnonref[j],col.ref=hic.colref[j],mai=mai.alt,hic.ylab=hic.ylab[j],ylab.las=ylab.las,cex.annot=hic.cex.txt[j],hic.capt.SP=hic.capt.region[j,2],hic.capt.EP=hic.capt.region[j,3],hic.capt.col=hic.capt.col[j],hic.capt.density=hic.capt.density[j],hic.capt.lines.lwd=hic.capt.lines.lwd,hic.plotThroughGaps=hic.plotThroughGaps,col.throughGaps=hic.colThroughGaps)  
					}else if(hic.style[j]=="stacked"){
						hic.dat[[j]]<-hic.dat[[j]][hic.dat[[j]][,4]==1,] # need to only keep the non-reference bins when stacking (want to count how many times a bin is interacted with)
						hic.tmp<-paste(hic.dat[[j]][,1],hic.dat[[j]][,2],hic.dat[[j]][,3],sep="_")
						hic.pos<-names(table(hic.tmp))
						hic.freq<-as.integer(table(hic.tmp))
						hic.tmp<-cbind(matrix(as.numeric(unlist(strsplit(hic.pos,split="_"))),ncol=3,byrow=T),hic.freq)
						hic.tmp<-hic.tmp[order(hic.tmp[,1],hic.tmp[,2],hic.tmp[,3]),]
						
						# here we are adding zeroes between intensity blocks; otherwise it would look weird if hic.stack.style[j]=="lineplot"
						if(nrow(hic.tmp)>1){
							hic.tmp2<-numeric(0)
							for(i in 2:nrow(hic.tmp)){
								if(hic.tmp[i-1,1]==hic.tmp[i,1] & abs(hic.tmp[i-1,3]-hic.tmp[i,2])>1){
									hic.tmp2<-rbind(hic.tmp2,c(hic.tmp[i-1,c(1,3,3)],0))
									hic.tmp2<-rbind(hic.tmp2,c(hic.tmp[i,c(1,2,2)],0))
								}
							}
							hic.tmp<-rbind(hic.tmp,hic.tmp2)
							hic.tmp<-hic.tmp[order(hic.tmp[,1],hic.tmp[,2],hic.tmp[,3]),]
							rm(hic.tmp2)
						}
							
						if(!is.null(hic.stack.ylim.top[j])){if(is.null(hic.stack.ylim.bot[j]) & !is.na(hic.stack.ylim.top[j])){hic.stack.ylim.bot[j]<-min(hic.tmp[,4])}else if(is.na(hic.stack.ylim.bot[j]) & !is.na(hic.stack.ylim.top[j])){hic.stack.ylim.bot[j]<-min(hic.tmp[,4])}}
						if(!is.null(hic.stack.ylim.bot[j])){if(is.null(hic.stack.ylim.top[j]) & !is.na(hic.stack.ylim.bot[j])){hic.stack.ylim.top[j]<-max(hic.tmp[,4])}else if(is.na(hic.stack.ylim.top[j]) & !is.na(hic.stack.ylim.bot[j])){hic.stack.ylim.top[j]<-max(hic.tmp[,4])}}
							int_track_with_breaks(int.dat=hic.tmp,chr=chr,SP=SP,EP=EP,col.vect=rep(hic.stack.col,nrow(hic.tmp)),plot.axes="y",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=mai.alt,ylab=hic.ylab[j],lwd.line=hic.stack.lwd,int.ylim=c(hic.stack.ylim.bot[j],hic.stack.ylim.top[j]),int.style=hic.stack.style[j],ylab.las=ylab.las,cex.annot=hic.cex.txt[j],int.refbox=matrix(nrow=1,hic.capt.region[j,2:3]),int.refbox.col=hic.capt.col[j],int.refbox.density=hic.capt.density[j],int.refbox.lines.lwd=hic.capt.lines.lwd)
					}else if(hic.style[j]=="arches"){
						if(!is.na(hic.arches.ylim.upper[j]) & !is.na(hic.arches.ylim.lower[j])){hic.arches.ylim<-c(hic.arches.ylim.lower[j],hic.arches.ylim.upper[j])}else{hic.arches.ylim<-NULL}
						if(hic.arches.yaxis[j]){axes.tmp<-"y"}else{axes.tmp<-"n"}
						interaction_track_with_breaks(hic.dat=hic.dat[[j]],chr=chr,SP=SP,EP=EP,compute_ntracks_only=FALSE,side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,plot.axes=axes.tmp,mai=mai.alt,hic.ylab=hic.ylab[j],ylab.las=ylab.las,cex.annot=hic.cex.txt[j],hic.capt.SP=hic.capt.region[j,2],hic.capt.EP=hic.capt.region[j,3],hic.capt.col=hic.capt.col[j],hic.capt.density=hic.capt.density[j],arches=TRUE,arches.type="round",arches.twist=hic.arches.twist[j],arches.lwd=hic.arches.lwd[j],arches.nsegments=hic.arches.nsegments[j],arches.neglog10=hic.arches.neglog10[j],arches.ylim=hic.arches.ylim,arches.col=hic.arches.col[j],arches.varicol=hic.arches.varicol[j],arches.dir=hic.arches.dir[j],relplotheight=hic.relplotheight[j],hic.capt.lines.lwd=hic.capt.lines.lwd,hic.plotThroughGaps=hic.plotThroughGaps,col.throughGaps=hic.colThroughGaps,hic.plotThroughGapsMaxY=hic.plotThroughGapsMaxY)
					}else if(hic.style[j]=="flatarches"){
						if(!is.na(hic.arches.ylim.upper[j]) & !is.na(hic.arches.ylim.lower[j])){hic.arches.ylim<-c(hic.arches.ylim.lower[j],hic.arches.ylim.upper[j])}else{hic.arches.ylim<-NULL}
						if(hic.arches.yaxis[j]){axes.tmp<-"y"}else{axes.tmp<-"n"}
						interaction_track_with_breaks(hic.dat=hic.dat[[j]],chr=chr,SP=SP,EP=EP,compute_ntracks_only=FALSE,side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,plot.axes=axes.tmp,mai=mai.alt,hic.ylab=hic.ylab[j],ylab.las=ylab.las,cex.annot=hic.cex.txt[j],hic.capt.SP=hic.capt.region[j,2],hic.capt.EP=hic.capt.region[j,3],hic.capt.col=hic.capt.col[j],hic.capt.density=hic.capt.density[j],arches=TRUE,arches.type="flat",arches.twist=hic.arches.twist[j],arches.lwd=hic.arches.lwd[j],arches.nsegments=hic.arches.nsegments[j],arches.neglog10=hic.arches.neglog10[j],arches.ylim=hic.arches.ylim,arches.col=hic.arches.col[j],arches.varicol=hic.arches.varicol[j],arches.dir=hic.arches.dir[j],relplotheight=hic.relplotheight[j],hic.capt.lines.lwd=hic.capt.lines.lwd,hic.plotThroughGaps=hic.plotThroughGaps,col.throughGaps=hic.colThroughGaps)
					}
					
      				add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom) 
      				if(length(hic.letter)>1){
      					add.letter(letter=hic.letter[j],width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
      				}else if(length(hic.letter)==1 && j==1){
      					add.letter(letter=hic.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
      				}
				}
			}
		}	
		
# plot intensity track(s) (if requested)
		if(length(grep(x=track.order.top.down[index],pattern="^int",perl=T))>0 && n.int>0){
			for(i in 1:n.int){
				if(track.order.top.down[index]=="int" | track.order.top.down[index]==paste("int",i,sep="")){
					cat(paste("...plotting intensity track number ",i,"...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
					if(!is.na(int.refbox[1])){
						refbox<-matrix(ncol=2,int.ref.list[[i]][,2:3])
					}else{
						refbox<-NA
					}
					if(!is.na(int.ref2box[1])){
						ref2box<-matrix(ncol=2,int.ref2.list[[i]][,2:3])
					}else{
						ref2box<-NA
					}
					col.vect<-rep("black",nrow(int.dat[[i]]))
					if(!is.na(int.ylim.upper[i]) & !is.na(int.ylim.lower[i])){int.ylim<-c(int.ylim.lower[i],int.ylim.upper[i])}else{int.ylim<-NULL}
					int_track_with_breaks(int.dat=int.dat[[i]],chr=chr,SP=SP,EP=EP,col.vect=col.vect,plot.axes="y",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=mai.alt,ylab=int.ylab[i],lwd.line=int.lwd[i],int.ylim=int.ylim,int.style=int.style[i],ylab.las=ylab.las,int.refbox=refbox,int.refbox.col=int.refbox.col,int.refbox.density=int.refbox.density,int.refbox.lines.lwd=int.refbox.lines.lwd,int.ref2box=ref2box,int.ref2box.col=int.ref2box.col,int.ref2box.density=int.ref2box.density,int.ref2box.lines.lwd=int.ref2box.lines.lwd,cex.annot=int.cex.txt[i],smooth.window=int.smooth.window,smooth.method=int.smooth.method)
					
      				add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
      				if(length(int.letter)>1){
      					add.letter(letter=int.letter[i],width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)  
      				}else if(length(int.letter)==1 & i==1){
      					add.letter(letter=int.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex) 
      				}
      			}
			}
		}
	
# plot association track (if requested)
		if(track.order.top.down[index]=='assoc' && !is.na(assoc.file)){
			cat("...plotting association track...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			
			association_track_with_breaks(assoc.dat=assoc.dat,chr=chr,SP=SP,EP=EP,side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,plot.axes="n",lwd.bars=assoc.lwd.bars,col.vect=colorRampPalette(assoc.cols)(256),pval.scale=assoc.pvalscale,mai=mai.alt,assoc.ylab=assoc.ylab,ylab.las=ylab.las,cex.annot=assoc.cex.txt)
			
      		add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
      		add.letter(letter=assoc.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)   
		}
	
# plot dix track (if requested)
		if(length(grep(x=track.order.top.down[index],pattern="^dix",perl=T))>0 && !is.na(dix.file)){
			for(i in 1:n.dix){
				if(track.order.top.down[index]=="dix" | track.order.top.down[index]==paste("dix",i,sep="")){
					cat(paste("...plotting directionality index track number ",i,"...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
					dix.dat.tmp<-dix.dat[,c(1:3,dix.cols.toplot[i])]
					col.dix<-rep(NA,nrow(dix.dat.tmp))
					col.dix[dix.dat.tmp[,4]>0]<-dix.col.pos[i]
					col.dix[dix.dat.tmp[,4]<0]<-dix.col.neg[i]
					if(!is.na(dix.ylim.lower[i]) & !is.na(dix.ylim.upper[i])){dix.ylim<-c(dix.ylim.lower[i],dix.ylim.upper[i])}else{dix.ylim<-NULL}
					int_track_with_breaks(int.dat=dix.dat.tmp,chr=chr,SP=SP,EP=EP,col.vect=col.dix,plot.axes="y",side=1,gap_width=gap_width,gaps=gaps.list,gaps.sizes.x=gaps.sizes,plot.bars=T,plot.bars.outsideplot=T,mai=mai.alt,lwd.line=1e-5,int.cex.axis=1,ylab=dix.ylab[i],ylab.las=ylab.las,cex.annot=dix.cex.txt[i],int.ylim=dix.ylim)
					
      				add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
      				if(length(dix.letter)>1){
      					add.letter(letter=dix.letter[i],width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
      				}else if(length(dix.letter)==1 & i==1){
						add.letter(letter=dix.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
      				}
      			}
			}
		}
		
# plot feature track(s) (if requested)
		if(length(grep(x=track.order.top.down[index],pattern="^feat",perl=T))>0 && n.feat>0){
			for(i in 1:n.feat){
				if(track.order.top.down[index]=="feat" | track.order.top.down[index]==paste("feat",i,sep="")){
					cat(paste("...plotting feature track number ",i,"...\n",sep=""),file=paste(outprefix,"_R.log",sep=""),append=T)
 				feat_track_with_breaks(dat=feat.dat[[i]],chr=chr,SP=SP,EP=EP,compute_ntracks_only=FALSE,gap_width=gap_width,gaps=gaps.list,gaps.sizes.x=gaps.sizes,plot.bars=T,plot.bars.outsideplot=T,feat.height=feat.height,feat.cex.txt=feat.cex.txt[i],feat.txt.hori.offset=feat.txt.hori.offset[i],feat.txt.vert.offset=feat.txt.vert.offset[i],cex.annot=feat.cex.txt[i],feat.textpos=feat.textpos[i],feat.lwd=feat.lwd[i],feat.ylab=feat.ylab[i],feat.col=feat.col[i],mai=mai.alt,track.feat=track.feat[[i]],ylab.las=ylab.las,side=1)
						
      					add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
      					if(length(feat.letter)>1){
      						add.letter(letter=feat.letter[i],width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
      					}else if(length(feat.letter)==1 & i==1){
							add.letter(letter=feat.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)
      					}
      			}
			}
		}
		
# plot heat map track (if requested)
		if(track.order.top.down[index]=='heat' && !is.na(heat.file)){
			cat("...plotting heat map...\n",file=paste(outprefix,"_R.log",sep=""),append=T)
			#heat.dat<-heat.dat[heat.dat[,5]!=0,]
			if(is.null(heat.cols)){
				heat.colscale<-NULL
			}else{
				heat.colscale<-colorRampPalette(heat.cols)(256)
			}
			if(heat.type!="general" | !heat.general.yaxis){
				heat.zlim.effective<-heat_track_with_breaks(heat.dat=heat.dat,chr=chr,SP=SP,EP=EP,heatSP=heat.SP,heatEP=heat.EP,bw=heat.bw,plot.axes="n",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=mai.alt,lwd.line=1e-6,col.scale=heat.colscale,ylim=heat.ylim,zlim=heat.zlim,heat.ylab=heat.ylab,ylab.las=ylab.las,cex.annot=heat.cex.txt,chop.data.gaps=heat.chop.data.gaps,logscale=heat.logscale,heat.dir=heat.dir,heat.type=heat.type)
			}else{
				heat.zlim.effective<-heat_track_with_breaks(heat.dat=heat.dat,chr=chr,SP=SP,EP=EP,heatSP=heat.SP,heatEP=heat.EP,bw=heat.bw,plot.axes="y",side=1,gaps=gaps.list,gaps.sizes.x=gaps.sizes,gap_width=gap_width,plot.bars=T,plot.bars.outsideplot=T,mai=mai.alt,lwd.line=1e-6,col.scale=heat.colscale,ylim=heat.ylim,zlim=heat.zlim,heat.ylab=heat.ylab,ylab.las=ylab.las,cex.annot=heat.cex.txt,chop.data.gaps=heat.chop.data.gaps,logscale=heat.logscale,heat.dir=heat.dir,heat.type=heat.type)
			}
			
			add.lines(filename=lines.file,dat=lines.dat,SP=SP,EP=EP,gaps=gaps.list,gap_width=gap_width,side=1,lines.lwd=lines.lwd,lines.lty=lines.lty,lines.col=lines.col,zoom.status=zoomed,nonzoom.draw=lines.nonzoom,zoom.draw=lines.zoom)
			add.letter(letter=heat.letter,width.plot=width.plot,l.marg=l.marg,r.marg=r.marg,letter.cex=letter.cex)		
		} 
		
# stop iterating over the track order
}

# add legend(s) if specified
	if(legend.plot){
		leg.counter<-0
		for(leg in legend.order){
            
			# annotation track legend
			if(legend.annot & length(grep(leg,pattern="^annot",perl=T))>0){
				if(length(legend.annot.xlab)!=n.legend.annot){legend.annot.xlab<-rep(legend.annot.xlab[1],n.legend.annot)}
				if(length(legend.annot.nrow)!=n.legend.annot){legend.annot.nrow<-rep(legend.annot.nrow[1],n.legend.annot)}
				if(length(legend.annot.axline)!=n.legend.annot){legend.annot.axline<-rep(legend.annot.axline[1],n.legend.annot)}
				legend.annot.xlab[legend.annot.xlab=="NA"]<-NA
				if(length(legend.annot.xspace)!=n.legend.annot & n.legend.annot>1){legend.annot.xspace<-rep(legend.annot.xspace[1],n.legend.annot)}
				legend.annot.xspace.orig<-legend.annot.xspace
				for(k in 1:n.legend.annot){
					if(leg==paste("annot",k,sep="") | leg=="annot"){
						leg.counter<-leg.counter+1
						if(n.legend.annot>1 | (n.legend.annot==1 & length(grep(legend.annot.xspace.orig[1],pattern=";"))>0)){legend.annot.xspace<-as.numeric(unlist(strsplit(legend.annot.xspace.orig[k],split=";")))}else{legend.annot.xspace<-as.numeric(legend.annot.xspace)}
						par(mai=legend.mai,mgp=c(legend.annot.axline[k],1,0))
						n.states<-nrow(legend.annot.dat[[k]])
						if(is.na(legend.annot.nrow[k])){
							if(is.element(el=legend.where,set=c("left_top","left_bottom","right_top","right_bottom"))){legend.annot.nrow[k]<-n.states}else{legend.annot.nrow[k]<-1}
						}
					
						legend.annot.ncol<-ceiling(n.states/legend.annot.nrow[k])
						if(length(legend.annot.xspace)==1 & legend.annot.ncol>1){legend.annot.xspace<-rep(legend.annot.xspace,legend.annot.ncol)}
						legend.annot.xspace<-c(0,legend.annot.xspace)
						plot(type="n",xlim=c(0,sum(legend.annot.xspace)),ylim=c(0.5,legend.annot.nrow[k]),asp=1,axes=FALSE,xlab=legend.annot.xlab[k],ylab="",main="",x=0,y=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
						ii<-1
						jj<-legend.annot.nrow[k]
						for(i in 1:nrow(legend.annot.dat[[k]])){
							if(legend.annot.rgb){col<-as.integer(unlist(strsplit(legend.annot.dat[[k]][i,2],split=",")))}else{col<-as.vector(col2rgb(legend.annot.dat[[k]][i,2]))}
							polygon(x=c(sum(legend.annot.xspace[1:ii]),sum(legend.annot.xspace[1:ii]),sum(legend.annot.xspace[1:ii])+0.5,sum(legend.annot.xspace[1:ii])+0.5),y=c(jj-0.5,jj,jj,jj-0.5),col=rgb(red=col[1],green=col[2],blue=col[3],maxColorValue=255),border="black",lwd=0.5)
							text(labels=legend.annot.dat[[k]][i,1],x=sum(legend.annot.xspace[1:ii])+0.65,y=jj-0.25,pos=4,cex=legend.cex.txt[leg.counter])
							ii<-ii+1
							if(ii>legend.annot.ncol){
								ii<-1
								jj<-jj-1
							}
						}
						par(mgp=c(3,1,0))
					}
					legend.annot.xspace<-legend.annot.xspace.orig
				}
			}

			# interaction track legend
			if(legend.hic & length(grep(leg,pattern="^hic",perl=T))>0){
				leg.hic.cols<-unique(paste(hic.colref,hic.colnonref,sep=":"))
				leg.hic.colref<-matrix(byrow=T,nrow=n.legend.hic,unlist(strsplit(leg.hic.cols,split=":")))[,1]
				leg.hic.colnonref<-matrix(byrow=T,nrow=n.legend.hic,unlist(strsplit(leg.hic.cols,split=":")))[,2]
				for(k in 1:n.legend.hic){
					if(leg==paste("hic",k,sep="") | leg=="hic"){
						leg.counter<-leg.counter+1
						if(!legend.hic.hori){
							par(mai=legend.mai,mgp=c(legend.hic.axline[k],1,0),xpd=TRUE)
							if(legend.hic.capt & sum(is.na(hic.capt.region))==0){legend.hic.ylim<-c(-2.5,5)}else{legend.hic.ylim<-c(0,5)}
							plot(type="n",xlim=c(0,0.65+legend.hic.xlim[k]),ylim=legend.hic.ylim,axes=FALSE,xlab=legend.hic.xlab[k],ylab="",main="",x=0,y=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
							polygon(x=c(0,0,0.5,0.5),y=c(4,5,5,4),col=leg.hic.colref[k],border=leg.hic.colref[k])
							polygon(x=c(0,0,0.5,0.5),y=c(2,3,3,2),col=leg.hic.colnonref[k],border=leg.hic.colnonref[k])
							segments(x0=0,x1=0.5,y0=0.5,y1=0.5,col=leg.hic.colnonref[k])
							text(labels=c(legend.hic.labref[k],legend.hic.labnonref[k],legend.hic.lablink[k]),x=0.65,y=c(4.5,2.5,0.5),pos=4,cex=legend.cex.txt[leg.counter])
							if(legend.hic.capt & sum(is.na(hic.capt.region))==0){
								segments(x0=0,x1=0,y0=(-2.5),y1=(-1.5),col=hic.capt.col)
								segments(x0=0.5,x1=0.5,y0=(-2.5),y1=(-1.5),col=hic.capt.col)
								delta.ylim<-1
								for(i in 1:hic.capt.density){
									lines(x=c(0,0.5),y=c(-2.5+(i-1)*delta.ylim/hic.capt.density,-2.5+i*delta.ylim/hic.capt.density),col=hic.capt.col)
								}
								text(labels=legend.hic.labcapt[k],x=0.65,y=(-2),pos=4,cex=legend.cex.txt[leg.counter])
							}
							par(mgp=c(3,1,0),xpd=FALSE)
						}else{
							par(mai=legend.mai,mgp=c(legend.hic.axline[k],1,0),xpd=TRUE)
							if(legend.hic.capt & sum(is.na(hic.capt.region))==0){legend.hic.xlim.tmp<-c(0,4*(0.65+legend.hic.xlim[k]))}else{legend.hic.xlim.tmp<-c(0,3*(0.65+legend.hic.xlim[k]))}
							plot(type="n",xlim=legend.hic.xlim.tmp,ylim=c(0,2),axes=FALSE,xlab=legend.hic.xlab[k],ylab="",main="",x=0,y=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
							polygon(x=c(0,0,0.5,0.5),y=c(0.5,1.5,1.5,0.5),col=leg.hic.colref[k],border=leg.hic.colref[k])
							polygon(x=c(0.65+legend.hic.xlim[k],0.65+legend.hic.xlim[k],1.15+legend.hic.xlim[k],1.15+legend.hic.xlim[k]),y=c(0.5,1.5,1.5,0.5),col=leg.hic.colnonref[k],border=leg.hic.colnonref[k])
							segments(x0=2*(0.65+legend.hic.xlim[k]),x1=2*(0.65+legend.hic.xlim[k])+0.5,y0=1,y1=1,col=leg.hic.colnonref[k])
							text(labels=c(legend.hic.labref[k],legend.hic.labnonref[k],legend.hic.lablink[k]),x=c(0.65,0.65+(0.65+legend.hic.xlim[k]),0.65+2*(0.65+legend.hic.xlim[k])),y=c(1,1,1),pos=4,cex=legend.cex.txt[leg.counter])
							if(legend.hic.capt & sum(is.na(hic.capt.region))==0){
								segments(x0=3*(0.65+legend.hic.xlim[k]),x1=3*(0.65+legend.hic.xlim[k]),y0=0.5,y1=1.5,col=hic.capt.col)
								segments(x0=3*(0.65+legend.hic.xlim[k])+0.5,x1=3*(0.65+legend.hic.xlim[k])+0.5,y0=0.5,y1=1.5,col=hic.capt.col)
								delta.ylim<-1
								for(i in 1:hic.capt.density){
									lines(x=c(3*(0.65+legend.hic.xlim[k]),3*(0.65+legend.hic.xlim[k])+0.5),y=c(0.5+(i-1)*delta.ylim/hic.capt.density,0.5+i*delta.ylim/hic.capt.density),col=hic.capt.col)
								}
								text(labels=legend.hic.labcapt[k],x=0.65+3*(0.65+legend.hic.xlim[k]),y=1,pos=4,cex=legend.cex.txt[leg.counter])
							}
							par(mgp=c(3,1,0),xpd=FALSE)
						}
					}
				}
			}	

			# heat map legend
			if(legend.heat & leg=="heat"){
				leg.counter<-leg.counter+1
				ncol<-256
				#if(is.null(heat.zlim)){heat.zlim<-heat.zlim.effective}
				heat.zlim<-sort(heat.zlim.effective)
				#heat.zlim<-sort(heat.zlim)
				heat.cols<-colorRampPalette(heat.cols)(ncol)
				legend.heat.bw<-(heat.zlim[2]-heat.zlim[1])/ncol
				if(!legend.heat.hori){
					par(mai=legend.mai,mgp=c(legend.heat.axline,1,0),xpd=TRUE)
					plot(type="n",xlim=c(0,2.15),ylim=heat.zlim,axes=FALSE,xlab=legend.heat.xlab,ylab="",main="",x=0,y=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
					poly.x<-numeric(0)
					poly.y<-numeric(0)
					for(i in 1:ncol){
						poly.x<-c(poly.x,c(0,0,1,1,NA))
						poly.y<-c(poly.y,c((i-1)*legend.heat.bw,i*legend.heat.bw,i*legend.heat.bw,(i-1)*legend.heat.bw,NA)+heat.zlim[1])
					}
					polygon(x=poly.x,y=poly.y,border=heat.cols,col=heat.cols)
					polygon(x=c(0,0,1,1),y=heat.zlim[c(1,2,2,1)],border="black",col=NA)
					lab.ticks<-c(heat.zlim[1],heat.zlim[1]+(heat.zlim[2]-heat.zlim[1])/3,heat.zlim[1]+2*(heat.zlim[2]-heat.zlim[1])/3,heat.zlim[2])
					if(!heat.logscale){
						text(labels=round(lab.ticks,2),x=1.15,y=lab.ticks,pos=4,cex=legend.cex.txt[leg.counter])
					}else{
						text(labels=round(exp(lab.ticks),2),x=1.15,y=lab.ticks,pos=4,cex=legend.cex.txt[leg.counter])
					}
					par(mgp=c(3,1,0),xpd=FALSE)
					segments(x0=rep(1,length(lab.ticks)),x1=rep(1.15,length(lab.ticks)),y0=lab.ticks,y1=lab.ticks)
				}else{
					par(mai=legend.mai,mgp=c(legend.heat.axline,1,0),xpd=TRUE)
					plot(type="n",ylim=c(-1,1),xlim=heat.zlim,axes=FALSE,xlab=legend.heat.xlab,ylab="",main="",y=0,x=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
					poly.x<-numeric(0)
					poly.y<-numeric(0)
					for(i in 1:ncol){
						poly.y<-c(poly.y,c(0,1,1,0,NA))
						poly.x<-c(poly.x,c((i-1)*legend.heat.bw,(i-1)*legend.heat.bw,i*legend.heat.bw,i*legend.heat.bw,NA)+heat.zlim[1])
					}
					polygon(x=poly.x,y=poly.y,border=heat.cols,col=heat.cols)
					polygon(y=c(0,1,1,0),x=heat.zlim[c(1,1,2,2)],border="black",col=NA)
					lab.ticks<-c(heat.zlim[1],heat.zlim[1]+(heat.zlim[2]-heat.zlim[1])/3,heat.zlim[1]+2*(heat.zlim[2]-heat.zlim[1])/3,heat.zlim[2])
					if(!heat.logscale){
						text(labels=round(lab.ticks,2),y=-0.15,x=lab.ticks,pos=1,cex=legend.cex.txt[leg.counter])
					}else{
						text(labels=round(exp(lab.ticks),2),y=-0.15,x=lab.ticks,pos=1,cex=legend.cex.txt[leg.counter])
					}
					par(mgp=c(3,1,0),xpd=FALSE)
					segments(y0=rep(0,length(lab.ticks)),y1=rep(-0.15,length(lab.ticks)),x0=lab.ticks,x1=lab.ticks)
				}
			}

			# association track legend
			if(legend.assoc & leg=="assoc"){
				leg.counter<-leg.counter+1
				ncol<-256
				if(is.null(assoc.pvalscale)){assoc.pvalscale<-c(min(assoc.dat[assoc.dat[,3]>0,3]),max(assoc.dat[,3]))}
				assoc.pvalscale<-sort(-log10(assoc.pvalscale))
				assoc.cols<-colorRampPalette(assoc.cols)(ncol)
				legend.assoc.bw<-(assoc.pvalscale[2]-assoc.pvalscale[1])/ncol
				par(mai=legend.mai)
				if(!legend.assoc.hori){
					par(mgp=c(legend.assoc.axline,1,0), xpd=TRUE)
					plot(type="n",xlim=c(0,2.15),ylim=assoc.pvalscale,axes=FALSE,xlab=legend.assoc.xlab,ylab="",main="",x=0,y=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
					poly.x<-numeric(0)
					poly.y<-numeric(0)
					for(i in 1:ncol){
						poly.x<-c(poly.x,c(0,0,1,1,NA))
						poly.y<-c(poly.y,c((i-1)*legend.assoc.bw,i*legend.assoc.bw,i*legend.assoc.bw,(i-1)*legend.assoc.bw,NA)+assoc.pvalscale[1])
					}
					polygon(x=poly.x,y=poly.y,border=assoc.cols,col=assoc.cols)
					polygon(x=c(0,0,1,1),y=assoc.pvalscale[c(1,2,2,1)],border="black",col=NA)
					lab.ticks<-setdiff(unique(trunc(seq(assoc.pvalscale[1],assoc.pvalscale[2],length=ncol))),0)
					lab.ticks<-lab.ticks[(lab.ticks %% legend.assoc.pvalticksround) == 0]
					text(labels=paste("1e-",round(lab.ticks,digits=2),sep=""),x=1.15,y=lab.ticks,pos=4,cex=legend.cex.txt[leg.counter])
					text(labels=1,x=1.15,y=0,pos=4,cex=legend.cex.txt[leg.counter])
					if(assoc.pvalscale[2]!=round(assoc.pvalscale[2])){
						label.tmp<-paste(sprintf("%.2f",10^(-assoc.pvalscale[2] %% 1)),ceiling(assoc.pvalscale[2]),sep="e-")
						text(labels=label.tmp,x=1.15,y=assoc.pvalscale[2],pos=4,cex=legend.cex.txt[leg.counter])
						lab.ticks<-c(lab.ticks,assoc.pvalscale[2])
					}
					par(mgp=c(3,1,0), xpd=FALSE)
					segments(x0=rep(1,length(lab.ticks)),x1=rep(1.15,length(lab.ticks)),y0=lab.ticks,y1=lab.ticks)
					segments(x0=1,x1=1.15,y0=0,y1=0)
				}else{
					par(mgp=c(legend.assoc.axline,1,0), xpd=TRUE)
					plot(type="n",ylim=c(-1,1),xlim=assoc.pvalscale,axes=FALSE,xlab=legend.assoc.xlab,ylab="",main="",y=0,x=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
					poly.x<-numeric(0)
					poly.y<-numeric(0)
					for(i in 1:ncol){
						poly.y<-c(poly.y,c(0,1,1,0,NA))
						poly.x<-c(poly.x,c((i-1)*legend.assoc.bw,(i-1)*legend.assoc.bw,i*legend.assoc.bw,i*legend.assoc.bw,NA)+assoc.pvalscale[1])
					}
					polygon(x=poly.x,y=poly.y,border=assoc.cols,col=assoc.cols)
					polygon(y=c(0,1,1,0),x=assoc.pvalscale[c(1,1,2,2)],border="black",col=NA)
					lab.ticks<-setdiff(unique(trunc(seq(assoc.pvalscale[1],assoc.pvalscale[2],length=ncol))),0)
					lab.ticks<-lab.ticks[(lab.ticks %% legend.assoc.pvalticksround) == 0]
					text(labels=paste("1e-",round(lab.ticks,digits=2),sep=""),y=-0.15,x=lab.ticks,pos=1,cex=legend.cex.txt[leg.counter])
					text(labels=1,y=-0.15,x=0,pos=1,cex=legend.cex.txt[leg.counter])
					if(assoc.pvalscale[2]!=round(assoc.pvalscale[2])){
						label.tmp<-paste(sprintf("%.2f",10^(-assoc.pvalscale[2] %% 1)),ceiling(assoc.pvalscale[2]),sep="e-")
						text(labels=label.tmp,y=-0.15,x=assoc.pvalscale[2],pos=1,cex=legend.cex.txt[leg.counter])
						lab.ticks<-c(lab.ticks,assoc.pvalscale[2])
					}
					par(mgp=c(3,1,0),xpd=FALSE)
					segments(y0=rep(0,length(lab.ticks)),y1=rep(-0.15,length(lab.ticks)),x0=lab.ticks,x1=lab.ticks)
					segments(y0=0,y1=-0.15,x0=0,x1=0)
				}
			}

			# dix track legend(s)
			if(legend.dix && length(grep(leg,pattern="^dix",perl=T))>0){
				for(k in 1:n.legend.dix){
					if(leg==paste("dix",k,sep="") | leg=="dix"){
						leg.counter<-leg.counter+1
						#if(dix.col.rgb){
						#	col.tmp<-as.integer(unlist(strsplit(dix.col.pos[k],split="_")))
						#	dix.col.pos<-rgb(red=col.tmp[1],green=col.tmp[2],blue=col.tmp[3],maxColorValue=255)
						#	col.tmp<-as.integer(unlist(strsplit(dix.col.neg[k],split="_")))
						#	dix.col.neg<-rgb(red=col.tmp[1],green=col.tmp[2],blue=col.tmp[3],maxColorValue=255)
						#}
						
						par(mai=legend.mai,mgp=c(legend.dix.axline[k],1,0),xpd=TRUE)
						if(!legend.dix.hori){
							plot(type="n",xlim=c(0,2.5),ylim=c(0.5,3.5),axes=FALSE,xlab=legend.dix.xlab[k],ylab="",main="",x=0,y=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
							cols.tmp<-unique(paste(dix.col.pos,dix.col.neg,sep=":"))[k]
							col.pos.tmp<-unlist(strsplit(cols.tmp,split=":"))[1]
							col.neg.tmp<-unlist(strsplit(cols.tmp,split=":"))[2]
							polygon(x=c(0,0,1,1),y=c(2.5,3.5,3.5,2.5),col=col.pos.tmp,border=dix.col.pos[k])
							polygon(x=c(0,0,1,1),y=c(0.5,1.5,1.5,0.5),col=col.neg.tmp,border=dix.col.neg[k])
							text(labels=c(legend.dix.labpos[k],legend.dix.labneg[k]),x=1.25,y=c(3,1),pos=4,cex=legend.cex.txt[leg.counter])
						}else{
							plot(type="n",ylim=c(-1,2.5),xlim=c(0.5,3.5),axes=FALSE,xlab=legend.dix.xlab[k],ylab="",main="",y=0,x=1,adj=legend.adj[leg.counter],cex.lab=legend.cex.lab[leg.counter])
							cols.tmp<-unique(paste(dix.col.pos,dix.col.neg,sep=":"))[k]
							col.pos.tmp<-unlist(strsplit(cols.tmp,split=":"))[1]
							col.neg.tmp<-unlist(strsplit(cols.tmp,split=":"))[2]
							polygon(y=c(0,0,1,1),x=c(0.5,1.5,1.5,0.5),col=col.pos.tmp,border=dix.col.pos[k])
							polygon(y=c(0,0,1,1),x=c(2.5,3.5,3.5,2.5),col=col.neg.tmp,border=dix.col.neg[k])
							text(labels=c(legend.dix.labpos[k],legend.dix.labneg[k]),y=-0.25,x=c(1,3),pos=1,cex=legend.cex.txt[leg.counter])
						}
						par(mgp=c(3,1,0),xpd=FALSE)
					}
				}
			}
		
		}
	}

# close the graphics device
dev.off()


##########
## EXIT ##
##########

# final log file entries
	cat("Done.\n\n",file=paste(outprefix,"_R.log",sep=""),append=T) 	
	cat("This is the end.\n",file=paste(outprefix,"_R.log",sep=""),append=T)

