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
