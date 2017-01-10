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
