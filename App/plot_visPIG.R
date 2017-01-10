###########################
## MINIMAL DOCUMENTATION ##
###########################
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
####### MAIN SCRIPT BODY #######
################################
     ######################


######################
## SOURCE R SCRIPTS ##
######################

source(paste(script_dir,"axis_breaks.R",sep=""))
source(paste(script_dir,"interaction_track.R",sep=""))
source(paste(script_dir,"gene_track.R",sep=""))
source(paste(script_dir,"association_track.R",sep=""))
source(paste(script_dir,"annotation_track.R",sep=""))
source(paste(script_dir,"intensity_track.R",sep=""))
source(paste(script_dir,"heatmap_track.R",sep=""))
source(paste(script_dir,"feature_track.R",sep=""))
source(paste(script_dir,"snap_track.R",sep=""))

###########################
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
		dix.dat<-try(read.table(dix.file,sep="",header=F,colClasses=c("character",rep("numeric",dix.ncol-1))),silent=TRUE)
		if(length(grep(dix.dat,pattern="Error"))>0){stop(paste(sep="","Cannot read the directionality index data file. Please check the input file format is correct."))}
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
