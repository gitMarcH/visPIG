outscript<-"../App/download_R_code/visPIG.R"

# paste the script header
	system(paste("cat ./visPIG_download_header.R > ",outscript,sep=""))
	cat("\n",file=outscript,append=T)

# concatenate the R scripts that get sourced
	system(paste("cat ../App/scripts/axis_breaks.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)
	system(paste("cat ../App/scripts/annotation_track.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)
	system(paste("cat ../App/scripts/association_track.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)
	system(paste("cat ../App/scripts/feature_track.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)
	system(paste("cat ../App/scripts/gene_track.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)
	system(paste("cat ../App/scripts/heatmap_track.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)
	system(paste("cat ../App/scripts/intensity_track.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)
	system(paste("cat ../App/scripts/interaction_track.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)
	system(paste("cat ../App/scripts/snap_track.R >> ",outscript,sep="")); cat("\n",file=outscript,append=T)

# hard-code (at least into existence) some extra parameters (typically found in server.R)
	cat("\n",file=outscript,append=T)
	cat("     ######################\n",file=outscript,append=T)
	cat("################################\n",file=outscript,append=T)    
	cat("####### EXTRA PARAMETERS #######\n",file=outscript,append=T)
	cat("################################\n",file=outscript,append=T)
	cat("     ######################\n\n",file=outscript,append=T)

	cat("dynamicHelp <<- \"Advice & troubleshooting:\n\"\n",file=outscript,append=T)
	cat("\n\n",file=outscript,append=T)

# write the lines that check if a parameter file has been specified and, if yes, load the parameters
	system(paste("cat ./process_pars_file_template.R >> ",outscript,sep=""))
	params<-read.csv('./params.csv')
	for(i in 1:nrow(params)){
		if(params$type[i]!="HTML"){
			if(params$type[i]=="logical"){ cat(paste("\t",params$id[i],"<-as.logical(",params$id[i],")\n",sep=""),file=outscript,append=T) }
			if(params$type[i]=="integer"){ cat(paste("\t",params$id[i],"<-as.integer(",params$id[i],")\n",sep=""),file=outscript,append=T) }
			if(params$type[i]=="numeric"){ cat(paste("\t",params$id[i],"<-as.numeric(",params$id[i],")\n",sep=""),file=outscript,append=T) }
		}
	}
	cat("\n\n",file=outscript,append=T)
	
# set specific parameters to NULL, rather than NA if they have not been specified explicitly
	cat("# set specific parameters to NULL, rather than NA if they have not been specified explicitly\n",file=outscript,append=T)
	cat("\tif(!is.null(annot.bgclass)){if(sum(is.na(annot.bgclass))>0){annot.bgclass<-NULL}}\n",file=outscript,append=T)
	cat("\tif(!is.null(assoc.pvalscale)){if(sum(is.na(assoc.pvalscale))>0){assoc.pvalscale<-NULL}}\n",file=outscript,append=T)
	cat("\tif(!is.null(heat.ylim)){if(sum(is.na(heat.ylim))>0){heat.ylim<-NULL}}\n",file=outscript,append=T)
	cat("\tif(!is.null(heat.zlim)){if(sum(is.na(heat.zlim))>0){heat.zlim<-NULL}}\n",file=outscript,append=T)
	cat("\tif(!is.null(gene.cex.arrows)){if(sum(is.na(gene.cex.arrows))>0){gene.cex.arrows<-NULL}}\n",file=outscript,append=T)
	cat("\tif(!is.null(gerp.ylim)){if(sum(is.na(gerp.ylim))>0){gerp.ylim<-NULL}}\n",file=outscript,append=T)
	cat("\tif(!is.null(phast.ylim)){if(sum(is.na(phast.ylim))>0){phast.ylim<-NULL}}\n",file=outscript,append=T)
	cat("\n\n",file=outscript,append=T)
	
# set the output file for the plot
	cat("# set the output file for the plot\n",file=outscript,append=T)
	cat("\tfile<-paste(outprefix,\".\",plot.type,sep=\"\")\n\n\n",file=outscript,append=T)
	cat("\toutfile<-paste(outprefix,\".\",plot.type,sep=\"\")\n\n\n",file=outscript,append=T)

# concatenate (most) of the lines from plot_visPIG.R
	cat("     ######################\n",file=outscript,append=T)
	cat("################################\n",file=outscript,append=T)    
	cat("####### MAIN SCRIPT BODY #######\n",file=outscript,append=T)
	cat("################################\n",file=outscript,append=T)
	cat("     ######################\n\n",file=outscript,append=T)
	
	system(paste("awk '{if(NR>42) print}' ../App/plot_visPIG.R >> ",outscript,sep=""))
	cat("\n",file=outscript,append=T)
	
# create the tar ball for download
	system("tar -zcf ../App/www/visPIG.tar.gz -C ../App/download_R_code/ visPIG.R README.txt GNU_GPLv3_license.txt")
