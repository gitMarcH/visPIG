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
