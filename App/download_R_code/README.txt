     ######################
################################     
### COPYRIGHT & USER LICENCE ###
################################
     ######################

## visPIG -- Visual Plotting Interface for Genetics
## R code developed by the Institute of Cancer Research (ICR), UK
## Code authors: Marc Henrion & Matthew Scales

## SNAP track R code adapted from R code publicly available from:
##              Diabetes Genetics Initiative of Broad Institute of Harvard and MIT, Lund University and 
##                                  Novartis Institutes of BioMedical Research
##        Whole-genome association analysis identifies novel loci for type 2 diabetes and triglyceride levels
##                             Science 2007 Jun 1;316(5829):1331-6. Epub 2007 Apr 26


     ######################
################################     
############ README ############
################################
     ######################

Thank you for downloading the R code.

System requirements:
i) R (available from http://cran.r-project.org/); tested on R versions > 3.0.2, but should run even with quite old version of R; no additional R packages required
ii) as much RAM memory as required by your input files

To run the code with the parameters you downloaded from the visPIG website, just open up a terminal and run (parameters between square brackets are optional):

	R --vanilla < visPIG.R --args pars.file=saved.parameters.txt regions.file=regions.file [command_line_argument=value]

where
	saved.parameters.txt = the downloaded file with input parameter values; you can edit this file if you want to change some parameters
	regions.file = the file specifying which regions should be plotted on the x-axis

and where the command_line_argument can be one of (NOTE: some input file parameters terminate in *.file, others in *.files):
	outprefix -- specifying both the output folder and file name start (there will be one log file and one pdf or png image; if unspecified, the default being used is "./visPIG_out")
	plot.type -- one of "pdf" or "png" (defaults to "pdf" if unspecified)
	annot.files -- comma-separated list of the location(s) and name(s) of the multi-stage annotation file(s)
	assoc.file -- location and name of the association data file
	bd.file -- location and name of the two-stage annotation file
	dix.file -- location and name of the directionality index file
	gerp.files -- comma-separated list of the location(s) and name(s) of the GERP files (you can specify multiple files, e.g. one per chromosome, but they will all be merged as one large file and not result in multiple tracks)
	gerp.filter.file -- location and name of the file with the positions for which to plot the GERP data (if not specified all positions in the gerp.files set of files will be plotted); only used if gerp.noblocks==TRUE
	heat.file -- location and name of the heat map file
	hic.files -- location and name of the interaction data file
	int.files -- comma-separated list of the location(s) and name(s) of the intensity data file(s)
	phast.files -- comma-separated list of the location(s) and name(s) of the phastCons file(s) (you can specify multiple files, e.g. one per chromosome, but they will all be merged as one large file and not result in multiple tracks)
	phast.filter.file -- location and name of the file with the positions for which to plot the phastCons data (if not specified all positions in the phast.files set of files will be plotted); only used if phast.noblocks==TRUE
	snap.data.files -- comma-separated list of the location(s) and name(s) of the SNAP data file(s)
	snap.rate.files -- comma-separated list of the location(s) and name(s) of the SNAP rate file(s)
	snap.snp2.file -- location and name of the data file for the second SNP for the SNAP track
	feat.files -- comma-separated list of the location(s) and name(s) of the feature annotation file(s)
	lines.file -- location and name of the file containing the locations of the vertical lines that should be overplotted
	gene.file -- location and name of the gene file
	chrsizes.file -- location and name of the file specifying the sizes of the chromosomes (required if your region file contains more than one chromosome)


     ######################
################################     
######### FILE FORMATS #########
################################
     ######################

Please consult the help pages on http://vispig.icr.ac.uk/ for input file formats.

