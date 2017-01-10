# Read in and parse the script parameters from the web GUI, uploaded parameter file and the default parameter value, in that order.




# Parameters for files

if(input$regions_file!="" && !is.null(input$regions_file)){ in.value <- input$regions_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_regions_file)==FALSE){regions.file<-c(NA) } else {regions.file<-in.value$datapath; }
if(input$parameters_file!="" && !is.null(input$parameters_file)){ in.value <- input$parameters_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_parameters_file)==FALSE){parameters.file<-c(NA) } else {parameters.file<-in.value$datapath; }
if(input$annot_files!="" && !is.null(input$annot_files)){ in.value <- input$annot_files} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_annot_files)==FALSE){annot.files<-c(NA) } else {annot.files<-in.value$datapath; }
	if(is.na(in.value[1])){n.annot <- 0 } else if(!is.null(input$consider_annot_files) && !is.na(input$consider_annot_files) && input$consider_annot_files==FALSE){n.annot <- 0 } else {n.annot <- length(annot.files) }
if(input$assoc_file!="" && !is.null(input$assoc_file)){ in.value <- input$assoc_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_assoc_file)==FALSE){assoc.file<-c(NA) } else {assoc.file<-in.value$datapath; }
if(input$dix_file!="" && !is.null(input$dix_file)){ in.value <- input$dix_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_dix_file)==FALSE){dix.file<-c(NA) } else {dix.file<-in.value$datapath; }
if(input$gerp_files!="" && !is.null(input$gerp_files)){ in.value <- input$gerp_files} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_gerp_files)==FALSE){gerp.files<-c(NA) } else {gerp.files<-in.value$datapath; }
if(input$heat_file!="" && !is.null(input$heat_file)){ in.value <- input$heat_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_heat_file)==FALSE){heat.file<-c(NA) } else {heat.file<-in.value$datapath; }
if(input$hic_files!="" && !is.null(input$hic_files)){ in.value <- input$hic_files} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_hic_files)==FALSE){hic.files<-c(NA) } else {hic.files<-in.value$datapath; }
	if(is.na(in.value[1])){n.hic <- 0 } else if(!is.null(input$consider_hic_files) && !is.na(input$consider_hic_files) && input$consider_hic_files==FALSE){n.hic <- 0 } else {n.hic <- length(hic.files) }
if(input$int_files!="" && !is.null(input$int_files)){ in.value <- input$int_files} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_int_files)==FALSE){int.files<-c(NA) } else {int.files<-in.value$datapath; }
	if(is.na(in.value[1])){n.int <- 0 } else if(!is.null(input$consider_int_files) && !is.na(input$consider_int_files) && input$consider_int_files==FALSE){n.int <- 0 } else {n.int <- length(int.files) }
if(input$phast_files!="" && !is.null(input$phast_files)){ in.value <- input$phast_files} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_phast_files)==FALSE){phast.files<-c(NA) } else {phast.files<-in.value$datapath; }
	if(is.na(in.value[1])){n.phast <- 0 } else if(!is.null(input$consider_phast_files) && !is.na(input$consider_phast_files) && input$consider_phast_files==FALSE){n.phast <- 0 } else {n.phast <- length(phast.files) }
if(input$snap_data_files!="" && !is.null(input$snap_data_files)){ in.value <- input$snap_data_files} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_snap_data_files)==FALSE){snap.data.files<-c(NA) } else {snap.data.files<-in.value$datapath; }
	if(is.na(in.value[1])){n.snap <- 0 } else if(!is.null(input$consider_snap_data_files) && !is.na(input$consider_snap_data_files) && input$consider_snap_data_files==FALSE){n.snap <- 0 } else {n.snap <- length(snap.data.files) }
if(input$snap_rate_files!="" && !is.null(input$snap_rate_files)){ in.value <- input$snap_rate_files} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_snap_rate_files)==FALSE){snap.rate.files<-c(NA) } else {snap.rate.files<-in.value$datapath; }
	if(is.na(in.value[1])){n.snap <- 0 } else if(!is.null(input$consider_snap_rate_files) && !is.na(input$consider_snap_rate_files) && input$consider_snap_rate_files==FALSE){n.snap <- 0 } else {n.snap <- length(snap.rate.files) }
if(input$feat_files!="" && !is.null(input$feat_files)){ in.value <- input$feat_files} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_feat_files)==FALSE){feat.files<-c(NA) } else {feat.files<-in.value$datapath; }
	if(is.na(in.value[1])){n.feat <- 0 } else if(!is.null(input$consider_feat_files) && !is.na(input$consider_feat_files) && input$consider_feat_files==FALSE){n.feat <- 0 } else {n.feat <- length(feat.files) }
if(input$lines_file!="" && !is.null(input$lines_file)){ in.value <- input$lines_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_lines_file)==FALSE){lines.file<-c(NA) } else {lines.file<-in.value$datapath; }
if(input$gene_file!="" && !is.null(input$gene_file)){ in.value <- input$gene_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_gene_file)==FALSE){gene.file<-c("./reference_files/gene.file") } else {gene.file<-in.value$datapath; }
if(input$chrsizes_file!="" && !is.null(input$chrsizes_file)){ in.value <- input$chrsizes_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_chrsizes_file)==FALSE){chrsizes.file<-c("./reference_files/chrsizes.file") } else {chrsizes.file<-in.value$datapath; }
if(input$track_order_top_down!="" && !is.null(input$track_order_top_down)){ in.value <- input$track_order_top_down} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){track.order.top.down<-c('title','heat','feat','dix','bd','assoc','int','hic','annot','phast','gerp','snap','gene','axis') } else {track.order.top.down<-as.character(in.value) }


# Parameters for gene_track

if(input$gene_relplotheight!="" && !is.null(input$gene_relplotheight)){ in.value <- input$gene_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.relplotheight<-c(NA) } else {gene.relplotheight<-as.numeric(in.value) }
if(input$gene_letter!="" && !is.null(input$gene_letter)){ in.value <- input$gene_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.letter<-c(NA) } else {gene.letter<-as.character(in.value) }
if(input$gene_ylab!="" && !is.null(input$gene_ylab)){ in.value <- input$gene_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.ylab<-c(NA) } else {gene.ylab<-as.character(in.value) }
if(input$gene_ylab_cex_txt!="" && !is.null(input$gene_ylab_cex_txt)){ in.value <- input$gene_ylab_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.ylab.cex.txt<-c(NA) } else {gene.ylab.cex.txt<-as.numeric(in.value) }
if(input$gene_col!="" && !is.null(input$gene_col)){ in.value <- input$gene_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.col<-c(NA) } else {gene.col<-in.value }
if(input$gene_igs!="" && !is.null(input$gene_igs)){ in.value <- input$gene_igs} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.igs<-c(NA) } else {gene.igs<-as.numeric(in.value) }
if(input$gene_cex_txt!="" && !is.null(input$gene_cex_txt)){ in.value <- input$gene_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.cex.txt<-c(NA) } else {gene.cex.txt<-as.numeric(in.value) }
if(input$gene_cex_arrows!="" && !is.null(input$gene_cex_arrows)){ in.value <- input$gene_cex_arrows} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.cex.arrows<-c(NULL) } else {gene.cex.arrows<-as.numeric(in.value) }
if(input$gene_lwd!="" && !is.null(input$gene_lwd)){ in.value <- input$gene_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gene.lwd<-c(NA) } else {gene.lwd<-as.numeric(in.value) }


# Parameters for gerp_track

if(input$gerp_relplotheight!="" && !is.null(input$gerp_relplotheight)){ in.value <- input$gerp_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.relplotheight<-c(NA) } else {gerp.relplotheight<-as.numeric(in.value) }
if(input$gerp_letter!="" && !is.null(input$gerp_letter)){ in.value <- input$gerp_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.letter<-c(NA) } else {gerp.letter<-as.character(in.value) }
if(input$gerp_ylab!="" && !is.null(input$gerp_ylab)){ in.value <- input$gerp_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.ylab<-c(NA) } else {gerp.ylab<-as.character(in.value) }
if(input$gerp_cex_txt!="" && !is.null(input$gerp_cex_txt)){ in.value <- input$gerp_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.cex.txt<-c(NA) } else {gerp.cex.txt<-as.numeric(in.value) }
if(input$gerp_lwd!="" && !is.null(input$gerp_lwd)){ in.value <- input$gerp_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.lwd<-c(NA) } else {gerp.lwd<-as.numeric(in.value) }
if(input$gerp_ylim!="" && !is.null(input$gerp_ylim)){ in.value <- input$gerp_ylim} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.ylim<-c(NULL) } else {gerp.ylim<-as.numeric(in.value) }
if(input$gerp_noblocks!="" && !is.null(input$gerp_noblocks)){ in.value <- input$gerp_noblocks} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.noblocks<-c(NA) } else {gerp.noblocks<-as.logical(in.value) }
if(input$gerp_filter_file!="" && !is.null(input$gerp_filter_file)){ in.value <- input$gerp_filter_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_gerp_filter_file)==FALSE){gerp.filter.file<-c(NA) } else {gerp.filter.file<-in.value$datapath; }
if(input$gerp_mai_top!="" && !is.null(input$gerp_mai_top)){ in.value <- input$gerp_mai_top} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.mai.top<-c(0) } else {gerp.mai.top<-as.numeric(in.value) }
if(input$gerp_mai_bot!="" && !is.null(input$gerp_mai_bot)){ in.value <- input$gerp_mai_bot} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gerp.mai.bot<-c(0) } else {gerp.mai.bot<-as.numeric(in.value) }


# Parameters for phast_track

if(input$phast_relplotheight!="" && !is.null(input$phast_relplotheight)){ in.value <- input$phast_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){phast.relplotheight<-c(NA) } else {phast.relplotheight<-as.numeric(in.value) }
if(input$phast_letter!="" && !is.null(input$phast_letter)){ in.value <- input$phast_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){phast.letter<-c(NA) } else {phast.letter<-as.character(in.value) }
if(input$phast_ylab!="" && !is.null(input$phast_ylab)){ in.value <- input$phast_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){phast.ylab<-c(NA) } else {phast.ylab<-as.character(in.value) }
if(input$phast_cex_txt!="" && !is.null(input$phast_cex_txt)){ in.value <- input$phast_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){phast.cex.txt<-c(NA) } else {phast.cex.txt<-as.numeric(in.value) }
if(input$phast_lwd!="" && !is.null(input$phast_lwd)){ in.value <- input$phast_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){phast.lwd<-c(NA) } else {phast.lwd<-as.numeric(in.value) }
if(input$phast_ylim!="" && !is.null(input$phast_ylim)){ in.value <- input$phast_ylim} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){phast.ylim<-c(NULL) } else {phast.ylim<-as.numeric(in.value) }
if(input$phast_noblocks!="" && !is.null(input$phast_noblocks)){ in.value <- input$phast_noblocks} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){phast.noblocks<-c(NA) } else {phast.noblocks<-as.logical(in.value) }
if(input$phast_filter_file!="" && !is.null(input$phast_filter_file)){ in.value <- input$phast_filter_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_phast_filter_file)==FALSE){phast.filter.file<-c(NA) } else {phast.filter.file<-in.value$datapath; }


# Parameters for annot_track

if(input$annot_relplotheight!="" && !is.null(input$annot_relplotheight)){ in.value <- input$annot_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){annot.relplotheight<-c(NA) } else {annot.relplotheight<-as.numeric(in.value) }
if(input$annot_letter!="" && !is.null(input$annot_letter)){ in.value <- input$annot_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){annot.letter<-c(NA) } else {annot.letter<-as.character(in.value) }
if(input$annot_ylab!="" && !is.null(input$annot_ylab)){ in.value <- input$annot_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){annot.ylab<-c(rep(NA,max(n.annot,1))) } else {annot.ylab<-as.character(in.value) }
if(input$annot_cex_txt!="" && !is.null(input$annot_cex_txt)){ in.value <- input$annot_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){annot.cex.txt<-c(NA) } else {annot.cex.txt<-as.numeric(in.value) }
if(input$annot_lwd!="" && !is.null(input$annot_lwd)){ in.value <- input$annot_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){annot.lwd<-c(NA) } else {annot.lwd<-as.numeric(in.value) }
if(input$annot_bgclass!="" && !is.null(input$annot_bgclass)){ in.value <- input$annot_bgclass} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){annot.bgclass<-c(NULL) } else {annot.bgclass<-in.value }
if(input$annot_mai_top!="" && !is.null(input$annot_mai_top)){ in.value <- input$annot_mai_top} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){annot.mai.top<-c(NA) } else {annot.mai.top<-as.numeric(in.value) }
if(input$annot_mai_bot!="" && !is.null(input$annot_mai_bot)){ in.value <- input$annot_mai_bot} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){annot.mai.bot<-c(NA) } else {annot.mai.bot<-as.numeric(in.value) }


# Parameters for hic_track

if(input$hic_relplotheight!="" && !is.null(input$hic_relplotheight)){ in.value <- input$hic_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.relplotheight<-c(NA) } else {hic.relplotheight<-as.numeric(in.value) }
if(input$hic_letter!="" && !is.null(input$hic_letter)){ in.value <- input$hic_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.letter<-c(NA) } else {hic.letter<-as.character(in.value) }
if(input$hic_ylab!="" && !is.null(input$hic_ylab)){ in.value <- input$hic_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.ylab<-c(NA) } else {hic.ylab<-as.character(in.value) }
if(input$hic_cex_txt!="" && !is.null(input$hic_cex_txt)){ in.value <- input$hic_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.cex.txt<-c(NA) } else {hic.cex.txt<-as.numeric(in.value) }
if(input$hic_style!="" && !is.null(input$hic_style)){ in.value <- input$hic_style} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.style<-c(NA) } else {hic.style<-as.character(in.value) }
if(input$hic_plotThroughGaps!="" && !is.null(input$hic_plotThroughGaps)){ in.value <- input$hic_plotThroughGaps} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.plotThroughGaps<-c(NA) } else {hic.plotThroughGaps<-as.logical(in.value) }
if(input$hic_colThroughGaps!="" && !is.null(input$hic_colThroughGaps)){ in.value <- input$hic_colThroughGaps} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.colThroughGaps<-c(NA) } else {hic.colThroughGaps<-in.value }
if(input$hic_colref!="" && !is.null(input$hic_colref)){ in.value <- input$hic_colref} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.colref<-c(NA) } else {hic.colref<-in.value }
if(input$hic_colnonref!="" && !is.null(input$hic_colnonref)){ in.value <- input$hic_colnonref} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.colnonref<-c(NA) } else {hic.colnonref<-in.value }
if(input$hic_stack_ylim_bot!="" && !is.null(input$hic_stack_ylim_bot)){ in.value <- input$hic_stack_ylim_bot} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.stack.ylim.bot<-c(NULL) } else {hic.stack.ylim.bot<-as.numeric(in.value) }
if(input$hic_stack_ylim_top!="" && !is.null(input$hic_stack_ylim_top)){ in.value <- input$hic_stack_ylim_top} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.stack.ylim.top<-c(NULL) } else {hic.stack.ylim.top<-as.numeric(in.value) }
if(input$hic_stack_style!="" && !is.null(input$hic_stack_style)){ in.value <- input$hic_stack_style} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.stack.style<-c(NA) } else {hic.stack.style<-as.character(in.value) }
if(input$hic_stack_col!="" && !is.null(input$hic_stack_col)){ in.value <- input$hic_stack_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.stack.col<-c(black) } else {hic.stack.col<-in.value }
if(input$hic_stack_lwd!="" && !is.null(input$hic_stack_lwd)){ in.value <- input$hic_stack_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.stack.lwd<-c(0.5) } else {hic.stack.lwd<-as.numeric(in.value) }
if(input$hic_arches_twist!="" && !is.null(input$hic_arches_twist)){ in.value <- input$hic_arches_twist} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.twist<-c(NA) } else {hic.arches.twist<-as.logical(in.value) }
if(input$hic_arches_lwd!="" && !is.null(input$hic_arches_lwd)){ in.value <- input$hic_arches_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.lwd<-c(NA) } else {hic.arches.lwd<-as.numeric(in.value) }
if(input$hic_arches_nsegments!="" && !is.null(input$hic_arches_nsegments)){ in.value <- input$hic_arches_nsegments} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.nsegments<-c(NA) } else {hic.arches.nsegments<-as.integer(round(as.numeric(in.value))) }
if(input$hic_arches_neglog10!="" && !is.null(input$hic_arches_neglog10)){ in.value <- input$hic_arches_neglog10} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.neglog10<-c(NA) } else {hic.arches.neglog10<-as.logical(in.value) }
if(input$hic_arches_ylim_lower!="" && !is.null(input$hic_arches_ylim_lower)){ in.value <- input$hic_arches_ylim_lower} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.ylim.lower<-c(NA) } else {hic.arches.ylim.lower<-as.numeric(in.value) }
if(input$hic_arches_ylim_upper!="" && !is.null(input$hic_arches_ylim_upper)){ in.value <- input$hic_arches_ylim_upper} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.ylim.upper<-c(NA) } else {hic.arches.ylim.upper<-as.numeric(in.value) }
if(input$hic_arches_yaxis!="" && !is.null(input$hic_arches_yaxis)){ in.value <- input$hic_arches_yaxis} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.yaxis<-c(NA) } else {hic.arches.yaxis<-as.logical(in.value) }
if(input$hic_arches_col!="" && !is.null(input$hic_arches_col)){ in.value <- input$hic_arches_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.col<-c(NA) } else {hic.arches.col<-in.value }
if(input$hic_arches_varicol!="" && !is.null(input$hic_arches_varicol)){ in.value <- input$hic_arches_varicol} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.varicol<-c(NA) } else {hic.arches.varicol<-as.logical(in.value) }
if(input$hic_arches_dir!="" && !is.null(input$hic_arches_dir)){ in.value <- input$hic_arches_dir} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.arches.dir<-c(NA) } else {hic.arches.dir<-as.character(in.value) }
if(input$hic_plotThroughGapsMaxY!="" && !is.null(input$hic_plotThroughGapsMaxY)){ in.value <- input$hic_plotThroughGapsMaxY} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.plotThroughGapsMaxY<-c(NA) } else {hic.plotThroughGapsMaxY<-as.logical(in.value) }
if(input$hic_capt_region!="" && !is.null(input$hic_capt_region)){ in.value <- input$hic_capt_region} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.capt.region<-c(NA) } else {hic.capt.region<-as.character(in.value) }
if(input$hic_capt_col!="" && !is.null(input$hic_capt_col)){ in.value <- input$hic_capt_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.capt.col<-c(NA) } else {hic.capt.col<-in.value }
if(input$hic_capt_density!="" && !is.null(input$hic_capt_density)){ in.value <- input$hic_capt_density} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.capt.density<-c(NA) } else {hic.capt.density<-as.numeric(in.value) }
if(input$hic_capt_lines_lwd!="" && !is.null(input$hic_capt_lines_lwd)){ in.value <- input$hic_capt_lines_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){hic.capt.lines.lwd<-c(NA) } else {hic.capt.lines.lwd<-as.numeric(in.value) }


# Parameters for int_track

if(input$int_relplotheight!="" && !is.null(input$int_relplotheight)){ in.value <- input$int_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.relplotheight<-c(NA) } else {int.relplotheight<-as.numeric(in.value) }
if(input$int_letter!="" && !is.null(input$int_letter)){ in.value <- input$int_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.letter<-c(NA) } else {int.letter<-as.character(in.value) }
if(input$int_ylab!="" && !is.null(input$int_ylab)){ in.value <- input$int_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.ylab<-c(NA) } else {int.ylab<-as.character(in.value) }
if(input$int_cex_txt!="" && !is.null(input$int_cex_txt)){ in.value <- input$int_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.cex.txt<-c(NA) } else {int.cex.txt<-as.numeric(in.value) }
if(input$int_lwd!="" && !is.null(input$int_lwd)){ in.value <- input$int_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.lwd<-c(NA) } else {int.lwd<-as.numeric(in.value) }
if(input$int_ylim_lower!="" && !is.null(input$int_ylim_lower)){ in.value <- input$int_ylim_lower} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.ylim.lower<-c(rep(NA,n.int)) } else {int.ylim.lower<-as.numeric(in.value) }
if(input$int_ylim_upper!="" && !is.null(input$int_ylim_upper)){ in.value <- input$int_ylim_upper} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.ylim.upper<-c(rep(NA,n.int)) } else {int.ylim.upper<-as.numeric(in.value) }
if(input$int_style!="" && !is.null(input$int_style)){ in.value <- input$int_style} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.style<-c(NA) } else {int.style<-as.character(in.value) }
if(input$int_smooth_window!="" && !is.null(input$int_smooth_window)){ in.value <- input$int_smooth_window} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.smooth.window<-c(NA) } else {int.smooth.window<-as.integer(round(as.numeric(in.value))) }
if(input$int_smooth_method!="" && !is.null(input$int_smooth_method)){ in.value <- input$int_smooth_method} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.smooth.method<-c(NA) } else {int.smooth.method<-as.character(in.value) }
if(input$int_refbox!="" && !is.null(input$int_refbox)){ in.value <- input$int_refbox} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.refbox<-c(NA) } else {int.refbox<-as.character(in.value) }
if(input$int_refbox_col!="" && !is.null(input$int_refbox_col)){ in.value <- input$int_refbox_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.refbox.col<-c(NA) } else {int.refbox.col<-in.value }
if(input$int_refbox_density!="" && !is.null(input$int_refbox_density)){ in.value <- input$int_refbox_density} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.refbox.density<-c(NA) } else {int.refbox.density<-as.numeric(in.value) }
if(input$int_refbox_lines_lwd!="" && !is.null(input$int_refbox_lines_lwd)){ in.value <- input$int_refbox_lines_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.refbox.lines.lwd<-c(NA) } else {int.refbox.lines.lwd<-as.numeric(in.value) }
if(input$int_ref2box!="" && !is.null(input$int_ref2box)){ in.value <- input$int_ref2box} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.ref2box<-c(NA) } else {int.ref2box<-as.character(in.value) }
if(input$int_ref2box_col!="" && !is.null(input$int_ref2box_col)){ in.value <- input$int_ref2box_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.ref2box.col<-c(NA) } else {int.ref2box.col<-in.value }
if(input$int_ref2box_density!="" && !is.null(input$int_ref2box_density)){ in.value <- input$int_ref2box_density} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.ref2box.density<-c(NA) } else {int.ref2box.density<-as.numeric(in.value) }
if(input$int_ref2box_lines_lwd!="" && !is.null(input$int_ref2box_lines_lwd)){ in.value <- input$int_ref2box_lines_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){int.ref2box.lines.lwd<-c(NA) } else {int.ref2box.lines.lwd<-as.numeric(in.value) }


# Parameters for assoc_track

if(input$assoc_relplotheight!="" && !is.null(input$assoc_relplotheight)){ in.value <- input$assoc_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){assoc.relplotheight<-c(NA) } else {assoc.relplotheight<-as.numeric(in.value) }
if(input$assoc_letter!="" && !is.null(input$assoc_letter)){ in.value <- input$assoc_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){assoc.letter<-c(NA) } else {assoc.letter<-as.character(in.value) }
if(input$assoc_ylab!="" && !is.null(input$assoc_ylab)){ in.value <- input$assoc_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){assoc.ylab<-c(NA) } else {assoc.ylab<-as.character(in.value) }
if(input$assoc_cex_txt!="" && !is.null(input$assoc_cex_txt)){ in.value <- input$assoc_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){assoc.cex.txt<-c(NA) } else {assoc.cex.txt<-as.numeric(in.value) }
if(input$assoc_cols!="" && !is.null(input$assoc_cols)){ in.value <- input$assoc_cols} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){assoc.cols<-c(NA) } else {assoc.cols<-in.value }
if(input$assoc_pvalscale!="" && !is.null(input$assoc_pvalscale)){ in.value <- input$assoc_pvalscale} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){assoc.pvalscale<-c(NULL) } else {assoc.pvalscale<-as.numeric(in.value) }
if(input$assoc_lwd_bars!="" && !is.null(input$assoc_lwd_bars)){ in.value <- input$assoc_lwd_bars} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){assoc.lwd.bars<-c(1.00E-05) } else {assoc.lwd.bars<-as.numeric(in.value) }


# Parameters for dix_track

if(input$dix_relplotheight!="" && !is.null(input$dix_relplotheight)){ in.value <- input$dix_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.relplotheight<-c(NA) } else {dix.relplotheight<-as.numeric(in.value) }
if(input$dix_letter!="" && !is.null(input$dix_letter)){ in.value <- input$dix_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.letter<-c(NA) } else {dix.letter<-as.character(in.value) }
if(input$dix_ylab!="" && !is.null(input$dix_ylab)){ in.value <- input$dix_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.ylab<-c(NA) } else {dix.ylab<-in.value }
if(input$dix_cex_txt!="" && !is.null(input$dix_cex_txt)){ in.value <- input$dix_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.cex.txt<-c(NA) } else {dix.cex.txt<-as.numeric(in.value) }
if(input$dix_col_pos!="" && !is.null(input$dix_col_pos)){ in.value <- input$dix_col_pos} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.col.pos<-c(NA) } else {dix.col.pos<-in.value }
if(input$dix_col_neg!="" && !is.null(input$dix_col_neg)){ in.value <- input$dix_col_neg} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.col.neg<-c(NA) } else {dix.col.neg<-in.value }
if(input$dix_col_rgb!="" && !is.null(input$dix_col_rgb)){ in.value <- input$dix_col_rgb} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.col.rgb<-c(NA) } else {dix.col.rgb<-as.logical(in.value) }
if(input$dix_cols_toplot!="" && !is.null(input$dix_cols_toplot)){ in.value <- input$dix_cols_toplot} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.cols.toplot<-c(4) } else {dix.cols.toplot<-as.integer(round(as.numeric(in.value))) }
	if(is.na(in.value[1])){n.dix <- 0 } else if(!is.null(input$consider_dix_cols_toplot) && !is.na(input$consider_dix_cols_toplot) && input$consider_dix_cols_toplot==FALSE){n.dix <- 0 } else {n.dix <- length(dix.cols.toplot) }
if(input$dix_ylim_lower!="" && !is.null(input$dix_ylim_lower)){ in.value <- input$dix_ylim_lower} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.ylim.lower<-c(rep(NA,n.dix)) } else {dix.ylim.lower<-as.numeric(in.value) }
if(input$dix_ylim_upper!="" && !is.null(input$dix_ylim_upper)){ in.value <- input$dix_ylim_upper} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){dix.ylim.upper<-c(rep(NA,n.dix)) } else {dix.ylim.upper<-as.numeric(in.value) }


# Parameters for zoom

if(input$zoom!="" && !is.null(input$zoom)){ in.value <- input$zoom} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom<-c(NA) } else {zoom<-as.logical(in.value) }
if(input$zoom_chr!="" && !is.null(input$zoom_chr)){ in.value <- input$zoom_chr} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.chr<-c(NA) } else {zoom.chr<-as.integer(round(as.numeric(in.value))) }
if(input$zoom_SP!="" && !is.null(input$zoom_SP)){ in.value <- input$zoom_SP} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.SP<-c(NA) } else {zoom.SP<-as.numeric(in.value) }
if(input$zoom_EP!="" && !is.null(input$zoom_EP)){ in.value <- input$zoom_EP} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.EP<-c(NA) } else {zoom.EP<-as.numeric(in.value) }
if(input$zoom_track_order_top_down!="" && !is.null(input$zoom_track_order_top_down)){ in.value <- input$zoom_track_order_top_down} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.track.order.top.down<-c(NA) } else {zoom.track.order.top.down<-as.character(in.value) }
if(input$zoom_vertical_spacing!="" && !is.null(input$zoom_vertical_spacing)){ in.value <- input$zoom_vertical_spacing} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.vertical.spacing<-c(NA) } else {zoom.vertical.spacing<-as.numeric(in.value) }
if(input$zoom_lines_col!="" && !is.null(input$zoom_lines_col)){ in.value <- input$zoom_lines_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.lines.col<-c(NA) } else {zoom.lines.col<-in.value }
if(input$zoom_lines_alpha!="" && !is.null(input$zoom_lines_alpha)){ in.value <- input$zoom_lines_alpha} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.lines.alpha<-c(NA) } else {zoom.lines.alpha<-as.numeric(in.value) }
if(input$zoom_lines_lwd!="" && !is.null(input$zoom_lines_lwd)){ in.value <- input$zoom_lines_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.lines.lwd<-c(NA) } else {zoom.lines.lwd<-as.numeric(in.value) }
if(input$zoom_lines_lty!="" && !is.null(input$zoom_lines_lty)){ in.value <- input$zoom_lines_lty} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.lines.lty<-c(NA) } else {zoom.lines.lty<-as.numeric(in.value) }
if(input$zoom_axis_format!="" && !is.null(input$zoom_axis_format)){ in.value <- input$zoom_axis_format} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.axis.format<-c(NA) } else {zoom.axis.format<-as.character(in.value) }
if(input$zoom_labels_show_unit!="" && !is.null(input$zoom_labels_show_unit)){ in.value <- input$zoom_labels_show_unit} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.labels.show.unit<-c(NA) } else {zoom.labels.show.unit<-as.logical(in.value) }
if(input$zoom_axis_tcl!="" && !is.null(input$zoom_axis_tcl)){ in.value <- input$zoom_axis_tcl} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.axis.tcl<-c(NA) } else {zoom.axis.tcl<-as.numeric(in.value) }
if(input$zoom_axis_lend!="" && !is.null(input$zoom_axis_lend)){ in.value <- input$zoom_axis_lend} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.axis.lend<-c(NA) } else {zoom.axis.lend<-as.integer(round(as.numeric(in.value))) }
if(input$zoom_x_lwd!="" && !is.null(input$zoom_x_lwd)){ in.value <- input$zoom_x_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.x.lwd<-c(NA) } else {zoom.x.lwd<-as.numeric(in.value) }
if(input$zoom_xlab!="*" && !is.null(input$zoom_xlab)){ in.value <- input$zoom_xlab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.xlab<-c(NA) } else {zoom.xlab<-as.character(in.value) }
if(input$zoom_x_labels_up!="" && !is.null(input$zoom_x_labels_up)){ in.value <- input$zoom_x_labels_up} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){zoom.x.labels.up<-c(NA) } else {zoom.x.labels.up<-as.logical(in.value) }


# Parameters for feat_track

if(input$feat_relplotheight!="" && !is.null(input$feat_relplotheight)){ in.value <- input$feat_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.relplotheight<-c(NA) } else {feat.relplotheight<-as.numeric(in.value) }
if(input$feat_letter!="" && !is.null(input$feat_letter)){ in.value <- input$feat_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.letter<-c(NA) } else {feat.letter<-as.character(in.value) }
if(input$feat_ylab!="" && !is.null(input$feat_ylab)){ in.value <- input$feat_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.ylab<-c(NA) } else {feat.ylab<-as.character(in.value) }
if(input$feat_cex_txt!="" && !is.null(input$feat_cex_txt)){ in.value <- input$feat_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.cex.txt<-c(NA) } else {feat.cex.txt<-as.numeric(in.value) }
if(input$feat_cex_lab!="" && !is.null(input$feat_cex_lab)){ in.value <- input$feat_cex_lab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.cex.lab<-c(NA) } else {feat.cex.lab<-as.numeric(in.value) }
if(input$feat_lwd!="" && !is.null(input$feat_lwd)){ in.value <- input$feat_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.lwd<-c(NA) } else {feat.lwd<-as.numeric(in.value) }
if(input$feat_height!="" && !is.null(input$feat_height)){ in.value <- input$feat_height} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.height<-c(NA) } else {feat.height<-as.numeric(in.value) }
if(input$feat_col!="" && !is.null(input$feat_col)){ in.value <- input$feat_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.col<-c(NA) } else {feat.col<-in.value }
if(input$feat_textpos!="" && !is.null(input$feat_textpos)){ in.value <- input$feat_textpos} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.textpos<-c(NA) } else {feat.textpos<-as.character(in.value) }
if(input$feat_txt_vert_offset!="" && !is.null(input$feat_txt_vert_offset)){ in.value <- input$feat_txt_vert_offset} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.txt.vert.offset<-c(NA) } else {feat.txt.vert.offset<-as.numeric(in.value) }
if(input$feat_txt_hori_offset!="" && !is.null(input$feat_txt_hori_offset)){ in.value <- input$feat_txt_hori_offset} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){feat.txt.hori.offset<-c(NA) } else {feat.txt.hori.offset<-as.numeric(in.value) }


# Parameters for heat_track

if(input$heat_relplotheight!="" && !is.null(input$heat_relplotheight)){ in.value <- input$heat_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.relplotheight<-c(NA) } else {heat.relplotheight<-as.numeric(in.value) }
if(input$heat_letter!="" && !is.null(input$heat_letter)){ in.value <- input$heat_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.letter<-c(NA) } else {heat.letter<-as.character(in.value) }
if(input$heat_ylab!="" && !is.null(input$heat_ylab)){ in.value <- input$heat_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.ylab<-c(NA) } else {heat.ylab<-as.character(in.value) }
if(input$heat_cex_txt!="" && !is.null(input$heat_cex_txt)){ in.value <- input$heat_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.cex.txt<-c(NA) } else {heat.cex.txt<-as.numeric(in.value) }
if(input$heat_chr!="" && !is.null(input$heat_chr)){ in.value <- input$heat_chr} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.chr<-c(NA) } else {heat.chr<-as.integer(round(as.numeric(in.value))) }
if(input$heat_SP!="" && !is.null(input$heat_SP)){ in.value <- input$heat_SP} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.SP<-c(NA) } else {heat.SP<-as.integer(round(as.numeric(in.value))) }
if(input$heat_EP!="" && !is.null(input$heat_EP)){ in.value <- input$heat_EP} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.EP<-c(NA) } else {heat.EP<-as.integer(round(as.numeric(in.value))) }
if(input$heat_type!="" && !is.null(input$heat_type)){ in.value <- input$heat_type} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.type<-c(NA) } else {heat.type<-as.character(in.value) }
if(input$heat_cols!="" && !is.null(input$heat_cols)){ in.value <- input$heat_cols} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.cols<-c(NA) } else {heat.cols<-in.value }
if(input$heat_ylim!="" && !is.null(input$heat_ylim)){ in.value <- input$heat_ylim} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.ylim<-c(NULL) } else {heat.ylim<-as.numeric(in.value) }
if(input$heat_zlim!="" && !is.null(input$heat_zlim)){ in.value <- input$heat_zlim} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.zlim<-c(NULL) } else {heat.zlim<-as.numeric(in.value) }
if(input$heat_logscale!="" && !is.null(input$heat_logscale)){ in.value <- input$heat_logscale} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.logscale<-c(NA) } else {heat.logscale<-as.logical(in.value) }
if(input$heat_dir!="" && !is.null(input$heat_dir)){ in.value <- input$heat_dir} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.dir<-c(NA) } else {heat.dir<-as.character(in.value) }
if(input$heat_bw!="" && !is.null(input$heat_bw)){ in.value <- input$heat_bw} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.bw<-c(NA) } else {heat.bw<-as.numeric(in.value) }
if(input$heat_chop_data_gaps!="" && !is.null(input$heat_chop_data_gaps)){ in.value <- input$heat_chop_data_gaps} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.chop.data.gaps<-c(0) } else {heat.chop.data.gaps<-as.logical(in.value) }
if(input$heat_general_yaxis!="" && !is.null(input$heat_general_yaxis)){ in.value <- input$heat_general_yaxis} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){heat.general.yaxis<-c(NA) } else {heat.general.yaxis<-as.logical(in.value) }


# Parameters for snap_track

if(input$snap_relplotheight!="" && !is.null(input$snap_relplotheight)){ in.value <- input$snap_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.relplotheight<-c(NA) } else {snap.relplotheight<-as.numeric(in.value) }
if(input$snap_letter!="" && !is.null(input$snap_letter)){ in.value <- input$snap_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.letter<-c(NA) } else {snap.letter<-in.value }
if(input$snap_ylab!="" && !is.null(input$snap_ylab)){ in.value <- input$snap_ylab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.ylab<-c(NA) } else {snap.ylab<-in.value }
if(input$snap_cex_txt!="" && !is.null(input$snap_cex_txt)){ in.value <- input$snap_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.cex.txt<-c(NA) } else {snap.cex.txt<-as.numeric(in.value) }
if(input$snap_cex_axis!="" && !is.null(input$snap_cex_axis)){ in.value <- input$snap_cex_axis} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.cex.axis<-c(NA) } else {snap.cex.axis<-as.numeric(in.value) }
if(input$snap_snp_label_cex!="" && !is.null(input$snap_snp_label_cex)){ in.value <- input$snap_snp_label_cex} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.snp.label.cex<-c(NA) } else {snap.snp.label.cex<-as.numeric(in.value) }
if(input$snap_snp!="" && !is.null(input$snap_snp)){ in.value <- input$snap_snp} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.snp<-c(NA) } else {snap.snp<-in.value }
if(input$snap_snp_offset_x!="" && !is.null(input$snap_snp_offset_x)){ in.value <- input$snap_snp_offset_x} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.snp.offset.x<-c(0) } else {snap.snp.offset.x<-as.numeric(in.value) }
if(input$snap_snp_offset_y!="" && !is.null(input$snap_snp_offset_y)){ in.value <- input$snap_snp_offset_y} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.snp.offset.y<-c(0) } else {snap.snp.offset.y<-as.numeric(in.value) }
if(input$snap_r2_legend_x_offset!="" && !is.null(input$snap_r2_legend_x_offset)){ in.value <- input$snap_r2_legend_x_offset} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.r2.legend.x.offset<-c(NA) } else {snap.r2.legend.x.offset<-as.numeric(in.value) }
if(input$snap_chr!="" && !is.null(input$snap_chr)){ in.value <- input$snap_chr} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.chr<-c(NA) } else {snap.chr<-in.value }
if(input$snap_ylim!="" && !is.null(input$snap_ylim)){ in.value <- input$snap_ylim} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.ylim<-c(NA) } else {snap.ylim<-as.numeric(in.value) }
if(input$snap_snp2_file!="" && !is.null(input$snap_snp2_file)){ in.value <- input$snap_snp2_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_snap_snp2_file)==FALSE){snap.snp2.file<-c(NA) } else {snap.snp2.file<-in.value$datapath; }
if(input$snap_snp2_snp!="" && !is.null(input$snap_snp2_snp)){ in.value <- input$snap_snp2_snp} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.snp2.snp<-c(NA) } else {snap.snp2.snp<-as.character(in.value) }
if(input$snap_snp2_offset_x!="" && !is.null(input$snap_snp2_offset_x)){ in.value <- input$snap_snp2_offset_x} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.snp2.offset.x<-c(NA) } else {snap.snp2.offset.x<-as.numeric(in.value) }
if(input$snap_snp2_offset_y!="" && !is.null(input$snap_snp2_offset_y)){ in.value <- input$snap_snp2_offset_y} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){snap.snp2.offset.y<-c(NA) } else {snap.snp2.offset.y<-as.numeric(in.value) }


# Parameters for lines

if(input$lines_lwd!="" && !is.null(input$lines_lwd)){ in.value <- input$lines_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){lines.lwd<-c(NA) } else {lines.lwd<-as.numeric(in.value) }
if(input$lines_lty!="" && !is.null(input$lines_lty)){ in.value <- input$lines_lty} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){lines.lty<-c(NA) } else {lines.lty<-as.integer(round(as.numeric(in.value))) }
if(input$lines_col!="" && !is.null(input$lines_col)){ in.value <- input$lines_col} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){lines.col<-c(NA) } else {lines.col<-in.value }
if(input$lines_nonzoom!="" && !is.null(input$lines_nonzoom)){ in.value <- input$lines_nonzoom} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){lines.nonzoom<-c(TRUE) } else {lines.nonzoom<-as.logical(in.value) }
if(input$lines_zoom!="" && !is.null(input$lines_zoom)){ in.value <- input$lines_zoom} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){lines.zoom<-c(TRUE) } else {lines.zoom<-as.logical(in.value) }


# Parameters for graphics

if(input$width_plot!="" && !is.null(input$width_plot)){ in.value <- input$width_plot} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){width.plot<-c(NA) } else {width.plot<-as.numeric(in.value) }
if(input$height_plot!="" && !is.null(input$height_plot)){ in.value <- input$height_plot} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){height.plot<-c(NA) } else {height.plot<-as.numeric(in.value) }
if(input$left_margin!="" && !is.null(input$left_margin)){ in.value <- input$left_margin} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){left.margin<-c(NA) } else {left.margin<-as.numeric(in.value) }
if(input$right_margin!="" && !is.null(input$right_margin)){ in.value <- input$right_margin} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){right.margin<-c(NA) } else {right.margin<-as.numeric(in.value) }
if(input$title_top_margin!="" && !is.null(input$title_top_margin)){ in.value <- input$title_top_margin} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){title.top.margin<-c(NA) } else {title.top.margin<-as.numeric(in.value) }
if(input$top_margin!="" && !is.null(input$top_margin)){ in.value <- input$top_margin} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){top.margin<-c(NA) } else {top.margin<-as.numeric(in.value) }
if(input$bottom_margin!="" && !is.null(input$bottom_margin)){ in.value <- input$bottom_margin} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){bottom.margin<-c(NA) } else {bottom.margin<-as.numeric(in.value) }
if(input$sep_margin!="" && !is.null(input$sep_margin)){ in.value <- input$sep_margin} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){sep.margin<-c(0.05) } else {sep.margin<-as.numeric(in.value) }
if(input$ylab_las!="" && !is.null(input$ylab_las)){ in.value <- input$ylab_las} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){ylab.las<-c(NA) } else {ylab.las<-as.integer(round(as.numeric(in.value))) }
if(input$letter_cex!="" && !is.null(input$letter_cex)){ in.value <- input$letter_cex} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){letter.cex<-c(NA) } else {letter.cex<-as.numeric(in.value) }
if(input$xlab!="*" && !is.null(input$xlab)){ in.value <- input$xlab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){xlab<-c(NA) } else {xlab<-as.character(in.value) }
if(input$xlab_cex!="" && !is.null(input$xlab_cex)){ in.value <- input$xlab_cex} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){xlab.cex<-c(NA) } else {xlab.cex<-as.numeric(in.value) }
if(input$axis_relplotheight!="" && !is.null(input$axis_relplotheight)){ in.value <- input$axis_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.relplotheight<-c(1) } else {axis.relplotheight<-as.numeric(in.value) }
if(input$axis_line!="" && !is.null(input$axis_line)){ in.value <- input$axis_line} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.line<-c(NA) } else {axis.line<-as.numeric(in.value) }
if(input$xlab_line!="" && !is.null(input$xlab_line)){ in.value <- input$xlab_line} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){xlab.line<-c(1) } else {xlab.line<-as.numeric(in.value) }
if(input$axis_range_line!="" && !is.null(input$axis_range_line)){ in.value <- input$axis_range_line} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.range.line<-c(NA) } else {axis.range.line<-as.numeric(in.value) }
if(input$axis_tickmarks_line!="" && !is.null(input$axis_tickmarks_line)){ in.value <- input$axis_tickmarks_line} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.tickmarks.line<-c(NA) } else {axis.tickmarks.line<-as.numeric(in.value) }
if(input$axis_addlines!="" && !is.null(input$axis_addlines)){ in.value <- input$axis_addlines} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.addlines<-c(NA) } else {axis.addlines<-as.logical(in.value) }
if(input$x_lwd!="" && !is.null(input$x_lwd)){ in.value <- input$x_lwd} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){x.lwd<-c(NA) } else {x.lwd<-as.numeric(in.value) }
if(input$x_axis_format!="" && !is.null(input$x_axis_format)){ in.value <- input$x_axis_format} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){x.axis.format<-c(NA) } else {x.axis.format<-as.character(in.value) }
if(input$x_labels_show_unit!="" && !is.null(input$x_labels_show_unit)){ in.value <- input$x_labels_show_unit} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){x.labels.show.unit<-c(NA) } else {x.labels.show.unit<-as.logical(in.value) }
if(input$axis_tcl!="" && !is.null(input$axis_tcl)){ in.value <- input$axis_tcl} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.tcl<-c(NA) } else {axis.tcl<-as.numeric(in.value) }
if(input$axis_lend!="" && !is.null(input$axis_lend)){ in.value <- input$axis_lend} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.lend<-c(NA) } else {axis.lend<-as.integer(round(as.numeric(in.value))) }
if(input$axis_labels_slanted!="" && !is.null(input$axis_labels_slanted)){ in.value <- input$axis_labels_slanted} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.labels.slanted<-c(NA) } else {axis.labels.slanted<-as.logical(in.value) }
if(input$axis_labels_angle!="" && !is.null(input$axis_labels_angle)){ in.value <- input$axis_labels_angle} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){axis.labels.angle<-c(300) } else {axis.labels.angle<-as.numeric(in.value) }
if(input$gap_width!="" && !is.null(input$gap_width)){ in.value <- input$gap_width} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gap_width<-c(NA) } else {gap_width<-as.numeric(in.value) }
if(input$gaps_sizes_offset_x!="" && !is.null(input$gaps_sizes_offset_x)){ in.value <- input$gaps_sizes_offset_x} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){gaps.sizes.offset.x<-c(NA) } else {gaps.sizes.offset.x<-as.numeric(in.value) }
if(input$title_relplotheight!="" && !is.null(input$title_relplotheight)){ in.value <- input$title_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){title.relplotheight<-c(NA) } else {title.relplotheight<-as.numeric(in.value) }
if(input$title_letter!="" && !is.null(input$title_letter)){ in.value <- input$title_letter} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){title.letter<-c(NA) } else {title.letter<-as.character(in.value) }
if(input$title!="" && !is.null(input$title)){ in.value <- input$title} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){title<-c(NA) } else {title<-as.character(in.value) }
if(input$title_cex_txt!="" && !is.null(input$title_cex_txt)){ in.value <- input$title_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){title.cex.txt<-c(NA) } else {title.cex.txt<-as.numeric(in.value) }
if(input$title_pos!="" && !is.null(input$title_pos)){ in.value <- input$title_pos} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){title.pos<-c(NA) } else {title.pos<-as.numeric(in.value) }
if(input$title_chr!="" && !is.null(input$title_chr)){ in.value <- input$title_chr} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){title.chr<-c(NA) } else {title.chr<-as.numeric(in.value) }
if(input$title_showgaps!="" && !is.null(input$title_showgaps)){ in.value <- input$title_showgaps} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){title.showgaps<-c(NA) } else {title.showgaps<-as.logical(in.value) }
if(input$buffer_relplotheight!="" && !is.null(input$buffer_relplotheight)){ in.value <- input$buffer_relplotheight} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){buffer.relplotheight<-c(NA) } else {buffer.relplotheight<-as.numeric(in.value) }
if(input$max_chr!="" && !is.null(input$max_chr)){ in.value <- input$max_chr} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){max_chr<-c(22) } else {max_chr<-as.integer(round(as.numeric(in.value))) }


# Parameters for legends

if(input$legend_where!="" && !is.null(input$legend_where)){ in.value <- input$legend_where} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.where<-c(NA) } else {legend.where<-as.character(in.value) }
if(input$legend_width!="" && !is.null(input$legend_width)){ in.value <- input$legend_width} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.width<-c(NA) } else {legend.width<-as.numeric(in.value) }
if(input$legend_height!="" && !is.null(input$legend_height)){ in.value <- input$legend_height} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.height<-c(NA) } else {legend.height<-as.numeric(in.value) }
if(input$legend_rows!="" && !is.null(input$legend_rows)){ in.value <- input$legend_rows} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.rows<-c(NA) } else {legend.rows<-as.integer(round(as.numeric(in.value))) }
if(input$legend_cols!="" && !is.null(input$legend_cols)){ in.value <- input$legend_cols} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.cols<-c(NA) } else {legend.cols<-as.integer(round(as.numeric(in.value))) }
if(input$legend_assoc!="" && !is.null(input$legend_assoc)){ in.value <- input$legend_assoc} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.assoc<-c(NA) } else {legend.assoc<-as.logical(in.value) }
if(input$legend_hic!="" && !is.null(input$legend_hic)){ in.value <- input$legend_hic} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic<-c(NA) } else {legend.hic<-as.logical(in.value) }
if(input$legend_heat!="" && !is.null(input$legend_heat)){ in.value <- input$legend_heat} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.heat<-c(NA) } else {legend.heat<-as.logical(in.value) }
if(input$legend_annot!="" && !is.null(input$legend_annot)){ in.value <- input$legend_annot} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.annot<-c(NA) } else {legend.annot<-as.logical(in.value) }
if(input$legend_dix!="" && !is.null(input$legend_dix)){ in.value <- input$legend_dix} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.dix<-c(NA) } else {legend.dix<-as.logical(in.value) }
if(input$legend_mai!="" && !is.null(input$legend_mai)){ in.value <- input$legend_mai} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.mai<-c(NA) } else {legend.mai<-as.numeric(in.value) }
if(input$legend_cex_lab!="" && !is.null(input$legend_cex_lab)){ in.value <- input$legend_cex_lab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.cex.lab<-c(NA) } else {legend.cex.lab<-as.numeric(in.value) }
if(input$legend_cex_txt!="" && !is.null(input$legend_cex_txt)){ in.value <- input$legend_cex_txt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.cex.txt<-c(NA) } else {legend.cex.txt<-as.numeric(in.value) }
if(input$legend_adj!="" && !is.null(input$legend_adj)){ in.value <- input$legend_adj} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.adj<-c(NA) } else {legend.adj<-as.numeric(in.value) }
if(input$legend_annot_xlab!="" && !is.null(input$legend_annot_xlab)){ in.value <- input$legend_annot_xlab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.annot.xlab<-c(NA) } else {legend.annot.xlab<-as.character(in.value) }
if(input$legend_annot_file!="" && !is.null(input$legend_annot_file)){ in.value <- input$legend_annot_file} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1]) || as.logical(input$consider_legend_annot_file)==FALSE){legend.annot.file<-c(NA) } else {legend.annot.file<-in.value$datapath; }
if(input$legend_annot_nrow!="" && !is.null(input$legend_annot_nrow)){ in.value <- input$legend_annot_nrow} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.annot.nrow<-c(NA) } else {legend.annot.nrow<-as.integer(round(as.numeric(in.value))) }
if(input$legend_annot_xspace!="" && !is.null(input$legend_annot_xspace)){ in.value <- input$legend_annot_xspace} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.annot.xspace<-c(NA) } else {legend.annot.xspace<-as.character(in.value) }
if(input$legend_annot_rgb!="" && !is.null(input$legend_annot_rgb)){ in.value <- input$legend_annot_rgb} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.annot.rgb<-c(NA) } else {legend.annot.rgb<-as.logical(in.value) }
if(input$legend_annot_axline!="" && !is.null(input$legend_annot_axline)){ in.value <- input$legend_annot_axline} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.annot.axline<-c(NA) } else {legend.annot.axline<-as.numeric(in.value) }
if(input$legend_hic_xlab!="" && !is.null(input$legend_hic_xlab)){ in.value <- input$legend_hic_xlab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.xlab<-c(NA) } else {legend.hic.xlab<-as.character(in.value) }
if(input$legend_hic_labref!="" && !is.null(input$legend_hic_labref)){ in.value <- input$legend_hic_labref} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.labref<-c(NA) } else {legend.hic.labref<-as.character(in.value) }
if(input$legend_hic_labnonref!="" && !is.null(input$legend_hic_labnonref)){ in.value <- input$legend_hic_labnonref} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.labnonref<-c(NA) } else {legend.hic.labnonref<-as.character(in.value) }
if(input$legend_hic_lablink!="" && !is.null(input$legend_hic_lablink)){ in.value <- input$legend_hic_lablink} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.lablink<-c(NA) } else {legend.hic.lablink<-as.character(in.value) }
if(input$legend_hic_capt!="" && !is.null(input$legend_hic_capt)){ in.value <- input$legend_hic_capt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.capt<-c(NA) } else {legend.hic.capt<-as.logical(in.value) }
if(input$legend_hic_labcapt!="" && !is.null(input$legend_hic_labcapt)){ in.value <- input$legend_hic_labcapt} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.labcapt<-c(NA) } else {legend.hic.labcapt<-as.character(in.value) }
if(input$legend_hic_xlim!="" && !is.null(input$legend_hic_xlim)){ in.value <- input$legend_hic_xlim} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.xlim<-c(NA) } else {legend.hic.xlim<-as.numeric(in.value) }
if(input$legend_hic_hori!="" && !is.null(input$legend_hic_hori)){ in.value <- input$legend_hic_hori} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.hori<-c(NA) } else {legend.hic.hori<-as.logical(in.value) }
if(input$legend_hic_axline!="" && !is.null(input$legend_hic_axline)){ in.value <- input$legend_hic_axline} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.hic.axline<-c(NA) } else {legend.hic.axline<-as.numeric(in.value) }
if(input$legend_heat_xlab!="" && !is.null(input$legend_heat_xlab)){ in.value <- input$legend_heat_xlab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.heat.xlab<-c(NA) } else {legend.heat.xlab<-as.character(in.value) }
if(input$legend_heat_hori!="" && !is.null(input$legend_heat_hori)){ in.value <- input$legend_heat_hori} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.heat.hori<-c(NA) } else {legend.heat.hori<-as.logical(in.value) }
if(input$legend_heat_axline!="" && !is.null(input$legend_heat_axline)){ in.value <- input$legend_heat_axline} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.heat.axline<-c(NA) } else {legend.heat.axline<-as.numeric(in.value) }
if(input$legend_assoc_xlab!="" && !is.null(input$legend_assoc_xlab)){ in.value <- input$legend_assoc_xlab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.assoc.xlab<-c(NA) } else {legend.assoc.xlab<-as.character(in.value) }
if(input$legend_assoc_pvalticksround!="" && !is.null(input$legend_assoc_pvalticksround)){ in.value <- input$legend_assoc_pvalticksround} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.assoc.pvalticksround<-c(NA) } else {legend.assoc.pvalticksround<-as.integer(round(as.numeric(in.value))) }
if(input$legend_assoc_hori!="" && !is.null(input$legend_assoc_hori)){ in.value <- input$legend_assoc_hori} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.assoc.hori<-c(NA) } else {legend.assoc.hori<-as.logical(in.value) }
if(input$legend_assoc_axline!="" && !is.null(input$legend_assoc_axline)){ in.value <- input$legend_assoc_axline} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.assoc.axline<-c(NA) } else {legend.assoc.axline<-as.numeric(in.value) }
if(input$legend_dix_xlab!="" && !is.null(input$legend_dix_xlab)){ in.value <- input$legend_dix_xlab} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.dix.xlab<-c(NA) } else {legend.dix.xlab<-as.character(in.value) }
if(input$legend_dix_labpos!="" && !is.null(input$legend_dix_labpos)){ in.value <- input$legend_dix_labpos} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.dix.labpos<-c(NA) } else {legend.dix.labpos<-as.character(in.value) }
if(input$legend_dix_labneg!="" && !is.null(input$legend_dix_labneg)){ in.value <- input$legend_dix_labneg} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.dix.labneg<-c(NA) } else {legend.dix.labneg<-as.character(in.value) }
if(input$legend_dix_hori!="" && !is.null(input$legend_dix_hori)){ in.value <- input$legend_dix_hori} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.dix.hori<-c(NA) } else {legend.dix.hori<-as.logical(in.value) }
if(input$legend_dix_axline!="" && !is.null(input$legend_dix_axline)){ in.value <- input$legend_dix_axline} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.dix.axline<-c(NA) } else {legend.dix.axline<-as.numeric(in.value) }
if(input$legend_order!="" && !is.null(input$legend_order)){ in.value <- input$legend_order} else{ in.value <- NA }
	 if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}
	 if(is.na(in.value[1])){legend.order<-c('assoc','annot','hic','dix','heat') } else {legend.order<-as.character(in.value) }