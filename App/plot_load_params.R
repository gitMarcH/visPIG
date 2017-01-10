# Load the parameters from a text file and populate the web form.
 


# Parameters for files
if(length(grep("^regions.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^regions.file=",save.dat)],split="regions.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"regions_file", value=push.to.form)}
if(length(grep("^parameters.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^parameters.file=",save.dat)],split="parameters.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"parameters_file", value=push.to.form)}
if(length(grep("^annot.files=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.files=",save.dat)],split="annot.files="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_files", value=push.to.form)}
if(length(grep("^assoc.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^assoc.file=",save.dat)],split="assoc.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"assoc_file", value=push.to.form)}
if(length(grep("^dix.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.file=",save.dat)],split="dix.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_file", value=push.to.form)}
if(length(grep("^gerp.files=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.files=",save.dat)],split="gerp.files="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_files", value=push.to.form)}
if(length(grep("^heat.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.file=",save.dat)],split="heat.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_file", value=push.to.form)}
if(length(grep("^hic.files=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.files=",save.dat)],split="hic.files="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_files", value=push.to.form)}
if(length(grep("^int.files=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.files=",save.dat)],split="int.files="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_files", value=push.to.form)}
if(length(grep("^phast.files=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.files=",save.dat)],split="phast.files="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_files", value=push.to.form)}
if(length(grep("^snap.data.files=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.data.files=",save.dat)],split="snap.data.files="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_data_files", value=push.to.form)}
if(length(grep("^snap.rate.files=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.rate.files=",save.dat)],split="snap.rate.files="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_rate_files", value=push.to.form)}
if(length(grep("^feat.files=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.files=",save.dat)],split="feat.files="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_files", value=push.to.form)}
if(length(grep("^lines.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^lines.file=",save.dat)],split="lines.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"lines_file", value=push.to.form)}
if(length(grep("^gene.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.file=",save.dat)],split="gene.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_file", value=push.to.form)}
if(length(grep("^chrsizes.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^chrsizes.file=",save.dat)],split="chrsizes.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"chrsizes_file", value=push.to.form)}
if(length(grep("^track.order.top.down=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^track.order.top.down=",save.dat)],split="track.order.top.down="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"track_order_top_down", value=push.to.form)}



# Parameters for gene_track
if(length(grep("^gene.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.relplotheight=",save.dat)],split="gene.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_relplotheight", value=push.to.form)}
if(length(grep("^gene.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.letter=",save.dat)],split="gene.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_letter", value=push.to.form)}
if(length(grep("^gene.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.ylab=",save.dat)],split="gene.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_ylab", value=push.to.form)}
if(length(grep("^gene.ylab.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.ylab.cex.txt=",save.dat)],split="gene.ylab.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_ylab_cex_txt", value=push.to.form)}
if(length(grep("^gene.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.col=",save.dat)],split="gene.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_col", value=push.to.form)}
if(length(grep("^gene.igs=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.igs=",save.dat)],split="gene.igs="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_igs", value=push.to.form)}
if(length(grep("^gene.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.cex.txt=",save.dat)],split="gene.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_cex_txt", value=push.to.form)}
if(length(grep("^gene.cex.arrows=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.cex.arrows=",save.dat)],split="gene.cex.arrows="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_cex_arrows", value=push.to.form)}
if(length(grep("^gene.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gene.lwd=",save.dat)],split="gene.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gene_lwd", value=push.to.form)}



# Parameters for gerp_track
if(length(grep("^gerp.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.relplotheight=",save.dat)],split="gerp.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_relplotheight", value=push.to.form)}
if(length(grep("^gerp.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.letter=",save.dat)],split="gerp.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_letter", value=push.to.form)}
if(length(grep("^gerp.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.ylab=",save.dat)],split="gerp.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_ylab", value=push.to.form)}
if(length(grep("^gerp.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.cex.txt=",save.dat)],split="gerp.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_cex_txt", value=push.to.form)}
if(length(grep("^gerp.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.lwd=",save.dat)],split="gerp.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_lwd", value=push.to.form)}
if(length(grep("^gerp.ylim=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.ylim=",save.dat)],split="gerp.ylim="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_ylim", value=push.to.form)}
if(length(grep("^gerp.noblocks=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.noblocks=",save.dat)],split="gerp.noblocks="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_noblocks", value=push.to.form)}
if(length(grep("^gerp.filter.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.filter.file=",save.dat)],split="gerp.filter.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_filter_file", value=push.to.form)}
if(length(grep("^gerp.mai.top=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.mai.top=",save.dat)],split="gerp.mai.top="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_mai_top", value=push.to.form)}
if(length(grep("^gerp.mai.bot=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gerp.mai.bot=",save.dat)],split="gerp.mai.bot="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gerp_mai_bot", value=push.to.form)}



# Parameters for phast_track
if(length(grep("^phast.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.relplotheight=",save.dat)],split="phast.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_relplotheight", value=push.to.form)}
if(length(grep("^phast.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.letter=",save.dat)],split="phast.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_letter", value=push.to.form)}
if(length(grep("^phast.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.ylab=",save.dat)],split="phast.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_ylab", value=push.to.form)}
if(length(grep("^phast.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.cex.txt=",save.dat)],split="phast.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_cex_txt", value=push.to.form)}
if(length(grep("^phast.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.lwd=",save.dat)],split="phast.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_lwd", value=push.to.form)}
if(length(grep("^phast.ylim=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.ylim=",save.dat)],split="phast.ylim="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_ylim", value=push.to.form)}
if(length(grep("^phast.noblocks=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.noblocks=",save.dat)],split="phast.noblocks="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_noblocks", value=push.to.form)}
if(length(grep("^phast.filter.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^phast.filter.file=",save.dat)],split="phast.filter.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"phast_filter_file", value=push.to.form)}



# Parameters for annot_track
if(length(grep("^annot.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.relplotheight=",save.dat)],split="annot.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_relplotheight", value=push.to.form)}
if(length(grep("^annot.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.letter=",save.dat)],split="annot.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_letter", value=push.to.form)}
if(length(grep("^annot.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.ylab=",save.dat)],split="annot.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_ylab", value=push.to.form)}
if(length(grep("^annot.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.cex.txt=",save.dat)],split="annot.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_cex_txt", value=push.to.form)}
if(length(grep("^annot.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.lwd=",save.dat)],split="annot.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_lwd", value=push.to.form)}
if(length(grep("^annot.bgclass=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.bgclass=",save.dat)],split="annot.bgclass="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_bgclass", value=push.to.form)}
if(length(grep("^annot.mai.top=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.mai.top=",save.dat)],split="annot.mai.top="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_mai_top", value=push.to.form)}
if(length(grep("^annot.mai.bot=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^annot.mai.bot=",save.dat)],split="annot.mai.bot="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"annot_mai_bot", value=push.to.form)}



# Parameters for hic_track
if(length(grep("^hic.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.relplotheight=",save.dat)],split="hic.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_relplotheight", value=push.to.form)}
if(length(grep("^hic.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.letter=",save.dat)],split="hic.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_letter", value=push.to.form)}
if(length(grep("^hic.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.ylab=",save.dat)],split="hic.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_ylab", value=push.to.form)}
if(length(grep("^hic.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.cex.txt=",save.dat)],split="hic.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_cex_txt", value=push.to.form)}
if(length(grep("^hic.style=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.style=",save.dat)],split="hic.style="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_style", value=push.to.form)}
if(length(grep("^hic.plotThroughGaps=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.plotThroughGaps=",save.dat)],split="hic.plotThroughGaps="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_plotThroughGaps", value=push.to.form)}
if(length(grep("^hic.colThroughGaps=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.colThroughGaps=",save.dat)],split="hic.colThroughGaps="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_colThroughGaps", value=push.to.form)}
if(length(grep("^hic.colref=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.colref=",save.dat)],split="hic.colref="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_colref", value=push.to.form)}
if(length(grep("^hic.colnonref=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.colnonref=",save.dat)],split="hic.colnonref="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_colnonref", value=push.to.form)}
if(length(grep("^hic.stack.ylim.bot=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.stack.ylim.bot=",save.dat)],split="hic.stack.ylim.bot="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_stack_ylim_bot", value=push.to.form)}
if(length(grep("^hic.stack.ylim.top=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.stack.ylim.top=",save.dat)],split="hic.stack.ylim.top="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_stack_ylim_top", value=push.to.form)}
if(length(grep("^hic.stack.style=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.stack.style=",save.dat)],split="hic.stack.style="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_stack_style", value=push.to.form)}
if(length(grep("^hic.stack.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.stack.col=",save.dat)],split="hic.stack.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_stack_col", value=push.to.form)}
if(length(grep("^hic.stack.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.stack.lwd=",save.dat)],split="hic.stack.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_stack_lwd", value=push.to.form)}
if(length(grep("^hic.arches.twist=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.twist=",save.dat)],split="hic.arches.twist="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_twist", value=push.to.form)}
if(length(grep("^hic.arches.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.lwd=",save.dat)],split="hic.arches.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_lwd", value=push.to.form)}
if(length(grep("^hic.arches.nsegments=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.nsegments=",save.dat)],split="hic.arches.nsegments="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_nsegments", value=push.to.form)}
if(length(grep("^hic.arches.neglog10=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.neglog10=",save.dat)],split="hic.arches.neglog10="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_neglog10", value=push.to.form)}
if(length(grep("^hic.arches.ylim.lower=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.ylim.lower=",save.dat)],split="hic.arches.ylim.lower="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_ylim_lower", value=push.to.form)}
if(length(grep("^hic.arches.ylim.upper=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.ylim.upper=",save.dat)],split="hic.arches.ylim.upper="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_ylim_upper", value=push.to.form)}
if(length(grep("^hic.arches.yaxis=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.yaxis=",save.dat)],split="hic.arches.yaxis="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_yaxis", value=push.to.form)}
if(length(grep("^hic.arches.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.col=",save.dat)],split="hic.arches.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_col", value=push.to.form)}
if(length(grep("^hic.arches.varicol=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.varicol=",save.dat)],split="hic.arches.varicol="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_varicol", value=push.to.form)}
if(length(grep("^hic.arches.dir=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.arches.dir=",save.dat)],split="hic.arches.dir="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_arches_dir", value=push.to.form)}
if(length(grep("^hic.plotThroughGapsMaxY=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.plotThroughGapsMaxY=",save.dat)],split="hic.plotThroughGapsMaxY="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_plotThroughGapsMaxY", value=push.to.form)}
if(length(grep("^hic.capt.region=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.capt.region=",save.dat)],split="hic.capt.region="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_capt_region", value=push.to.form)}
if(length(grep("^hic.capt.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.capt.col=",save.dat)],split="hic.capt.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_capt_col", value=push.to.form)}
if(length(grep("^hic.capt.density=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.capt.density=",save.dat)],split="hic.capt.density="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_capt_density", value=push.to.form)}
if(length(grep("^hic.capt.lines.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^hic.capt.lines.lwd=",save.dat)],split="hic.capt.lines.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"hic_capt_lines_lwd", value=push.to.form)}



# Parameters for int_track
if(length(grep("^int.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.relplotheight=",save.dat)],split="int.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_relplotheight", value=push.to.form)}
if(length(grep("^int.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.letter=",save.dat)],split="int.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_letter", value=push.to.form)}
if(length(grep("^int.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.ylab=",save.dat)],split="int.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_ylab", value=push.to.form)}
if(length(grep("^int.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.cex.txt=",save.dat)],split="int.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_cex_txt", value=push.to.form)}
if(length(grep("^int.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.lwd=",save.dat)],split="int.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_lwd", value=push.to.form)}
if(length(grep("^int.ylim.lower=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.ylim.lower=",save.dat)],split="int.ylim.lower="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_ylim_lower", value=push.to.form)}
if(length(grep("^int.ylim.upper=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.ylim.upper=",save.dat)],split="int.ylim.upper="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_ylim_upper", value=push.to.form)}
if(length(grep("^int.style=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.style=",save.dat)],split="int.style="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_style", value=push.to.form)}
if(length(grep("^int.smooth.window=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.smooth.window=",save.dat)],split="int.smooth.window="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_smooth_window", value=push.to.form)}
if(length(grep("^int.smooth.method=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.smooth.method=",save.dat)],split="int.smooth.method="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_smooth_method", value=push.to.form)}
if(length(grep("^int.refbox=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.refbox=",save.dat)],split="int.refbox="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_refbox", value=push.to.form)}
if(length(grep("^int.refbox.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.refbox.col=",save.dat)],split="int.refbox.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_refbox_col", value=push.to.form)}
if(length(grep("^int.refbox.density=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.refbox.density=",save.dat)],split="int.refbox.density="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_refbox_density", value=push.to.form)}
if(length(grep("^int.refbox.lines.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.refbox.lines.lwd=",save.dat)],split="int.refbox.lines.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_refbox_lines_lwd", value=push.to.form)}
if(length(grep("^int.ref2box=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.ref2box=",save.dat)],split="int.ref2box="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_ref2box", value=push.to.form)}
if(length(grep("^int.ref2box.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.ref2box.col=",save.dat)],split="int.ref2box.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_ref2box_col", value=push.to.form)}
if(length(grep("^int.ref2box.density=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.ref2box.density=",save.dat)],split="int.ref2box.density="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_ref2box_density", value=push.to.form)}
if(length(grep("^int.ref2box.lines.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^int.ref2box.lines.lwd=",save.dat)],split="int.ref2box.lines.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"int_ref2box_lines_lwd", value=push.to.form)}



# Parameters for assoc_track
if(length(grep("^assoc.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^assoc.relplotheight=",save.dat)],split="assoc.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"assoc_relplotheight", value=push.to.form)}
if(length(grep("^assoc.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^assoc.letter=",save.dat)],split="assoc.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"assoc_letter", value=push.to.form)}
if(length(grep("^assoc.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^assoc.ylab=",save.dat)],split="assoc.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"assoc_ylab", value=push.to.form)}
if(length(grep("^assoc.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^assoc.cex.txt=",save.dat)],split="assoc.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"assoc_cex_txt", value=push.to.form)}
if(length(grep("^assoc.cols=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^assoc.cols=",save.dat)],split="assoc.cols="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"assoc_cols", value=push.to.form)}
if(length(grep("^assoc.pvalscale=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^assoc.pvalscale=",save.dat)],split="assoc.pvalscale="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"assoc_pvalscale", value=push.to.form)}
if(length(grep("^assoc.lwd.bars=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^assoc.lwd.bars=",save.dat)],split="assoc.lwd.bars="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"assoc_lwd_bars", value=push.to.form)}



# Parameters for dix_track
if(length(grep("^dix.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.relplotheight=",save.dat)],split="dix.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_relplotheight", value=push.to.form)}
if(length(grep("^dix.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.letter=",save.dat)],split="dix.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_letter", value=push.to.form)}
if(length(grep("^dix.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.ylab=",save.dat)],split="dix.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_ylab", value=push.to.form)}
if(length(grep("^dix.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.cex.txt=",save.dat)],split="dix.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_cex_txt", value=push.to.form)}
if(length(grep("^dix.col.pos=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.col.pos=",save.dat)],split="dix.col.pos="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_col_pos", value=push.to.form)}
if(length(grep("^dix.col.neg=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.col.neg=",save.dat)],split="dix.col.neg="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_col_neg", value=push.to.form)}
if(length(grep("^dix.col.rgb=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.col.rgb=",save.dat)],split="dix.col.rgb="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_col_rgb", value=push.to.form)}
if(length(grep("^dix.cols.toplot=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.cols.toplot=",save.dat)],split="dix.cols.toplot="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_cols_toplot", value=push.to.form)}
if(length(grep("^dix.ylim.lower=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.ylim.lower=",save.dat)],split="dix.ylim.lower="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_ylim_lower", value=push.to.form)}
if(length(grep("^dix.ylim.upper=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^dix.ylim.upper=",save.dat)],split="dix.ylim.upper="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"dix_ylim_upper", value=push.to.form)}



# Parameters for zoom
if(length(grep("^zoom=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom=",save.dat)],split="zoom="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom", value=push.to.form)}
if(length(grep("^zoom.chr=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.chr=",save.dat)],split="zoom.chr="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_chr", value=push.to.form)}
if(length(grep("^zoom.SP=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.SP=",save.dat)],split="zoom.SP="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_SP", value=push.to.form)}
if(length(grep("^zoom.EP=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.EP=",save.dat)],split="zoom.EP="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_EP", value=push.to.form)}
if(length(grep("^zoom.track.order.top.down=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.track.order.top.down=",save.dat)],split="zoom.track.order.top.down="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_track_order_top_down", value=push.to.form)}
if(length(grep("^zoom.vertical.spacing=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.vertical.spacing=",save.dat)],split="zoom.vertical.spacing="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_vertical_spacing", value=push.to.form)}
if(length(grep("^zoom.lines.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.lines.col=",save.dat)],split="zoom.lines.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_lines_col", value=push.to.form)}
if(length(grep("^zoom.lines.alpha=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.lines.alpha=",save.dat)],split="zoom.lines.alpha="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_lines_alpha", value=push.to.form)}
if(length(grep("^zoom.lines.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.lines.lwd=",save.dat)],split="zoom.lines.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_lines_lwd", value=push.to.form)}
if(length(grep("^zoom.lines.lty=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.lines.lty=",save.dat)],split="zoom.lines.lty="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_lines_lty", value=push.to.form)}
if(length(grep("^zoom.axis.format=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.axis.format=",save.dat)],split="zoom.axis.format="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_axis_format", value=push.to.form)}
if(length(grep("^zoom.labels.show.unit=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.labels.show.unit=",save.dat)],split="zoom.labels.show.unit="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_labels_show_unit", value=push.to.form)}
if(length(grep("^zoom.axis.tcl=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.axis.tcl=",save.dat)],split="zoom.axis.tcl="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_axis_tcl", value=push.to.form)}
if(length(grep("^zoom.axis.lend=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.axis.lend=",save.dat)],split="zoom.axis.lend="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_axis_lend", value=push.to.form)}
if(length(grep("^zoom.x.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.x.lwd=",save.dat)],split="zoom.x.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_x_lwd", value=push.to.form)}
if(length(grep("^zoom.xlab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.xlab=",save.dat)],split="zoom.xlab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_xlab", value=push.to.form)}
if(length(grep("^zoom.x.labels.up=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^zoom.x.labels.up=",save.dat)],split="zoom.x.labels.up="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"zoom_x_labels_up", value=push.to.form)}



# Parameters for feat_track
if(length(grep("^feat.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.relplotheight=",save.dat)],split="feat.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_relplotheight", value=push.to.form)}
if(length(grep("^feat.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.letter=",save.dat)],split="feat.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_letter", value=push.to.form)}
if(length(grep("^feat.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.ylab=",save.dat)],split="feat.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_ylab", value=push.to.form)}
if(length(grep("^feat.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.cex.txt=",save.dat)],split="feat.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_cex_txt", value=push.to.form)}
if(length(grep("^feat.cex.lab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.cex.lab=",save.dat)],split="feat.cex.lab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_cex_lab", value=push.to.form)}
if(length(grep("^feat.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.lwd=",save.dat)],split="feat.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_lwd", value=push.to.form)}
if(length(grep("^feat.height=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.height=",save.dat)],split="feat.height="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_height", value=push.to.form)}
if(length(grep("^feat.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.col=",save.dat)],split="feat.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_col", value=push.to.form)}
if(length(grep("^feat.textpos=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.textpos=",save.dat)],split="feat.textpos="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_textpos", value=push.to.form)}
if(length(grep("^feat.txt.vert.offset=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.txt.vert.offset=",save.dat)],split="feat.txt.vert.offset="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_txt_vert_offset", value=push.to.form)}
if(length(grep("^feat.txt.hori.offset=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^feat.txt.hori.offset=",save.dat)],split="feat.txt.hori.offset="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"feat_txt_hori_offset", value=push.to.form)}



# Parameters for heat_track
if(length(grep("^heat.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.relplotheight=",save.dat)],split="heat.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_relplotheight", value=push.to.form)}
if(length(grep("^heat.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.letter=",save.dat)],split="heat.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_letter", value=push.to.form)}
if(length(grep("^heat.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.ylab=",save.dat)],split="heat.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_ylab", value=push.to.form)}
if(length(grep("^heat.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.cex.txt=",save.dat)],split="heat.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_cex_txt", value=push.to.form)}
if(length(grep("^heat.chr=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.chr=",save.dat)],split="heat.chr="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_chr", value=push.to.form)}
if(length(grep("^heat.SP=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.SP=",save.dat)],split="heat.SP="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_SP", value=push.to.form)}
if(length(grep("^heat.EP=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.EP=",save.dat)],split="heat.EP="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_EP", value=push.to.form)}
if(length(grep("^heat.type=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.type=",save.dat)],split="heat.type="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_type", value=push.to.form)}
if(length(grep("^heat.cols=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.cols=",save.dat)],split="heat.cols="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_cols", value=push.to.form)}
if(length(grep("^heat.ylim=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.ylim=",save.dat)],split="heat.ylim="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_ylim", value=push.to.form)}
if(length(grep("^heat.zlim=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.zlim=",save.dat)],split="heat.zlim="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_zlim", value=push.to.form)}
if(length(grep("^heat.logscale=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.logscale=",save.dat)],split="heat.logscale="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_logscale", value=push.to.form)}
if(length(grep("^heat.dir=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.dir=",save.dat)],split="heat.dir="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_dir", value=push.to.form)}
if(length(grep("^heat.bw=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.bw=",save.dat)],split="heat.bw="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_bw", value=push.to.form)}
if(length(grep("^heat.chop.data.gaps=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.chop.data.gaps=",save.dat)],split="heat.chop.data.gaps="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_chop_data_gaps", value=push.to.form)}
if(length(grep("^heat.general.yaxis=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^heat.general.yaxis=",save.dat)],split="heat.general.yaxis="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"heat_general_yaxis", value=push.to.form)}



# Parameters for snap_track
if(length(grep("^snap.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.relplotheight=",save.dat)],split="snap.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_relplotheight", value=push.to.form)}
if(length(grep("^snap.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.letter=",save.dat)],split="snap.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_letter", value=push.to.form)}
if(length(grep("^snap.ylab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.ylab=",save.dat)],split="snap.ylab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_ylab", value=push.to.form)}
if(length(grep("^snap.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.cex.txt=",save.dat)],split="snap.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_cex_txt", value=push.to.form)}
if(length(grep("^snap.cex.axis=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.cex.axis=",save.dat)],split="snap.cex.axis="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_cex_axis", value=push.to.form)}
if(length(grep("^snap.snp.label.cex=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.snp.label.cex=",save.dat)],split="snap.snp.label.cex="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_snp_label_cex", value=push.to.form)}
if(length(grep("^snap.snp=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.snp=",save.dat)],split="snap.snp="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_snp", value=push.to.form)}
if(length(grep("^snap.snp.offset.x=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.snp.offset.x=",save.dat)],split="snap.snp.offset.x="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_snp_offset_x", value=push.to.form)}
if(length(grep("^snap.snp.offset.y=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.snp.offset.y=",save.dat)],split="snap.snp.offset.y="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_snp_offset_y", value=push.to.form)}
if(length(grep("^snap.r2.legend.x.offset=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.r2.legend.x.offset=",save.dat)],split="snap.r2.legend.x.offset="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_r2_legend_x_offset", value=push.to.form)}
if(length(grep("^snap.chr=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.chr=",save.dat)],split="snap.chr="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_chr", value=push.to.form)}
if(length(grep("^snap.ylim=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.ylim=",save.dat)],split="snap.ylim="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_ylim", value=push.to.form)}
if(length(grep("^snap.snp2.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.snp2.file=",save.dat)],split="snap.snp2.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_snp2_file", value=push.to.form)}
if(length(grep("^snap.snp2.snp=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.snp2.snp=",save.dat)],split="snap.snp2.snp="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_snp2_snp", value=push.to.form)}
if(length(grep("^snap.snp2.offset.x=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.snp2.offset.x=",save.dat)],split="snap.snp2.offset.x="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_snp2_offset_x", value=push.to.form)}
if(length(grep("^snap.snp2.offset.y=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^snap.snp2.offset.y=",save.dat)],split="snap.snp2.offset.y="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"snap_snp2_offset_y", value=push.to.form)}



# Parameters for lines
if(length(grep("^lines.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^lines.lwd=",save.dat)],split="lines.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"lines_lwd", value=push.to.form)}
if(length(grep("^lines.lty=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^lines.lty=",save.dat)],split="lines.lty="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"lines_lty", value=push.to.form)}
if(length(grep("^lines.col=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^lines.col=",save.dat)],split="lines.col="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"lines_col", value=push.to.form)}
if(length(grep("^lines.nonzoom=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^lines.nonzoom=",save.dat)],split="lines.nonzoom="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"lines_nonzoom", value=push.to.form)}
if(length(grep("^lines.zoom=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^lines.zoom=",save.dat)],split="lines.zoom="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"lines_zoom", value=push.to.form)}



# Parameters for graphics
if(length(grep("^width.plot=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^width.plot=",save.dat)],split="width.plot="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"width_plot", value=push.to.form)}
if(length(grep("^height.plot=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^height.plot=",save.dat)],split="height.plot="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"height_plot", value=push.to.form)}
if(length(grep("^left.margin=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^left.margin=",save.dat)],split="left.margin="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"left_margin", value=push.to.form)}
if(length(grep("^right.margin=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^right.margin=",save.dat)],split="right.margin="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"right_margin", value=push.to.form)}
if(length(grep("^title.top.margin=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^title.top.margin=",save.dat)],split="title.top.margin="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"title_top_margin", value=push.to.form)}
if(length(grep("^top.margin=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^top.margin=",save.dat)],split="top.margin="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"top_margin", value=push.to.form)}
if(length(grep("^bottom.margin=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^bottom.margin=",save.dat)],split="bottom.margin="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"bottom_margin", value=push.to.form)}
if(length(grep("^sep.margin=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^sep.margin=",save.dat)],split="sep.margin="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"sep_margin", value=push.to.form)}
if(length(grep("^ylab.las=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^ylab.las=",save.dat)],split="ylab.las="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"ylab_las", value=push.to.form)}
if(length(grep("^letter.cex=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^letter.cex=",save.dat)],split="letter.cex="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"letter_cex", value=push.to.form)}
if(length(grep("^xlab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^xlab=",save.dat)],split="xlab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"xlab", value=push.to.form)}
if(length(grep("^xlab.cex=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^xlab.cex=",save.dat)],split="xlab.cex="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"xlab_cex", value=push.to.form)}
if(length(grep("^axis.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.relplotheight=",save.dat)],split="axis.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_relplotheight", value=push.to.form)}
if(length(grep("^axis.line=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.line=",save.dat)],split="axis.line="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_line", value=push.to.form)}
if(length(grep("^xlab.line=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^xlab.line=",save.dat)],split="xlab.line="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"xlab_line", value=push.to.form)}
if(length(grep("^axis.range.line=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.range.line=",save.dat)],split="axis.range.line="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_range_line", value=push.to.form)}
if(length(grep("^axis.tickmarks.line=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.tickmarks.line=",save.dat)],split="axis.tickmarks.line="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_tickmarks_line", value=push.to.form)}
if(length(grep("^axis.addlines=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.addlines=",save.dat)],split="axis.addlines="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_addlines", value=push.to.form)}
if(length(grep("^x.lwd=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^x.lwd=",save.dat)],split="x.lwd="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"x_lwd", value=push.to.form)}
if(length(grep("^x.axis.format=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^x.axis.format=",save.dat)],split="x.axis.format="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"x_axis_format", value=push.to.form)}
if(length(grep("^x.labels.show.unit=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^x.labels.show.unit=",save.dat)],split="x.labels.show.unit="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"x_labels_show_unit", value=push.to.form)}
if(length(grep("^axis.tcl=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.tcl=",save.dat)],split="axis.tcl="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_tcl", value=push.to.form)}
if(length(grep("^axis.lend=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.lend=",save.dat)],split="axis.lend="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_lend", value=push.to.form)}
if(length(grep("^axis.labels.slanted=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.labels.slanted=",save.dat)],split="axis.labels.slanted="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_labels_slanted", value=push.to.form)}
if(length(grep("^axis.labels.angle=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^axis.labels.angle=",save.dat)],split="axis.labels.angle="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"axis_labels_angle", value=push.to.form)}
if(length(grep("^gap_width=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gap_width=",save.dat)],split="gap_width="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gap_width", value=push.to.form)}
if(length(grep("^gaps.sizes.offset.x=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^gaps.sizes.offset.x=",save.dat)],split="gaps.sizes.offset.x="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"gaps_sizes_offset_x", value=push.to.form)}
if(length(grep("^title.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^title.relplotheight=",save.dat)],split="title.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"title_relplotheight", value=push.to.form)}
if(length(grep("^title.letter=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^title.letter=",save.dat)],split="title.letter="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"title_letter", value=push.to.form)}
if(length(grep("^title=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^title=",save.dat)],split="title="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"title", value=push.to.form)}
if(length(grep("^title.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^title.cex.txt=",save.dat)],split="title.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"title_cex_txt", value=push.to.form)}
if(length(grep("^title.pos=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^title.pos=",save.dat)],split="title.pos="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"title_pos", value=push.to.form)}
if(length(grep("^title.chr=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^title.chr=",save.dat)],split="title.chr="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"title_chr", value=push.to.form)}
if(length(grep("^title.showgaps=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^title.showgaps=",save.dat)],split="title.showgaps="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"title_showgaps", value=push.to.form)}
if(length(grep("^buffer.relplotheight=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^buffer.relplotheight=",save.dat)],split="buffer.relplotheight="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"buffer_relplotheight", value=push.to.form)}
if(length(grep("^max_chr=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^max_chr=",save.dat)],split="max_chr="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"max_chr", value=push.to.form)}



# Parameters for legends
if(length(grep("^legend.where=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.where=",save.dat)],split="legend.where="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_where", value=push.to.form)}
if(length(grep("^legend.width=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.width=",save.dat)],split="legend.width="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_width", value=push.to.form)}
if(length(grep("^legend.height=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.height=",save.dat)],split="legend.height="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_height", value=push.to.form)}
if(length(grep("^legend.rows=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.rows=",save.dat)],split="legend.rows="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_rows", value=push.to.form)}
if(length(grep("^legend.cols=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.cols=",save.dat)],split="legend.cols="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_cols", value=push.to.form)}
if(length(grep("^legend.assoc=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.assoc=",save.dat)],split="legend.assoc="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_assoc", value=push.to.form)}
if(length(grep("^legend.hic=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic=",save.dat)],split="legend.hic="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic", value=push.to.form)}
if(length(grep("^legend.heat=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.heat=",save.dat)],split="legend.heat="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_heat", value=push.to.form)}
if(length(grep("^legend.annot=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.annot=",save.dat)],split="legend.annot="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_annot", value=push.to.form)}
if(length(grep("^legend.dix=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.dix=",save.dat)],split="legend.dix="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_dix", value=push.to.form)}
if(length(grep("^legend.mai=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.mai=",save.dat)],split="legend.mai="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_mai", value=push.to.form)}
if(length(grep("^legend.cex.lab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.cex.lab=",save.dat)],split="legend.cex.lab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_cex_lab", value=push.to.form)}
if(length(grep("^legend.cex.txt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.cex.txt=",save.dat)],split="legend.cex.txt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_cex_txt", value=push.to.form)}
if(length(grep("^legend.adj=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.adj=",save.dat)],split="legend.adj="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_adj", value=push.to.form)}
if(length(grep("^legend.annot.xlab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.annot.xlab=",save.dat)],split="legend.annot.xlab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_annot_xlab", value=push.to.form)}
if(length(grep("^legend.annot.file=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.annot.file=",save.dat)],split="legend.annot.file="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_annot_file", value=push.to.form)}
if(length(grep("^legend.annot.nrow=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.annot.nrow=",save.dat)],split="legend.annot.nrow="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_annot_nrow", value=push.to.form)}
if(length(grep("^legend.annot.xspace=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.annot.xspace=",save.dat)],split="legend.annot.xspace="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_annot_xspace", value=push.to.form)}
if(length(grep("^legend.annot.rgb=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.annot.rgb=",save.dat)],split="legend.annot.rgb="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_annot_rgb", value=push.to.form)}
if(length(grep("^legend.annot.axline=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.annot.axline=",save.dat)],split="legend.annot.axline="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_annot_axline", value=push.to.form)}
if(length(grep("^legend.hic.xlab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.xlab=",save.dat)],split="legend.hic.xlab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_xlab", value=push.to.form)}
if(length(grep("^legend.hic.labref=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.labref=",save.dat)],split="legend.hic.labref="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_labref", value=push.to.form)}
if(length(grep("^legend.hic.labnonref=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.labnonref=",save.dat)],split="legend.hic.labnonref="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_labnonref", value=push.to.form)}
if(length(grep("^legend.hic.lablink=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.lablink=",save.dat)],split="legend.hic.lablink="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_lablink", value=push.to.form)}
if(length(grep("^legend.hic.capt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.capt=",save.dat)],split="legend.hic.capt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_capt", value=push.to.form)}
if(length(grep("^legend.hic.labcapt=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.labcapt=",save.dat)],split="legend.hic.labcapt="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_labcapt", value=push.to.form)}
if(length(grep("^legend.hic.xlim=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.xlim=",save.dat)],split="legend.hic.xlim="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_xlim", value=push.to.form)}
if(length(grep("^legend.hic.hori=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.hori=",save.dat)],split="legend.hic.hori="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_hori", value=push.to.form)}
if(length(grep("^legend.hic.axline=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.hic.axline=",save.dat)],split="legend.hic.axline="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_hic_axline", value=push.to.form)}
if(length(grep("^legend.heat.xlab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.heat.xlab=",save.dat)],split="legend.heat.xlab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_heat_xlab", value=push.to.form)}
if(length(grep("^legend.heat.hori=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.heat.hori=",save.dat)],split="legend.heat.hori="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_heat_hori", value=push.to.form)}
if(length(grep("^legend.heat.axline=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.heat.axline=",save.dat)],split="legend.heat.axline="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_heat_axline", value=push.to.form)}
if(length(grep("^legend.assoc.xlab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.assoc.xlab=",save.dat)],split="legend.assoc.xlab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_assoc_xlab", value=push.to.form)}
if(length(grep("^legend.assoc.pvalticksround=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.assoc.pvalticksround=",save.dat)],split="legend.assoc.pvalticksround="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_assoc_pvalticksround", value=push.to.form)}
if(length(grep("^legend.assoc.hori=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.assoc.hori=",save.dat)],split="legend.assoc.hori="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_assoc_hori", value=push.to.form)}
if(length(grep("^legend.assoc.axline=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.assoc.axline=",save.dat)],split="legend.assoc.axline="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_assoc_axline", value=push.to.form)}
if(length(grep("^legend.dix.xlab=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.dix.xlab=",save.dat)],split="legend.dix.xlab="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_dix_xlab", value=push.to.form)}
if(length(grep("^legend.dix.labpos=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.dix.labpos=",save.dat)],split="legend.dix.labpos="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_dix_labpos", value=push.to.form)}
if(length(grep("^legend.dix.labneg=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.dix.labneg=",save.dat)],split="legend.dix.labneg="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_dix_labneg", value=push.to.form)}
if(length(grep("^legend.dix.hori=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.dix.hori=",save.dat)],split="legend.dix.hori="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_dix_hori", value=push.to.form)}
if(length(grep("^legend.dix.axline=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.dix.axline=",save.dat)],split="legend.dix.axline="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_dix_axline", value=push.to.form)}
if(length(grep("^legend.order=",save.dat))>0){
	push.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^legend.order=",save.dat)],split="legend.order="))[2],split=","))
	if(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} 
	updateTextInput(session,"legend_order", value=push.to.form)}
