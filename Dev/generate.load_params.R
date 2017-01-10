# read in the parameters_template file
parameter <- read.csv('./params.csv')
# We remove the blank lines from the parameter data frame that were left in the csv simply for ease of reading.
parameter <- parameter[parameter$id!='' | parameter$type=='HTML',]

# Find out what categories of parameter we have
cats <- levels(as.factor(parameter$category_id))
cats <- cats[cats!='']
tmp<-matrix(rep(NA,length(cats)*2),nrow=length(cats))
tmp[,1]<-cats
for(i in 1:length(cats)){
	tmp[i,2]<-grep(tmp[i,1],parameter$category_id)[1]
}
cats<-tmp[order(tmp[,2]),1]

# For each category
#   a)		Create the HTML user interface for each parameter category. 
#	b)		Create the lines to read in and parse the parameter value for the R script.

#	Firstly we set up the blank files we are going to write to.
load.params.R <- 'plot_load_params.R'

cat('# Load the parameters from a text file and populate the web form.\n ',file=load.params.R,append=FALSE)


# Now we loop through all parameters, first looping through the categories then through the parameters in that category.

# For each category
for(i in 1:length(cats)){

	cat(paste('\n\n\n# Parameters for ',cats[i],'\n',sep=''),file=load.params.R,append=TRUE)	

	# Find the indices of the parameters associated with the current category
	indices <- grep(paste(cats[i]),parameter$category_id)
					
# For each parameter in this category
		for(j in 1:length(indices)){
			
			if(parameter[indices[j],]$type!='HTML'){
				# Read the input value from the web GUI, the uploaded parameter file, or use the default value, in that order.
				
				cat(paste('if(length(grep("^',parameter[indices[j],]$id,'=",save.dat))>0){\n\tpush.to.form<-unlist(strsplit(unlist(strsplit(save.dat[grep("^',parameter[indices[j],]$id,'=",save.dat)],split="',parameter[indices[j],]$id,'="))[2],split=","))',sep=''),file=load.params.R,append=TRUE)						
				
				cat(paste('\n\tif(push.to.form == "NA" || is.na(push.to.form)){push.to.form<-""} \n\tupdateTextInput(session,"',gsub('\\.','_',parameter[indices[j],]$id),'", value=push.to.form)}\n',sep=''),file=load.params.R,append=TRUE)											

			}
		}
}