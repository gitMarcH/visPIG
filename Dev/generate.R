library(shiny)
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
ui.html <- 'parameter_options.html'
parse.R <- 'plot_read_params.R'
save.params.R <- 'plot_save_params.R'

cat(paste(''),file=ui.html,append=FALSE)
cat(paste('# Place the parameters, that are set, in a string ready for saving to a text file\n\nsaved_params_str<-c()\n'),file=save.params.R,append=FALSE)
cat(paste('# Read in and parse the script parameters from the web GUI, uploaded parameter file and the default parameter value, in that order.\n\n'),file=parse.R,append=FALSE)

# Now we loop through all parameters, first looping through the categories then through the parameters in that category.

# For each category
for(i in 1:length(cats)){
	cat(paste('<div style="text-align:right;" data-display-if=\"input.parameter_set==\'',cats[i],'\'\">\n',sep=''),file=ui.html,append=TRUE)
	cat(paste('\n\n\n# Parameters for ',cats[i],'\n',sep=''),file=parse.R,append=TRUE)	
	
	
	# Find the indices of the parameters associated with the current category
	indices <- grep(paste(cats[i]),parameter$category_id)
					
# For each parameter in this category, write a) the HTML for the inputs and b) the R for reading and parsing the inputs.
		for(j in 1:length(indices)){
			# a) the HTML:
			if(parameter[indices[j],]$type!='HTML'){			
			cat(paste('\n\t<div title="',parameter[indices[j],]$tooltip,'" class="',ifelse(j%%2==0,'even_row','odd_row'),'">\n\t',parameter[indices[j],]$label,'\n',sep=''),file=ui.html,append=TRUE)
			}
			if(parameter[indices[j],]$default_check==TRUE && !is.na(parameter[indices[j],]$default_check)){	cat(paste('(* = default)',sep=''),file=ui.html,append=TRUE)
}
#file	
			if(parameter[indices[j],]$type=='file'){
				cat(paste('\t<input id="',gsub('\\.','_',parameter[indices[j],]$id),'" type="file" />\n',sep=''),file=ui.html,append=TRUE)
				cat(paste('\t<input id="consider_',gsub('\\.','_',parameter[indices[j],]$id),'" type="checkbox" checked="checked" />\n',sep=''),file=ui.html,append=TRUE)

#files
			} else if(parameter[indices[j],]$type=='files'){
				cat(paste('\t<input id="',gsub('\\.','_',parameter[indices[j],]$id),'" type="file" multiple="multiple" />\n',sep=''),file=ui.html,append=TRUE)				
				cat(paste('\t<input id="consider_',gsub('\\.','_',parameter[indices[j],]$id),'" type="checkbox" checked="checked" />\n',sep=''),file=ui.html,append=TRUE)
#logical
			} else if(parameter[indices[j],]$type=='logical'){
				cat(paste('\t<select id="',gsub('\\.','_',parameter[indices[j],]$id),'"><option value="TRUE"',ifelse(parameter[indices[j],]$default==TRUE,' selected',''),'>Yes</option><option value="FALSE">No</option></select>\n',sep=''),file=ui.html,append=TRUE)								
#textarea
			}else if(parameter[indices[j],]$type_GUI=='textarea'){
				cat(paste('\t<textarea id="',gsub('\\.','_',parameter[indices[j],]$id),'" cols="',parameter[indices[j],]$width,'" rows=',parameter[indices[j],]$height,' ></textarea>\n',sep=''),file=ui.html,append=TRUE)								
#select
			}else if(parameter[indices[j],]$type_GUI=='select'){
				tmp.choices<-unlist(strsplit(gsub('"','',as.character(parameter[indices[j],]$choices)),','))
				cat(paste(selectInput(gsub('\\.','_',parameter[indices[j],]$id),'',tmp.choices,tmp.choices[1]),sep=''),file=ui.html,append=TRUE)								
#slider
			}else if(parameter[indices[j],]$type_GUI=='slider'){
				tmp.limits<-as.numeric(unlist(strsplit(gsub('"','',as.character(parameter[indices[j],]$limits)),',')))
				cat(paste('<span style="width:120px;"><input id="',gsub('\\.','_',parameter[indices[j],]$id),'" type="slider" value="',tmp.limits[4],'" class="jslider" data-from="',tmp.limits[1],'" data-to="',tmp.limits[2],'" data-step="',tmp.limits[3],'" data-skin="plastic" data-round="false" data-locale="us" data-format="#,##0.#####" data-smooth="false"/> </span>',sep=''),file=ui.html,append=TRUE)								
#color
			}else if(parameter[indices[j],]$type=='color'){
				cat(paste('\t<input id="',gsub('\\.','_',parameter[indices[j],]$id),'" title = "Specify colour as a name e.g. red or &#13As hex code eg #FF0000, &#13Unless there is an option below to specify colour as rgb in which case use the format 255_0_0" type="text" value="',paste(parameter[indices[j],]$default,sep=''),'" />\n',sep=''),file=ui.html,append=TRUE)
#HTML
			}else if(parameter[indices[j],]$type=='HTML'){
				cat(paste(parameter[indices[j],]$label,'\n',sep=''),file=ui.html,append=TRUE)				
#numeric
			}else if(parameter[indices[j],]$type=='numeric'){
				cat(paste('\t<input id="',gsub('\\.','_',parameter[indices[j],]$id),'" type="text" value="',ifelse(is.na(parameter[indices[j],]$default),'',paste(parameter[indices[j],]$default)),'" />\n',sep=''),file=ui.html,append=TRUE)
#general
			}else{
				cat(paste('\t<input id="',gsub('\\.','_',parameter[indices[j],]$id),'" type="text" value="',ifelse(is.na(parameter[indices[j],]$default),'',paste(parameter[indices[j],]$default)),'" />\n',sep=''),file=ui.html,append=TRUE)
			}	
			if(parameter[indices[j],]$type!='HTML'){
			cat(paste('\t</div>\n\n',sep=''),file=ui.html,append=TRUE)	
			}
		
			

			# b) the R for parsing:
			
			
			if(parameter[indices[j],]$type!='HTML'){
				# Read the input value from the web GUI, the uploaded parameter file, or use the default value, in that order.
				
				if(is.na(parameter[indices[j],]$default_check) || parameter[indices[j],]$default_check==FALSE ){
				cat(paste('\nif(input$',gsub('\\.','_',parameter[indices[j],]$id),'!="" && !is.null(input$',gsub('\\.','_',parameter[indices[j],]$id),')){ in.value <- input$',gsub('\\.','_',parameter[indices[j],]$id),sep=''),file=parse.R,append=TRUE)						
				cat(paste('} else{ in.value <- NA }',sep=''),file=parse.R,append=TRUE)	
				} else {
				   cat(paste('\nif(input$',gsub('\\.','_',parameter[indices[j],]$id),'!="*" && !is.null(input$',gsub('\\.','_',parameter[indices[j],]$id),')){ in.value <- input$',gsub('\\.','_',parameter[indices[j],]$id),sep=''),file=parse.R,append=TRUE)						
				   cat(paste('} else{ in.value <- NA }',sep=''),file=parse.R,append=TRUE)					   
				}
				
				# For variables with vector information e.g. 'a,b,c'				
				cat(paste('\n\t if(typeof(in.value)=="character" && in.value!=""){in.value <- unlist(strsplit(in.value,split=","))}',sep=''),file=parse.R,append=TRUE)
				
				# Now we get to parsing:
				if((is.na(parameter[indices[j],]$auto_val) || parameter[indices[j],]$auto_val=='')){
					tmp.param.auto_val <- 'NA'
				}else{
					tmp.param.auto_val <- as.character(parameter[indices[j],]$auto_val)
				}
				if(parameter[indices[j],]$type=='file'){
					cat(paste('\n\t if(is.na(in.value[1]) || as.logical(input$consider_',gsub('\\.','_',parameter[indices[j],]$id),')==FALSE){',parameter[indices[j],]$id,'<-c(',tmp.param.auto_val,') } else {',parameter[indices[j],]$id,'<-in.value$datapath; }',sep=''),file=parse.R,append=TRUE)
				} else if(parameter[indices[j],]$type=='files'){
					cat(paste('\n\t if(is.na(in.value[1]) || as.logical(input$consider_',gsub('\\.','_',parameter[indices[j],]$id),')==FALSE){',parameter[indices[j],]$id,'<-c(',tmp.param.auto_val,') } else {',parameter[indices[j],]$id,'<-in.value$datapath; }',sep=''),file=parse.R,append=TRUE)
				} else if(parameter[indices[j],]$type=='logical'){
					cat(paste('\n\t if(is.na(in.value[1])){',parameter[indices[j],]$id,'<-c(',tmp.param.auto_val,') } else {',parameter[indices[j],]$id,'<-as.logical(in.value) }',sep=''),file=parse.R,append=TRUE)
				} else if(parameter[indices[j],]$type=='numeric'){
					cat(paste('\n\t if(is.na(in.value[1])){',parameter[indices[j],]$id,'<-c(',tmp.param.auto_val,') } else {',parameter[indices[j],]$id,'<-as.numeric(in.value) }',sep=''),file=parse.R,append=TRUE)
				} else if(parameter[indices[j],]$type=='integer'){
					cat(paste('\n\t if(is.na(in.value[1])){',parameter[indices[j],]$id,'<-c(',tmp.param.auto_val,') } else {',parameter[indices[j],]$id,'<-as.integer(round(as.numeric(in.value))) }',sep=''),file=parse.R,append=TRUE)
				} else if(parameter[indices[j],]$type=='character'){
					cat(paste('\n\t if(is.na(in.value[1])){',parameter[indices[j],]$id,'<-c(',tmp.param.auto_val,') } else {',parameter[indices[j],]$id,'<-as.character(in.value) }',sep=''),file=parse.R,append=TRUE)
				}else{
					cat(paste('\n\t if(is.na(in.value[1])){',parameter[indices[j],]$id,'<-c(',tmp.param.auto_val,') } else {',parameter[indices[j],]$id,'<-in.value }',sep=''),file=parse.R,append=TRUE)
				}
				

				# For the variables that are used to count stuff
				if(parameter[indices[j],]$count_var!=''){ 
					#cat(paste('\n\tif(!is.na(in.value[1])){',parameter[indices[j],]$count_var,' <- length(',parameter[indices[j],]$id,')} else {',parameter[indices[j],]$count_var,' <- 0 }',sep=''),file=parse.R,append=TRUE)
					cat(paste('\n\tif(is.na(in.value[1])){',parameter[indices[j],]$count_var,' <- 0 } else if(!is.null(input$consider_',gsub('\\.','_',parameter[indices[j],]$id),') && !is.na(input$consider_',gsub('\\.','_',parameter[indices[j],]$id),') && input$consider_',gsub('\\.','_',parameter[indices[j],]$id),'==FALSE){',parameter[indices[j],]$count_var,' <- 0 } else {',parameter[indices[j],]$count_var,' <- length(',parameter[indices[j],]$id,') }',sep=''),file=parse.R,append=TRUE)
				}
			}
			# c) For the saving of parameters script.
			if(!(parameter[indices[j],]$type %in% c('files','file','HTML'))){
				outstring <- paste('saved_params_str<-c(saved_params_str,paste("',parameter[indices[j],]$id,'=",paste(',parameter[indices[j],]$id,',collapse=","),sep=""))\n',sep='')
				cat(outstring,file=save.params.R,append=TRUE) 
			}
			
		}
	cat(paste('</div>\n',sep=''),file=ui.html,append=TRUE)

}