# Shiny integration for Hic plotting script  
# -------------------------------------------------------------------------------------------------------------------------------

library(shiny)
library(datasets)

# Stuff that goes here is visible across all sessions. 
# -------------------------------------------------------------------------------------------------------------------------------

# This would be a good place to load big data.
# Upload limit: 250mb
up.lim<-250
options(shiny.maxRequestSize=up.lim*1024^2)
#options(shiny.maxRequestSize=-1) # no limit for now; NB files larger than ~600MB seem to be ignored, no matter what maxRequestSize is set...
script_dir<-'./scripts/' # Note that the filepaths are relative to this file, which is in /visPIG/App/ (or was, last time I checked)
outprefix<-'./log/log'
awk_cmd<-'awk'




# The Shiny Server Function below handles contains functions that update certain outputs dependent on the state of certain inputs
# -------------------------------------------------------------------------------------------------------------------------------

shinyServer(function(input, output, session) {
	

	######################
	#  Preview Image     #
	######################
	
	output$web_image <- renderImage({  
		dynamicHelp <<- 'Advice & troubleshooting:\n'
		input$replot # Take a dependency on the action button...
		isolate({  #isolate means that output$web_image is not updated just because input$ variables it uses change.
		updateTextInput(session=session,inputId='troubleshoot_count',value=as.numeric(input$troubleshoot_count)+1)     # This is to make sure the dynamicHelp output is 'refreshed'  
		# plot to the png:  
		outfile <- tempfile(pattern='web-plot-',tmpdir='./temp-web-images/',fileext='.png')
		if(input$render_all == TRUE){
			source("./plot_read_params.R",local=TRUE)
			plot.type<-"png"
			source("./plot_visPIG.R",local=TRUE)						 
		}	
		# Return a list containing the filename
		list(src = outfile,
		   contentType = 'image/png',
		   width = width.plot*72,
		   height = height.plot*72,
		   alt = "Hi-C plot region")
		}) }
		,deleteFile=TRUE
	)

	######################
	#   PDF download     #
	######################	
	
	output$downloadPDF <- downloadHandler(
										  
		filename = function(){paste('plot_',Sys.Date(),'_GMT.pdf',sep='')},

		content=function(file){
		# plot to the pdf:
		source("./plot_read_params.R",local=TRUE)
		plot.type<-"pdf"
		source("./plot_visPIG.R",local=TRUE)						
		},
		contentType='image/pdf'
	) 

	######################
	# Parameter Download #
	######################	

	output$downloadParameters <- downloadHandler(
	  filename = function(){"saved.parameters.txt"},
	  content=function(file){
		source("./plot_read_params.R",local=TRUE)
		source("./plot_save_params.R",local=TRUE)
		writeLines(saved_params_str,file)
	  },
	  contentType='text'
	)  

	
	######################
	#  Help Text		 #
	######################

	output$dynamicHelp <- renderText({
		input$replot
		input$troubleshoot_count
		if(length(dynamicHelp)>1){
		print(paste(dynamicHelp,collapse='\n\n'))
		}else{print('')}									 
	})

	
	#########################
	# Load Params from file #
	#########################	
	
	observe({		
	  # Load the parameters from a text file and populate the web form.
	  if(!is.null(input$parameters_file)){
	  save.dat<-as.vector(t(read.table(input$parameters_file$datapath,sep="\n")))
	  source("./plot_load_params.R",local=TRUE)  
	  }
	})

	##########################
	# Load up default Params #
	##########################
	
	observe({  	
	  if(as.numeric(input$initialized)==0){
		save.dat<-as.vector(t(read.table('./reference_files/parameters.default.txt',sep="\n")))
		source("./plot_load_params.R",local=TRUE) 
	  }
	  updateTextInput(session=session,inputId='initialized',value=1)
	})

})