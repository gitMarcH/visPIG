if('shiny' %in% rownames(installed.packages())){
	library(shiny)
	runApp("./App/")
}else{
	stop("Please install the shiny package.")
}
