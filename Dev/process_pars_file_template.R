     ######################
################################     
####### LOAD  PARAMETERS #######
################################
     ######################

# read in parameters file, treating everything as.character()
	pars<-read.table(pars.file,sep="\n",header=F,colClasses="character")

# process line-by-line
	for(i in 1:nrow(pars)){

# split on "=" and the parameters value on ","
		var_name<-unlist(strsplit(pars[i,],split="="))[1]
		var_value<-unlist(strsplit(unlist(strsplit(pars[i,],split="="))[2],split=","))
		if(!is.na(var_value[1]) & !is.null(var_value[1])){
			var_value[var_value=="" | var_value=="NA"]<-NA
		}
		if(!is.na(var_value[1]) & !is.null(var_value[1])){
			var_value[var_value=="NULL"]<-NULL
		}
	
# assign value to variable name using do.call("<-",list(var_name,var_value)) or assign(var_name,var_value)
		do.call("<-",list(var_name,var_value))

# close loop on line
	}

# assign correct object type
