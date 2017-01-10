library(shiny)

templateFiles <- c('index_template.html')
outFiles <- c('index.html')

for(i in 1:length(templateFiles)){
	fileName <- templateFiles[i]
	outFile <- outFiles[i]
	template <- readChar(fileName, file.info(fileName)$size)

	while(regexpr("<&.*&>",template)>0){
		
		rexpr1 <- regexpr("<&",template)
		rexpr2 <- regexpr("&>",template)
		
		str2rep <- substr(template,rexpr1,rexpr2+1)

		content <- gsub("<&","",str2rep)
		content <- gsub("&>","",content)
		content <- gsub(" ","",content)
		content <- gsub("\n","",content)

		fileName <- paste("./",content,sep="")
		b <- readChar(fileName, file.info(fileName)$size)
		template <- gsub(str2rep,b,template)

	}
	#output the content filled template to file:
	cat(template,file=outFile)
}