# visPIG

There are 2 ways to use visPIG:
a) through a html GUI
b) headless from a terminal


#################
# a) from a GUI #
#################

To use the App: 'source' the R file 'Run_the_app.R' whilst ensuring that the current working directory is the directory in which Run_the_app.R lives. For example by:

	i) Dragging Run_the_app.R's icon into an R console, or

	ii) Executing the following command in R (replacing "your_local_folder_name" as appropriate):
		source(your_local_folder_name/Run_the_app.R, chdir=TRUE)

After your R terminal should display a line like this:
	Listening on http://127.0.0.1:7863
	
Just copy this http link into a browser of your choice (Firefox works for sure) and visPIG should be up and running.


Note:
The R package 'shiny' needs to be installed.
To do this, run the following command in R:
install.packages("shiny")


############################
# b) from the command line #
############################

Navigate to the directory
	visPIG/App/download_R_code/
and consult the README.txt in that directory.

