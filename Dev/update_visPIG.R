source("./generate.R",chdir=T)
source("./fill_template.R",chdir=T)
source("./generate.load_params.R",chdir=T)
source("./generate_downloadable_script.R",chdir=T)

system("cp plot_load_params.R ../App/plot_load_params.R")
system("cp plot_read_params.R ../App/plot_read_params.R")
system("cp plot_save_params.R ../App/plot_save_params.R")
system("cp index.html ../App/www/index.html")
system("rm plot_load_params.R plot_read_params.R plot_save_params.R index.html")
