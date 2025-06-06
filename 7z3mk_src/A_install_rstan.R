version

source("/usit/abel/u1/stephd/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE) ## Computer-specific setting of folders
libloc <- "/usit/abel/u1/stephd/nobackup/R/3.2.1"
dir.create(libloc)
.libPaths(libloc)
repo <- "http://cran.us.r-project.org"
### Install packages on Abel: Installing rstan, with recipe from Sabry at Abel-core. 
if(version$major == "3" & version$minor == "2.1") {

#	install.packages("scales", lib=libloc, repos=repo)
#	install.packages("gtable", lib=libloc, repos=repo)
#	install.packages("gridExtra", lib=libloc, repos=repo)

#	library("gtable",lib=libloc)
#	library("scales",lib=libloc)
#	library("gridExtra",lib=libloc)

#	install.packages("ggplot2", lib=libloc, repos=repo)

#	library("ggplot2",lib=libloc)
	install.packages(c("Rcpp", "rstan", "StanHeaders"),lib=libloc, repos=repo)
#	install.packages(c("BH","bitops","colorspace","dichromat","digest","ggplot2","gridExtra","gtable","inline","KernSmooth","labeling","lattice","loo","magrittr","MASS","Matrix","matrixStats","munsell","plyr","RColorBrewer","Rcpp","RcppEigen","RCurl","reshape2","rstan","rstudioapi","RUnit","scales","StanHeaders","stringi","stringr"), lib=libloc, repos=repo)
		
} else {stop("R version must be R 3.2.1 to get rstan on Abel.")}

library(rstan, lib=libloc)