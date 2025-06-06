version

source("/usit/abel/u1/stephd/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE) ## Computer-specific setting of folders
libloc <- "/usit/abel/u1/stephd/nobackup/R/Library"
dir.create(libloc)
.libPaths(libloc)


### Install packages on Abel: Installing rstan, with recipe from Sabry at Abel-core. 
install.packages(c("ggplot2","gridExtra","gtable","magrittr","Matrix","matrixStats","plyr","reshape2","stringi","stringr","foreign", "dplyr", "data.table", "tidyr", "abind", "ISOcodes", "psych", "tibble", "readxl", "rstan"), lib=libloc, repos="http://cran.us.r-project.org")

## Make files for jobarray in Abel
for(i in 1:50) writeLines(text = paste0("arg <- ", i), con = file(paste(dir.list$syntax, "Num", i, sep="/")))


old.packages(repos=repo, lib.loc=libloc)
update.packages(lib.loc=libloc, ask=FALSE, repos=repo)
