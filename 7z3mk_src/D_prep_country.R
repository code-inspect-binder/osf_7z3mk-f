###########################################################################################
################ Main file for p1 - Content Perspective ###################################
###########################################################################################

if(Sys.getenv("COMPUTERNAME") != "UV-CEMOPC-05") {
    source("/usit/abel/u1/stephd/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE) ## Computer-specific setting of folders
} else source("S:/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE)
source(paste(dir.list$aux, "load.R", sep="/")) ## Load packages


###########################################################################################
### Which country is this running?
# source(paste(dir.list$aux, "get.xlsx.R", sep="/")) # Helper function that reads in Excel files given 
cnt.inf <- 
    readxl::read_excel(paste(dir.list$input, "Countries.xlsx", sep="/"), sheet = TXX) %>%
   .[.$G8 & !grepl(", US", .$Country.name), "ISO.alpha"]
arg <- unique(gsub("\\.R|\\.RData", "", list.files(include.dirs = F, recursive = F)))
country <- cnt.inf[[as.integer(arg[[1]])]]
print(paste0("This is country ", country, " from file ", arg))
###########################################################################################

### Run data preparation (requires auxiliary files, TIMSS data files, etc)
source(file = paste(dir.list$syntax, paste("D_prep_country_data.R", sep="-"), sep = "/"), echo = TRUE)
source(file = paste(dir.list$syntax, paste("D_prep_country_desc.R", sep="-"), sep = "/"), echo = TRUE) ### Run descriptives (adds a bit more info)
#load(file = paste(dir.list$out.gen, paste0("p1-t-data-", country, ".RData"), sep="/"))

