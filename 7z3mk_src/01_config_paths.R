######################################################################
TXX <- "T15"
# Library path & Working paths

if(!Sys.getenv("COMPUTERNAME") %in% c("UV-CEMOPC-12", "UV-CEMOPC-05")) {
    ## ABEL high-performance cluster
    
    ### Find $SCRATCH path
    #scratch <- dirname(parent.frame(2)$ofile)
    #print(scratch)
    dir.list <-  list(root = "/usit/abel/u1/stephd") #  $SCRATCH folder => cd $SCRATCH + root = getwd(). $SUBMITDIR => "/usit/abel/u1/stephd/"
    libloc <- paste(dir.list$root, "nobackup", "R", "Library", sep="/")
    dir.list$aux <- paste(dir.list$root, "nobackup", "AuxR", sep="/") ##  general auxiliary syntax function files (only reads in functions)
    dir.list$data <- paste(dir.list$root, "nobackup", "Data", TXX, "G8", "Data", sep="/") ##  where to download database and TIMSS' own XLSX-files (T11_G8_ItemInformation.xlsx), or to find these
    dir.list$items <- paste(dir.list$root, "nobackup", "Data", TXX, "G8", "Items", sep="/") ##  where to download database and TIMSS' own XLSX-files (T11_G8_ItemInformation.xlsx), or to find these
    dir.list$input <- paste(dir.list$root, "nobackup", "Data", "Input", sep="/") # Reading in user-inputed data
    dir.list$almanac <- paste(dir.list$input, "TIMSS_ItemPercentCorrect", sep="/") # Reading in user-inputed data
    
    dir.list$project <- paste(dir.list$root, "Articles/A1-ContentDomains", sep="/")
    # dir.list$almanac <- paste(dir.list$project, "Input", sep="/") # Reading in user-inputed data
    dir.list$syntax <- paste(dir.list$project, "Syntax", sep="/") ##  syntax files (executes code) and some config xlsx files
    dir.list$out.gen <- paste(dir.list$project, "Output-general", TXX, sep="/") ## general descriptives, dataframe save, etc output
	dir.list$out.mod <- paste(dir.list$project, "Models", TXX, sep="/") #Finished output (tables, plots, etc)
    dir.list$out.mod1 <- paste("/projects/researchers/researchers01/stephd/Models1", TXX, sep="/") # Where to store (large) RData files
} else {
    ## Office computer
    
    # Library path & Working paths
    libloc <- if(Sys.getenv("COMPUTERNAME") %in% c("UV-CEMOPC-05")) "D:/R" else "//tsclient/D/R"
    dir.list <- list(root = "S:") #
    dir.list$aux <- paste(dir.list$root, "AuxR", sep="/") ##  general auxiliary syntax function files (only reads in functions)
    dir.list$data <- paste(dir.list$root, "Data", TXX, "G8", "Data", sep="/") ##  where to download database and TIMSS' own XLSX-files (T11_G8_ItemInformation.xlsx), or to find these
    dir.list$items <- paste(dir.list$root, "Data", TXX, "G8", "Items", sep="/") ##  where to download database and TIMSS' own XLSX-files (T11_G8_ItemInformation.xlsx), or to find these
    dir.list$input <- paste(dir.list$root, "Data", "Input", sep="/") # Reading in user-inputed data
    dir.list$almanac <- paste(dir.list$input, "TIMSS_ItemPercentCorrect", sep="/") # Reading in user-inputed data
    
    dir.list$project <- paste(dir.list$root, "Articles/A1-ContentDomains", sep="/")
    dir.list$syntax <- paste(dir.list$project, "Syntax", sep="/") ##  syntax files (executes code) and some config xlsx files
    dir.list$out.gen <- paste(dir.list$project, "Output-general", TXX, sep="/") ## general descriptives, dataframe save, etc output
	dir.list$out.mod <- paste(dir.list$project, "Models", TXX, sep="/") #Finished output (tables, plots, etc)
    dir.list$out.mod1 <- paste(dir.list$project, "Models1", TXX, sep="/")
}
###########################################################################################
source(paste(dir.list$aux, "load.R", sep="/")) ## Load packages
numWorkers <- min(c(4, if(grepl("linux", version$os)) as.numeric(Sys.getenv("SLURM_NTASKS")) - 1 else parallel::detectCores()))
numWorkers <- 4
options(mc.cores = numWorkers)

### Which country is this running? Infer from Abel file system
cnt.inf <- 
	readxl::read_excel(path=paste(dir.list$input, "Countries.xlsx", sep="/"), sheet = TXX) %>%
	dplyr::filter(G8, !grepl(", US", .$Country.name)) %>%
	pull(ISO.alpha)
arg <- unique(gsub("^([0-9]*).*\\.R", "\\1", list.files(include.dirs = F, recursive = F, pattern = "^[0-9]+.*\\.R")))
country <- cnt.inf[[as.integer(arg[[1]])]]
country <- "NOR" # Drop this line if you want to run all countries (on own risk)

print(paste0("This is country ", country, " from file ", arg))

### Run data preparation (requires auxiliary files, TIMSS data files, etc)
try(load(file = paste(dir.list$out.gen, paste0("data_", country, ".RData"), sep="/")))

model <- "t1"
for(folder in dir.list) dir.create(folder, showWarnings=FALSE)

dir.list$out.mod1 <- paste(dir.list$out.mod1, paste(model, country, sep="_"), sep="/"); dir.create(dir.list$out.mod1, showWarnings = FALSE, recursive = TRUE)
dir.list$out.mod <- paste(dir.list$out.mod, paste(model, country, sep="_"), sep="/"); dir.create(dir.list$out.mod, showWarnings = FALSE, recursive = TRUE)
dir.list$out.mod.graph <- paste(dir.list$out.mod, "Graph", sep="/"); dir.create(dir.list$out.mod.graph, showWarnings = FALSE, recursive = TRUE)
path.prefix <- paste(dir.list$syntax, paste(model, "stan", sep="-"), paste(model, "stan-", sep="-"), sep = "/")

###########################################################################################
