if(Sys.getenv("COMPUTERNAME") != "UV-CEMOPC-05") {
    source("/usit/abel/u1/stephd/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE) ## Computer-specific setting of folders
} else source("S:/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE)

### Run model in Stan (requires rstan)
source(file = paste0(path.prefix, 1, ".R"), echo = TRUE)
