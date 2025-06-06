if(Sys.getenv("COMPUTERNAME") != "UV-CEMOPC-05") {
    source("/usit/abel/u1/stephd/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE) ## Computer-specific setting of folders
} else source("S:/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE)

### Compute model-based estimates from posterior distribution
source(file = paste0(path.prefix, 2, ".R"), echo = TRUE, max.deparse.length = 1500)
### Compute summaries of extended posterior distribution
source(file = paste0(path.prefix, 3, ".R"), echo = TRUE)
### Diagnostic plots (only trace and hist for all, rest of plots limited to estimated pars)
source(file = paste0(path.prefix, 4, ".R"), echo = TRUE)
### Output tables
source(file = paste0(path.prefix, 5, ".R"), echo = TRUE)
### Posterior (population) model predictions of item responses and proportions correct 
source(file = paste0(path.prefix, 6, ".R"), echo = TRUE)
### Caterpillar plots of TOPIC/DOMAIN difficulties (FOR ARTICLE)
source(file = paste0(path.prefix, 7, ".R"), echo = TRUE)

