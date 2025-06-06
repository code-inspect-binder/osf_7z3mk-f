##################################################################
### Posterior predictive model checking for item proportions
##################################################################
stan_props_file <- paste(dir.list$out.mod1, paste(model, country, "stan-props.RData", sep="_"), sep="/")
stan_props_result <- try(load(file = stan_props_file)) # proportions: iterations by items
stan_prop_sum_file <- paste(dir.list$out.mod1, paste(model, country, "stan-props-summary.RData", sep="_"), sep="/")
stan_prop_sum_result <- try(load(file = stan_prop_sum_file)) 
xlsx_file <- paste(dir.list$out.mod, paste(model, country, "stan.xlsx", sep="_"), sep="/")
xlsx_sheets <- try(openxlsx::getSheetNames(xlsx_file))
if(class(stan_props_result) == "try-error" | class(stan_prop_sum_result) == "try-error" | !"prop_TOPIC" %in% xlsx_sheets) { 

if(!exists("t1.sum")) load(file = paste(dir.list$out.mod1, paste(model, country, "stan-summary.RData", sep="_"), sep="/"))
if(!exists("t1.summary")) load(file = paste(dir.list$out.mod1, paste(model, country, "stan-summary-out.RData", sep="_"), sep="/")) # Needs the T_p_ and B_i_hat_

### Use existing Excel workbook for output
WB <- openxlsx::loadWorkbook(file = paste(dir.list$out.mod, paste(model, country, "stan.xlsx", sep="_"), sep="/"))
addWorksheet(wb = WB, sheetName = "prop_ITEM")
addWorksheet(wb = WB, sheetName = "prop_TOPIC")
addWorksheet(wb = WB, sheetName = "prop_DOMAIN")


# library(parallel)
workerFunction <- function(ROWS) {
	MAT <- t1.sum[ROWS, ,drop=FALSE]
	COLNAMES <- colnames(MAT)
	#Set up empty matrix
	theta.col <- grep("^T_p_", COLNAMES,value=F)
	beta.col <- grep("^B_i_hat_", COLNAMES,value=F)
	N_B <- length(beta.col)
	N_T <- length(theta.col)
	props <- matrix(NA, nrow = length(ROWS), ncol = N_B, byrow = TRUE, dimnames = list(NULL, colnames(MAT[, beta.col])))
	# props <- matrix(NA, nrow = nrow(t1.sum), ncol = N_B, byrow =T, dimnames = list(rownames=NULL, colnames=colnames(t1.sum[, beta.col])))

	compiler::enableJIT(3) ## Performance trick
	for(iteration_row in seq_len(length(ROWS))) { ## For each iteration, calculate the expected eta, convert eta to (expected) probability, if this is smaller than random proportion==>0
	    THETA <- MAT[iteration_row, theta.col, drop=FALSE]
		# print(typeof(THETA))
		eta <- # eta = theta + mean - beta
		    matrix(THETA, nrow=N_T, ncol = N_B, byrow = FALSE) -
		    matrix(MAT[iteration_row, beta.col, drop=FALSE], nrow=N_T, ncol = N_B,  byrow = TRUE)
		Y <- matrixStats::colMeans2(x = (matrix(runif(N_T * N_B), nrow = N_T, ncol = N_B) <= 1/(1+exp(-eta))) + 0)
		props[iteration_row, ] <- Y # Take proportion across item responses
	  }
	compiler::enableJIT(0)
	colnames(props) <- colnames(MAT[, beta.col])
	props

}
splits <- suppressWarnings(split(seq_len(nrow(t1.sum)), rep(seq_len(4), each=ceiling(nrow(t1.sum)/4))))
props <- plyr::llply(splits, workerFunction) %>% do.call(rbind, .)
### Pre-allocation for computed params
link.itm <- 
  dplyr::data_frame(itm = as.character(as.factor(data$Item.ID)), b=as.integer(as.factor(data$Item.ID)), DOMAIN = as.character(as.factor(data$DOMAIN)), TOPIC = as.character(as.factor(data$TOPIC))) %>%
  dplyr::distinct(itm, .keep_all=TRUE) %>%
  dplyr::arrange(b) %>%
  as.data.frame()
link.top <- 
  dplyr::data_frame(top = as.character(as.factor(data$TOPIC)), b=as.integer(as.factor(data$TOPIC)), DOMAIN = as.character(as.factor(data$DOMAIN))) %>%
  dplyr::distinct(top, .keep_all=TRUE) %>%
  dplyr::arrange(b) %>%
  as.data.frame()
link.dom <- 
  dplyr::data_frame(dom = as.character(as.factor(data$DOMAIN)), b=as.integer(as.factor(data$DOMAIN))) %>%
  dplyr::distinct(dom, .keep_all=TRUE) %>%
  dplyr::arrange(b) %>%
  as.data.frame()
z.colnames <- c(paste0("B_t_hat_", link.top$top), 
				paste0("B_d_i_hat_", link.dom$dom))
z <- matrix(NA, nrow=nrow(props), ncol=length(z.colnames), dimnames=list(NULL, z.colnames))
props <- cbind(props, z)
rm(z, z.colnames)
print("done2")

### Take content group averages iteration-wise
for(content in link.top$top) {
 items.idx <- link.itm %>% .[.$TOPIC == content, "itm"] %>% paste0("B_i_hat_", .) %>% match(., colnames(props))
 props[, paste0("B_t_hat_", content)] <- matrixStats::rowMeans2(props, cols = items.idx)
}

for(content in link.dom$dom) {
 items.idx <- link.itm %>% .[.$DOMAIN == content, "itm"] %>% paste0("B_i_hat_", .) %>% match(., colnames(props))
 props[, paste0("B_d_i_hat_", content)] <- matrixStats::rowMeans2(props, cols = items.idx)
print("done3")
}

### Summarize props and add alamanc numbers
sumsum <- function(mat) {
	cbind(	
			Mean = matrixStats::colMeans2(mat), 
			Median = matrixStats::colMedians(mat), 
			CI.95L = matrixStats::colQuantiles(mat, probs=.025),
			CI.95H = matrixStats::colQuantiles(mat, probs=.975))
}

desc_ITEM <- 
    desc.ITEM %>%
    transmute(Item.ID, TOPIC = as.character(TOPIC), DOMAIN = as.character(DOMAIN), 
              Almanac_I = int.almanac.ITEM/100, Almanac_T = int.almanac.TOPIC/100, Almanac_D = int.almanac.DOMAIN/100)

prop_sum <- 
    props %>%
    sumsum %>% 
    as_data_frame() %>%
    mutate(Group = gsub(".*_hat_", "", colnames(props))) %>%
    left_join(x=., y=desc_ITEM, by=c("Group" = "Item.ID")) %>%
    left_join(x=., y=dplyr::transmute(distinct(desc_ITEM, TOPIC, .keep_all=T), TOPIC, TOPIC.y = TOPIC, DOMAIN.y=DOMAIN, Almanac_T.y=Almanac_T, Almanac_D.y=Almanac_D), by=c("Group" = "TOPIC")) %>% 
    left_join(x=., y=dplyr::transmute(distinct(desc_ITEM, DOMAIN, .keep_all=T), DOMAIN, DOMAIN.z=DOMAIN, Almanac_D.z=Almanac_D), by=c("Group" = "DOMAIN")) %>% 
    mutate(Mean, Median, CI.95L, CI.95H, Group, 
         DOMAIN = ifelse(!is.na(DOMAIN), DOMAIN, ifelse(!is.na(DOMAIN.y), DOMAIN.y, DOMAIN.z)),
         TOPIC = ifelse(!is.na(TOPIC), TOPIC, ifelse(!is.na(TOPIC.y), TOPIC.y, NA)),
         Almanac = ifelse(!is.na(Almanac_I), Almanac_I, ifelse(!is.na(Almanac_T.y), Almanac_T.y, Almanac_D.z)),
         Almanac_T = ifelse(!is.na(Almanac_T.y), Almanac_T.y, Almanac_T),
         Almanac_D = ifelse(!is.na(Almanac_D), Almanac_D, ifelse(!is.na(Almanac_D.y), Almanac_D.y, Almanac_D.z)),
         Overlap = factor(ifelse(CI.95L > Almanac, "Over", ifelse(CI.95H < Almanac, "Under", "-"))),
		 Domain = dplyr::recode(DOMAIN, `Biology` = "B", `Chemistry` = "C", `Earth Science` = "E", `Physics` = "P")) %>%
    dplyr::select(-matches("\\.y$|\\.z$"))

prop_sum_domain <-
  prop_sum %>%
  dplyr::filter(grepl("S0", Group)) %>%
  group_by(DOMAIN) %>%
  summarize(D_Total = sum(grepl("", Overlap)), D_Over = sum(grepl("Over", Overlap))/D_Total, D_Under = sum(grepl("Under", Overlap))/D_Total)
prop_sum_topic <-
    prop_sum %>% 
    dplyr::filter(grepl("S0", Group)) %>%
    group_by(TOPIC) %>%
    summarize(T_Total = sum(grepl("", Overlap)), T_Over = sum(grepl("Over", Overlap))/T_Total, T_Under = sum(grepl("Under", Overlap))/T_Total)
prop_sum <-
    left_join(x=prop_sum, y=prop_sum_domain, by="DOMAIN") %>%
    left_join(x=., y=prop_sum_topic, by="TOPIC")


prop_sum %>%
    dplyr::filter(Group %in% unique(desc_ITEM$Item.ID)) %>%
    transmute(Domain, TOPIC, Item.ID=Group, Mean, CI.95L, CI.95H, Almanac_I, Overlap) %>%
    arrange(Domain, TOPIC, Mean) %>%
    openxlsx::writeData(wb = WB, sheet = "prop_ITEM", x=.)

prop_sum %>%
  dplyr::filter(Group %in% unique(desc_ITEM$TOPIC)) %>%
  transmute(Domain, TOPIC, Mean, CI.95L, CI.95H, Almanac_T, T_Over, T_At = 1 - T_Over - T_Under, T_Under) %>%
  arrange(Domain, Mean) %>%
  openxlsx::writeData(wb = WB, sheet = "prop_TOPIC", x=.)
  
prop_sum %>%
  dplyr::filter(Group %in% unique(desc_ITEM$DOMAIN)) %>%
  transmute(Domain, Mean, CI.95L, CI.95H, Almanac_D, D_Over, D_At = 1 - D_Over - D_Under, D_Under) %>%
  openxlsx::writeData(wb = WB, sheet = "prop_DOMAIN", x=.)

# ### Get "international almanac proportions" from data set
# load(paste(dir.list$out.gen, paste0("data_int.RData"), sep="/"))
# prop.sum2 <- 
#     desc.ITEM %>%
#     select(Item.ID, starts_with("int.almanac")) %>%
#     left_join(x=prop.sum.item, y=., by=c("Group" = "Item.ID")) %>%
#     left_join(x=select(dplyr::filter(prop.sum, Group==TOPIC), TOPIC=Group), y=., by=c("TOPIC"))

  
openxlsx::saveWorkbook(wb = WB, file = xlsx_file, overwrite = TRUE) 
save(props, file = stan_props_file) # proportions: iterations by items
save(prop_sum, WB, file = stan_prop_sum_file) #summary statistics of proportions per item

rm(props) # As objects are massive, drop after use
}
