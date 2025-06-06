### Model-based estimates and summaries for t1.stan
stan_summary_file <- paste(dir.list$out.mod1, paste(model, country, "stan-summary.RData", sep="_"), sep="/")
stan_summary_result <- try(load(file = stan_summary_file), silent=TRUE)

link.stu <- 
  dplyr::data_frame(stu = as.character(as.factor(data$STUDID)), b=as.integer(as.factor(data$STUDID)), CLASSID = as.character(as.factor(data$CLASSID))) %>%
  dplyr::distinct(stu, .keep_all=TRUE) %>%
  dplyr::arrange(b) %>%
    as.data.frame()
link.cla <- 
  dplyr::data_frame(cla = as.character(as.factor(data$CLASSID)), b=as.integer(as.factor(data$CLASSID))) %>%
  dplyr::distinct(cla, .keep_all=TRUE) %>%
  dplyr::arrange(b) %>%
    as.data.frame()

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
  
if(class(stan_summary_result) == "try-error") {

stan_file <- paste(dir.list$out.mod1, paste(model, country, "stan.RData", sep="_"), sep="/")
load(file = stan_file)


### link Stan parameter names for items and persons to real IDs

  
########
t1.sum <- 
  t1.stan %>%
  as.matrix(.) %>% # Will drop burnins, see rstan::as.array
  magrittr::set_colnames(., value = c("m", 
                            paste0("T_p_", link.stu$stu), 
                            paste0("T_c_raw_", link.cla$cla), 
                            paste0("B_t_raw_", link.top$top), 
                            paste0("B_i_raw_", link.itm$itm), 
                            "wgtstu", "sigma_theta_P", "sigma_theta_C", "sigma_beta_T", "sigma_beta_I", "lp__"))
rm(t1.stan)

## set columns that will be filled (for efficiency and also to allow <- assignment on matrices)
content.pairs.domain <- combn(link.dom$dom, 2, simplify = F)
content.pairs.topic <- combn(link.top$top, 2, simplify = F)
z.colnames <- c(paste0("T_c_", link.cla$cla),
				paste0("B_t_", link.top$top), paste0("V_t_", link.top$top),
				paste0("e_i_", link.itm$itm),
				paste0("B_d_", link.dom$dom), paste0("V_d_", link.dom$dom),
				paste0("B_d_i_", link.dom$dom), paste0("V_d_i_", link.dom$dom),
				paste0("e_t_", link.top$top), 
				paste0("B_tt_", sapply(content.pairs.topic, paste0, collapse="_")), 
				paste0("V_tt_", sapply(content.pairs.topic, paste0, collapse="_")), 
				paste0("B_dd_", sapply(content.pairs.domain, paste0, collapse="_")), 
				paste0("V_dd_", sapply(content.pairs.domain, paste0, collapse="_")),
				paste0("B_dd_i_", sapply(content.pairs.domain, paste0, collapse="_")), 
				paste0("V_dd_i_", sapply(content.pairs.domain, paste0, collapse="_")),
				paste0("B_i_hat_", link.itm$itm),
				"V_a_domains", "V_a_topics", "V_a_items", "V_a_topics_res", "V_a_items_res", "V_a_total", "V_a_classes", "V_a_students", 
				"VCP_a_person_side", "VCP_a_classes", "VCP_a_students", "VCP_a_item_side", "VCP_a_domains", "VCP_a_topics", "VCP_a_items", 
				"VCP_a_person_item", "VCP_p_classes", "VCP_p_students", "VCP_i_domains", "VCP_i_topics", "VCP_i_items")
z <- matrix(NA, ncol=length(z.colnames), nrow=nrow(t1.sum), dimnames=list(NULL, z.colnames))
t1.sum <- cbind(t1.sum, z)
rm(z, z.colnames)


####### Calculate model-implied 'parameters' ########
## Each section here sets up a parameter to be calculated for each group, then performs it.
### Class averages: Only used to calculate person ability variance and class variance. 
for(classID in link.cla$cla) {
 pupils <- link.stu %>% .[.$CLASSID == classID, "stu"] %>% paste0("T_p_", .) %>% match(., colnames(t1.sum))
 #print(pupils)
 insert <- matrixStats::rowMeans2(t1.sum, cols=pupils, na.rm=TRUE)
 #print(insert)
 t1.sum[, paste0("T_c_", classID)] <- insert
 #print(head(t1.sum[, paste0("T_c_", classID)]))
}
#print(t1.sum[!complete.cases(t1.sum), ], digits=2)

### Topic average (B_t = mean(B_i_raw) and topic variance , aggregated from the model-based estimates of B_i
for(content in link.top$top) {
 items <-  link.itm %>% .[.$TOPIC == content, "itm"] %>% paste0("B_i_raw_", .) %>% match(., colnames(t1.sum))
 t1.sum[, paste0("B_t_", content)] <- matrixStats::rowMeans2(t1.sum, cols=items, na.rm=TRUE)
 t1.sum[, paste0("V_t_", content)] <- matrixStats::rowVars(t1.sum, cols=items, na.rm=TRUE)
}
print("check4")	

#print(grep("B_i_raw_", colnames(t1.sum), value=TRUE))
#print(grep("B_i_raw_", colnames(t1.sum), value=F))
### Item residual (e_i = B_i_raw - B_t )
for(content in link.itm$itm) {
  topic <- link.itm %>% .[.$itm == content, "TOPIC"]
  t1.sum[, paste0("e_i_", content)] <- t1.sum[, paste0("B_i_raw_", content)] - t1.sum[, paste0("B_t_", topic)]
}
print("check5")	

### Domain averages of topic difficulties (B_d = mean(B_t)) and respective domain variances
for(content in link.dom$dom) {
 topics <- link.top %>% .[.$DOMAIN == content, "top"] %>% paste0("B_t_", .) %>% match(., colnames(t1.sum))
 t1.sum[, paste0("B_d_", content)] <- matrixStats::rowMeans2(t1.sum, cols=topics, na.rm=TRUE)
 t1.sum[, paste0("V_d_", content)] <- matrixStats::rowVars(t1.sum, cols=topics, na.rm=TRUE)
}
print("check6")	

### Domain averages of item difficulties (B_d_i = mean(B_i_raw)) and respective domain variances
for(content in link.dom$dom) {
 items <- link.itm %>% .[.$DOMAIN == content, "itm"] %>% paste0("B_i_raw_", .) %>% match(., colnames(t1.sum))
 t1.sum[, paste0("B_d_i_", content)] <-  matrixStats::rowMeans2(t1.sum, cols=items, na.rm=TRUE)
 t1.sum[, paste0("V_d_i_", content)] <-  matrixStats::rowVars(t1.sum, cols=items, na.rm=TRUE)
}
print("check7")	

### Topic residual (e_t = B_t - B_d)
for(content in link.top$top) {
 domain <- link.top %>% .[.$top == content, "DOMAIN"]
 t1.sum[, paste0("e_t_", content)] <- t1.sum[, paste0("B_t_", content)] - t1.sum[, paste0("B_d_", domain)] 
}
############# Differences #################
### Between-topic differences (n_diffs=153)
for(content in content.pairs.topic) {
 t1.sum[, paste0("B_tt_", paste0(content, collapse="_"))] <- t1.sum[, paste0("B_t_", content[[1]])] - t1.sum[, paste0("B_t_", content[[2]])] 
 t1.sum[, paste0("V_tt_", paste0(content, collapse="_"))] <- t1.sum[, paste0("V_t_", content[[1]])] - t1.sum[, paste0("V_t_", content[[2]])] 
}
print("check8")	
### Between-domain differences for topic difficulties (n_diffs=6)
for(content in content.pairs.domain) {
 in.content <- paste0(content, collapse="_")
 t1.sum[, paste0("B_dd_", in.content)] <- t1.sum[, paste0("B_d_", content[[1]])] - t1.sum[, paste0("B_d_", content[[2]])] 
 t1.sum[, paste0("V_dd_", in.content)] <- t1.sum[, paste0("V_d_", content[[1]])] - t1.sum[, paste0("V_d_", content[[2]])] 
 t1.sum[, paste0("B_dd_i_", in.content)] <- t1.sum[, paste0("B_d_i_", content[[1]])] - t1.sum[, paste0("B_d_i_", content[[2]])] 
 t1.sum[, paste0("V_dd_i_", in.content)] <- t1.sum[, paste0("V_d_i_", content[[1]])] - t1.sum[, paste0("V_d_i_", content[[2]])] 
}


############### Predicted values #############
### Predicted item difficulties: B_i_hat_
for(content in link.itm$itm) {
 domain <- link.itm %>% .[.$itm == content, "DOMAIN"]
 topic <- link.itm %>% .[.$itm == content, "TOPIC"]
 t1.sum[, paste0("B_i_hat_", content)] <- t1.sum[, paste0("B_d_", domain)] + t1.sum[, paste0("e_t_", topic)] + t1.sum[, paste0("e_i_", content)] 
}

### Overall poor-man's variance estimates (run separately to avoid memory crash)
t1.sum[, "V_a_classes"] <- matrixStats::rowVars(t1.sum, cols=match(paste0("T_c_", link.cla$cla), colnames(t1.sum)), na.rm=TRUE)
t1.sum[, "V_a_students"] <- matrixStats::rowVars(t1.sum, cols=match(paste0("T_p_", link.stu$stu), colnames(t1.sum)), na.rm=TRUE)
t1.sum[, "V_a_domains"] <- matrixStats::rowVars(t1.sum, cols=match(paste0("B_d_", link.dom$dom), colnames(t1.sum)), na.rm=TRUE)
t1.sum[, "V_a_topics"] <- matrixStats::rowVars(t1.sum, cols=match(paste0("B_t_", link.top$top), colnames(t1.sum)), na.rm=TRUE)
t1.sum[, "V_a_items"] <- matrixStats::rowVars(t1.sum, cols=match(paste0("B_i_hat_", link.itm$itm), colnames(t1.sum)), na.rm=TRUE)
t1.sum[, "V_a_topics_res"] <- matrixStats::rowVars(t1.sum, cols=match(paste0("e_t_", link.top$top), colnames(t1.sum)), na.rm=TRUE)
t1.sum[, "V_a_items_res"] <- matrixStats::rowVars(t1.sum, cols=match(paste0("e_i_", link.itm$itm), colnames(t1.sum)), na.rm=TRUE)
t1.sum[, "V_a_total"] <- matrixStats::rowSums2(t1.sum, cols=match(grep("res$|V_a_total", grep("^V_a_", colnames(t1.sum), value = T), value=T, invert = T), colnames(t1.sum)), na.rm=TRUE) + pi^2/3


print("check9")	

### Add Variance Component Proportions
t1.sum[, "VCP_a_person_side"] <- (t1.sum[, "V_a_classes"] + t1.sum[, "V_a_students"]) / t1.sum[, "V_a_total"]
t1.sum[, "VCP_a_classes"] <- t1.sum[, "V_a_classes"] / t1.sum[, "V_a_total"]
t1.sum[, "VCP_a_students"] <- t1.sum[, "V_a_students"] / t1.sum[, "V_a_total"]
t1.sum[, "VCP_a_item_side"] <- (t1.sum[, "V_a_domains"] + t1.sum[, "V_a_topics"] + t1.sum[, "V_a_items"]) / t1.sum[, "V_a_total"] 
t1.sum[, "VCP_a_domains"] <- t1.sum[, "V_a_domains"] / t1.sum[, "V_a_total"]
t1.sum[, "VCP_a_topics"] <- t1.sum[, "V_a_topics"] / t1.sum[, "V_a_total"]
t1.sum[, "VCP_a_items"] <- t1.sum[, "V_a_items"] / t1.sum[, "V_a_total"]
t1.sum[, "VCP_a_person_item"] <- (pi^2/3)/ t1.sum[, "V_a_total"]
t1.sum[, "VCP_p_classes"] <- t1.sum[, "V_a_classes"] / (t1.sum[, "V_a_students"] + t1.sum[, "V_a_classes"])
t1.sum[, "VCP_p_students"] <- t1.sum[, "V_a_students"] / (t1.sum[, "V_a_students"] + t1.sum[, "V_a_classes"])
t1.sum[, "VCP_i_domains"] <- t1.sum[, "V_a_domains"] / (t1.sum[, "V_a_domains"] + t1.sum[, "V_a_topics"] + t1.sum[, "V_a_items"])
t1.sum[, "VCP_i_topics"] <- t1.sum[, "V_a_topics"] / (t1.sum[, "V_a_domains"] + t1.sum[, "V_a_topics"] + t1.sum[, "V_a_items"])
t1.sum[, "VCP_i_items"] <- t1.sum[, "V_a_items"] / (t1.sum[, "V_a_domains"] + t1.sum[, "V_a_topics"] + t1.sum[, "V_a_items"])

save(list=c(paste(model, "sum", sep="."), "dims"), file = stan_summary_file)
# As object is massive, drop from memory after use

rm(content, content.pairs.domain, content.pairs.topic, domain, items, topic, topics, pupils)
}
