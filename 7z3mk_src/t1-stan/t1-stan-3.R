### Create nice looking summary data frame
stan_summary_out_file <- paste(dir.list$out.mod1, paste(model, country, "stan-summary-out.RData", sep="_"), sep="/")
stan_summary_out_result <- try(load(file = stan_summary_out_file), silent=TRUE)

if(class(stan_summary_out_result) == "try-error") {

t1.summary <-
    array(data = t1.sum, dim = c(dim(t1.sum)[1]/dims[[3]], dims[[3]], dim(t1.sum)[2]), 
          dimnames = list(iterations = NULL,
                          chains = paste0("chain:", seq_len(dims$chains)),
                          parameters=colnames(t1.sum))) #rstan::monitor wants it as iterations-chains-parameters
t1.summary <- #cannot combine with previous as t1.summary must be stored first
  rstan::monitor(t1.summary, probs = c(0.5, .025, .975), digits_summary = 4, print = F, warmup = 0) # Burn-in has already been discarded
rows <- rownames(t1.summary)
t1.summary %<>%
  as_data_frame(.) %>%
  mutate(par = rows,
		 B_i = gsub("[[:alpha:]]_i.*_|[[:alpha:]]_i_.*_", "", par), # Creates temporary columns for joining with desc.ITEM, etc
         B_t = gsub("[[:alpha:]]_t_", "", par),
         B_d = gsub("[[:alpha:]]_d_|[[:alpha:]]_d_i_", "", par)) %>%
  left_join(x=., y = mutate(select(desc.ITEM, Item.ID, DOMAIN, TOPIC), DOMAIN = as.character(DOMAIN)), by=c('B_i' = "Item.ID")) %>%
  left_join(x=., y = mutate(select(desc.TOPIC, DOMAIN, TOPIC), DOMAIN = as.character(DOMAIN), TOPIC = as.character(TOPIC)), by=c('B_t' = "TOPIC")) %>%
  mutate(DOMAIN = gsub("[[:alpha:]]_d_i_|[[:alpha:]]_d_|[[:alpha:]]_.._", "", ifelse(!is.na(DOMAIN.x), DOMAIN.x, ifelse(!is.na(DOMAIN.y), DOMAIN.y, ifelse(!is.na(B_d), B_d, NA)))),
         TOPIC = gsub("[[:alpha:]]_d_i_|[[:alpha:]]_d_|[[:alpha:]]_.._", "", ifelse(!is.na(TOPIC), as.character(TOPIC), ifelse(!is.na(B_t), B_t, NA))),
         Domain = ifelse(DOMAIN == "Biology", "B", ifelse(DOMAIN == "Chemistry", "C", ifelse(DOMAIN == "Earth Science", "E", ifelse(DOMAIN=="Physics", "P", NA))))) %>%
  select(-B_i, -B_t, -B_d, -DOMAIN.x, -DOMAIN.y) %>%
  arrange(par)
  
save(list=c(paste(model, "summary", sep=".")), file = stan_summary_out_file)
}
