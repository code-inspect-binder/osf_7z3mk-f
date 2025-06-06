### Output pretty tables in excel workbook
stan_summary_out_file <- paste(dir.list$out.mod1, paste(model, country, "stan-summary-out.RData", sep="_"), sep="/")
if(!exists("t1.summary")) load(file = stan_summary_out_file)

xlsx_file <- paste(dir.list$out.mod, paste(model, country, "stan.xlsx", sep="_"), sep="/")
stan_summary_xlsx_file <- paste(dir.list$out.mod1, paste(model, country, "stan-summary-xlsx.RData", sep="_"), sep="/")
stan_summary_xlsx_file_res <- try(load(file=stan_summary_xlsx_file), silent=TRUE)

if(class(stan_summary_xlsx_file_res) == "try-error" | !file.exists(xlsx_file)) {

WB <- openxlsx::createWorkbook(creator = "")
addWorksheet(wb = WB, sheetName = "VCP")
addWorksheet(wb = WB, sheetName = "out.DOMTOP")
addWorksheet(wb = WB, sheetName = "out.DOMTOP.DOM")
addWorksheet(wb = WB, sheetName = "out.DOMTOP.TOP")
addWorksheet(wb = WB, sheetName = "out.DOM.diff")
addWorksheet(wb = WB, sheetName = "out.TOP.diff")
addWorksheet(wb = WB, sheetName = "out.TOP.diff.var")
addWorksheet(wb = WB, sheetName = "out.ITEM")
# addWorksheet(wb = WB, sheetName = "item.inliers")
# addWorksheet(wb = WB, sheetName = "item.inliers2")
# addWorksheet(wb = WB, sheetName = "item.inliers3")


###################################
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

### Assemble VCP table (model-based variances)
Var <- t1.summary %>% 
  dplyr::filter(grepl("^V_a_", par), !grepl("^V_a_.*_res$", par)) %>% 
  transmute(par = stringi::stri_trans_totitle(gsub("V_a_", "", par)), Var=`50%`, Var.L=`2.5%`, Var.H=`97.5%`) %>%
  rbind(list("Person_item", pi^2/3, pi^2/3, pi^2/3))

VCPa <- t1.summary %>% 
  dplyr::filter(grepl("^VCP_a_", par)) %>% 
  transmute(par = stringi::stri_trans_totitle(gsub("VCP_a_", "", par)), Of.total = `50%`, Of.total.L=`2.5%`, Of.total.H=`97.5%`) %>%
  rbind(list("Total", 1.0, 1.0, 1.0))
VCPp <- t1.summary %>% 
  dplyr::filter(grepl("^VCP_p_", par)) %>% 
  transmute(par = stringi::stri_trans_totitle(gsub("VCP_p_", "", par)), Of.person = `50%`, Of.person.L=`2.5%`, Of.person.H=`97.5%`) %>%
  rbind(list("Person_side", 1.0, 1.0, 1.0))
VCPi <- t1.summary %>% 
  dplyr::filter(grepl("^VCP_i_", par)) %>% 
  transmute(par = stringi::stri_trans_totitle(gsub("VCP_i_", "", par)), Of.item = `50%`, Of.item.L=`2.5%`, Of.item.H=`97.5%`) %>%
  rbind(list("Item_side", 1.0, 1.0, 1.0))
VCP <- 
  full_join(Var, VCPa, by="par") %>% 
  full_join(., VCPp, by="par") %>% 
  full_join(., VCPi, by="par") %>% 
  slice(c(6, 9, 1, 4, 8, 2, 5, 3, 7))
VCP %>% 
  round_df(., 3) %>% 
  cbind(.) %>% 
  openxlsx::writeData(wb = WB, x=., sheet = "VCP", rowNames = FALSE)
rm(VCPa, VCPp, VCPi, Var)

### Produce pretty table for domains and topics output 
out.DOMTOP <- 
  t1.summary %>%
  select(Domain, par, Est=`50%`, CI2.5=`2.5%`, CI97.5=`97.5%`) %>%
  dplyr::filter(grepl("[[:alpha:]]_d_|[[:alpha:]]_t_", par), !grepl("[[:alpha:]]_d_i_|e_t_", par)) %>% # Using topic averages
  mutate(Group = gsub("[[:alpha:]]_d_|[[:alpha:]]_t_", "", par))
out.DOMTOP.a <- out.DOMTOP %>% dplyr::filter(grepl("^B\\_", .$par)) %>% select(Domain, Group, Est, CI2.5, CI97.5)
out.DOMTOP.b <- out.DOMTOP %>% dplyr::filter(!grepl("^B\\_", .$par)) %>% select(Domain, Group, Var=Est, V_CI2.5=CI2.5, V_CI97.5=CI97.5)
out.DOMTOP <- 
  left_join(x=out.DOMTOP.a, y=out.DOMTOP.b, by=c("Domain", "Group")) %>%
  mutate(Group = gsub("Earths", "Earth`s", Group))
out.DOMTOP.a <- out.DOMTOP[1:4,] %>% arrange(Domain, Group)
out.DOMTOP.b <- out.DOMTOP[-(1:4),] %>% arrange(Domain, Est)
out.DOMTOP <- rbind(out.DOMTOP.a, NA, out.DOMTOP.b)
out.DOMTOP <- cbind(out.DOMTOP[ , c("Domain", "Group")], round(out.DOMTOP[ , 3:ncol(out.DOMTOP)], digits = 3))
rm(out.DOMTOP.a, out.DOMTOP.b)
#Replace this with an append-argument when updated in openxlsx
openxlsx::writeData(wb = WB, sheet = "out.DOMTOP", x=out.DOMTOP, rowNames = FALSE)


### Produce pretty table no. 2 for domains 
out.DOMTOP2 <- 
  t1.summary %>%
  select(Domain, par, Est=`50%`, CI2.5=`2.5%`, CI97.5=`97.5%`) %>%
  dplyr::filter(grepl("[[:alpha:]]_d_|[[:alpha:]]_t_", par), !grepl("e_t_", par)) %>%
  mutate(Group = gsub("[[:alpha:]]_d_|[[:alpha:]]_t_", "", par))
out.DOMTOP2.a <- out.DOMTOP2 %>% dplyr::filter(grepl("B_d_", .$par)) %>% dplyr::filter(!grepl("B_d_i", .$par)) %>% select(Domain, Group, Est, CI2.5, CI97.5)
out.DOMTOP2.b <- out.DOMTOP2 %>% dplyr::filter(grepl("^V_d_", .$par)) %>% dplyr::filter(!grepl("^V_d_i", .$par)) %>% select(Domain, Group, Var=Est, V_CI2.5=CI2.5, V_CI97.5=CI97.5)
out.DOMTOP2.c <- out.DOMTOP2 %>% dplyr::filter(grepl("B_d_i", .$par)) %>% select(Domain, Group, Est, CI2.5, CI97.5) %>% mutate(Group = gsub("^i_", "", Group))
out.DOMTOP2.d <- out.DOMTOP2 %>% dplyr::filter(grepl("^V_d_i", .$par)) %>% select(Domain, Group, Var=Est, V_CI2.5=CI2.5, V_CI97.5=CI97.5)  %>% mutate(Group = gsub("^i_", "", Group))
out.DOMTOP2.e <- out.DOMTOP2 %>% dplyr::filter(grepl("^B_t_", .$par)) %>% select(Domain, Group, Est, CI2.5, CI97.5)
out.DOMTOP2.f <- out.DOMTOP2 %>% dplyr::filter(grepl("^V_t_", .$par)) %>% select(Domain, Group, Var=Est, V_CI2.5=CI2.5, V_CI97.5=CI97.5)
out.DOMTOP2.x <- 
  left_join(x=out.DOMTOP2.a, y=out.DOMTOP2.b, by=c("Group", "Domain")) %>%
  left_join(x=., y=out.DOMTOP2.c, by=c("Group", "Domain")) %>%
  left_join(x=., y=out.DOMTOP2.d, by=c("Group", "Domain")) %>%
  mutate(N.Topics = c(6,3,4,5)) %>%
  left_join(x=., y=select(desc.DOMAIN, DOMAIN, N.Items), by=c('Group' = 'DOMAIN'))
out.DOMTOP2.x <- cbind(out.DOMTOP2.x[ , c("Domain", "Group")], round(out.DOMTOP2.x[ , 3:ncol(out.DOMTOP2.x)], digits = 2))
out.DOMTOP2.x %<>% 
  mutate(i_SD2.5 = Est.y - 1.96*sqrt(Var.y), i_SD97.5 = Est.y + 1.96*sqrt(Var.y)) %>%
  select(D=Domain, Domain=Group, 
         t_Est=Est.x, t_CI2.5 = CI2.5.x, t_CI95.5 = CI97.5.x, t_V = Var.x, t_V_CI2.5 = V_CI2.5.x, t_V_CI97.5 = V_CI97.5.x, t_N = N.Topics,
         i_Est=Est.y, i_CI2.5 = CI2.5.y, i_CI97.5 = CI97.5.y, i_V = Var.y, i_V_CI2.5 = V_CI2.5.y, i_V_CI97.5 = V_CI97.5.y, i_SD2.5, i_SD97.5, i_N = N.Items)
out.DOMTOP2.y <-
  left_join(x=out.DOMTOP2.e, y=out.DOMTOP2.f, by=c("Group", "Domain")) %>%
  left_join(x=., y=select(desc.TOPIC, TOPIC, N.Items), by=c('Group' = 'TOPIC')) %>%
  mutate(i_SD2.5 = Est - 1.96*sqrt(Var), i_SD97.5 = Est + 1.96*sqrt(Var)) %>%
  arrange(Domain, -Est)
out.DOMTOP2.y <- cbind(out.DOMTOP2.y[ , c("Domain", "Group")], round(out.DOMTOP2.y[ , 3:ncol(out.DOMTOP2.y)], digits = 2))
rm(out.DOMTOP2, out.DOMTOP2.a, out.DOMTOP2.b, out.DOMTOP2.c, out.DOMTOP2.d, out.DOMTOP2.e, out.DOMTOP2.f)
#Replace this with an append-argument when updated in openxlsx
openxlsx::writeData(wb = WB, sheet = "out.DOMTOP.DOM", x=out.DOMTOP2.x, rowNames = FALSE)
out.DOMTOP2.y %>% 
  dplyr::filter(!is.na(Domain)) %>%
  openxlsx::writeData(wb = WB, sheet = "out.DOMTOP.TOP", x=., rowNames = FALSE)

### Produce pretty table no. 2 for differences between domains
out.DOM.diff <- 
  t1.summary %>%
  dplyr::filter(grepl("._dd_", par)) %>%
  select(par, CI2.5=`2.5%`, CI97.5=`97.5%`) %>%
  mutate(par = gsub("dd_", "", par),
         i = ifelse(grepl("_i_", par), 1,NA),
         var = ifelse(grepl("^V_", par), 1, NA),
         diff = ifelse(sign(CI97.5)==sign(CI2.5), T, F),
         par = gsub("._i_|^._", "", par)) %>%
  separate(par, into=c("DOMAIN1", "DOMAIN2"), sep="_")
#Replace this with an append-argument when updated in openxlsx
openxlsx::writeData(wb = WB, sheet = "out.DOM.diff", x=out.DOM.diff, rowNames = FALSE)


### Produce pretty table no. 2 for differences between topics
out.TOP.diff <- 
  t1.summary %>%
  dplyr::filter(grepl("B_tt_", par)) %>%
  select(par, CI2.5=`2.5%`, CI97.5=`97.5%`) %>%
  mutate(par = gsub("B_tt_", "", par),
         diff = ifelse(sign(CI97.5)==sign(CI2.5), T, F)) %>%
  separate(par, into=c("TOPIC1", "TOPIC2"), sep="_")
#Replace this with an append-argument when updated in openxlsx
openxlsx::writeData(wb = WB, sheet = "out.TOP.diff", x=out.TOP.diff, rowNames = FALSE)

### Produce pretty table no. 2 for differences between topic variances
out.TOP.diff.var <- 
  t1.summary %>%
  dplyr::filter(grepl("V_tt_", par)) %>%
  select(par, CI2.5=`2.5%`, CI97.5=`97.5%`) %>%
  mutate(par = gsub("V_tt_", "", par),
         diff = ifelse(sign(CI97.5)==sign(CI2.5), T, F)) %>%
  separate(par, into=c("TOPIC1", "TOPIC2"), sep="_")
#Replace this with an append-argument when updated in openxlsx
openxlsx::writeData(wb = WB, sheet = "out.TOP.diff.var", x=out.TOP.diff.var, rowNames = FALSE)



### Variance different from zero: Characteristics, Classification, and Life Processes of Organisms_Life Cycles, Reproduction, and Heredi
t1.summary %>% filter(grepl("^V_tt_Char", par)) %>% slice(13) %>% select(par, `2.5%`, `97.5%`)


### Item output and outliers, first creating general data frame
out.ITEM.a <- t1.summary %>% dplyr::filter(grepl("^B_i_hat_", par)) %>% mutate(par = gsub("^B_i_hat_", "", par))
out.ITEM.b <- t1.summary %>% dplyr::filter(grepl("^e_i_", par)) %>% mutate(par = gsub("^e_i_", "", par))

out.ITEM <- 
  inner_join(out.ITEM.a, out.ITEM.b, by =c("par", "Domain", "TOPIC", "DOMAIN")) %>%
  select(Domain, Item.ID=par, DOMAIN, TOPIC, 
         B_i=mean.x, B_i_se=se_mean.x, B_i_L = `2.5%.x`, B_i_H = `97.5%.x`, 
         e_i=mean.y, e_i_L = `2.5%.y`, e_i_H = `97.5%.y`) %>%
  mutate(Item=paste(Domain, row_number(Domain), sep=""),
         TOPIC = gsub("Earths", "Earth`s", as.character(TOPIC)),
         B_i = round(B_i, 3),
         B_i_L = round(B_i_L, 3),
         B_i_H = round(B_i_H, 3),
         B_i_se = round(B_i_se, 10),
         e_i = round(e_i, 3),
         e_i_L = round(e_i_L, 3),
         e_i_H = round(e_i_H, 3)
  ) %>%
  left_join(., select(desc.ITEM, Item.ID, Released=matches("Released|Secured"), Label), by = 'Item.ID') %>%
  left_join(., select(out.DOMTOP[1:4, ], Domain, B_d = Est, B_d_L = CI2.5, B_d_H = CI97.5), by = "Domain") %>%
  left_join(., select(out.DOMTOP[6:23, ], Group, B_t = Est, B_t_L = CI2.5, B_t_H = CI97.5), by = c('TOPIC' = "Group")) %>%
  mutate(`O` = ifelse(ntile(B_i, 100) > 97.5, "High", ifelse(ntile(B_i, 100) < 2.5, "Low", NA)),
         `T` = ifelse(ntile(e_i, 100) > 97.5, "High", ifelse(ntile(e_i, 100) < 2.5, "Low", NA))) %>%
  group_by(Domain) %>%
  mutate(`D` = ifelse(ntile(B_i, 100) > 97.5, "High", ifelse(ntile(B_i, 100) < 2.5, "Low", NA))) %>%
  ungroup()


### Exporting out.ITEM data frame to XLS for article
out.ITEM %>% # , 
  dplyr::filter(!is.na(`O`) | !is.na(`D`) | !is.na(`T`)) %>%
  arrange(Domain, TOPIC) %>%
  group_by(Domain) %>%
  mutate(Item=paste(Domain, row_number(Domain), sep="")) %>%
  ungroup() %>%
  transmute(Item, Item.ID = gsub("^S0", "", Item.ID), Topic=strtrim(abbreviate(TOPIC, 35), 20), `O`, `D`, `T`, 
            Rel=ifelse(Released %in% c("Yes", "Restricted Use"), "Y", "N"), Label=abbreviate(Label, minlength=35)) %>%
  cbind(.) %>%
  openxlsx::writeData(wb = WB, sheet = "out.ITEM", x = ., rowNames = FALSE)

### Exporting list of content group-representative items
# out.ITEM %>%
  # dplyr::filter(is.na(`O`), is.na(`D`), is.na(`T`), Released %in% c("Yes", "Restricted Use")) %>%
  # arrange(Domain, TOPIC) %>%
  # left_join(., y=select(desc.ITEM, Item.ID, TYPE), by="Item.ID") %>% #COGDOM, , Norway
  # select(Domain, TOPIC, Item.ID, Released, Label, TYPE) %>% #COGDOM, , Norway
  # cbind(.) %>%
  # openxlsx::writeData(wb = WB, sheet = "item.inliers", x = ., rowNames = FALSE)

# out.ITEM %>% 
  # left_join(x=., y=desc.ITEM, by=c("Item.ID", "DOMAIN", "TOPIC", "Released")) %>%
  # filter(DOMAIN == "Earth Science", Released %in% c("Yes", "Restricted Use")) %>% 
  # transmute(Item.ID, e_D=B_i-B_d, TOPIC) %>% #, COGDOM 
  # arrange(e_D) %>% 
  # openxlsx::writeData(wb = WB, sheet = "item.inliers2", x = ., rowNames = FALSE)

# out.ITEM %>% 
  # left_join(x=., y=desc.ITEM, by=c("Item.ID", "DOMAIN", "TOPIC", "Released")) %>%
  # filter(DOMAIN == "Physics", Released %in% c("Yes", "Restricted Use")) %>% 
  # transmute(Item.ID, e_D=B_i-B_d, TOPIC) %>% #, COGDOM 
  # arrange(e_D) %>% 
  # openxlsx::writeData(wb = WB, sheet = "item.inliers3", x = ., rowNames = FALSE)

openxlsx::saveWorkbook(wb = WB, file = xlsx_file, overwrite = TRUE)
save(out.DOMTOP, out.ITEM, out.DOM.diff, out.TOP.diff, out.DOMTOP2.x, out.DOMTOP2.y, VCP, WB, 
	file = stan_summary_xlsx_file)

rm(out.ITEM.a, out.ITEM.b)
}