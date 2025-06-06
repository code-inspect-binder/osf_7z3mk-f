if(Sys.getenv("COMPUTERNAME") != "UV-CEMOPC-05") {
    source("/usit/abel/u1/stephd/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE) ## Computer-specific setting of folders
} else source("S:/Articles/A1-ContentDomains/Syntax/01_config_paths.R", echo=TRUE)


### Join item design matrices ###
## Read in xlxs spreadsheet with item attributes (x=ITEMID, y=ITEMATTR) and merge with main dataframe

library(readxl)
sheets <- paste0(dir.list$almanac, "/", TXX, "_G8_SCI_ItemPercentCorrect.xlsx") %>%
{if(file.exists(.)) readxl::excel_sheets(.)}

if(!is.null(sheets)) {
    if(TXX == "T15") {
        alma <-
            lapply(sheets,
                   function(Sheet) readxl::read_excel(path = paste0(dir.list$almanac, "/", TXX, "_G8_SCI_ItemPercentCorrect.xlsx"), sheet = Sheet, range = "C5:D53")) %>%
            lapply(., set_colnames, value=c("Country.name", "Alm.percent")) %>%
            lapply(., mutate, Alm.percent = as.character(Alm.percent)) %>%
            bind_rows %>%
            dplyr::filter(grepl("International Avg.", Country.name)) %>% #Norway \\(9\\)|
            mutate(Block = rep(sheets, each=1)) %>%
            separate(col = Block, into = c("Block", "Block.Seq"), sep = "_", remove = T) %>%
            spread(key = Country.name, value = Alm.percent) %>%
            transmute(Block, 
                      Block.Seq, 
                      int.almanac.ITEM=as.integer(`International Avg.`))#, Alm.Nor=as.integer(`Norway (9)`))
    } else if(TXX == "T11" || TXX == "T07") { # 2011 or 2007
        itemlist <-
            lapply(sheets,
                   function(Sheet) readxl::read_excel(path = paste0(dir.list$almanac, "/", TXX, "_G8_SCI_ItemPercentCorrect.xlsx"), sheet = Sheet, range = "A2:A3")) %>%
            sapply(., '[[', 1) %>%
            strsplit(split = "_| ") %>%
            lapply(., '[', 1:2)
        alma <-
            lapply(sheets,
                   function(Sheet) readxl::read_excel(path = paste0(dir.list$almanac, "/", TXX, "_G8_SCI_ItemPercentCorrect.xlsx"), sheet = Sheet, range = "A5:R52")) %>%
            lapply(., slice, 47) %>%
            lapply(., select, matches("^DIFF$|^V1$")) %>%
            lapply(., set_colnames, value=c("int.almanac.ITEM")) %>%
            bind_rows %>%
            transmute(Block = unlist(lapply(itemlist, '[', 1)),
                      Block.Seq = unlist(lapply(itemlist, '[', 2)),
                      int.almanac.ITEM=as.numeric(int.almanac.ITEM))
    } #else if(TXX == "T03") {
    # 	itemlist <-
    # 		lapply(sheets,
    # 			   function(Sheet) readxl::read_excel(path = paste0(dir.list$almanac, "/", TXX, "_G8_SCI_ItemPercentCorrect.xlsx"), sheet = Sheet, range = "A2:A3")) %>%
    # 		sapply(., '[[', 1) %>%
    # 		strsplit(split = "_| ") %>%
    # 		lapply(., '[', 1:2)
    # 	alma <-
    # 		lapply(sheets,
    # 			   function(Sheet) readxl::read_excel(path = paste0(dir.list$almanac, "/", TXX, "_G8_SCI_ItemPercentCorrect.xlsx"), sheet = Sheet, range = "A5:R52")) %>%
    # 		lapply(., slice, 47) %>%
    # 		lapply(., select, matches("^DIFF$|^V1$")) %>%
    # 		lapply(., set_colnames, value=c("Alm.int.ITEM")) %>%
    # 		bind_rows %>%
    # 		transmute(Block = unlist(lapply(itemlist, '[', 1)),
    # 				  Block.Seq = unlist(lapply(itemlist, '[', 2)),
    # 				  Alm.int.ITEM=as.numeric(Alm.int.ITEM))
    # }
}

desc.ITEM <-
    readxl::read_excel(path = paste(dir.list$items, paste0(TXX, "_G8_ItemInformation.xlsx"), sep="/"), sheet = "SCI") %>%
    set_colnames(value = gsub("Scaling.*Status", "In.Scaling", make.names(colnames(.)))) %>%
    arrange(Content.Domain, Topic.Area) %>%
    mutate(Topic.Area = gsub("their", "Their", Topic.Area), ### Clean original entries from TIMSS authors
           Topic.Area = gsub("Earth.s", "Earths", Topic.Area),
           TOPIC = factor(Topic.Area),
           DOMAIN = factor(Content.Domain),
           TYPE = ifelse(ItemType == "MC", 0, ifelse(Maximum.Points == 2, 2, 1)),
           TYPE = factor(TYPE, labels = c('0' = "MC", '1' = "CR", '2' = "PC")), ## TYPE(0=MC; 1=CR; 2=PC);
           In.Scaling = factor(In.Scaling, levels = c("No", "Yes"))) %>% 
    select(-Grade, -Subject, -Cycle) %>%
	left_join(x=., y=alma, by=c("Block", "Block.Seq")) %>%
    left_join(x=., y=summarize(group_by(., DOMAIN), int.almanac.DOMAIN = mean(int.almanac.ITEM, na.rm=TRUE)), by="DOMAIN") %>%
    left_join(x=., y=summarize(group_by(., TOPIC), int.almanac.TOPIC = mean(int.almanac.ITEM, na.rm=TRUE)), by="TOPIC")

# omit <- c(
#   "S052092A", "S052092B", "S052092C", "S052092D",
#   "S052095B", "S052095C", "S052095D",
#   "S032530A", "S032530B",
#   "S052015A", "S052015B", "S052015C", "S052015D", "S052015E", "S052015F",
#   "S052043A", "S052043B", "S052043C", "S052043D",
#   "S032650A", "S032650B",
#   "S042173A", "S042173B", "S042173C", "S042173D", "S042173E",
#   "S052095A", "S042311", "S052221", "S042401") # The "deleted" items
omit <- desc.ITEM %>% dplyr::filter(In.Scaling != "Yes") %>% select(Item.ID) %>% unlist() %>% unname() # Same as the above
keep <- setdiff(desc.ITEM$Item.ID, omit)
desc.ITEM %<>% dplyr::filter(In.Scaling == "Yes") #same as !Item.ID %in% omit #Keeping all but No or Deleted = omit

#### Retreive the international responses for all items
### This operation might require a computer with >8GB RAM
source(file = paste(dir.list$aux, "GetIEAData.R", sep = "/")) ## This function reads in IEA data and recodes as specified


data_int <-
    GetIEAData(countries=NULL, achievement.only = TRUE, fixed.var.names = FALSE, recursive = TRUE, download = FALSE,
               dir = dir.list$data, wave = gsub("T", "", TXX), grade = "G8", subject = "SCI", vars="^S0|WGT|ID|BSDAGE",
               scoring = TRUE, item.df = desc.ITEM, item.id = "Item.ID", key.var = "Key",
               CR.recode = "10:19=1; 20:29=1; 70:79=0; 96=NA; 99=0;") %>% 
    select(-one_of(omit)) ### Dropping deleted items

### Descriptive statistics and diagnostics
## Join item Simple.Props to desc.ITEM
prop <- ldply(data_int[, keep], mean, na.rm = TRUE, digits = 3, .id = "Item.ID")
miss <- colwise(function(x) sum(is.na(x)))(data_int[, keep])
desc.ITEM %<>% 
    left_join(by = "Item.ID", x = ., y = data.frame(Item.ID = as.character(prop$Item.ID), Simple.Prop = prop$V1)) %>%
    left_join(by = "Item.ID", x = ., y = data.frame(Item.ID = names(miss), Missing = as.integer(miss)))

desc.STUD <- list(
    DuplicateSTUDIDs = nrow(data_int) == length(unique(data_int$STUDID)),  			#no duplicated student ID?
    SchoolOn2Countries = lme4::isNested(data_int$SCHOOLID, data_int$CNTRYID),
    ClassOn2Countries = lme4::isNested(data_int$CLASSID, data_int$CNTRYID),
    StudentOn2Countries = lme4::isNested(data_int$STUDID, data_int$CNTRYID),
    ClassOn2Schools = lme4::isNested(data_int$CLASSID, data_int$SCHOOLID),
    StudentOn2Schools = lme4::isNested(data_int$STUDID, data_int$SCHOOLID),
    StudentOn2Class = lme4::isNested(data_int$STUDID, data_int$CLASSID)
)

#################################################################################
source(paste(dir.list$aux, "recontrast.R", sep="/"))
### Wide to long
## Note: One STUDID is lost if na.rm = TRUE because all missing items		   
data_int_long <-
    data_int %>%
    reshape2::melt(., measure.vars = keep, variable.name = "Item.ID", value.name = "Y", na.rm = T, id.vars = grep("WGT|ID|BSDAGE", colnames(.), value = TRUE)) %>%
    left_join(x=., y=select(desc.ITEM, Item.ID, DOMAIN, TOPIC), by = "Item.ID") %>% ## Merge item attributes data frame and main dataset
    mutate(DOMAIN1=ifelse(DOMAIN == "Biology", 1, 0),
           DOMAIN2=ifelse(DOMAIN == "Chemistry", 1, 0),
           DOMAIN3=ifelse(DOMAIN == "Earth Science", 1, 0),
           DOMAIN4=ifelse(DOMAIN == "Physics", 1, 0)) %>%
    recontrast(data = ., type = "sum", lim = 250)

save(data_int, data_int_long, file = paste(dir.list$out.gen, "data_int.RData", sep="/"))

desc.ITEM <- 
    data_int_long %>%
    group_by(IDCNTRY, Item.ID) %>%
    summarize(int.data = mean(Y, na.rm=TRUE)) %>%
    group_by(Item.ID) %>%
    summarize(int.data.ITEM = mean(int.data, na.rm=TRUE)) %>%
    ungroup() %>%
    left_join(x=desc.ITEM, y=., by="Item.ID")

desc.ITEM %<>%
    group_by(TOPIC) %>%
    summarize(int.data.TOPIC = mean(int.data.ITEM, na.rm=TRUE),
              Simple.Prop.T = mean(Simple.Prop, na.rm = TRUE)) %>%
    left_join(x=desc.ITEM, y=., by="TOPIC")
desc.ITEM %<>%
    group_by(DOMAIN) %>%
    summarize(int.data.DOMAIN = mean(int.data.ITEM, na.rm=TRUE),
              Simple.Prop.D = mean(Simple.Prop, na.rm = TRUE)) %>%
    left_join(x=desc.ITEM, y=., by="DOMAIN")


#### Save country-specific data sets for easy access
cnts <- 
    readxl::read_excel(path = paste(dir.list$input, "Countries.xlsx", sep="/"), sheet=TXX) %>%
    dplyr::filter(as.integer(ISO.num) %in% unique(data_int_long$IDCNTRY))

lapply(X = as.integer(pull(cnts, ISO.num)), FUN = function(cnt_id) {
    data <- 
        data_int_long %>%
        dplyr::filter(IDCNTRY == cnt_id)
    
        ### Make data-list for Stan models
    dat = list(
        N   = nrow(data),
        N_P = length(unique(data$STUDID)),
        N_C = length(unique(data$CLASSID)),
        N_I = length(unique(data$Item.ID)),
        N_T = length(unique(data$TOPIC)),
        N_D = length(unique(data$DOMAIN)),
        
        Y 	= data$Y,
        Domain  = as.numeric(as.factor(data$DOMAIN)),
        Domain1  = data$DOMAIN1,
        Domain2  = data$DOMAIN2,
        Domain3  = data$DOMAIN3,
        Domain4  = data$DOMAIN4,
        Topic	= as.numeric(as.factor(data$TOPIC)),
        Item	= as.numeric(as.factor(data$Item.ID)),
        Person = as.numeric(as.factor(data$STUDID)),
        Class = as.numeric(as.factor(data$CLASSID))
    )
    dat$Domain_t <- distinct(data, TOPIC, .keep_all = TRUE) %>% select(DOMAIN) %>% unlist() %>% unname() %>% as.factor() %>% as.numeric()
    dat$Topic_i <- distinct(data, Item.ID, .keep_all = TRUE) %>% select(TOPIC) %>% unlist() %>% unname() %>% as.factor() %>% as.numeric()
    dat$Class_p <- distinct(data, STUDID, .keep_all = TRUE) %>% select(CLASSID) %>% unlist() %>% unname() %>% as.factor() %>% as.numeric()
    dat$TOTWGT <- distinct(data, STUDID, .keep_all = TRUE) %>% select(TOTWGT) %>% unlist() %>% unname()
    
    
    ##################################################################################
    ##### Descriptives #####
    ##################################################################################
    # 18 students have missing on all items and thus dropped from further analyses
    ## Missing data is 824529 / 957924 = 86% [In collapsed df: 813348 / 945624] before omitting deleted items
    
    ## Descriptives by groups: Identifier, median, absolute range (max-min), total count of observations, and number of unique items
    
    ### Simple.Prop correct across items, and Simple.Prop covered in the intended curriculum across items
    Means216 <- 
        data %>%
        dplyr::distinct(Item.ID, .keep_all = TRUE) %>%
        select(Item.ID, DOMAIN, TOPIC) %>%
        data.frame(., Simple.Prop = with(data, aggregate(Y, list(Item.ID), mean, na.rm=T)[,2]))
    
    
    ### Descriptive stats for number of observations per unit (DOMAIN, TOPIC, OBJECTIVE & ITEM)
    Means4 <- 
        data %>%
        ddply(., c("DOMAIN", "Item.ID"), nrow) %>%
        ddply(., "DOMAIN", summarise, Medians = median(V1), Mins = min(V1), Maxes = max(V1),
              Abs.range = max(V1) - min(V1), Total = sum(V1), N.Items = length(unique(Item.ID)))
    Means18 <- 
        data %>%
        ddply(., c("DOMAIN", "TOPIC", "Item.ID"), nrow) %>%
        ddply(., c("DOMAIN", "TOPIC"), summarise, Medians = median(V1), Mins = min(V1), Maxes = max(V1),
              Abs.range = max(V1) - min(V1), Total = sum(V1), N.Items = length(unique(Item.ID)))
    Means216b <- 
        data %>%
        ddply(., c("DOMAIN", "TOPIC", "Item.ID"), nrow) %>%
        ddply(., "Item.ID", summarise, Total = sum(V1))
    
    # ### Data frame
    desc.SCIENCE.NA <- data.frame(Var = colnames(data),
                                  UniqueValues = sapply(data, function(x) length(unique(x))),
                                  MissIncl = "Yes")
    desc.SCIENCE <- data.frame(Var = colnames(data),
                               UniqueValues = sapply(data, function(x) length(unique(x))),
                               MissIncl = "No")
    row.names(desc.SCIENCE) <- row.names(desc.SCIENCE) <- NULL
    desc.SCIENCE %<>% 
        full_join(., desc.SCIENCE.NA, by=c("Var", "UniqueValues")) %>%
        arrange(desc(UniqueValues))
    rm(desc.SCIENCE.NA)
    ### Students: Data points (items) + Duplicate ID variables
    desc.STUD %<>%
        cbind(., data.frame(
            mean.I = mean(table(data$STUDID)),
            min.I = min(table(data$STUDID)),
            max.I = max(table(data$STUDID)),
            median.I = median(table(data$STUDID)),
            N.included.STUDENT = n_distinct(data$IDSTUD),
            N.included.CLASS = n_distinct(data$IDCLASS),
            N.included.SCHOOL = n_distinct(data$IDSCHOOL),
            m.age = mean(distinct(data, IDSTUD, .keep_all=T)$BSDAGE, na.rm=TRUE)
        ))
    #View(desc.STUD)
    
    ### DOMAIN
    desc.DOMAIN <- data.frame(
        Means4,
        Simple.Prop = with(data, aggregate(Y, list(DOMAIN), mean, na.rm=T))[,2], #Simple average of responses
        # Simple.Prop = with(Means216, aggregate(Simple.Prop, list(DOMAIN), mean, na.rm=T))[,2], # Average of items
        Item.Var = with(Means216, aggregate(Simple.Prop, list(DOMAIN), mean, na.rm=T))[,2]*(1-with(Means216, aggregate(Simple.Prop, list(DOMAIN), mean, na.rm=T))[,2]))
    
    attr(desc.DOMAIN, c("Items", "Items.sd")) <- c(summary(desc.DOMAIN$N.Items), Std.Dev. = round(sd(desc.DOMAIN$N.Items), 2))
    #View(desc.DOMAIN, title="DOMAIN")
    
    
    ### TOPIC
    desc.TOPIC <- data.frame(
        Means18,
        Simple.Prop = with(data, aggregate(Y, list(TOPIC), mean, na.rm=T))[,2], #Simple average of responses
        # Simple.Prop = with(Means216, aggregate(Simple.Prop, list(TOPIC), mean))[,2], # Average of items
        Item.Var = with(Means216, aggregate(Simple.Prop, list(TOPIC), mean))[,2]*(1-with(Means216, aggregate(Simple.Prop, list(TOPIC), mean))[,2]))
    attr(desc.TOPIC, c("Items", "Items.sd")) <- c(summary(desc.TOPIC$N.Items), Std.Dev. = round(sd(desc.TOPIC$N.Items), 2))

    ### ITEM
    desc.ITEM %<>% 
        left_join(x = ., y = data_frame(
            Item.ID = Means216b$Item.ID, 
            Simple.Prop = Means216$Simple.Prop,
            Total = Means216b$Total,
            Item.Var = Simple.Prop*(1-Simple.Prop)),
            by = c("Item.ID", "Simple.Prop"))
    

    
    ### DOMTOP: combined domain+topic+total
    desc.DOMTOP <- plyr::rbind.fill(desc.DOMAIN[, c("DOMAIN", "N.Items", "Simple.Prop", "Item.Var", "Mins", "Medians", "Maxes", "Total")],
                                    desc.TOPIC[, c("DOMAIN", "TOPIC", "N.Items", "Simple.Prop", "Item.Var", "Mins", "Medians", "Maxes", "Total")],
                                    data.frame(DOMAIN = "TOTAL", 
                                               N.Items = length(unique(data$Item.ID)), 
                                               Simple.Prop = mean(data$Y, na.rm = TRUE), 
                                               Item.Var = mean(data$Y, na.rm = TRUE)*(1-mean(data$Y, na.rm = TRUE)),
                                               Mins = min(desc.ITEM$Total), 
                                               Medians = median(desc.ITEM$Total),
                                               Maxes = max(desc.ITEM$Total), 
                                               Total = nrow(data))) %>%
        select(DOMAIN, TOPIC, N.Items, Simple.Prop, Item.Var, Mins, Medians, Maxes, Total) %>%
        mutate(Domain = ifelse(DOMAIN == "Biology", "B", 
                               ifelse(DOMAIN == "Chemistry", "C", 
                                      ifelse(DOMAIN == "Earth Science", "E", 
                                             ifelse(DOMAIN == "Physics", "P", NA)))))
    # 
    # ### Add eyeballed-outlier column based on model output (later). Placed here due to various model runs
    # out <- list(
    #     c("S052046","S052006", "S042063"), #Abs easy
    #     c("S042400", "S052165A", "S042293B", "S042195"), #abs hard
    #     c("S052006", "S042038", "S042053", "S052003", "S032530Z", "S052093"), #Bio easy
    #     c("S052263A", "S052263B", "S042030"), # Biology hard
    #     c("S042063", "S052046"), #Chem easy
    #     c("S042400"), #Chem hard
    #     c(""), # Earth Science easy
    #     c(""), # ES hard
    #     c("S042238C"), #Phy easy
    #     c("S042195", "S042293B", "S052165A"), #Phy hard
    #     #Easier than expected
    #     c("S052046", #Chemical change
    #       "S032614", "S052093", "S032530Z", "S052003", # Life cycles, Preproduction ... (4 harder than the remaining very easy ones)
    #       "S052006", #Diversity, Adaptation, ...
    #       "S042173Z"), #Physical States and ... (outlier)
    #     #Much harder than the topic and domain would suggest
    #     c("S042195", #Electricity and magnestism
    #       "S042293B", #Forces and motion
    #       "S042400", #Classification and Composition of Matter
    #       "S052165A", #Energy transformations, Heat and Temperature
    #       "S042030", #Life cycles
    #       "S052263A", #Cells and their functions
    #       "S052032", #Earth's processes, ...
    #       "S052085A") 
    # )
    # names(out) <- c("Abs easy", "Abs hard", "Bio easy", "Bio hard", "Chem easy", "Chem hard", "ES easy", "ES hard", "Phy easy", "Phy hard", "Rel top easy", "Rel top hard")
    # desc.ITEM$Eyeball <- ""
    # for(i in 1:length(out)) {
    #     desc.ITEM %<>% mutate(Eyeball = ifelse(Item.ID %in% out[[i]], paste(Eyeball, names(out)[[i]], sep=", "), Eyeball))
    # }
    # desc.ITEM %<>% mutate(Eyeball = gsub("^, ", "", Eyeball))
    # desc.ITEM %>%
    #     mutate(D = ifelse(DOMAIN == "Biology", "B", ifelse(DOMAIN == "Chemistry", "C", ifelse(DOMAIN == "Earth Science", "E", "P")))) %>%
    #     select(D, TOPIC, Item.ID, Simple.Prop, Eyeball) %>%
    #     filter(Eyeball != "") %>%
    #     openxlsx::write.xlsx(., file = paste(dir.list$out.gen, paste0(country, "-item.outliers.xlsx"), sep="/"), sheetName = "Outliers", row.names=F)
    # 
    # ### Descriptive statistics for DOMTOP
    # desc.DOMTOP %>%
    #     mutate(D = ifelse(N.Items>38, T, F),
    #            TOPIC = ifelse(is.na(TOPIC), as.character(DOMAIN), as.character(TOPIC))) %>%
    #     transmute(Domain=Domain, 
    #               TOPIC = gsub("Earths", "Earth's", TOPIC), 
    #               `Number of items` = N.Items, Prob=sprintf("%1.0f %%", 100*Simple.Prop), 
    #               Min=Mins, Avg = Medians, Max=Maxes,  Total, D) %>%
    #     arrange(desc(D), Domain, Prob) %>%
    #     select(-D, -Prob) %>%
    #     slice(c(5,1:4, 6:23)) %>%
    #     openxlsx::write.xlsx(., file = paste(dir.list$out.gen, paste0(country, "-desc.DOMTOP.xlsx"), sep="/"), sheetName = "desc.DOMTOP", row.names=F)
    
    save(data, dat, desc.ITEM,  desc.STUD, desc.ITEM, desc.TOPIC, desc.DOMAIN, desc.SCIENCE, desc.DOMTOP, 
         file = paste(dir.list$out.gen, paste0("data_", cnts[as.integer(cnts$ISO.num) == cnt_id, "ISO.alpha"], ".RData"), sep="/"))
    NULL
    })

