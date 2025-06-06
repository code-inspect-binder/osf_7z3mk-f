############################################################
### PLOTTING with content groups ###
############################################################
stan_summary_xlsx_file <- paste(dir.list$out.mod1, paste(model, country, "stan-summary-xlsx.RData", sep="_"), sep="/")
stan_props_summary_file <- paste(dir.list$out.mod1, paste(model, country, "stan-props-summary.RData", sep="_"), sep="/")
if(!exists("out.DOMTOP")) load(file = stan_summary_xlsx_file)  # Needs the T_p_ and B_i_hat_
if(!exists("prop_sum")) load(file = stan_props_summary_file) #summary statistics of proportions per item

cut_off_value <- 50
library(ggplot2)
library(grid)

# 1. Caterpillar plot of topic averages of item difficulties, with error bars and domain specificiation
TopCat <- function(df, prob=F) {
  # df is a summary data frame with rows={domains, topics, items} and cols={Est, TOPIC, etc}
  df1 <- dplyr::filter(df, Group == DOMAIN)  ## Extract DOMAIN rows
  df2 <- dplyr::filter(df, Group == TOPIC) %>% ## Extract TOPIC rows
      mutate(Order = reorder(TOPIC, seq_along(Order))) 
  df3 <- dplyr::filter(df, grepl("S0", Group)) ## Extract ITEM rows

	ggplot(df2) + 
    facet_grid(DOMAIN ~ ., scales="free", space = "free", drop=T, switch = "y") +
    geom_hline(data=df1, aes(yintercept = Est), linetype = "longdash", size=0.6, colour="gray60") +
    geom_hline(data=df1, aes(yintercept = CI2.5), linetype = "dotted", size=0.4, colour="gray60") +
    geom_hline(data=df1, aes(yintercept = CI97.5), linetype = "dotted", size=0.4, colour="gray60") +
	geom_point(aes(x=Order, y=Est), shape=16, size=1.8) + # Needed to ensure correct ordering
	geom_violin(data=df3, aes(x=TOPIC, y=Est), trim = T, linetype="dashed", color=NA, fill="grey") +
	geom_errorbar(aes(x=Order, ymin=CI2.5, ymax=CI97.5), width = .2, size=.8) +
    geom_point(aes(x=Order, y=Est), shape=16, size=1.8) +
	geom_point(aes(x=TOPIC, y=Comparison), shape=4, size=1.8) +
    coord_flip() +
	scale_x_discrete(position="top") +
	scale_y_continuous(limits = if(prob) c(0,1) else c(-2,2), expand = c(0, 0),
						breaks = function(lims) {seq(from = lims[1], to = lims[2], by = (lims[2]-lims[1])/5)}, 
						labels = function(breaks) {sprintf(fmt = "%1.1f", breaks)}) +
    labs(y=if(prob) "Avg. item proportion correct" else "Avg. item difficulty (logits)", x="") + 
    theme_bw() +
    theme(text = element_text(size=20),
    	  panel.grid = element_blank(),
          panel.grid.major.x = element_line(linetype="1F", colour="gray90"),
		  panel.border = element_blank(),
		  panel.spacing = unit(2, "mm"),
          axis.text = element_text(color="gray25", size = 16),
		  axis.title.x = element_text(hjust = 0.5, colour="gray10")
          ) 
}

### Logits
bind_rows(DOM = dplyr::transmute(out.DOMTOP2.x, 
                                     DOMAIN= Domain, TOPIC = NA, Group = DOMAIN, Est = t_Est, CI2.5 = t_CI2.5, CI97.5 = t_CI95.5, Comparison = NA),
              TOP = dplyr::transmute(out.DOMTOP2.y,
                           DOMAIN= as.character(factor(Domain, labels = as.character(desc.DOMAIN$DOMAIN))), 
                           TOPIC = gsub("Earths", "Earth`s", Group),
                           TOPIC = unlist(lapply(strwrap(TOPIC, width=cut_off_value, simplify=FALSE), paste, collapse="\n")),
                           Group = gsub("Earths", "Earth`s", Group),
                           Group = unlist(lapply(strwrap(Group, width=cut_off_value, simplify=FALSE), paste, collapse="\n")), 
                           Est, CI2.5, CI97.5) %>%
                  dplyr::left_join(x=., y=dplyr::transmute(out.DOMTOP2.x, DOMAIN=Domain, Comparison=NA), by="DOMAIN") %>% #t_Est
                  arrange(DOMAIN, Est) %>%
                  mutate(Order = 1:nrow(.)) %>%
                  dplyr::filter(!is.na(DOMAIN)),
              ITM = dplyr::transmute(out.ITEM, 
                           DOMAIN, 
                           TOPIC = gsub("Earths", "Earth`s", as.character(TOPIC)),
                           TOPIC = unlist(lapply(strwrap(TOPIC, width=cut_off_value, simplify=FALSE), paste, collapse="\n")),
                           Group=Item.ID, 
                           Est=B_i, CI2.5=B_i_L, CI97.5=B_i_H, Comparison = B_t), .id = "SOURCE") %>%
        as_tibble() %>% 
        TopCat(df = .) %>%
	ggsave(plot=., filename = paste(dir.list$out.mod, paste(model, country, "topics_logit.png", sep="-"), sep="/"), width = 350, height = 200, units = "mm", dpi=350)

### Probabilities
dplyr::transmute(prop_sum, 
                         DOMAIN, 
                         TOPIC = gsub("Earths", "Earth`s", TOPIC), 
                         TOPIC = unlist(lapply(strwrap(TOPIC, width=cut_off_value, simplify=FALSE), paste, collapse="\n")),
                         Group = gsub("Earths", "Earth`s", Group),
                         Group = unlist(lapply(strwrap(Group, width=cut_off_value, simplify=FALSE), paste, collapse="\n")),
                         Est = Median, CI2.5 = CI.95L, CI97.5 = CI.95H, Comparison=Almanac) %>% 
        arrange(DOMAIN, Est) %>%
        mutate(Order = 1:nrow(.)) %>%
        TopCat(df=.,prob=T) %>%
	ggsave(plot=., filename = paste(dir.list$out.mod, paste(model, country, "topics_prop.png", sep="-"), sep="/"), width = 400, height = 200, units = "mm", dpi=350)


# # 3. Correlations and plots for wgt proportions, fixed and random
### Topics
#prop_sum %>% 
#  dplyr::filter(Group %in% desc.TOPIC$TOPIC) %>%
#  arrange(Domain, Simple.Prop) %>%
#  ggplot(data = .) + 
#    geom_point(aes(x=Group, y=Simple.Prop), shape=16) + 
#    geom_point(aes(x=Group, y=Almanac), shape=4) + 
#    facet_grid(facets=.~Domain, scales = "free_x", space = "free_x", drop=T) +
#    theme(axis.text.x = element_text(angle = 50, hjust = 1, color="black", size = 12)) %>%
#	ggsave(plot=., filename = paste(dir.list$out.mod, paste(model, country, "topics_cor.png", sep="-"), sep="/"), width = 297, height = 210, units = "mm", dpi=300)

