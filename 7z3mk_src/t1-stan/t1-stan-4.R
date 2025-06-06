### DIAGNOSTICS PLOTS ###
stan_file <- paste(dir.list$out.mod1, paste( model, country, "stan.RData", sep="_"), sep="/")
if(!exists("t1.stan")) load(file = stan_file)
stan_summary_file <- paste(dir.list$out.mod1, paste( model, country, "stan-summary.RData", sep="_"), sep="/")
if(!exists("t1.sum")) load(file = stan_summary_file)
stan_summary_out_file <- paste(dir.list$out.mod1, paste( model, country, "stan-summary-out.RData", sep="_"), sep="/")
if(!exists("t1.summary")) load(file = stan_summary_out_file)

if(TRUE) try({
n_rows <- nrow(t1.sum)
pre <- paste0("/", model, "-")
pars <- colnames(t1.sum) %>% .[grepl("^B_|^s", .)] %>% .[!grepl("B_i_hat", .)]
t1.sum <- cbind(t1.sum, 
					Iteration = rep(seq(from=n_rows/dims$chains+1, to=2*n_rows/dims$chains), times=dims$chains), 
					Chain = factor(rep(seq_len(dims$chains), each=n_rows/dims$chains)))
t1.sum.graph <- as.data.frame(t1.sum)
res <- c(width=300, height=100, dpi=600)

#########
plot <- "trace"
prefix <- paste0(dir.list$out.mod.graph, pre, plot, "-")
if(any(!file.exists(paste0(prefix, pars, ".png")))) {
for(par in pars) {
  p <- 
    ggplot(data = t1.sum.graph[, c("Iteration", "Chain", par)], 
			aes_string(x = "Iteration", y=paste0("`", par, "`"))) + 
    geom_line(size=.001, alpha=.8) +  scale_x_discrete(expand = c(0,0)) + facet_wrap(facets=~Chain, nrow=2) + theme_classic()
	ggsave(filename = paste0(prefix, par, ".png"), plot = p, width = 300, height = 100,dpi = 600, units = "mm", limitsize=F)
}
}
plot <- "hist"
prefix <- paste0(dir.list$out.mod.graph, pre, plot, "-")
if(any(!file.exists(paste0(prefix, pars, ".png")))) {
for(par in pars) {
  p <- ggplot(data = t1.sum.graph[, c("Iteration", "Chain", par)], aes_string(paste0("`", par, "`"))) + 
    geom_histogram(bins=200, fill="red") + facet_wrap(facets = ~ Chain, nrow=2) + theme_classic()
  if(!file.exists(paste0(prefix, par, ".png"))) ggsave(filename = paste0(prefix, par, ".png"), plot = p, width = 300, height = 100,dpi = 600, units = "mm")
}
}
## Currently does not work with computed parameters
plots <- c("stan_ac", "stan_dens", "stan_plot", "stan_mcse", "stan_ess", "stan_rhat")
N_items <- t1.stan@par_dims$beta_I
library(rstan)
for(plot in plots) {
  prefix <- paste0(dir.list$out.mod.graph, pre, plot, "-")
  infix <- ''
  if(match(plot, plots) < 4) infix <- 'include = T, nrow = 4,'
  if(match(plot, plots) %in% 4:6) infix <- paste0(infix, 'bins=30,')
  pars <- grep("^mu|^sigma_", names(t1.stan), value=T)
  png(file = paste0(prefix, "sigma-%d.png"), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  print(eval(parse(text = paste0(plot, '(t1.stan,', infix,  'pars = pars)'))))
  dev.off()
  # pars <- grep("^B_d_", t1.summary$par, value=T)
  # png(file = paste0(prefix, "B_d_%d.png"), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  # print(eval(parse(text = paste(plot, '(t1.stan,', infix,  'pars = pars)', sep=""))))
  # dev.off()
  # pars <- grep("^V_d_", t1.summary$par, value=T)
  # png(file = paste0(prefix, "V_d_%d.png"), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  # print(eval(parse(text = paste(plot, '(t1.stan,', infix,  'pars = pars)', sep=""))))
  # dev.off()
  pars <- grep("^beta_T", names(t1.stan), value=T)
  png(file = paste0(prefix, "B_t_%d.png"), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  print(eval(parse(text = paste(plot, '(t1.stan,', infix,  'pars = pars)', sep=""))))
  dev.off()
  # pars <- grep("^e_t_", t1.summary$par, value=T)
  # png(file = paste0(prefix, "e_t_%d.png"), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  # print(eval(parse(text = paste(plot, '(t1.stan,', infix,  'pars = pars)', sep=""))))
  # dev.off()
  # pars <- grep("^V_t_", t1.summary$par, value=T)
  # png(file = paste0(prefix, "V_t_%d.png"), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  # print(eval(parse(text = paste(plot, '(t1.stan,', infix,  'pars = pars)', sep=""))))
  # dev.off()
  #
  # png(file = paste0(prefix, "B_i_S0-%d.png"), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  # for(item in split(items, ceiling(1:216/16))) {
  #   pars <- items[match(item, items)]
  #   print(eval(parse(text = paste0(plot, '(t1.stan,', infix,  'pars = pars)'))))
  # }
  # dev.off()
  png(file = paste0(prefix, "B_i_raw_%d.png"), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  for(item in split(1:N_items, ceiling(1:N_items/16))) {
    pars <- paste0("beta_I[", item, "]")
    print(eval(parse(text = paste0(plot, '(t1.stan,', infix,  'pars = pars)'))))
  }
  dev.off()
}


### Technical diagnostics
sink(file = paste(dir.list$out.mod.graph, paste0(model, "convergence_diagnostics.txt"), sep="/"))
lapply(get_sampler_params(t1.stan, inc_warmup = TRUE), summary, digits = 2)
sink()
prefix <- paste(dir.list$out.mod.graph, pre, "stan_diag", "-", sep="")
png(file = paste(prefix, "%d.png", sep=""), width = 10000, height = 5000, units = "px", res=300, type="cairo")
print(stan_diag(t1.stan, nrow=3))
dev.off()

### Technical diagnostics #2
prefix <- paste(dir.list$out.mod.graph, pre, "stan_par", "-", sep="")
pars <- t1.stan@sim$fnames_oi
for (par in pars[!grepl("theta_P|theta_C|beta_I|lp__", pars)]) {
  png(file = paste(prefix, "%d.png", sep=""), width = 10000, height = 5000, units = "px", res=300, type="cairo")
  print(stan_par(t1.stan, par = par)[[1]])
  dev.off()
}
rm(t1.stan, plot, pre, pars, prefix)
})