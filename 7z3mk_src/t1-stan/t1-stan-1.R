library("rstan",lib=libloc)
rstan_options(auto_write = TRUE); options(mc.cores = 4)
stan_file <- paste(dir.list$out.mod1, paste(model, country, "stan.RData", sep="_"), sep="/")
stan_file_result <- try(load(file = stan_file))
if(class(stan_file_result) == "try-error") {
### Items in topics; pupils in classes; 
### Centered version
t1.syntax <- "
 data {
    int<lower=1> N_P; // number of students
    int<lower=1> N_C; // number of classes
    int<lower=1> N_T; // number of topics
    int<lower=1> N_I; // number of items
    int<lower=1> N; // number of observations
    int<lower=1, upper=N_P> Person[N]; // student for observation n
    int<lower=1, upper=N_C> Class[N]; // class for observation n
    int<lower=1, upper=N_C> Class_p[N_P]; // class for student p
    int<lower=1, upper=N_T> Topic[N]; // topic for observation n
    int<lower=1, upper=N_T> Topic_i[N_I]; // topic for item i
    int<lower=1, upper=N_I> Item[N]; // item for observation n
    real TOTWGT[N_P]; // sampling weight for student p
    int<lower=0, upper=1> Y[N]; // response for observation n
  }
  parameters {
    real mu_beta; // overall mean

    real theta_P[N_P]; // pupil ability
    real theta_C[N_C]; // class average ability
    real beta_T[N_T]; // topic average difficulty
    real beta_I[N_I]; // item dev difficulty
    real wgtstu; // wgt effect
    
    real<lower=0> sigma_theta_P; // sd
    real<lower=0> sigma_theta_C; // sd
    real<lower=0> sigma_beta_T; // sd
    real<lower=0> sigma_beta_I; // sd
    }
  model {
    row_vector[N] eta;
    
    mu_beta ~ normal(0, 1);
    wgtstu ~ normal(0,1);
    sigma_theta_P ~ cauchy(0, .25);
    sigma_theta_C ~ cauchy(0, .25);
    sigma_beta_T ~ cauchy(0, .25);
    sigma_beta_I ~ cauchy(0, .25);
    
    theta_C ~ normal(0, sigma_theta_C);

    for (p in 1:N_P) {
      theta_P[p] ~ normal(theta_C[Class_p[p]] + wgtstu*TOTWGT[p], sigma_theta_P); 
    }
    beta_T ~ normal(mu_beta, sigma_beta_T);

    for (i in 1:N_I) {
      beta_I[i] ~ normal(beta_T[Topic_i[i]], sigma_beta_I);
    }
    
    for (n in 1:N){
      eta[n] = theta_P[Person[n]] - beta_I[Item[n]];
    }
    Y ~ bernoulli_logit(eta);
  }
"

t1.time <- proc.time()
t1.stan <- stan(model_code = t1.syntax, data = dat, iter = 20000, chains = numWorkers, cores = numWorkers, control = list(adapt_delta = 0.95, max_treedepth = 15))
t1.time <- proc.time() - t1.time
(dims <- list(iterations = t1.stan@sim$iter/2, parameters=t1.stan@sim$n_flatnames, chains=t1.stan@sim$chains))
save(list=c(paste(model, c("stan", "syntax", "time"), sep="."), "dat", "dims"), file = stan_file)
}

