library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

Sys.setenv(LANG = "en")