
# setup for mcmc samples, in total (iter - warmup) * chains samples are obtained.
iter   <- 6e4L
warmup <- 1e4L
chains <- 6L

# arguments for Stan's sampling algorithm
control <- list(adapt_delta = 0.9)

source("R/utils.R")
tb <- data.frame(iter = iter, warmup = warmup, chains = chains, total = (iter - warmup) * chains)
writeTable(x = tb, file = "mcmcSettings.csv")