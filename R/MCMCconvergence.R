rm(list = ls())

library(dplyr)
library(ggplot2)
library(colorspace)
source("R/utils.R")

# baseline ----
samplesArray <- readRDS("results/samplesArrayBaseline.rds")

# trace plots of MCMC convergence ----
colnames(samplesArray) <- 1:ncol(samplesArray)
dat <- as.data.frame.table(samplesArray, base = "iter")
dat$iterations <- seq_len(dim(samplesArray)[1L])

# let's only visualize the first 10000 iterations after warmup
nIter <- 1e4
dat2 <- dat[dat$iter <= nIter, ]

dat2$parameters <- recode(dat2$parameters,
  "b_Intercept" = "Intercept",
  "Grade 11"    = "Grade~11",
  "Grade 12"    = "Grade~12",
  "varSchool"   = "sigma[w]^2~(school)",
  "varStudent"  = "sigma[u]^2~(student)",
  "varTask"     = "sigma[v]^2~(task)",
  "varResidual" = "sigma[epsilon]^2"
)
levels(dat2$parameters) <- levels(dat2$parameters)[c(7, 1, 2, 4, 5, 6, 3)]

graphTrace <- ggplot(data = dat2, aes(x = iterations, y = Freq, group = chains, color = chains)) +
  geom_line(alpha = .8) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(1, nIter / 2, nIter)) +
  facet_wrap(~parameters, scales = "free", labeller = label_parsed) +
  labs(x = "Iteration", y = "Value of Posterior Sample") +
  theme_bw(base_size = 24)

saveFigure("traceplotsBaseline.pdf", graphTrace, 10, 10)

# autocorrelation
noLags <- 20
autocor <- tapply(dat$Freq, list(dat$parameters, dat$chains),
                  function(x) acf(x, plot = FALSE, lag.max = noLags)$acf[, , 1])

datAcf <- tibble(
  # drop the zero lag, which is 1 by definition
  acf        = unlist(lapply(autocor, `[`, -1L)),
  lag        = rep(seq_len(noLags), prod(dim(autocor))),
  Chain      = factor(rep(1:ncol(autocor), each = noLags * nrow(autocor))),
  parameters = factor(rep(rep(rownames(autocor), each = noLags), ncol(autocor)))
)

datAcf$parameters <- recode(datAcf$parameters,
  "b_Intercept" = "Intercept",
  "Grade 11"    = "Grade~11",
  "Grade 12"    = "Grade~12",
  "varSchool"   = "sigma[w]^2~(school)",
  "varStudent"  = "sigma[u]^2~(student)",
  "varTask"     = "sigma[v]^2~(task)",
  "varResidual" = "sigma[epsilon]^2"
)

graphAcf <- ggplot(data = datAcf, aes(x = lag, y = acf, group = Chain, color = Chain)) +
  geom_line(alpha = .8) +
  scale_color_viridis_d() +
  facet_wrap(~parameters, scales = "free", labeller = label_parsed) +
  labs(x = "Iteration", y = "Autocorrelation") +
  theme_bw(base_size = 24)

saveFigure("autocorrelationBaseline.pdf", graphAcf, 10, 10)

# TODO: combine plots with shared legend?

# experimental ----
samplesArray <- readRDS("results/samplesArrayExperimental.rds")

# trace plots of MCMC convergence ----
colnames(samplesArray) <- 1:ncol(samplesArray)
dat <- as.data.frame.table(samplesArray, base = "iter")
dat$iterations <- seq_len(dim(samplesArray)[1L])

# let's only visualize the first 10000 iterations after warmup
nIter <- 1e4
dat2 <- dat[dat$iter <= nIter, ]

dat2$parameters <- recode(dat2$parameters,
  "varSchool"   = "sigma[w]^2~(school)",
  "varStudent"  = "sigma[u]^2~(student)",
  "varResidual" = "sigma[epsilon]^2"
)

graphTrace <- ggplot(data = dat2, aes(x = iterations, y = Freq, group = chains, color = chains)) +
  geom_line(alpha = .8) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(1, nIter / 2, nIter)) +
  facet_wrap(~parameters, scales = "free", labeller = label_parsed) +
  labs(x = "Iteration", y = "Value of Posterior Sample", color = "Chain") +
  theme_bw(base_size = 24)

saveFigure("traceplotsExperimental.pdf", graphTrace, 10, 10)

# autocorrelation
noLags <- 20
autocor <- tapply(dat$Freq, list(dat$parameters, dat$chains),
                  function(x) acf(x, plot = FALSE, lag.max = noLags)$acf[, , 1])

datAcf <- tibble(
  # drop the zero lag, which is 1 by definition
  acf        = unlist(lapply(autocor, `[`, -1L)),
  lag        = rep(seq_len(noLags), prod(dim(autocor))),
  Chain      = factor(rep(1:ncol(autocor), each = noLags * nrow(autocor))),
  parameters = factor(rep(rep(rownames(autocor), each = noLags), ncol(autocor)))
)

datAcf$parameters <- recode(datAcf$parameters,
  "b_Intercept" = "Intercept",
  "b[1]"        = "T~2",
  "b[2]"        = "T~3",
  "sd_SI"       = "sigma[w]^2~(school)",
  "sd_SI:PI"    = "sigma[u]^2~(student)",
  "sigma"       = "sigma[epsilon]^2"
)

graphAcf <- ggplot(data = datAcf, aes(x = lag, y = acf, group = Chain, color = Chain)) +
  geom_line(alpha = .8) +
  scale_color_viridis_d() +
  facet_wrap(~parameters, scales = "free", labeller = label_parsed) +
  labs(x = "Iteration", y = "Autocorrelation") +
  theme_bw(base_size = 24)

saveFigure("autocorrelationExperimental.pdf", graphAcf, 10, 10)
