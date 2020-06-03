# Analyzes the baseline dataset and stores the posterior samples.
rm(list = ls())

library(brms)
dat <- readRDS("data/cleanedBaseline.rds")

fileBrmsFitBaseline <- file.path("results", "brmsFit_baseline.rds")
if (file.exists(fileBrmsFitBaseline)) { # Load results from disk

  brmres <- readRDS(fileBrmsFitBaseline)

} else { # Resample model

  # load common MCMC settings (no iterations, warmup, chains, etc.)
  source("R/mcmcSettings.R")

  # run the chains in parallel
  cores <- min(parallel::detectCores() - 1L, chains)

  # TODO: single or double | for school index?, priors?
  formula <- Score_Mean ~ 1 + Grade + (1 | Task_index) + (1 || School_index / Participant_index)
  brmres <- brm(
    formula = formula, data = dat, iter = iter, warmup = warmup, chains = chains, cores = cores,
    control = control, save_model = file.path("stanmodels", "baselineModel.stan")
  )
  saveRDS(brmres, file = fileBrmsFitBaseline)
}

# the stan object
res <- brmres$fit

# write reduced samples to disk
nms <- names(res)
idx <- !(startsWith(nms, prefix = "z_") | startsWith(nms, prefix = "r_") | startsWith(nms, prefix = "lp__"))

samplesMatrix <- as.matrix(res, pars = nms[idx])
samplesArray  <- as.array (res, pars = nms[idx])

# square all standard deviations and use sensible names
newNames <- c("Intercept", "Grade 11", "Grade 12", "varSchool",
              "varStudent", "varTask", "varResidual")
colnames(samplesMatrix) <- newNames
dimnames(samplesArray)[[3L]] <- newNames

idx2Square <- startsWith(newNames, "var")
samplesMatrix[, idx2Square]  <- samplesMatrix[, idx2Square]^2
samplesArray[, , idx2Square] <- samplesArray[, , idx2Square]^2

# write all results to disk
if (!dir.exists("results")) dir.create("results")
saveRDS(samplesMatrix, file = file.path("results", "samplesBaseline.rds"))
saveRDS(samplesArray,  file = file.path("results", "samplesArrayBaseline.rds"))

# sanity check -- do the posterior means correspond to frequentist point estimates?
# samplesMatrix <- readRDS("results/samplesBaseline.rds")
# colMeans(samplesMatrix)
# Intercept    Grade 11    Grade 12   varSchool  varStudent     varTask varResidual
# 84.400678    7.671797   13.456119   13.814421   97.135068   10.886369  198.918210
#
# resFreq <- lme4::lmer(Score_Mean ~ 1 + Grade + (1 | Task_index) + (1 | School_index / Participant_index), data = dat)
# summary(resFreq)
# Random effects:
#   Groups                         Name        Variance Std.Dev.
# Participant_index:School_index (Intercept)  96.508   9.824
# School_index                   (Intercept)  12.493   3.534
# Task_index                     (Intercept)   9.696   3.114
# Residual                                   198.511  14.089
#
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)     84.399      1.124  75.119
# GradeGrade 11    7.669      1.102   6.962
# GradeGrade 12   13.469      1.673   8.050

# save task effects separately
tasknames <- sort(unique(dat$Task_Code))
idx2 <- startsWith(nms, prefix = "r_Task_index")
nms[idx2]

samplesTaskEffects <- as.matrix(res, pars = nms[idx2])
colnames(samplesTaskEffects) <- tasknames
saveRDS(samplesTaskEffects, "results/samplesBaselineTaskEffectsRaw.rds")

averageTaskEffects <- matrix(NA, nrow(samplesTaskEffects), 4L, dimnames = list(NULL, LETTERS[1:4]))
for (i in 1:4) {
  idx <- 1:4 + 8L * (i - 1L)
  averageTaskEffects[, i] <- rowMeans(samplesTaskEffects[, idx])
}
saveRDS(averageTaskEffects, "results/samplesBaselineAverageTaskEffects.rds")

# compute task effects

# diagnostics
rstan::check_hmc_diagnostics(res)
# Divergences:
#   0 of 300000 iterations ended with a divergence.
#
# Tree depth:
#   1083 of 300000 iterations saturated the maximum tree depth of 10 (0.361%).
# Try increasing 'max_treedepth' to avoid saturation.
#
# Energy:
#   E-BFMI indicated no pathological behavior.


# brms may print this warning:
#  "Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable."
# however, the code below shows that for each chain both the "Tail Effective Samples Size" and the
# "Bulk Effective Samples Size" are above the criteria of 100.

mon <- rstan::monitor(res)
range(mon$Tail_ESS)
# 100017 255349
range(mon$Bulk_ESS)
#  59056 425906
range(mon$Rhat)
# 0.9999901 1.0001112
saveRDS(mon, file = file.path("results", "monitorFitBaseline.rds"))

