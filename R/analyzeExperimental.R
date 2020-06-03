# Analyzes the baseline dataset and stores the posterior samples.
rm(list = ls())

library(brms)

dat <- readRDS("data/cleanedExperimental.rds")

fileBrmsFitExperimental <- file.path("results", "brmsFit_experimental.rds")
if (file.exists(fileBrmsFitExperimental)) { # Load results from disk

  brmres <- readRDS(fileBrmsFitExperimental)

} else { # Resample model

  # load common MCMC settings (no iterations, warmup, chains, etc.)
  source("R/mcmcSettings.R")

  # run the chains in parallel
  cores <- min(parallel::detectCores() - 1L, chains)

  # TODO: single or double | for school index?, priors?
  formula <- Score_Mean ~ 1 + Taak + (1 || School_index / Participant_index)
  brmres <- brm(
    formula = formula, data = dat, iter = iter, warmup = warmup, chains = chains, cores = cores,
    control = control, save_model = file.path("stanmodels", "experimentalModel.stan"),
    prior = set_prior("cauchy(0, 1)", class = "b", coef = "", group = "", resp = "",
                      dpar = "", nlpar = "", lb = NA, ub = NA, check = TRUE)
  )
  saveRDS(brmres, file = fileBrmsFitExperimental)
}

# the stan object
res <- brmres$fit

# write reduced samples to disk
nms <- names(res)
idx <- !(startsWith(nms, prefix = "z_") | startsWith(nms, prefix = "r_") | startsWith(nms, prefix = "lp__"))

samplesMatrix <- as.matrix(res, pars = nms[idx])
samplesArray  <- as.array (res, pars = nms[idx])

newNames <- c("Intercept", "T2", "T3", "varSchool", "varStudent", "varResidual")
colnames(samplesMatrix) <- newNames
dimnames(samplesArray)[[3L]] <- newNames

idx2Square <- startsWith(newNames, "var")
samplesMatrix[, idx2Square]  <- samplesMatrix[, idx2Square]^2
samplesArray[, , idx2Square] <- samplesArray[, , idx2Square]^2

# write all results to disk
if (!dir.exists("results")) dir.create("results")
saveRDS(samplesMatrix, file = file.path("results", "samplesExperimental.rds"))
saveRDS(samplesArray,  file = file.path("results", "samplesArrayExperimental.rds"))


# sanity check -- do the posterior means correspond to frequentist point estimates?
# colMeans(samplesMatrix)
# Intercept          T2          T3   varSchool  varStudent varResidual
# 75.89687    10.40294    10.99941    95.26036   101.18888   144.11466
#
# resFreq <- lme4::lmer(Score_Mean ~ 1 + Taak + (1 | School_index / Participant_index), data = dat)
# summary(resFreq)
# Random effects:
#   Groups                         Name        Variance  Std.Dev.
# Participant_index:School_index (Intercept) 9.800e+01 9.900e+00
# School_index                   (Intercept) 1.106e-07 3.325e-04
# Residual                                   1.414e+02 1.189e+01
# Number of obs: 262, groups:  Participant_index:School_index, 89; School_index, 2
#
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                    76.193      1.640  46.457
# Taakherschaalde score taak 2   10.447      1.783   5.861
# Taakherschaalde score taak 3   11.040      1.816   6.078


# brms may print this warning:
#  "Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable."
# however, the code below shows that for each chain both the "Tail Effective Samples Size" and the
# "Bulk Effective Samples Size" are above the criteria of 100.
mon <- rstan::monitor(res)
range(mon$Tail_ESS)
# 451 155941
range(mon$Bulk_ESS)
# 1107 138115
range(mon$Rhat)
# 1.000067 1.005247
saveRDS(mon, file = file.path("results", "monitorFitExperimental.rds"))


# TODO: rename the school index to 1, 2, 3 in data cleaning, to anonimize the results later
# TODO: write these fits to file and reference them in the manuscript.

