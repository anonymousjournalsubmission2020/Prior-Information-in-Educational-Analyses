rm(list = ls())

# determines whether results are written to file.
options("writeFiguresToFile" = TRUE)
options("writeTablesToFile"  = TRUE)
options("dirForFigures" = "figures")
options("dirForTables" = "tables")

# clean all data sets
source("R/cleanDatasets.R")

# descriptives of baseline data set
source("R/descriptivesBaseline.R")

# descriptives of baseline data set
source("R/descriptivesExperimental.R")

# analyze baseline data set
source("R/analyzeBaseline.R")

# analyze experimental dataset
source("R/analyzeExperimental.R")

# figures with mcmc diagnostics
source("R/MCMCconvergence.R")

# make scatter plot of posterior samples
source("R/descriptivesPosterior.R")

# compare baseline and experimental data set
source("R/plotBaselinevsExperimental.R")

# relate improvement in the experimental study to improvement over years in the baseline study
source("R/improvementInYears.R")