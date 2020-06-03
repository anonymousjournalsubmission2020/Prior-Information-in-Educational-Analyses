rm(list = ls())

# setup ----
library(GGally)
library(tibble)
library(coda)
Tex <- latex2exp::TeX
source("R/utils.R")

posteriorScatterPlot <- function(samples, thm, ...) {

  thm <- theme_bw(18) + theme(panel.border = element_blank())

  return(ggpairs(
    data = as.data.frame(samples),
    lower = list(
      continuous = function(data, mapping, ...) {
        ggplot(data = data, mapping = mapping) +
          geom_hex() +
          scale_fill_gradient(low = "grey60", high = "grey20") +
          thm
      },
      combo = ggally_dot_no_facet
    ),
    diag = list(
      continuous = function(data, mapping, ...) ggally_densityDiag(data, mapping, ...) + thm,
      # GGally defaults
      discrete = "barDiag", na = "naDiag"
    ),
    upper = list(
      continuous = function(data, mapping, ...) {
        xdata <- rlang::eval_tidy(mapping$x, data)
        ydata <- rlang::eval_tidy(mapping$y, data)
        cc <- cor(xdata, ydata)
        # round using significant digits, e.g., "-0.00065"
        # label <- paste(signif(cc, 3), sep = "", collapse = "")
        # round using fixed number of digits e.g., "-0.001"
        label <- formatC(cc, digits = 3, format = "f")
        p <- ggally_text(label = label, mapping, xP = 0.5, yP = 0.5, xrange = 0:1, yrange = 0:1,
                         size = 12, ...) +
          theme(legend.position = "none") + theme_void()
        return(p)
      },
      # GGally defaults
      combo = "box_no_facet", discrete = "facetbar", na = "na"
    ),
    ...
  ) + theme(
      text = element_text(size = 28),
      strip.background = element_rect(fill = "transparent",colour = "transparent")
    )
  )
}

posteriorDensityPlot <- function(df, colors) {

  return(ggplot(data = df, aes(x = samples, group = what, color = what, fill = what)) +
    geom_density(alpha = .7) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(color = "Measurement",
         fill  = "Measurement",
         x     = "Posterior Text Quality",
         y     = "Density") +
    theme_bw(base_size = 24) +
    theme(
      legend.direction = 'vertical',
      legend.position = 'right',
      # legend.title = element_blank(),
      legend.key = element_rect(size = 5),
      # legend.spacing.y = unit(5, "lines"),
      # legend.margin = margin(b = 3, unit = "cm"),
      legend.key.size = unit(2, "lines"),
      legend.spacing.y = unit(2.0, 'cm'),
      # legend.text = element_text(margin = margin(b = 10))
      # legend.key.size = unit(2, "cm")
    ))
}

posteriorSummaryTable <- function(samples) {

  samples2 <- as.mcmc(samples)
  mus <- colMeans(samples)
  sds <- apply(samples, 2, sd)
  hpd <- HPDinterval(samples2)
  df <- data.frame(Parameter = names(mus), Mean = mus, SD = sds, hpd, row.names = NULL)
  colnames(df)[4:5] <- c("Lower", "Upper")
  df$Parameter <- gsub("_", "\\_", df$Parameter, fixed = TRUE)
  return(df)
}

# baseline ----
samplesBaseline <- readRDS("results/samplesBaseline.rds")

# scattor plot of posterior samples
columnLabels <- c("Intercept", "Grade~11", "Grade~12", "sigma[w]^2~(school)", "sigma[u]^2~(student)",
                  "sigma[v]^2~(task)", "sigma[epsilon]^2")

gDescriptive <- posteriorScatterPlot(samplesBaseline, columnLabels = columnLabels,
                                     labeller = label_parsed)
saveFigure("posteriorDescriptivesBaseline.pdf",  gDescriptive, width = 20, height = 20)

# improvement over grades
averageTaskEffects  <- readRDS("results/samplesBaselineAverageTaskEffects.rds")

df <- tibble(
  what = rep(paste("Grade", 10:12), each = nrow(samplesBaseline)),
  samples = c(
    # V4 = estimate of V4
    samplesBaseline[, "Intercept"],
    # V5 = estimate of V4 + discrepancy of V5
    samplesBaseline[, "Intercept"] + samplesBaseline[, "Grade 11"],
    # V6 = estimate of V4 + discrepancy of V6
    samplesBaseline[, "Intercept"] + samplesBaseline[, "Grade 12"]
  )
)

# compare posterior distribution across grades
g <- posteriorDensityPlot(df, colsBaseline)
saveFigure("posteriorTextQualityBaseline.pdf", graph = g, width = 14, height = 7)

# table with posterior summary
tb <- posteriorSummaryTable(samplesBaseline)
tb$Parameter <- c("Intercept", "Grade 11", "Grade 12", "$\\sigma^2_w \\, \\mathrm{(school)}$",
                  "$\\sigma^2_u \\, \\mathrm{(student)}$", "$\\sigma^2_v \\, \\mathrm{(task)}$", "$\\sigma^2_\\epsilon$")
writeTable(tb, "postSummaryBaseline.csv")

# experimental ----
samplesExperimental  <- readRDS("results/samplesExperimental.rds")

# scattor plot of posterior samples
columnLabels <- c("Intercept", "T2", "T3", "sigma[w]^2~(school)", "sigma[u]^2~(student)", "sigma[epsilon]^2")

gDescriptive <- posteriorScatterPlot(samplesExperimental, columnLabels = columnLabels,
                                     labeller = label_parsed)
saveFigure("posteriorDescriptivesExperimental.pdf",  gDescriptive, width = 20, height = 20)

# improvement over measurement occasions
df <- tibble(
  what = factor(rep(1:3, each = nrow(samplesExperimental))),
  samples = c(
    # estimate of first measurement
    samplesExperimental[, "Intercept"],
    # estimate of first measurement + discrepancy of second measurement
    samplesExperimental[, "Intercept"] + samplesExperimental[, "T2"],
    # estimate of first measurement + discrepancy of third measurement
    samplesExperimental[, "Intercept"] + samplesExperimental[, "T3"]
  )
)

# compare posterior distribution across grades
df[["what"]] <- paste0("T", df[["what"]])
g <- posteriorDensityPlot(df, colsExperim)

saveFigure("posteriorTextQualityExperimental.pdf", graph = g, width = 14, height = 7)

# table with posterior summary
tb <- posteriorSummaryTable(samplesExperimental)
tb$Parameter <- c("Intercept", "Measurement 2", "Measurement 3", "$\\sigma^2_w \\, \\mathrm{(school)}$",
                  "$\\sigma^2_u \\, \\mathrm{(student)}$", "$\\sigma^2_\\epsilon$")
writeTable(tb, "postSummaryExperimental.csv")
