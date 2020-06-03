rm(list = ls())

library(tibble)
library(ggplot2)
TeX <- latex2exp::TeX
source("R/utils.R")

plotMeasureVersusTaskVariance <- function(df, cols, xlab = "", ylab = "Density", colorlab = "", title = NULL,
                                          legendInPlot = FALSE, peakLocation = "right") { #, textFill = textColor, ...) {
  g <- ggplot(data = df, aes(x = samples, group = what, color = what, fill = what)) +
      geom_density(alpha = .7) +
      scale_fill_manual(values = cols) +
      scale_color_manual(values = cols) +
      labs(color = colorlab,#"Measurement",
           fill  = colorlab,"Measurement",
           x     = xlab,#"Posterior Text Quality",
           y     = ylab) + #"Density") +
      theme_bw(base_size = 24)
  if ("title" %in% colnames(df))
    g <- g +
      facet_grid(cols = vars(title), labeller = label_parsed) +
      theme(strip.background = element_rect(fill = "transparent", color = "transparent"))

  if (!is.null(title))
    g <- g + ggtitle(title) + theme(plot.title = element_text(hjust = .5))
  if (legendInPlot) {
    idxPos <- if (peakLocation == "left") .2 else .8
    g <- g + theme(legend.position = c(idxPos, .95),
                   legend.background = element_rect(fill = "transparent", colour = "transparent"))
  }
  return(g)
}

samplesBaseline     <- readRDS("results/samplesBaseline.rds")
samplesExperimental <- readRDS("results/samplesExperimental.rds")
averageTaskEffects  <- readRDS("results/samplesBaselineAverageTaskEffects.rds")

df <- tibble(
  what = rep(c(paste("Baseline Grade", 10:12), paste("Experiment", 1:3)), each = nrow(samplesExperimental)),
  samples = c(
    # V4 = estimate of V4
    samplesBaseline[, "Intercept"],
    # V5 = estimate of V4 + discrepancy of V5
    samplesBaseline[, "Intercept"] + samplesBaseline[, "Grade 11"],
    # V6 = estimate of V4 + discrepancy of V6
    samplesBaseline[, "Intercept"] + samplesBaseline[, "Grade 12"],
    # T1 = intercept - average task effect of baseline
    samplesExperimental[, "Intercept"] - averageTaskEffects[, "B"],
    samplesExperimental[, "Intercept"] + samplesExperimental[, "T2"] - averageTaskEffects[, "A"],
    samplesExperimental[, "Intercept"] + samplesExperimental[, "T3"] - averageTaskEffects[, "D"]
  )
)

# means of experimental study after correcting for task category
m1 <- mean(samplesExperimental[, "Intercept"]) # mean T1
ms <- c(m1, colMeans(samplesExperimental[, c("T2", "T3")]) + m1)
nm <- unique(df[["what"]]); nm <- nm[startsWith(nm, "Experiment")]
tb <- data.frame(Mean = c(tapply(df[["samples"]], df[["what"]], mean)[nm], ms),
                 what = c(nm, paste("Exp", 1:3, "uncorrected")), row.names = NULL)[c(4:6, 1:3), ]
writeTable(tb, "postMeansCorrectedExperimental.csv")

# compare posterior distribution intercept baseline vs experimental
g <- plotMeasureVersusTaskVariance(df, cols, xlab = "Posterior Text Quality")
saveFigure("comparePosteriorTextQuality.pdf", graph = g, width = 14, height = 7)

# probability of task effect ----
# color of task effect
colTask <- col2hex("gray60")

# widht & height of pdf
width  <- 6
height <- 9
xlab   <- "Posterior Task Effect"

# probability that a random task has an effect as large as the observed effect
set.seed(123)
# look at the distribution of the difference between two tasks
# ss1 <- rnorm(nrow(samplesBaseline), 0, samplesBaseline[, "sd_TI"])
# ss2 <- rnorm(nrow(samplesBaseline), 0, samplesBaseline[, "sd_TI"])
# ss <- ss1 - ss2
# look at the distribution of the difference between a random task and the intercept
ss <- rnorm(nrow(samplesBaseline), 0, sqrt(samplesBaseline[, "varTask"]))
probMoreExtremeT2 <- mean(ss >= samplesExperimental[, "T2"] - averageTaskEffects[, "A"])
# 0.02110667

# specific task doesn't matter much
# mean(ss >= samplesExperimental[, "T2"] - matrixStats::rowMaxs(averageTaskEffects))
# 0.02161

nms <- c("Task Effect", "T2 - T1")
df2 <- tibble(
  what = rep(nms, each = nrow(samplesBaseline)),
  samples = c(
    ss,
    samplesExperimental[, "T2"] - averageTaskEffects[, "A"]
  )
)

title2 <- TeX(paste0(
  "$p(\\mathrm{Random\\, Task} \\geq \\mathrm{T2} - \\mathrm{T1}) = ", round(probMoreExtremeT2, 3), "$"
))

g2 <- plotMeasureVersusTaskVariance(df2, c(cols[5], colTask), xlab = xlab,
                                    title = title2, legendInPlot = TRUE, peakLocation = "left")

# probability that a random task has an effect as large as theobserved effect
probMoreExtremeT3 <- mean(ss >= samplesExperimental[, "T3"] - averageTaskEffects[, "D"])
#  0.00646

# specific task doesn't matter much
# mean(ss >= samplesExperimental[, "T3"] - matrixStats::rowMaxs(averageTaskEffects))
# 0.01512333

nms <- c("Task Effect", "T3 - T1")
df3 <- data.frame(
  what = rep(nms, each = nrow(samplesBaseline)),
  samples = c(
    ss,
    samplesExperimental[, "T3"] - averageTaskEffects[, "D"]
  )
)

title3 <- TeX(paste0(
  "$p(\\mathrm{Random\\, Task} \\geq \\mathrm{T3} - \\mathrm{T1}) = ", round(probMoreExtremeT3, 3), "$"
))
g3 <- plotMeasureVersusTaskVariance(df3, c(cols[6], colTask), xlab = xlab,
                                    title = title3, legendInPlot = TRUE, peakLocation = "left")


diff23 <- (samplesExperimental[, "T2"] - averageTaskEffects[, "D"]) -
  (samplesExperimental[, "T3"] - averageTaskEffects[, "A"])
probMoreExtremeT23 <- mean(ss >= diff23)
title23 <- TeX(paste0(
  "$p(\\mathrm{Random\\, Task} \\geq \\mathrm{T3} - \\mathrm{T2}) = ", round(probMoreExtremeT23, 3), "$"
))
nms <- c("Task Effect", "T3 - T2")
df4 <- data.frame(
  what = rep(nms, each = nrow(samplesBaseline)),
  samples = c(ss, diff23)
)
g4 <- plotMeasureVersusTaskVariance(df4, cols = c(rgb2(colorRamp(cols[5:6])(.5)), colTask),
                                    xlab = xlab, title = title23, legendInPlot = TRUE)



df234 <- rbind(df2, df3, df4)
nr <- nrow(samplesBaseline)
titles <- sapply(c(title2, title3, title23), function(x) do.call(paste, list(deparse(x), collapse = "")))
df234$title <- rep(titles, each = 2*nr)
g234 <- plotMeasureVersusTaskVariance(df234, cols = c(cols[5:6], rgb2(colorRamp(cols[5:6])(.5)), colTask), xlab = xlab,
                                      legendInPlot = TRUE) + theme(legend.position = c(.94, .95), strip.text = element_text(size = 24))


# saveFigure("compareTaskEffectToT2.pdf",  g2,       width, height)
# saveFigure("compareTaskEffectToT3.pdf",  g3,       width, height)
# saveFigure("compareTaskEffectToT23.pdf", g4,       width, height)
saveFigure("compareTaskEffects.pdf",     g234, 3 * width, height)
