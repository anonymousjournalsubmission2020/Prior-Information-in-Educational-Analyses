rm(list = ls())

library(ggplot2)
HPDinterval <- coda::HPDinterval
as.mcmc <- coda::as.mcmc
source("R/utils.R")

# computeCRIandDensity <- function(x) {
#   cri <- coda::HPDinterval(coda::as.mcmc(x))
#   d <- density(x, from = -1, to = 5)
#   df <- data.frame(x = d$x, y = d$y)
#   # y was picked by eye
#   dfh <- data.frame(xmin = cri[1], xmax = cri[2], y = 1.3)
#   return(list(df = df, dfh = dfh))
# }

computeCRIandDensity <- function(x) {
  cri <- coda::HPDinterval(coda::as.mcmc(x))
  d <- density(x, from = -1, to = 3)
  df <- data.frame(x = d$x, y = d$y)

  xx <- seq(cri[1], cri[2], length.out = 2^9)
  ymax <- approx(x = d$x, y = d$y, xout = xx)$y
  dfh <- data.frame(x = xx, ymin = 0, ymax = ymax)
  return(list(df = df, dfh = dfh))
}

progressPlot <- function(df, dfh, xlab = "Improvement in years", ylab = "Density", fill = NULL) {

  if ("g" %in% colnames(df)) {
    linesMapping  <- aes(x = x, y = y, color = g)
    ribbonMapping <- aes(x = x, ymin = ymin, ymax = ymax, fill = g)
  } else {
    linesMapping  <- aes(x = x, y = y)
    ribbonMapping <- aes(x = x, ymin = ymin, ymax = ymax)
  }

  g <- ggplot(data = df, linesMapping) +
    geom_line() +
    # geom_errorbarh(data = dfh, mapping = aes(xmin = xmin, xmax = xmax, y = y), height = .1, inherit.aes = FALSE) +
    geom_ribbon(data = dfh, mapping = ribbonMapping, inherit.aes = FALSE,
                alpha = .7) +
    scale_x_continuous(limits = c(-1, 3), breaks = -1:3) +
    labs(x = xlab, y = ylab) +
    theme_bw(base_size = 24) +
    theme(legend.position = "none")

  if ("g" %in% colnames(df))
    g <- g +
      facet_grid(cols = vars(g), labeller = label_parsed) +
      theme(strip.background = element_rect(fill = "transparent", color = "transparent"))

  if (!is.null(fill))
    g <- g + scale_fill_manual(values = fill) + scale_color_manual(values = fill)

  return(g)
}

samplesBaseline     <- readRDS("results/samplesBaseline.rds")
samplesExperimental <- readRDS("results/samplesExperimental.rds")
averageTaskEffects  <- readRDS("results/samplesBaselineAverageTaskEffects.rds")

# visualize the average effect of grade
# average of Grade 10 to Grade 11 and grade 11 to grade 12
# averageEffectGrade <- (samplesBaseline[, "b[1]"] + samplesBaseline[, "b[2]"] / 2) / 2
# Grade 10 to Grade 11
averageEffectGrade10_11 <- samplesBaseline[, "Grade 11"]
effectT21inGrade <- (samplesExperimental[, "T2"] - averageTaskEffects[, "A"]) / averageEffectGrade10_11
effectT31inGrade <- (samplesExperimental[, "T3"] - averageTaskEffects[, "D"]) / averageEffectGrade10_11

# difference between T3 and T2
effectT32inGrade <- effectT31inGrade - effectT21inGrade

d21 <- computeCRIandDensity(effectT21inGrade)
d31 <- computeCRIandDensity(effectT31inGrade)
d32 <- computeCRIandDensity(effectT32inGrade)

# plot with only T2 - T1 and T3  - T1
titles <- c("frac(T2 - T1, 'Grade 11' - 'Grade 10')", "frac(T3 - T1, 'Grade 11' - 'Grade 10')")
d21_31 <- d21
d21_31$df  <- rbind(d21_31$df, d31$df)
d21_31$dfh <- rbind(d21_31$dfh, d31$dfh)
d21_31$df$g <- rep(titles, each = 512)
d21_31$dfh$g <- rep(titles, each = 512)

g21 <- progressPlot(d21$df, d21$dfh)
g31 <- progressPlot(d31$df, d31$dfh)
g21_31 <- progressPlot(d21_31$df, d21_31$dfh, fill = colsExperim[-1])
g21_31

# credible interval
cri21 <- c(HPDinterval(as.mcmc(effectT21inGrade)), mean(effectT21inGrade))
cri31 <- c(HPDinterval(as.mcmc(effectT31inGrade)), mean(effectT31inGrade))
tb2save <- rbind(cri21, cri31)
colnames(tb2save) <- c("Lower", "Upper", "mean")

# width  <- 6
# saveFigure("improvementInYears.pdf", g21_31, 2 * width, width)
# writeTable(tb2save, "credibleIntervalsImprovement.csv")


# plot with T2 - T1, T3  - T1, and T3 - T2
titles <- c("frac(T2 - T1, 'Grade 11' - 'Grade 10')", "frac(T3 - T1, 'Grade 11' - 'Grade 10')", "frac(T3 - T2, 'Grade 11' - 'Grade 10')")
d21_31 <- d21
d21_31$df  <- rbind(d21_31$df, d31$df, d32$df)
d21_31$dfh <- rbind(d21_31$dfh, d31$dfh, d32$dfh)
d21_31$df$g <- rep(titles, each = 512)
d21_31$dfh$g <- rep(titles, each = 512)

g21 <- progressPlot(d21$df, d21$dfh)
g31 <- progressPlot(d31$df, d31$dfh)
g21_31 <- progressPlot(d21_31$df, d21_31$dfh, fill = c(colsExperim[-1], rgb2(colorRamp(colsExperim[-1])(.5))))
g21_31

# credible interval
cri21 <- c(HPDinterval(as.mcmc(effectT21inGrade)), mean(effectT21inGrade))
cri31 <- c(HPDinterval(as.mcmc(effectT31inGrade)), mean(effectT31inGrade))
cri32 <- c(HPDinterval(as.mcmc(effectT32inGrade)), mean(effectT32inGrade))
tb2save <- rbind(cri21, cri31, cri32)
colnames(tb2save) <- c("Lower", "Upper", "mean")

width  <- 6
height <- 9
saveFigure("improvementInAllYears.pdf", g21_31, 3 * width, height)
  writeTable(tb2save, "credibleIntervalsImprovementAllYears.csv")


