rm(list = ls())

library(ggplot2)
library(ggbeeswarm)
source("R/utils.R")

dat <- readRDS("data/cleanedExperimental.rds")

nunique <- function(x) length(unique(x))

sapply(dat[, c("Participant_index", "Taak", "School_index")], nunique)
# Participant_index              Taak      School_index
#                89                 3                 2
table(tapply(dat[["Taak"]], dat[["Participant_index"]], nunique))
# No. of tasks made: 3
# Student count    : 89

graph <- ggplot(data = dat, aes(x = factor(Taak), y = Score_Mean)) +
  geom_boxplot() +
  geom_quasirandom(color = "gray60", alpha = .85, size = 2.4) +
  scale_x_discrete(labels = c("1", "2", "3")) +
  labs(x = "Measurement Occasion / Task", y = "Text Quality") +
  theme_bw(base_size = 28)
graph

saveFigure("descriptivesExperimental.pdf", graph, width = 14, height = 7)
