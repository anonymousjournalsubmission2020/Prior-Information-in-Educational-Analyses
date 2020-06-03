rm(list = ls())

library(ggplot2)
library(dplyr)
source("R/utils.R")

dat <- readRDS("data/cleanedBaseline.rds")

nunique <- function(x) length(unique(x))

sapply(dat[, c("ID_Participant", "Task_Code", "Schoolnummer", "Grade")], nunique)
# ID_Participant      Task_Code   Schoolnummer          Grade
#            625             32             43              3
table(tapply(dat[["Task_Code"]], dat[["ID_Participant"]], nunique))
# No. of tasks made: 1   2   3   4
# Student count    : 17  30  81 497

range(table(dat[["Task_Code"]]))
# minimum times a task was made: 62
# maximum times a task was made: 84

dat$Grade <- recode(dat$Grade,
  "4" = "Grade 10",
  "5" = "Grade 11",
  "6" = "Grade 12"
)
xlab <- 1:32
xlab[!xlab %in% c(1, 8, 16, 24, 32)] <- ""

dat$xCoords <- as.numeric(factor(dat$Task_Code))

graph <- ggplot(data = dat, aes(x = xCoords, y = Score_Mean, group = factor(xCoords))) +
  geom_boxplot() +
  facet_wrap(~Grade) +
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32)) + #, limits = c(1, 32)) +
  labs(x = "Task", y = "Text Quality") +
  theme_bw(base_size = 44) +
  theme(#axis.text.x = element_text(size = 16, angle = 45),
        # axis.ticks.length.x = unit(ifelse(xlab == "", 0, 1), units = "cm"),
        # strip.text  = element_text(size = 42),
        strip.background = element_rect(fill = "transparent", color = "gray"))
# graph

saveFigure("descriptivesBaseline.pdf", graph, width = 25, height = 10)
