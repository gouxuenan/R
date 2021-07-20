library(tidyverse)
library(corrplot)
library(psych)

setwd("")

df <- read_tsv("")

A <- filter(df, layer == "A")

layerA <- filter(df, layer == "A") %>%
  select(!layer & !ID & !type & !group) %>%
  corr.test(use = "complete", method = "pearson", adjust = "none")

corrplot(layerA$r, p.mat = layerA$p, insig ="label_sig",sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.5, pch.col = "white", type = "upper", tl.pos = "d", tl.col = "black")

typeYS <- filter(df, type == "YS") %>%
  select(!layer & !ID & !type & !group) %>%
  corr.test(use = "complete", method = "pearson", adjust = "none")
corrplot(typeYS$r, p.mat = typeYS$p, insig ="label_sig",sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.5, pch.col = "white", type = "upper", tl.pos = "d", tl.col = "black")
