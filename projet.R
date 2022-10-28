library(tidyverse)
library(dplyr)

ross <- read.table("Rossignol.csv", sep = ";", header = T)

ross %>% select(-Individus.m,-sexe.m)
ross <- data.frame(lapply(ross, function(x) {gsub(",", ".", x)}))

ross <- ross %>% mutate_at(c(-1,-3), as.numeric)

summary(ross)


