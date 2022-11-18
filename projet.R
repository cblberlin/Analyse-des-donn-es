library(tidyverse)
library(dplyr)

ross <- read.table("Rossignol.csv", sep = ";", header = T)

ross %>% select(-Individus.m,-sexe.m)
ross <- data.frame(lapply(ross, function(x) {gsub(",", ".", x)}))

ross <- ross %>% mutate_at(c(-1,-3), as.numeric)

# analyse descriptive
hist(ross$annee.m)
hist(ross$poids.m)
hist(ross$longueur_aile.m)
hist(ross$longueur_du_tarsus.m)
hist(ross$longueur_de_la_queue.m)
hist(ross$projection_des_ailes.m)
hist(ross$longueur_du_bec.m)
hist(ross$largeur_du_bec.m)
hist(ross$hauteur_du_bec.m)
boxplot(ross$projection_des_ailes.m~ross$sexe.m)
# 
