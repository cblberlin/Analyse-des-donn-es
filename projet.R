library(tidyverse)
library(dplyr)

ross <- read.table("Rossignol.csv", sep = ";", header = T)

ross %>% select(-Individus.m,-sexe.m)
ross <- data.frame(lapply(ross, function(x) {gsub(",", ".", x)}))

ross <- ross %>% mutate_at(c(-1,-3), as.numeric)

# analyse descriptive
par(mfrow = c(3,3))
#hist(ross$annee.m, main = "Histogramme de l'année", xlab = "année", freq = TRUE)
hist(ross$age.m, main = "Histogramme d'âge", xlab = "âge", freq = TRUE)
hist(ross$poids.m, main = "Histrogramme du poids", xlab = "Poids", freq = TRUE)
#title("Histograme pour le poids")
hist(ross$longueur_aile.m, main = "Histogramme du longeur d'aille", xlab = "Longueur d'aile", freq = TRUE)
hist(ross$longueur_du_tarsus.m, main = "Histogramme du longueur du tarsus", xlab = "Longueur du tarsus", freq = TRUE)
hist(ross$longueur_de_la_queue.m, main = "Histogramme du longueur de la queue", xlab = "Longueur de la queue", freq = TRUE)
hist(ross$projection_des_ailes.m, main = "Histogramme de la projection des ailes", xlab = "La projection des ailes", freq = TRUE)
hist(ross$longueur_du_bec.m, main = "Histogramme du longueur du bec", xlab = "Longueur du bec", freq = TRUE)
hist(ross$largeur_du_bec.m, main = "Histogramme du largeur du bec", xlab = "Largeur du bec", freq = TRUE)
hist(ross$hauteur_du_bec.m, main = "Histogramme d'hauteur du bec", xlab = "Hauteur du bec", freq = TRUE)
#boxplot(ross$projection_des_ailes.m~ross$sexe.m)


# 
