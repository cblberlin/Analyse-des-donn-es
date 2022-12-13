library(tidyverse)
library(dplyr)
library(ggplot2)

# pré-traitement des données
ross <- read.table("Rossignol.csv", sep = ";", header = T)

ross %>% select(-Individus.m,-sexe.m)
ross <- data.frame(lapply(ross, function(x) {gsub(",", ".", x)}))

ross <- ross %>% mutate_at(c(-1,-3), as.numeric)

# Séparer en 2 groupes male et femelle
ross_male <- ross[which(ross$sexe.m == "male"),]
ross_femelle <- ross[which(ross$sexe.m == "femelle"),]

# Test de comparaison
histogram(~ross$poids.m | ross$sexe.m)
shapiro.test(ross_male$poids.m)
shapiro.test(ross_femelle$poids.m)
var.test(ross$poids.m~ross$sexe.m)
t.test(ross$poids.m~ross$sexe.m, var.equal=F)
chisq.test(ross$sexe.m, ross$poids.m)

# le résultat qui nous montre que
boxplot(ross$poids.m~ross$sexe.m)

var.test(ross$hauteur_du_bec.m~ross$sexe.m)
t.test(ross$hauteur_du_bec.m~ross$sexe.m, var.equal=T)

var.test(ross$projection_des_ailes.m~ross$sexe.m)
t.test(ross$projection_des_ailes.m~ross$sexe.m, var.equal=T)

# analyse descriptive
# Histograme des variables
par(mfrow = c(3,3))
#hist(ross$annee.m, main = "Histogramme de l'année", xlab = "année", freq = TRUE)
hist(ross$age.m, main = "Histogramme d'âge", xlab = "âge", freq = TRUE)
#H1 = ggplot(data = ross, aes(x = age.m)) + geom_histogram()

hist(ross$poids.m, main = "Histrogramme du poids", xlab = "Poids", freq = TRUE)
#title("Histograme pour le poids")
hist(ross$longueur_aile.m, main = "Histogramme du longeur d'aille", xlab = "Longueur d'aile", freq = TRUE)
hist(ross$longueur_du_tarsus.m, main = "Histogramme du longueur du tarsus", xlab = "Longueur du tarsus", freq = TRUE)
hist(ross$longueur_de_la_queue.m, main = "Histogramme du longueur de la queue", xlab = "Longueur de la queue", freq = TRUE)
hist(ross$projection_des_ailes.m, main = "Histogramme de la projection des ailes", xlab = "La projection des ailes", freq = TRUE)
hist(ross$longueur_du_bec.m, main = "Histogramme du longueur du bec", xlab = "Longueur du bec", freq = TRUE)
hist(ross$largeur_du_bec.m, main = "Histogramme du largeur du bec", xlab = "Largeur du bec", freq = TRUE)
hist(ross$hauteur_du_bec.m, main = "Histogramme d'hauteur du bec", xlab = "Hauteur du bec", freq = TRUE)

# Boxplot
par(mfrow = c(3,3))
boxplot(ross$age.m~ross$sexe.m)
boxplot(ross$poids.m~ross$sexe.m)
boxplot(ross$longueur_aile.m~ross$sexe.m)
boxplot(ross$longueur_du_tarsus.m~ross$sexe.m)
boxplot(ross$longueur_de_la_queue.m~ross$sexe.m)
boxplot(ross$projection_des_ailes.m~ross$sexe.m)
boxplot(ross$longueur_du_bec.m~ross$sexe.m)
boxplot(ross$hauteur_du_bec.m~ross$sexe.m)
boxplot(ross$largeur_du_bec.m~ross$sexe.m)

# Test de lois
shapiro.test(ross$age.m)
shapiro.test(ross$poids.m)
shapiro.test(ross$longueur_aile.m)
shapiro.test(ross$longueur_du_tarsus.m)
shapiro.test(ross$longueur_de_la_queue.m)
shapiro.test(ross$projection_des_ailes.m)
shapiro.test(ross$longueur_du_bec.m)
shapiro.test(ross$hauteur_du_bec.m)
shapiro.test(ross$largeur_du_bec.m)

# Corrélation
ross_quantitative = ross[c(-1, -2, - 3)]
mat_cor <- cor(ross_quantitative)
round(mat_cor, 2)

# Regression
reg_poids <- lm(ross_quantitative$poids.m~as.matrix(ross_quantitative[c(-2)]))
summary(reg_poids)

reg_tarsus <- lm(ross_quantitative$longueur_du_tarsus.m~as.matrix(ross_quantitative[c(-4)]))
summary(reg_tarsus)


