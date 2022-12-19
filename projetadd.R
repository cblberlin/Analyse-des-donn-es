library(tidyverse)
library(dplyr)
ross <- read.table("Rossignol.csv", sep = ";", header = T)
#ross %>% select(-Individus.m,-sexe.m)
ross <- data.frame(lapply(ross, function(x) {gsub(",", ".", x)}))
ross <- ross %>% mutate_at(c(-1,-3), as.numeric)
####

#library(epiR)
library(epiDisplay)
library(prettyR)
summary(ross$longueur_aile.m)
summary(ross$longueur_du_tarsus.m)
summary(ross$longueur_de_la_queue.m)
summary(ross$longueur_du_bec.m)
summary(ross$largeur_du_bec.m)
summary(ross$hauteur_du_bec.m)
summary(ross$projection_des_ailes.m)
summary(ross$age.m)
summary(ross$poids.m)
summary(ross$annee.m)
table(ross$sexe.m)
####
library(lattice)
#par(mfrow = c(3,3))
layout(matrix(1:3,3,3))
bwplot(ross$age.m~ross$sexe.m)
bwplot(ross$projection_des_ailes.m~ross$sexe.m)
bwplot(ross$hauteur_du_bec.m~ross$sexe.m)
bwplot(ross$longueur_du_tarsus.m~ross$sexe.m)
bwplot(ross$longueur_du_bec.m~ross$sexe.m)
bwplot(ross$longueur_de_la_queue.m~ross$sexe.m)
bwplot(ross$longueur_aile.m~ross$sexe.m)
bwplot(ross$largeur_du_bec.m~ross$sexe.m)
bwplot(ross$poids.m~ross$sexe.m)
####
histogram(~ ross$age.m | ross$sexe.m)
histogram(~ross$projection_des_ailes.m |ross$sexe.m)
histogram(~ross$hauteur_du_bec.m |ross$sexe.m)
histogram(~ross$longueur_du_tarsus.m | ross$sexe.m)
histogram(~ross$longueur_du_bec.m | ross$sexe.m)
histogram(~ross$longueur_de_la_queue.m |ross$sexe.m)
histogram(~ross$longueur_aile.m | ross$sexe.m)
histogram(~ross$largeur_du_bec.m | ross$sexe.m)
histogram(~ross$poids.m| ross$sexe.m)
####
# ###########Créer de sous-fichiers pour les groupes femelle et male
femelle <- ross[which(ross$sexe.m=='femelle'),]
male<- ross[which(ross$sexe.m=='male'),]
#if (ross$age.m<= )
####
#test de comparaison de moyenne
summary(male$poids.m)
summary(femelle$poids.m)
shapiro.test(x =male$poids.m)
shapiro.test(x =femelle$poids.m)
wilcox.test(male$poids.m, femelle$poids.m, alternative = "g", paired=F)
####
summary(male$age.m)
summary(femelle$age.m)
shapiro.test(x =male$age.m)
shapiro.test(x =femelle$age.m)
wilcox.test(male$age.m, femelle$age.m,alternative = "g", paired=F)
####
summary(male$longueur_du_tarsus.m)
summary(femelle$longueur_du_tarsus.m)
shapiro.test(x =male$longueur_du_tarsus.m)
shapiro.test(x =femelle$longueur_du_tarsus.m)
wilcox.test(male$longueur_du_tarsus.m, femelle$longueur_du_tarsus.m, alternative = "g", paired=F)
####
summary(male$projection_des_ailes.m)
summary(femelle$projection_des_ailes.m)
shapiro.test(x =male$projection_des_ailes.m)
shapiro.test(x =femelle$projection_des_ailes.m)
wilcox.test(male$longueur_du_tarsus.m, femelle$longueur_du_tarsus.m, alternative = "g", paired=F)
####
summary(male$longueur_du_bec.m)
summary(femelle$longueur_du_bec.m)
shapiro.test(x =male$longueur_du_bec.m)
shapiro.test(x =femelle$longueur_du_bec.m)
wilcox.test(male$longueur_du_bec.m, femelle$longueur_du_bec.m, alternative = "g", paired=F)
####
summary(male$longueur_de_la_queue.m)
summary(femelle$longueur_de_la_queue.m)
shapiro.test(x =male$longueur_de_la_queue.m)
shapiro.test(x =femelle$longueur_de_la_queue.m)
wilcox.test(male$longueur_de_la_queue.m, femelle$longueur_de_la_queue.m, alternative = "g", paired=F)
#####
summary(male$longueur_aile.m)
summary(femelle$longueur_aile.m)
shapiro.test(x =male$longueur_aile.m)
shapiro.test(x =femelle$longueur_aile.m)
wilcox.test(male$longueur_aile.m, femelle$longueur_aile.m, alternative = "g", paired=F)
####
summary(male$hauteur_du_bec.m)
summary(femelle$hauteur_du_bec.m)
shapiro.test(x =male$hauteur_du_bec.m)
shapiro.test(x =femelle$hauteur_du_bec.m)
wilcox.test(male$hauteur_du_bec.m, femelle$hauteur_du_bec.m, alternative = "g", paired=F)
#####
summary(male$largeur_du_bec.m)
summary(femelle$largeur_du_bec.m)
shapiro.test(x =male$largeur_du_bec.m)
shapiro.test(x =femelle$largeur_du_bec.m)
wilcox.test(male$largeur_du_bec.m, femelle$largeur_du_bec.m, alternative = "g", paired=F)

##corélation chez les femelle
library(corrplot)
layout(matrix(1:2,1,2))
mcar=femelle[,-c(1:3)]
mcor=cor(mcar)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
#corrplot(mcor, method="color")
#corrplot(mcor, method="number")
cor.test(male$age.m,male$poids.m)
cor.test(femelle$age.m,femelle$poids.m)
cor.test(ross$age.m,ross$poids.m)
##corélation chez les males
mcar=male[,-c(1:3)]
mcor=cor(mcar)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
#corrplot(mcor, method="color")
#corrplot(mcor, method="number")
####corialation
mcar=ross[,-c(1:3)]
mcor=cor(mcar)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
+corrplot(mcor, method="color")
####
library(missMDA)
library(FactoMineR)
library(factoextra)
zer=PCA(mcar)
fviz_pca_biplot(zer,geom.ind = "point", # Montre les points seulement (mais pas le "text")
                col.ind = ross$sexe.m, # colorer by groups
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),axes = c(1, 2),
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups")
####
lm1=glm(formula=ross$poids.m~ -ross$projection_des_ailes.m+ross$age.m+ross$hauteur_du_bec.m+ross$longueur_du_tarsus.m-ross$longueur_du_bec.m-ross$largeur_du_bec.m-ross$longueur_de_la_queue.m+ross$longueur_aile.m -ross$sexe.m,family=gaussian(link="identity"),data=ross)
summary(lm1)
anova(lm1)
confint(lm1, level=0.95)
layout(matrix(1:4,2,2))
plot(lm1)
hist(lm1$residuals)
qqnorm(lm1$residuals)
deviance(lm1)
############
n <- length(ross$poids.m)
df.residus <- data.frame(residu = rstudent(lm1))
IDval.ab <- (1:n)[abs(df.residus$residu)>2]
df.residus$ID <- rep("",n)
df.residus[IDval.ab,]$ID <- IDval.ab
df.residus$group <- rep("Non Suspect",n)
df.residus[IDval.ab,]$group <- "Suspect"

plot2 <- ggplot(data = df.residus) + aes(x=1:n, y = residu, color = group) + geom_point()
plot2 <- plot2 + geom_hline(yintercept = -2, col = "blue", linetype = 2)
plot2 <- plot2 + geom_hline(yintercept = 2, col = "blue", linetype = 2)
plot2 <- plot2 + geom_text(aes(label=ID),hjust=0, vjust=0)
plot2 <- plot2 + xlab('Index') + ylab('Résidus studentis?s')
#######
quant.t <- qt((1:n)/n,n-2)
df_qq <- data.frame(Obs = sort(df.residus$residu), Theo = quant.t)
qq.plot <-
  ggplot(data = df_qq, aes(x = Obs, y = Theo)) + geom_point(shape = 1, size = 2.5)
qq.plot <-
  qq.plot + geom_abline(
    slope = 1,
    intercept = 0,
    col = "blue",
    linetype = 2,
    size = 0.5
  )
qq.plot <- qq.plot + xlab("Quantiles empiriques des résidus") + ylab("Student T(n-p-1)")
qq.plot <- qq.plot + xlim(-3,3) + ylim(-5,5)

#######
H <- hatvalues(lm1)
p <- lm1$rank
seuil1 <- 2*p/n
seuil2 <- 3*p/n
df.H <- data.frame(H = H)
ID_levier <- (1:n)[df.H$H>seuil1]
df.H$ID <- rep("",n)
df.H[ID_levier,]$ID <- ID_levier
df.H$group <- rep("Non levier",n)
df.H[ID_levier,]$group <- "Levier"
plot3 <- ggplot(data = df.H) + aes(x=1:n, y = H, color=group) + geom_point()
plot3 <- plot3 + geom_hline(yintercept = seuil1, col = "blue", linetype = 2)
plot3 <- plot3 + geom_hline(yintercept = seuil2, col = "blue", linetype = 3)
plot3 <- plot3 + geom_text(aes(label=ID),hjust=0, vjust=0)
plot3 <- plot3 + xlab('Index') + ylab('hii')
#######Distance de cook
cook <- cooks.distance(lm1)
df.cook <- data.frame(cook = cook)
s1 <- qf(0.5,p,n-p)
s2 <- qf(0.1,p,n-p)
plot4 <- ggplot(data = df.cook) + aes(x=1:n, y = cook) + geom_point()
plot4 <- plot4 + geom_hline(yintercept = s1, col = "blue", linetype = 2)
plot4 <- plot4 + geom_hline(yintercept = s2, col = "blue", linetype = 3)
plot4 <- plot4 + xlab('Index') + ylab('Distance de Cook')
#########
library("gridExtra")
grid.arrange(qq.plot,plot2,plot4,plot3,ncol=2)
###########
lm2=glm(formula=ross$poids.m~ -ross$projection_des_ailes.m+ross$age.m+ross$hauteur_du_bec.m+ross$longueur_du_tarsus.m-ross$longueur_du_bec.m-ross$largeur_du_bec.m-ross$longueur_de_la_queue.m+ross$longueur_aile.m +ross$sexe.m+ross$age.m:ross$sexe.m,family=gaussian(link="identity"),data=ross)
summary(lm2)
anova(lm2)
confint(lm2, level=0.95)
layout(matrix(1:4,2,2))
plot(lm2)
shapiro.test(rstudent(lm))
####
############
n <- length(ross$poids.m)
df.residus <- data.frame(residu = rstudent(lm2))
IDval.ab <- (1:n)[abs(df.residus$residu)>2]
df.residus$ID <- rep("",n)
df.residus[IDval.ab,]$ID <- IDval.ab
df.residus$group <- rep("Non Suspect",n)
df.residus[IDval.ab,]$group <- "Suspect"

plot5 <- ggplot(data = df.residus) + aes(x=1:n, y = residu, color = group) + geom_point()
plot5 <- plot5 + geom_hline(yintercept = -2, col = "blue", linetype = 2)
plot5<- plot5 + geom_hline(yintercept = 2, col = "blue", linetype = 2)
plot5 <- plot5 + geom_text(aes(label=ID),hjust=0, vjust=0)
plot5 <- plot5 + xlab('Index') + ylab('Résidus studentis?s')
####
quant.t <- qt((1:n)/n,n-2)
df_qq <- data.frame(Obs = sort(df.residus$residu), Theo = quant.t)
qq.plot7 <-
  ggplot(data = df_qq, aes(x = Obs, y = Theo)) + geom_point(shape = 1, size = 2.5)
qq.plot7 <-
  qq.plot7 + geom_abline(
    slope = 1,
    intercept = 0,
    col = "blue",
    linetype = 2,
    size = 0.5
  )
qq.plot7 <- qq.plot7 + xlab("Quantiles empiriques des r?sidus") + ylab("Student T(n-p-1)")
qq.plot7 <- qq.plot7 + xlim(-3,3) + ylim(-5,5)
#######
#######
H <- hatvalues(lm2)
p <- lm2$rank
seuil1 <- 2*p/n
seuil2 <- 3*p/n
df.H <- data.frame(H = H)
ID_levier <- (1:n)[df.H$H>seuil1]
df.H$ID <- rep("",n)
df.H[ID_levier,]$ID <- ID_levier
df.H$group <- rep("Non levier",n)
df.H[ID_levier,]$group <- "Levier"
plot6 <- ggplot(data = df.H) + aes(x=1:n, y = H, color=group) + geom_point()
plot6 <- plot6 + geom_hline(yintercept = seuil1, col = "blue", linetype = 2)
plot6 <- plot6 + geom_hline(yintercept = seuil2, col = "blue", linetype = 3)
plot6 <- plot6 + geom_text(aes(label=ID),hjust=0, vjust=0)
plot6 <- plot6 + xlab('Index') + ylab('hii')
quant.t <- qt((1:n)/n,n-2)
df_qq <- data.frame(Obs = sort(df.residus$residu), Theo = quant.t)
qq.plot7 <-
  ggplot(data = df_qq, aes(x = Obs, y = Theo)) + geom_point(shape = 1, size = 2.5)
qq.plot7 <-
  qq.plot7 + geom_abline(
    slope = 1,
    intercept = 0,
    col = "blue",
    linetype = 2,
    size = 0.5
  )
qq.plot7 <- qq.plot7 + xlab("Quantiles empiriques des r?sidus") + ylab("Student T(n-p-1)")
qq.plot7 <- qq.plot7 + xlim(-4,4) + ylim(-5,5)
#######
#######Distance de cook
cook <- cooks.distance(lm1)
df.cook <- data.frame(cook = cook)
s1 <- qf(0.5,p,n-p)
s2 <- qf(0.1,p,n-p)
plot8 <- ggplot(data = df.cook) + aes(x=1:n, y = cook) + geom_point()
plot8 <- plot8 + geom_hline(yintercept = s1, col = "blue", linetype = 2)
plot8 <- plot8 + geom_hline(yintercept = s2, col = "blue", linetype = 3)
plot8 <- plot8 + xlab('Index') + ylab('Distance de Cook')
grid.arrange(qq.plot7,plot5,plot8,plot6,ncol=2)

