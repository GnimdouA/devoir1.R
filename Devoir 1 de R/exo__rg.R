# Révision Générale
# Exercice 1
# 1. création du r dcript
# 2. créer un data frame qui contient n lignes et m colonnes:
#créations des variables
set.seed(5)
id <- c(seq(1,110))
taille <- as.numeric(c(runif(110,160,180)))
poids <- as.numeric(c(runif(110,50,90)))
rcardiaque <- as.numeric(c(rep(70:79,times= 11)))
age <- as.numeric(c(rep(16:25, times=11)))
malade <- factor(rep(c("oui","non"),55))
diplome <- factor(rep(c(0,1),55))
sexe <- factor(rep(c("male","female"),55))
majeur <- factor(rep(rep(c("oui","non"), times=c(2,8)),times=11))
pauvrete <- factor(rep(rep(c("non","oui"), times=c(2,8)),times=11))
travail <- factor(rep(rep(c("oui","non"), times=c(6,4)),times=11))

# création du dataframe 
df <- data.frame(id, taille, poids, rcardiaque,age,malade, diplome, sexe,majeur, pauvrete,travail)

#3.Enregistrer le dataframe dans un objet noommé df.monNom
df.KABASSINA.KPEKPASSI <- df
#4. Exporter le dataframe  au format “csv” dont sep=“;”
setwd("C:/Users/HP ProBook/Documents/Mes Cours ISEP2/R/Cours traitement statistique avec R/Devoir 1 de R")
write.csv2(df.KABASSINA.KPEKPASSI,file="df.KABASSINA_KPEKPASSI.csv",row.names = FALSE,sep=";")

# 5. générer un rapport au format “html”