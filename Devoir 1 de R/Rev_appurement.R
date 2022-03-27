# Révision sur l'appurement

## Exercice 1

### 1. Importation de la base “céréales”
library(haven)
m_source <- "C:/Users/HP ProBook/Documents/Mes Cours ISEP2/R/fichers de cours/Base_etudiant"
cereale <- read_dta(file = paste(m_source,"cereales.dta", sep = "/"))

### 2. Rénomer l’ensemble des variables
#### convertissons cette base en dataframe pour mieux l'exploiter
cereale_df <- data.frame(cereale)
colnames(cereale_df) <- c("cleitrview","id.itrview","cereale.id","autr.creal", "quant.cons", 
                          "unit.cons","taill.cons","provce.oto","provce.otr",
                          "freq.achat","quant.acha", 
                          "unit.achat","taill.acha","p.lastacha")
### 3.. Décrire les variables d’intérêt
summary(cereale_df)

### 4.identifier les valeurs manquantes et les imputer
#### indice (ligne + colone) des na
which(is.na(cereale_df),arr.ind=TRUE)
#### emputation des na
ligne.na <- c(which(is.na(cereale_df),arr.ind=TRUE)[,1])# lignes contenant des na
df.without.na <- cereale_df[-ligne.na,] # suppression des lignes contenant des na

#df.without.na <- cereale_df[!which(is.na(cereale_df),arr.ind=TRUE)]
print(df.without.na)
###5. Importer la base table de conversion
library(readxl)
tab_conv_xl <- read_xlsx(paste(m_source,"table_conversion_2021.xlsx", sep = "/"))
tab.conv <- data.frame(tab_conv_xl)

### 6. Décrire la variable poids
tab.conv[,'poids'] <- as.numeric(tab.conv[,'poids'])
summary(tab.conv[,'poids'])

### 7. Créer une clé d’identification dans les deux bases
library(dplyr)
tab_conv <- mutate(tab.conv, cle = paste(produitID,uniteID,tailleID))
cereales <- mutate(df.without.na, cle = paste(cereale.id,unit.cons,taill.cons))

### 8. Fusionner les deux bases
fusion <-  merge(cereales, tab_conv, by="cle")

### 9. Convertir les quantités consommées en unités standards
fusion.1 <- mutate(fusion, quant.cons.kg =((quant.cons*poids/1000)/7)*365 )

### 10. Générer une variable “taille_men” comprise entre 1 et 30  et dont la moyenne est 9
fusion.1 <- mutate(fusion.1, taille_men = 9 )

### 11. Importer la base calories et fusionner celle avec la base. Cacluer la consommation calorie par tête.
### Nettoyer d’abord les valeurs manquantes et les valeurs aberrantes.
#### importation de la base calories
library(readxl)
calorie <- read_xlsx(paste(m_source,"calories.xlsx", sep = "/"))
calorie <- data.frame(calorie)
#### netoyage des valeurs manquantes
which(is.na(calorie),arr.ind=TRUE) # on a aucune valeur manquante donc passons à la suite

#### nettoyer les valeurs aberrantes.
#### Définissons la cote Z. La cote Z est l’écart de la moyenne mesurée en terme de nombre d’écart-type. 
#### Si une valeur est située à 3 écarts-type de la moyenne, la cote Z est de 3. 
#### On pourra détecter les valeurs aberrantes selon la distance des points 
#### en terme de cote Z, et retrancher les valeurs qui se situes au-delà d’une 
#### certaine limite. Il n’existe pas de distance standard: à vous de décider. 
#### Mais le nombre 3 est souvent utilisé. alors nous l'utiliserons

var <-  colnames(calorie)
for (i in 1:length(var)){
  calorie[,var[i]] <- as.numeric(calorie[,var[i]])
  variable <-  calorie[,var[i]]
  centered <- variable - mean(variable, na.rm = TRUE)
  limit <- 3 * sd(variable, na.rm = TRUE)
  io <- ifelse(abs(centered) > limit, 0, 1) # io = 0 si la valeur est aberrante et = 1 sinon
  base2 <- mutate(calorie, va = io)
  base2 <- filter(base2,va == 1) # ne conserver que les valeurs non aberrantes
  base2 <- base2[,-(length(var)+1)] # enlever la colonne va créée précédemment
  base2
}
calorie2 <- base2

#### fusioner les bases 
calorie2.0 <- mutate(calorie2, cle2 = produitID)
fusion2 <- mutate(fusion.1, cle2 = cereale.id)
fusion2.0  <-  merge(fusion2, calorie2.0, by="cle2")

####  Calcul de la consommation calorie par tête
base.finale <- mutate(fusion2.0, conso.cal.ind = (quant.cons.kg*kiloCalories)/taille_men)

## Exercice2

### 1. Fonction d’importation dont l’argument sera le chemin et nom du fichier
import.base <- function(path, name,extension){
 library(readr)
 library(haven)
 library(foreign)
 library(xlxs)
 setwd(path)
 if (extension == ".xlsx"){
   df <- data.frame(read.xlsx(file=name))
   df
 }
 else if (extension == ".dta"){
   df <- read.dta(file = name)
   df
 }
 else if (extension == ".sav"){
   df <- read.spss(file = name)
   df
 }
}

### 2. Fonction qui renome les variables
rename.var <- function( dataset_name,newnames){
 colnames(dataset_name) <- newnames
}

### 3. Fonction qui fusionne deux bases
fusion.base <- function(base1, base2, key){
merge.base <- merge(base1, base2, by= key)
merge.base
}

### 4. Fonction qui détecte les valeurs manquates
na.detection <- function(base){
  which(is.na(base),arr.ind=TRUE)
}

### 5. Fonction qui impute les valeurs manquantes
na.drop <- function(base){
  ligne.na <- c(which(is.na(base),arr.ind=TRUE)[,1])# lignes contenant des na
  df <- base[-ligne.na,] # suppression des lignes contenant des na
  df
}

### 6. Fonction qui détecte les valeurs aberrantes
#### Définissons la cote Z. La cote Z est l’écart de la moyenne mesurée en terme de nombre d’écart-type. 
#### Si une valeur est située à 3 écarts-type de la moyenne, la cote Z est de 3. 
#### On pourra détecter les valeurs aberrantes selon la distance des points 
#### en terme de cote Z, et retrancher les valeurs qui se situes au-delà d’une 
#### certaine limite. Il n’existe pas de distance standard: à vous de décider. 
#### Mais le nombre 3 est souvent utilisé. alors nous l'utiliserons

detection.va <- function(base ,variable , delimiteur = 3){
  centered <- variable - mean(variable, na.rm = TRUE)
  limit <- delimiter * sd(variable, na.rm = TRUE)
  io <- ifelse(abs(centered) > limit, 0, 1) # io = 0 si la valeur est aberrante et = 1 sinon
  base2.0 <- mutate(base, va = io)
  base2.0
}

### 7. Fonction qui corrige ces valeurs aberrantes

correction.va <- function(base,variable){
  base2 <- detection.va(base,variable)# détecter les valeurs aberrantes
  base2 <- filter(base2,va == 1) # ne conserver que les valeurs non aberrantes
  base2[,-va] # enlever la colonne io créée précédemment
  base2
}

