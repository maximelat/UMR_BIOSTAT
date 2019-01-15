# UMR_BIOSTAT

### Consignes d'utilisation 

D'abord il faut installer les bons packages : 

Packages à installer : 

```{r}
install.packages("rvest")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("dslabs")
install.packages("ROCR")
install.packages("gplots)
install.packages("stats")
install.packages("splines")
install.packages("pROC")
install.packages("Matrix)
install.packages("MASS)
install.packages("methods")
install.packages("grDevices")
install.packages("graphics")
install.packages("datasets)
```

> analyse.R  
télécharge et traite les données issues des sites fournits puis exportes 

```{r}source("./analyse.R")```
> synthese.R 

permet de creer un ensemble de test et les exporte dans le dossier figs/synthese/synthese++.pdf 

> testSWW.R 
```{r}source("./testSWW.R")```
permet de choisir le bon test Student Welch ou Wilcoxon (SWW) et de trouver la p-value correspondante

> inspiration  

http://ljk.imag.fr/membres/Clementine.Prieur/M1SSD/04exemple.pdf 

formules à lancer dans le fichier analyse.R à partir de ligne 116, voir les ### pour les choses à faire ou prendre des notes 

```{r}
library(rvest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
library(readxl)
library(SparseM)
library(quantreg)
library(tidyverse)

## importation des données
url <- "http://www.statsci.org/data/oz/ms212.txt"
eval7 <- read.delim(url)
download.file(url, "eval7.txt")
eval7$Weight
eval7$Weight[106]<- 72
eval7$Height[102]<- 168
eval7$Height[106]<- 193
eval7$Gender[eval7$Gender == 1] <- "Male"
eval7$Gender[eval7$Gender == 2] <- "Female"
eval7$Gender
eval7$Smokes[eval7$Smokes == 1] <- "Yes"
eval7$Smokes[eval7$Smokes == 2] <- "No"
eval7$Smokes
eval7$Alcohol[eval7$Alcohol == 1] <- "Yes"
eval7$Alcohol[eval7$Alcohol == 2] <- "No"
eval7$Alcohol
eval7$Exercise[eval7$Exercise == 1] <- "Hight"
eval7$Exercise[eval7$Exercise == 2] <- "Moderate"
eval7$Exercise[eval7$Exercise == 3] <- "Low"
eval7$Exercise
eval7$Ran[eval7$Ran == 1] <- "Ran"
eval7$Ran[eval7$Ran == 2] <- "Sat"
eval7$Ran
## ajout de delta et IMC 
eval7[106,] <- eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  mutate(delta = Pulse2-Pulse1)

## premier appercu des données

eval7 %>%  filter(Ran=="Ran") %>%
  ggplot(aes(Height, delta, label=Gender ,color=Gender, shape=Alcohol)) +
  geom_point(aes(Height, delta, label=Gender,color=Gender, shape=Alcohol)) +
  scale_shape() +  geom_smooth(method = lm, se = T)+
  labs( x = "Taille", y = "Delta entre Pulse1 et Pulse2",
        title ="Dispersion des échantillons",
        subtitle = "Echantillons d'Hommes et de femmes ayant courrus en fonction
de leur taille et de leur consommation d'alcool",
        caption = "Premier aperçu des données") +facet_grid(Gender ~.)

```

```{r}
paste("Hello", "World!")
```