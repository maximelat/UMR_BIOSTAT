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
```

```
Importation des données

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

>Anova Objectif 1 

```{R}
breaks.aov <-   aov(delta ~ IMC+Gender+Age+Smokes+Alcohol+Exercice+Ran+Height+Weight+Pulse1+Pulse2)

anova(breaks.aov)  
coef(breaks.aov)

breaks.aov <- aov(delta ~ Age+Smokes+Ran)

anova(breaks.aov)  
coef(breaks.aov)

```

Comparaison de moyennes Student Objectif 2

```{r}

delta_SmokesYes = filter(eval7, eval7$Smokes == "Yes")$Pulse2-filter(eval7, eval7$Smokes == "Yes")$Pulse1
delta_SmokesNo = filter(eval7, eval7$Smokes == "No")$Pulse2-filter(eval7, eval7$Smokes == "No")$Pulse1

t.test(delta_SmokesYes,na.omit(delta_SmokesNo),var.equal=T,alternative="two.sided")
t.test(delta_SmokesYes,na.omit(delta_SmokesNo),var.equal=F,alternative="two.sided")
wilcox.test(delta_SmokesYes,na.omit(delta_SmokesNo))
#On récupère la p value 
pvalue <- t.test(delta_SmokesYes,na.omit(delta_SmokesNo),var.equal=F,alternative="two.sided")[3]


boxplot(delta ~ Smokes,   main = paste("Boxplot des ∆ en fonction de Smokes + IC à 95%"), xlab=paste("p-value de machin ="),ylab = "Delta")
smokesB = gsub("Yes",1,Smokes)
smokesB = gsub("No",0,smokesB)

a=t.test(delta, conf.level=0.95)
inta=round(a$estimate-a$conf.int[1],2)
b=t.test(as.numeric(smokesB), conf.level=0.95)
intb=round(b$estimate-b$conf.int[1],2)
points(1, a$estimate, col = "Green",pch = 3)
a$estimate
points(1, a$conf.int[1], col = "blue",pch = "-")
a$conf.int[1]
points(1, a$conf.int[2], col = "red",pch = "-")
a$conf.int[2]

points(2, b$estimate, col = "green",pch = 3)
b$estimate
points(2, b$conf.int[1], col = "blue",pch = "-")
b$conf.int[1]
points(2, b$conf.int[2], col = "red",pch = "-")
b$conf.int[2]
    ```

```
##### Objectif 4

#Comparaison de pulse 1 Avant et après 95 
P1B <- filter(eval7, eval7$Year <= 95, eval7$Pulse1>76)[8]
P1BAS <- nrow(filter(P1B,Ran == "Sat")) 
P1BAR <- nrow(filter(P1B,Ran == "Ran" ))

P1H <- filter(eval7, eval7$Year > 95,eval7$Pulse1>76)[8]
P1HAS <- nrow(filter(P1H,Ran == "Sat")) 
P1HAR <- nrow(filter(P1H,Ran == "Ran" ))
#khi 2 
prop.test(c(P1BAS,P1HAS),c(P1BAS+P1BAR,P1HAS+P1HAR))

#Comparaison de pulse 1 Avant et après 95  chez les fumeurs 
P1B <- filter(eval7, eval7$Year %in% c(93,95), eval7$Smokes=="Yes")[8]
P1BAS <- nrow(filter(P1B,Ran == "Sat")) 
P1BAR <- nrow(filter(P1B,Ran == "Ran" ))

P1H <- filter(eval7, eval7$Year > 95 ,eval7$Smokes=="Yes" )[8]
P1HAS <- nrow(filter(P1H,Ran == "Sat")) 
P1HAR <- nrow(filter(P1H,Ran == "Ran" ))

MP <- rbind(c(P1BAS,P1BAR),c(P1HAS,P1HAR))
# Fisher
fisher.test(MP)
```
```{r}
paste("Hello", "World!")
```