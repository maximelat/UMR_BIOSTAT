# UMR_BIOSTAT

## Consignes d'utilisation 
## Rapport
Le dossier Rapport comporte le rapport écrit Rapport.pdf, ses figures ainsi que les slides de presentation orale (presentation.pdf).

## Analyses R 
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

> Rapport.R
Décline toutes les analyses présentes dans le rapport

> import_données.R
Importe et télécharge le jeux de données
```source("./import_donnees.R")```

> analyse.R  
Analyses complémentaires pour recherche d'informations pertinentes à présenter

> synthese.R 
Permet d'analyser en une fonction  deux vecteurs et le comparer. Le choix du test de comparaison est effectué grace à choix_test.R. 

```full(Var1,Var2,Nom du Dossier d'export dans figs,teste de normalité=False,echelle log=False)```




