---
title: "A Web Doc"
author: "John Doe"
date: "May 1, 2015"
output: pdf_document
---

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
install.packages("ROCR"")
install.packages("gplots"")
install.packages("stats"")
install.packages("splines"")
install.packages("pROC"")
install.packages("Matrix"")
install.packages("MASS"")
install.packages("methods"")
install.packages("grDevices"")
install.packages("graphics"")
install.packages("datasets"")
```

analyse.R
--> télécharge et traite les données issues des sites fournits

synthese.R 
--> permet de creer un ensemble de test et les exporte dans le dossier figs 

testSWW.R
--> permet de choisir le bon test Student Welch ou Wilcoxon (SWW) et de trouver la p-value correspondante

inspiration ######## http://ljk.imag.fr/membres/Clementine.Prieur/M1SSD/04exemple.pdf #######

