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

> analyse.R  
--> télécharge et traite les données issues des sites fournits puis exportes 

> synthese.R 
--> permet de creer un ensemble de test et les exporte dans le dossier figs/synthese/synthese++.pdf 

> testSWW.R  
--> permet de choisir le bon test Student Welch ou Wilcoxon (SWW) et de trouver la p-value correspondante

> inspiration  http://ljk.imag.fr/membres/Clementine.Prieur/M1SSD/04exemple.pdf 


```{r}
delta = eval7$Pulse2-eval7$Pulse1
    IMC = eval7$Weight/((eval7$Height/100)^2)
    Gender <- eval7$Gender
    Age <- eval7$Age
    Smokes <- eval7$Smokes
    Alcohol <- eval7$Alcohol
    Exercice <- eval7$Exercise
    Ran <- eval7$Ran

     breaks.aov <- aov(delta ~ IMC+Gender+Age+Smokes*Alcohol+Exercice*Ran)
  
     anova(breaks.aov)  
     coef(breaks.aov)
     
     
     breaks.aov <- aov(delta ~ Age+Smokes+Ran)
     
     anova(breaks.aov)  
     coef(breaks.aov)
```
