library(rvest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
library(readxl)
library(SparseM)
library(quantreg)
library(tidyverse)


#Import des données
source("./import_donnees.R")

############## Intro############
eval7 %>%  filter() %>%
  ggplot(aes(Height, delta, label=Gender ,color=Gender, shape=Ran)) +
  geom_boxplot(aes(Height, delta,alpha=0.1), outlier.shape = 4) +
  geom_point(aes(Height, delta, label=Gender,color=Gender, shape=Ran)) +
  scale_shape( ) +  
  geom_smooth(method = lm, se = T)+ 
  labs( x = "Taille", y = "Delta entre Pulse1 et Pulse2",
        title ="Dispertion des échantillons",
        subtitle = "Echantillons des deltas d'hommes et de femmes en fonction de la course ou non,
de leur taille et de leur consommation d'alcool",
        caption = "Premier aperçu des données") +
  facet_grid(Gender ~.)


eval7 %>%  filter(Ran=="Ran") %>%
  ggplot(aes(Height, Pulse1, label=Gender ,color=Gender)) +
  geom_point(aes(Height, Pulse1, label=Gender,color=Gender)) +
  scale_shape( ) +  
  geom_smooth(method = lm, se = T)+ 
  labs( x = "Taille", y = "Delta entre Pulse1 et Pulse2",
        title ="Dispertion des échantillons",
        subtitle = "Echantillons des Pulses1 des hommes et de femmes 
en fonction de la taille",
        caption = "Premier aperçu des données") +
  facet_grid(Gender ~.)


################ Objectif 1 ###################
delta = eval7$delta
IMC = eval7$Weight/((eval7$Height/100)^2)
Gender <- eval7$Gender
Age <- eval7$Age
Smokes <- eval7$Smokes
Alcohol <- eval7$Alcohol
Exercice <- eval7$Exercise
Ran <- eval7$Ran
Height <- eval7$Height
Weight <- eval7$Weight
Pulse1 <- eval7$Pulse1
Pulse2 <- eval7$Pulse2

breaks.aov <-   aov(delta ~ IMC+Gender+Age+Smokes+Alcohol+Exercice+Ran+Height+Weight)
anova(breaks.aov)  
coef(breaks.aov)

#L'age, fumer et courir sont facteur de variabilité de delta
breaks.aov <- aov(delta ~ Age+Smokes+Ran+Height)
anova(breaks.aov)  
coef(breaks.aov)

#Faire de l'exercice et avoir un IMC sont facteurs de variabilité de Pulse1
breaks.aov <- aov(Pulse1 ~ IMC+Exercice+Gender+Age+Smokes+Alcohol+Ran+Height+Weight)
anova(breaks.aov)  
coef(breaks.aov)

ExerciceL <- eval7 %>% filter(Exercice=="Low")
ExerciceM <- eval7 %>% filter(Exercice=="Moderate")
ExerciceH <- eval7 %>% filter(Exercice=="Hight")

breaks.aov <- aov(Pulse1 ~ Exercice)
anova(breaks.aov)  
coef(breaks.aov)

################ Objectif 2 ###################
delta_SmokesYes = filter(eval7, eval7$Alcohol == "Yes")$Pulse2-
  filter(eval7, eval7$Alcohol == "Yes")$Pulse1
delta_SmokesNo = filter(eval7, eval7$Alcohol == "No")$Pulse2-
  filter(eval7, eval7$Alcohol == "No")$Pulse1
sd(delta_SmokesYes)
sd(na.omit(delta_SmokesNo))

var.test(delta_SmokesYes,delta_SmokesNo)

t.test(delta_SmokesYes,na.omit(delta_SmokesNo),var.equal=T,alternative="two.sided")
t.test(delta_SmokesYes,na.omit(delta_SmokesNo),var.equal=F,alternative="two.sided")
wilcox.test(delta_SmokesYes,na.omit(delta_SmokesNo))
#On récupère la p value 
pvalue <- t.test(delta_SmokesYes,na.omit(delta_SmokesNo),var.equal=F,alternative="two.sided")[3]

boxplot(delta ~ Smokes,   main = paste("Boxplot des ∆ en fonction de Smokes + IC à 95%"), 
        xlab=paste("p-value de Welch =",format(as.numeric(pvalue),scientific=T)),
        ylab = "Delta")
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



################ Objectif 3 ###################
#Comparaison de la proportion de sujet ayant couru avant et après 95 
P1B <- filter(eval7, eval7$Year <= 95)[8]
P1BAS <- nrow(filter(P1B,Ran == "Sat")) 
P1BAR <- nrow(filter(P1B,Ran == "Ran" ))

P1H <- filter(eval7, eval7$Year > 95)[8]
P1HAS <- nrow(filter(P1H,Ran == "Sat")) 
P1HAR <- nrow(filter(P1H,Ran == "Ran" ))
#khi 2 
prop.test(c(P1BAS,P1HAS),c(P1BAS+P1BAR,P1HAS+P1HAR))

#Comparaison de la proportion de fumeurs ayant couru avant et après 95 
P1B <- filter(eval7, eval7$Year %in% c(93,95), eval7$Smokes=="Yes")[8]
P1BAS <- nrow(filter(P1B,Ran == "Sat")) 
P1BAR <- nrow(filter(P1B,Ran == "Ran" ))

P1H <- filter(eval7, eval7$Year > 95 ,eval7$Smokes=="Yes" )[8]
P1HAS <- nrow(filter(P1H,Ran == "Sat")) 
P1HAR <- nrow(filter(P1H,Ran == "Ran" ))

MP <- rbind(c(P1BAS,P1BAR),c(P1HAS,P1HAR))
# Fisher
fisher.test(MP)


########## PREZ


