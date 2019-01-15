library(rvest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
library(readxl)
library(SparseM)
library(quantreg)
library(tidyverse)

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

eval7[106,] <- eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  mutate(delta = Pulse2-Pulse1)


eval7 %>%  filter(Ran =="Ran") %>%
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  mutate(delta = Pulse2-Pulse1) %>% 
  ggplot(aes(IMC, delta, label=Gender ,color=Gender, shape=Ran,size=Smokes)) +
  geom_point(aes(IMC, delta, label=Gender,color=Gender, shape=Ran,size=Smokes)) +
  scale_shape( ) + geom_smooth()+
  labs( x = "IMC", y = "Delta entre deux pulses",
        title ="Dispertion des échantillons",
        subtitle = "Deltas en des hommes et des femmes fumeurs ou non en fonction de leur IMC",
        caption = "Premier aperçu des données")

eval7 %>%  filter(Ran=="Ran") %>%
  ggplot(aes(Height, delta, label=Gender ,color=Gender, shape=Alcohol)) +
  geom_point(aes(Height, delta, label=Gender,color=Gender, shape=Alcohol)) +
  scale_shape() +  geom_smooth(method = lm, se = T)+
  labs( x = "Taille", y = "Delta entre Pulse1 et Pulse2",
        title ="Dispersion des échantillons",
        subtitle = "Echantillons d'Hommes et de femmes ayant courrus en fonction
de leur taille et de leur consommation d'alcool",
        caption = "Premier aperçu des données") +facet_grid(Gender ~.)

eval7 %>%  filter(Ran=="Ran", Gender=="Female") %>%
  ggplot(aes(Height, delta, label=Gender ,color=Gender, shape=Alcohol)) +
  geom_point(aes(Height, delta, label=Gender,color=Gender, shape=Alcohol)) +
  scale_shape() +  geom_smooth(method = lm, se = T)+
  labs( x = "Taille", y = "Delta entre Pulse1 et Pulse2",
        title ="Dispersion des échantillons",
        subtitle = "Echantillons d'Hommes et de femmes ayant courrus en \nfonction de leur taille et de leur consommation d'alcool ",
        caption = "Premier aperçu des données")

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


breaks.aov <-   aov(delta ~ Height+Gender+Smokes+Ran)

anova(breaks.aov)  
coef(breaks.aov)

breaks.aov <- aov(IMC ~ Height+Weight+Gender+Alcohol)

anova(breaks.aov)  
coef(breaks.aov)



breaks.aov <- aov(Alcohol ~ Height+Weight+Gender+IMC)

anova(breaks.aov)  
coef(breaks.aov)

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

breaks.aov <- aov(delta ~ Height+Gender+Age+Smokes+Ran)

anova(breaks.aov)  
coef(breaks.aov)
eval7 %>%  

  ggplot(aes(Pulse1, Pulse2))+ 
  geom_quantile(aes(Pulse1, Pulse2) )


boxplot(delta ~ Smokes, data = eval7) 
dev.print(device = pdf, file = gsub(" ","",paste("./figs/boxplot_deltaVSSmokes_eval7.pdf")), bg="white")

boxplot(delta ~ Ran, data = eval7)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/boxplot_deltaVSgender_eval7.pdf")), bg="white")

full(na.omit(IMC),na.omit(Height),"",F,F)

