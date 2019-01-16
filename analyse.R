library(rvest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
library(readxl)


source("./import_donnees.R")

eval7
summary(eval7)


## densité de delta en fonction de Ran sexe
eval7 %>%
  mutate(delta = Pulse2-Pulse1) %>% 
  ggplot(aes(delta,color = Ran,fill = Ran)) + 
  geom_density(aes(delta, color = Ran,fill = Ran), alpha = 0.2)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/densite_DeltaVSGenre_eval7.pdf")), bg="white")



## densité de delta en fonction de la consomation frequente d'alcool
eval7 %>% 
  mutate(delta = Pulse2-Pulse1) %>% 
  ggplot(aes(delta,color = Alcohol,fill = Alcohol)) + 
  geom_density(aes(delta, color = Alcohol, fill = Alcohol), alpha = 0.2)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/densite_DeltaVSalcohol_eval7.pdf")), bg="white")

## densité IMC en fonction du sexe
eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  ggplot(aes(IMC,color = Gender,fill = Gender)) + 
  geom_density(aes(IMC, color = Gender,fill = Gender), alpha = 0.2)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/densite_IMCVSGenre_eval7.pdf")), bg="white")

## densité de l'IMC en fonction de fumeur ou non 
eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  ggplot(aes(IMC,color = Smokes,fill = Smokes)) + 
  geom_density(aes(IMC, color = Smokes,fill = Smokes), alpha = 0.2)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/densite_IMCVSSmokes_eval7.pdf")), bg="white")

## densité de l'IMC en fonction de alcohol ou non 
eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  ggplot(aes(IMC,color = Alcohol,fill = Alcohol)) + 
  geom_density(aes(IMC, color = Alcohol,fill = Alcohol), alpha = 0.2)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/densite_IMCVSAlcohol_eval7.pdf")), bg="white")


## BoxPlot 
delta <-  eval7$Pulse2-eval7$Pulse1

  boxplot(delta ~ Smokes, data = eval7) 
  dev.print(device = pdf, file = gsub(" ","",paste("./figs/boxplot_deltaVSSmokes_eval7.pdf")), bg="white")
  
  boxplot(delta ~ Ran, data = eval7)
  dev.print(device = pdf, file = gsub(" ","",paste("./figs/boxplot_deltaVSgender_eval7.pdf")), bg="white")
  
     
     
     ## test de comparaisons de moyennes indépendantes 

## ad hoc ? 

     require (ROCR); require(gplots) ; require(stats) ; require(utils) ; require(splines) ; require(pROC) ; require(Matrix) ; require(MASS) ; require(methods) ; require(grDevices) ; require(graphics) ; require(gplots) ; require(datasets)
     library(dplyr)
     library(ggplot2)
     library(dslabs)
     source("./testSWW.R")
     
     
     
     
     
     ########### TEste 
source("./synthese.R")
     



IMC_Male = filter(eval7, eval7$Gender == "Male")$Weight/(filter(eval7, eval7$Gender == "Male")$Height/100)^2
delta_Male = filter(eval7, eval7$Gender == "Male")$Pulse2-filter(eval7, eval7$Gender == "Male")$Pulse1
IMC_Female = filter(eval7, eval7$Gender == "Female")$Weight/(filter(eval7, eval7$Gender == "Female")$Height/100)^2
delta_Female = filter(eval7, eval7$Gender == "Female")$Pulse2-filter(eval7, eval7$Gender == "Female")$Pulse1

full(na.omit(delta_Male),na.omit(delta_Female),"",F,F)


delta_SmokesYes = filter(eval7, eval7$Smokes == "Yes")$Pulse2-filter(eval7, eval7$Smokes == "Yes")$Pulse1
delta_SmokesNo = filter(eval7, eval7$Smokes == "No")$Pulse2-filter(eval7, eval7$Smokes == "No")$Pulse1

full(na.omit(delta_SmokesYes),na.omit(delta_SmokesNo),"",F,F)

delta_RanYes = filter(eval7, eval7$Ran == "Ran")$Pulse2-filter(eval7, eval7$Ran == "Ran")$Pulse1
delta_RanNo = filter(eval7, eval7$Ran == "Sat")$Pulse2-filter(eval7, eval7$Ran == "Sat")$Pulse1
#Non appariés et pas de graphique 
full(na.omit(delta_RanYes),na.omit(delta_RanNo),"",F,F)


####### 
surpoidObese <- c(filter(eval7, IMC>30, Exercise == "Moderate")$delta,
      filter(eval7, IMC>30, Exercise == "Hight")$delta,
      filter(eval7, IMC>30, Exercise == "Low")$delta,
      filter(eval7, IMC>25, IMC <30, Exercise == "Hight")$delta,
      filter(eval7, IMC>25, IMC <30, Exercise == "Moderate")$delta,
      filter(eval7, IMC>25, IMC <30, Exercise == "Low")$delta)

denutriNormal <- c(filter(eval7, IMC<18.5, Exercise == "Hight")$delta,
filter(eval7, IMC<18.5, Exercise == "Moderate")$delta,
filter(eval7, IMC<18.5, Exercise == "Low")$delta,
filter(eval7, IMC>18.5, IMC <25, Exercise == "Hight")$delta,
filter(eval7, IMC>18.5, IMC <25, Exercise == "Moderate")$delta,
filter(eval7, IMC>18.5, IMC <25, Exercise == "Low")$delta)


t.test(denutriNormal,surpoidObese)

