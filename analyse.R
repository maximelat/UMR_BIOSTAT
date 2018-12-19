library(rvest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
url <- "http://www.statsci.org/data/oz/strokeass.txt"
eval2 <- read.delim(url)
download.file(url, "eval2.txt")

eval2



url <- "http://www.statsci.org/data/general/eysenck.txt"
eval4 <- read.delim(url)
download.file(url, "eval4.txt")

eval4


url <- "http://www.statsci.org/data/oz/stroke.txt"
eval5 <- read.delim(url)
download.file(url, "eval5.txt")

eval5


url <- "http://www.statsci.org/data/oz/ms212.txt"
eval7 <- read.delim(url)
download.file(url, "eval7.txt")
eval7$Height[102]<- 168
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

summary(eval7)


eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  mutate(delta = Pulse2-Pulse1) %>% 
  ggplot(aes(IMC, delta, label=Gender ,color=Gender, shape=Smokes)) +
  geom_point(aes(IMC, delta, label=Gender,color=Gender, shape=Smokes)) +
  scale_shape() +
 labs( x = "IMC", y = "Delta entre deux pulses",
            title ="Dispertion des échantillons",
            subtitle = "Deltas en des hommes et des femmes fumeurs ou non en fonction de leur IMC",
            caption = "Premier aperçu des données") 
 //annotate(geom = "text", x = 8, y = 9, label = "A")
  //geom_label(aes(IMC, delta,label=Gender, color=Gender)) 
  // geom_text(aes(IMC, delta, label=Gender, color=Gender)) 
  //facet_grid(Gender~.)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/dispersion_echantillons_eval7.pdf")), bg="white")



## densité de delta en fonction du sexe
eval7 %>%  
  mutate(delta = Pulse2-Pulse1) %>% 
  ggplot(aes(delta,color = Gender,fill = Gender)) + 
  geom_density(aes(delta, color = Gender,fill = Gender), alpha = 0.2)
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
  
  boxplot(delta ~ Gender, data = eval7)
  dev.print(device = pdf, file = gsub(" ","",paste("./figs/boxplot_deltaVSgender_eval7.pdf")), bg="white")
  

## anova 
  
  
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
     
     52.7-0.058*18+2.01+52.22
     
     
     ## quest ce qui est responsalbe de la variabilité de ceux qui ont courrus
     
     Ran_SmokesYes = filter(eval7, eval7$Ran == "Ran")$Weight/(filter(eval7, eval7$Gender == "Male")$Height/100)^2
     
     breaks.aov  <-  eval7 %>%  
       mutate(delta = Pulse2-Pulse1) %>%
       mutate(IMC = Weight/((Height/100)^2)) %>%  
       filter(Ran == "Ran" & Alcohol == "Yes") 
     
     aov(breaks.aov$Alcohol ~ breaks.aov$IMC+breaks.aov$Gender+breaks.aov$Age+breaks.aov$Alcohol+breaks.aov$Exercice*breaks.aov$Ran)
     
     breaks.aov
     
     
     anova(breaks.aov)  
     coef(breaks.aov)
     
     
     
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


murders %>% ggplot(aes(population, total, label=abb)) +
  geom_point(aes(population, total)) + geom_label(aes(population, total,label=abb))


gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day) & !is.na(gdp) & !is.na(infant_mortality))%>% 
  ggplot(aes(dollars_per_day,infant_mortality,color = region,label = country)) +  geom_point(aes( )) + scale_x_continuous(trans="log2") +geom_text(aes()) + facet_grid(year~.)

 


