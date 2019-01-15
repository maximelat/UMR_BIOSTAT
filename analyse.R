library(rvest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
library(readxl)


 eval1<- read_excel("./eval1.xlsx")
eval1


url <- "http://www.statsci.org/data/oz/strokeass.txt"
eval2 <- read.delim(url)
download.file(url, "eval2.txt")

eval2

url <- "http://www.statsci.org/data/general/fullmoon.txt"
eval3 <- read.delim(url)
download.file(url, "eval2.txt")

eval3

url <- "http://www.statsci.org/data/general/eysenck.txt"
eval4 <- read.delim(url)
download.file(url, "eval4.txt")

eval4


url <- "http://www.statsci.org/data/oz/stroke.txt"
eval5 <- read.delim(url)
download.file(url, "eval5.txt")

eval5

url <- "http://www.statsci.org/data/general/cholestg.txt"
eval6 <- read.delim(url)
download.file(url, "eval5.txt")

eval6



url <- "http://www.statsci.org/data/oz/ms212.txt"
eval7 <- read.delim(url)
download.file(url, "eval7.txt")
eval7$Height[102]
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
eval7 <- eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  mutate(delta = Pulse2-Pulse1)


eval7
summary(eval7)



eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  mutate(delta = Pulse2-Pulse1) %>% 
  ggplot(aes(IMC, delta, label=Gender ,color=Gender, shape=Ran,size=Smokes)) +
  geom_point(aes(IMC, delta, label=Gender,color=Gender, shape=Ran,size=Smokes)) +
  scale_shape( ) +
 labs( x = "IMC", y = "Delta entre deux pulses",
            title ="Dispertion des échantillons",
            subtitle = "Deltas en des hommes et des femmes fumeurs ou non en fonction de leur IMC",
            caption = "Premier aperçu des données") 


eval7 %>%  filter(Ran=="Ran") %>%
  ggplot(aes(Height, delta, label=Gender ,color=Gender, shape=Smokes)) +
  geom_point(aes(Height, delta, label=Gender,color=Gender, shape=Smokes)) +
  scale_shape() +
  labs( x = "Height", y = "Delta entre deux pulses",
        title ="Dispertion des échantillons",
        subtitle = "Deltas en des hommes et des femmes fumeurs ou non en fonction de leur IMC",
        caption = "Premier aperçu des données") 

 //annotate(geom = "text", x = 8, y = 9, label = "A")
  //geom_label(aes(IMC, delta,label=Gender, color=Gender)) 
  // geom_text(aes(IMC, delta, label=Gender, color=Gender)) 
  //facet_grid(Gender~.)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/dispersion_echantillons_eval7.pdf")), bg="white")



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
  

## anova 
  
  
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
    
     breaks.aov <- aov(delta ~ Height+Weight+IMC+Gender+Age+Smokes+Alcohol+Exercice+Ran)
  
     anova(breaks.aov)  
     coef(breaks.aov)
     
     breaks.aov <- aov(IMC ~ Height+Weight+Gender+Alcohol)
     
     anova(breaks.aov)  
     coef(breaks.aov)
     
     
     breaks.aov <- aov(delta ~ Height+Gender+Smokes+Ran)

    anova(breaks.aov)  
     coef(breaks.aov)
     
     52.7-0.058*18+2.01+52.22
     
     
     ## quest ce qui est responsalbe de la variabilité de ceux qui ont courrus
     
     Ran_SmokesYes = filter(eval7, eval7$Ran == "Ran" && eval7$Gender == "Male")$Weight/(filter(eval7, eval7$Ran == "Ran" && eval7$Gender == "Male")$Height/100)^2
     breaks.aov  <-  eval7 %>%  
       mutate(delta = Pulse2-Pulse1) %>%
       mutate(IMC = Weight/((Height/100)^2)) %>%  
       filter(Ran == "Ran" & Alcohol == "Yes") 
     
     breaks.aov <-   aov(breaks.aov$Alcohol ~ breaks.aov$IMC+breaks.aov$Gender+breaks.aov$Age+breaks.aov$Alcohol+breaks.aov$Exercice*breaks.aov$Ran)
     
     summary(eval7)
     
     
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


eval7
eval7Bis <- eval7 %>% filter( Gender  & Year < 95) %>% select(Gender)



Female_B95 <- filter(eval7, eval7$Year <= 95 & eval7$Gender == "Female")[8]
B95FS <- nrow(filter(Female_B95,Ran == "Sat")) 
B95FR <- nrow(filter(Female_B95,Ran == "Ran" ))

Female_A95 <- filter(eval7, eval7$Year >= 95 & eval7$Gender == "Female")[8]
A95FS <- nrow(filter(Female_A95,Ran == "Sat")) 
A95FR <- nrow(filter(Female_A95,Ran == "Ran" ))

prop.test(c(A95FR,B95FR),c(A95FS+A95FR,B95FR+B95FS))


male_B95 <- filter(eval7, eval7$Year <= 95 & eval7$Gender == "Male")[8]
B95MS <- nrow(filter(male_B95,Ran == "Sat")) 
B95MR <- nrow(filter(male_B95,Ran == "Ran" ))

male_A95 <- filter(eval7, eval7$Year >= 95 & eval7$Gender == "Male")[8]
A95MS <- nrow(filter(male_A95,Ran == "Sat")) 
A95MR <- nrow(filter(male_A95,Ran == "Ran" ))

prop.test(c(A95MR,B95MR),c(A95MS+A95MR,B95MR+B95MS))

p = 0.39

Alcohol_B95 <- filter(eval7, eval7$Year <= 95 & eval7$Alcohol == "Yes")[8]
B95SS <- nrow(filter(Alcohol_B95,Ran == "Sat")) 
B95SR <- nrow(filter(Alcohol_B95,Ran == "Ran" ))

Alcohol_A95 <- filter(eval7, eval7$Year >= 95 & eval7$Alcohol == "No")[8]
A95NS <- nrow(filter(Alcohol_A95,Ran == "Sat")) 
A95NR <- nrow(filter(Alcohol_A95,Ran == "Ran" ))

prop.test(c(B95SS,A95NS),c(B95SS+B95SR,A95NS+A95NR))



AnormalB <- filter(eval7, eval7$Year <= 95 & IMC <18 & IMC >25 )[8]
B95AS <- nrow(filter(AnormalB,Ran == "Sat")) 
B95AR <- nrow(filter(AnormalB,Ran == "Ran" ))

AnormalA <- filter(eval7, eval7$Year > 95 & IMC <18 | IMC >25)[8]
A95AS <- nrow(filter(AnormalA,Ran == "Sat")) 
A95AR <- nrow(filter(AnormalA,Ran == "Ran" ))

prop.test(c(B95AS,A95AS),c(B95AR+B95AS,A95AS+A95AR))

summary(Pulse1)

P1B <- filter(eval7, eval7$Year <= 95, eval7$Pulse1>76)[8]
P1BAS <- nrow(filter(P1B,Ran == "Sat")) 
P1BAR <- nrow(filter(P1B,Ran == "Ran" ))

P1H <- filter(eval7, eval7$Year > 95,eval7$Pulse1>76)[8]
P1HAS <- nrow(filter(P1H,Ran == "Sat")) 
P1HAR <- nrow(filter(P1H,Ran == "Ran" ))
prop.test(c(P1BAS,P1HAS),c(P1BAS+P1BAR,P1HAS+P1HAR))


P1B <- filter(eval7, eval7$Year %in% c(93,95), eval7$Smokes=="Yes")[8]
P1BAS <- nrow(filter(P1B,Ran == "Sat")) 
P1BAR <- nrow(filter(P1B,Ran == "Ran" ))

P1H <- filter(eval7, eval7$Year > 95 ,eval7$Smokes=="Yes" )[8]
P1HAS <- nrow(filter(P1H,Ran == "Sat")) 
P1HAR <- nrow(filter(P1H,Ran == "Ran" ))

MP <- rbind(c(P1BAS,P1BAR),c(P1HAS,P1HAR))
fisher.test(MP)



############### COMPTE RENDU 

Smokes_Pulse1 <- filter(eval7, Smokes == "Yes")$Pulse1
mean(Smokes_Pulse1)
#[1] 77.54545
sd(Smokes_Pulse1)
# [1] 9.574588
var(Smokes_Pulse1)
#[1] 91.67273
eval7

NoSmokes_Pulse1 <- filter(eval7, Smokes == "No")$Pulse1
mean(na.omit(NoSmokes_Pulse1))
#[1] 75.47959
sd(na.omit(NoSmokes_Pulse1))
#[1] 13.67459
var(na.omit(NoSmokes_Pulse1))
#[1] 186.9944


####### MAX

eval7 <- eval7 %>%  
  mutate(IMC = Weight/((Height/100)^2)) %>%  
  mutate(delta = Pulse2-Pulse1)
eval7

filter(eval7, IMC>18.5, IMC <25)

filter(eval7, IMC<18.5, Exercise == "Hight")$IMC
filter(eval7, IMC<18.5, Exercise == "Moderate")$IMC
filter(eval7, IMC<18.5, Exercise == "Low")$IMC

filter(eval7, IMC>18.5, IMC <25, Exercise == "Hight")$IMC
filter(eval7, IMC>18.5, IMC <25, Exercise == "Moderate")$IMC
filter(eval7, IMC>18.5, IMC <25, Exercise == "Low")$IMC

filter(eval7, IMC>25, IMC <30, Exercise == "Hight")$IMC
filter(eval7, IMC>25, IMC <30, Exercise == "Moderate")$IMC
filter(eval7, IMC>25, IMC <30, Exercise == "Low")$IMC

filter(eval7, IMC>30, Exercise == "Moderate")$IMC
filter(eval7, IMC>30, Exercise == "Hight")$IMC
filter(eval7, IMC>30, Exercise == "Low")$IMC

surpoidObese <- c(filter(eval7, IMC>30, Exercise == "Moderate")$Pulse1,
      filter(eval7, IMC>30, Exercise == "Hight")$Pulse1,
      filter(eval7, IMC>30, Exercise == "Low")$Pulse1,
      filter(eval7, IMC>25, IMC <30, Exercise == "Hight")$Pulse1,
      filter(eval7, IMC>25, IMC <30, Exercise == "Moderate")$Pulse1,
      filter(eval7, IMC>25, IMC <30, Exercise == "Low")$Pulse1)

denutriNormal <- c(filter(eval7, IMC<18.5, Exercise == "Hight")$Pulse1,
filter(eval7, IMC<18.5, Exercise == "Moderate")$Pulse1,
filter(eval7, IMC<18.5, Exercise == "Low")$Pulse1,
filter(eval7, IMC>18.5, IMC <25, Exercise == "Hight")$Pulse1,
filter(eval7, IMC>18.5, IMC <25, Exercise == "Moderate")$Pulse1,
filter(eval7, IMC>18.5, IMC <25, Exercise == "Low")$Pulse1)



NoSmokes_Pulse1 <- filter(eval7, Smokes == "No", Ran=="Ran")$delta
Smokes_Pulse1 <- filter(eval7, Smokes == "Yes",Ran=="Ran")$delta

full(na.omit(Smokes_Pulse1),na.omit(NoSmokes_Pulse1),"",F,F)

NoAl_Pulse1 <- filter(eval7, Alcohol == "No")$Pulse1
Al_Pulse1 <- filter(eval7, Alcohol == "Yes")$Pulse1
full(na.omit(NoAl_Pulse1 ),na.omit(Al_Pulse1) ,"",F,F)

NoAl_Pulse1 <- filter(eval7, Alcohol == "No")$Pulse1
Al_Pulse1 <- filter(eval7, Alcohol == "Yes")$Pulse1
full(na.omit(surpoidObese ),na.omit(denutriNormal) ,"",F,F)


boxplot(delta ~ Smokes)

