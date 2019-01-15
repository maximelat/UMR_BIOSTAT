library(dplyr)

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

eval7<-mutate(eval7,delta = eval7$Pulse2-eval7$Pulse1,IMC = eval7$Weight/((eval7$Height/100)^2))


eval7
