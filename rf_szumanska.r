#praca licencjacka-Patrycja Szumanska 31.05.2021
#Chcia�am zapyta� o m�j model i poprawno��.
#Czy sugeruje Profesor, �eby w pracy przedstawi� jeden z modeli, czy te� wszystkie, jako r�ne sposoby? 

# Lepiej pokaza� r�ne, to jest przy okazji sprawdzenie stabilno�ci wynik�w.

#My�la�am, �e w przypadku jednego z modeli to przedstwai� wp�yw zmiennych parametr�w(liczby drzew w modelu, mtry, podzia� zbioru) 

# Mo�e Pani pokaza� w szczeg�ach model najlepszy a pozosta�e modele opisa� mniej dok�adnie w tek�cie g��wnym, odsy�aj�c po wyniki szczeg�owe dot. tych pozosta�ych modeli np. do za��cznik�w.

#jaki maj� wp�yw na wsp�czynnik determinacji i bledy
# Chcia�am te� zapyta� czy wykres b��du MSE, same b��dy MSE i RMSE s� wystarczaj�ce?

# Tak.


#Dla moich danych trudno uzyska� mi dobre wyniki. Najlepszy dla modelu 3 jaki mi sie udalo jest dla ponizszych parametrow - 36%

# OK, takie s� dane. Nie przeskoczymy tego.

require(tidyverse)
require(stringr)
require(tm)
require(randomForest)
require(Metrics)
library(ipred)

set.seed(1100)
n<- 1000 #ntree
m<- 2# mtry
#podzial zbioru
a<-0.7#zbi�r uczacy
b<-0.3#zbi�r testowy

#################################################################################

# MODEL1
#W tym modelu doda�am dane rzeczywiste(ujednolicony format- tesktowe na liczbowe zmienione)

data <- read.csv("C:/Users/patsz/OneDrive/Pulpit/praca dyplomowa/ERKa/p2.csv")
names <- c("time","Mezczyzna", "wiek", "wojewodztwo", "w_miejscowosc", "drugi_kierunek", "publiczna", "dziedzina_studiow",
           "zaoczne", "nie_pracuje", "forma_ksiazki", "pochodzenie_ksiazki", "dzial_ksiazki", "powod", "czym_wybor", "kiedy","nowosci_nie","netflix_nie","lubie_czytac","pandemia_wiecej_ksiazek","wiecej_czasu","za_malo_czytam","zrodlo_wiedzy","dobry_prezent","y", "komentarze","dziesiatki","przedzialy_5")
names(data) <- names
###############
#import i obr�bka
data<- data[,-26]#usuniecie komentarzy do ankiety
data<- data[,-1]#usuniecie sygnatury czasowej

data<- data[,-26]#usunicie kolumn wykorzystywanych przy modelach 2 i 3
data<- data[,-25]#usunicie kolumn wykorzystywanych przy modelach 2 i 3


i = sample(2, nrow(data), replace = TRUE, prob = c(a, b))


#na binarne
data$Mezczyzna[data$Mezczyzna=="Kobieta"] <- 0 
data$Mezczyzna[data$Mezczyzna!=0] <- 1
data$Mezczyzna <- as.numeric(data$Mezczyzna)
data.train <- data[i==1,]
data.test <- data[i==2,]
# praca
data$nie_pracuje[data$nie_pracuje=="tak, pracuje"] <- 0 
data$nie_pracuje[data$nie_pracuje!=0] <- 1
data$nie_pracuje <- as.numeric(data$nie_pracuje)
data.train <- data[i==1,]
data.test <- data[i==2,]
#dwa kierunki
data$drugi_kierunek[data$drugi_kierunek=="nie, studiuje jeden kierunek"] <- 0 
data$drugi_kierunek[data$drugi_kierunek!=0] <- 1
data$drugi_kierunek <- as.numeric(data$drugi_kierunek)
data.train <- data[i==1,]
data.test <- data[i==2,]
#rodzaj uczelni
data$publiczna[data$publiczna=="Uczelnia prywatna"] <- 0 
data$publiczna[data$publiczna!=0] <- 1
data$publiczna <- as.numeric(data$publiczna)
data.train <- data[i==1,]
data.test <- data[i==2,]
#tryb studiowania
data$zaoczne[data$zaoczne=="dziennie"] <- 0 
data$zaoczne[data$zaoczne!=0] <- 1
data$zaoczne <- as.numeric(data$zaoczne)
data.train <- data[i==1,]
data.test <- data[i==2,]
#newsy
data$nowosci_nie[data$nowosci_nie=="tak"] <- 0 
data$nowosci_nie[data$nowosci_nie!=0] <- 1
data$nowosci_nie <- as.numeric(data$nowosci_nie)
data.train <- data[i==1,]
data.test <- data[i==2,]
#streaming
data$netflix_nie[data$netflix_nie=="tak, mam uzywam takich platform"] <- 0 
data$netflix_nie[data$netflix_nie!=0] <- 1
data$netflix_nie <- as.numeric(data$netflix_nie)
data.train <- data[i==1,]
data.test <- data[i==2,]

#########

model1<-randomForest(formula = y ~ .,data=data.train,ntree= n,mtry=m,do.trace=10, keep.forest =T)
print(model1)

plot(model1, main=NULL)
print("Wsp�lczynnik determinacji:", quote = F)
print(model1$rsq[n])
#wagi zmiennych
wz1<- round(importance(model1),2)
waga1<- wz1[order(wz1,decreasing = T),]
print("wagi poszczeg�lnych zmiennych", quote = F)
print(as.data.frame(waga1))


model1_p<-predict(model1, newdata=data.test)
mse_model1<-var(data.test$y-model1_p)
print("Blad MSE modelu 1 dla danych testowych:",quote = F)
print(mse_model1)
print("Blad MSE modelu 1 dla danych treningowych:",quote = F)
print(model1$mse[n])


#######################################################################################################################################################

# MODel2
#W tym modelu podzieli�am dane na kolejne dziesi�tki:
#je�li odpowied� by�a 0 to przydzielono warto�� 0
#je�li odpowied� by�a w przedziele 1-10 - to przydzielona warto�� 1
#je�li odpowied� by�a w przedziale 11-20 - to przydzielona warto�� 2
#je�li odpowied� by�a w przedziale 21-30 - to przydzielona warto�� 3
#je�li odpowied� by�a w przedziale 31-40 - to przydzielona warto�� 4
#je�li odpowied� by�a w przedziale 41-50 - to przydzielona warto�� 5

###############
#import i obr�bka
data <- read.csv("C:/Users/patsz/OneDrive/Pulpit/praca dyplomowa/ERKa/p2.csv")
names <- c("time","Mezczyzna", "wiek", "wojewodztwo", "w_miejscowosc", "drugi_kierunek", "publiczna", "dziedzina_studiow",
           "zaoczne", "nie_pracuje", "forma_ksiazki", "pochodzenie_ksiazki", "dzial_ksiazki", "powod", "czym_wybor", "kiedy","nowosci_nie","netflix_nie","lubie_czytac","pandemia_wiecej_ksiazek","wiecej_czasu","za_malo_czytam","zrodlo_wiedzy","dobry_prezent","y", "komentarze","dziesiatki","przedzialy_5")
names(data) <- names

data<- data[,-26]#usuniecie komentarzy do ankiety
data<- data[,-1]#usuniecie sygnatury czasowej
data<- data[,-26]#usunicie kolumn wykorzystywanych przy modelach 1 i 3
data<- data[,-24]#usunicie kolumn wykorzystywanych przy modelach 1 i 3

i = sample(2, nrow(data), replace = TRUE, prob = c(a, b))

#na binarne
data$Mezczyzna[data$Mezczyzna=="Kobieta"] <- 0 
data$Mezczyzna[data$Mezczyzna!=0] <- 1
data$Mezczyzna <- as.numeric(data$Mezczyzna)
data.train <- data[i==1,]
data.test <- data[i==2,]
# praca
data$nie_pracuje[data$nie_pracuje=="tak, pracuje"] <- 0 
data$nie_pracuje[data$nie_pracuje!=0] <- 1
data$nie_pracuje <- as.numeric(data$nie_pracuje)
data.train <- data[i==1,]
data.test <- data[i==2,]
#dwa kierunki
data$drugi_kierunek[data$drugi_kierunek=="nie, studiuje jeden kierunek"] <- 0 
data$drugi_kierunek[data$drugi_kierunek!=0] <- 1
data$drugi_kierunek <- as.numeric(data$drugi_kierunek)
data.train <- data[i==1,]
data.test <- data[i==2,]
#rodzaj uczelni
data$publiczna[data$publiczna=="Uczelnia prywatna"] <- 0 
data$publiczna[data$publiczna!=0] <- 1
data$publiczna <- as.numeric(data$publiczna)
data.train <- data[i==1,]
data.test <- data[i==2,]
#tryb studiowania
data$zaoczne[data$zaoczne=="dziennie"] <- 0 
data$zaoczne[data$zaoczne!=0] <- 1
data$zaoczne <- as.numeric(data$zaoczne)
data.train <- data[i==1,]
data.test <- data[i==2,]
#newsy
data$nowosci_nie[data$nowosci_nie=="tak"] <- 0 
data$nowosci_nie[data$nowosci_nie!=0] <- 1
data$nowosci_nie <- as.numeric(data$nowosci_nie)
data.train <- data[i==1,]
data.test <- data[i==2,]
#streaming
data$netflix_nie[data$netflix_nie=="tak, mam uzywam takich platform"] <- 0 
data$netflix_nie[data$netflix_nie!=0] <- 1
data$netflix_nie <- as.numeric(data$netflix_nie)
data.train <- data[i==1,]
data.test <- data[i==2,]




data[,24]<-as.factor(data[,24])

########
model2<-randomForest(formula = dziesiatki ~ .,data=data.train,ntree= n,mtry=m,do.trace=10, keep.forest =T)
print(model2)

plot(model2, main=NULL)#wykres bledu
print("Wsp�lczynnik determinacji:", quote = F)
print(model2$rsq[n])
#wagi zmiennych
wz2<- round(importance(model2),2)
waga2<- wz2[order(wz2,decreasing = T),]
print("wagi poszczeg�lnych zmiennych", quote = F)
print(as.data.frame(waga2))

model2_p<-predict(model2, newdata=data.test)
mse_model2<-var(data.test$dziesiatki-model2_p)
print("Blad MSE modelu 2 dla danych testowych:",quote = F)
print(mse_model2)
print("Blad MSE modelu 2 dla danych treningowych:",quote = F)
print(model2$mse[n])


###############################################################################################################################
# Model3 
# W tym modelu podzieli�am dane na 5 zbior�w, stara�am si� �eby by�y podobnie liczne
#1- od 0 do 3
#2- od 4 do 8 ksi��ek
#3- od 9 do 14 ksi��ek
#4- 15 do 20 ksi�zek
#5- od 21 ksi��ek

###############
#import i obr�bka
data <- read.csv("C:/Users/patsz/OneDrive/Pulpit/praca dyplomowa/ERKa/p2.csv")
names <- c("time","Mezczyzna", "wiek", "wojewodztwo", "w_miejscowosc", "drugi_kierunek", "publiczna", "dziedzina_studiow",
           "zaoczne", "nie_pracuje", "forma_ksiazki", "pochodzenie_ksiazki", "dzial_ksiazki", "powod", "czym_wybor", "kiedy","nowosci_nie","netflix_nie","lubie_czytac","pandemia_wiecej_ksiazek","wiecej_czasu","za_malo_czytam","zrodlo_wiedzy","dobry_prezent","y", "komentarze","dziesiatki","przedzialy_5")
names(data) <- names

data<- data[,-26]#usuniecie komentarzy do ankiety
data<- data[,-1]#usuniecie sygnatury czasowej
data<- data[,-24]#usunicie kolumn wykorzystywanych przy modelach 1 i 2
data<- data[,-24]#usunicie kolumn wykorzystywanych przy modelach 1 i 2

i = sample(2, nrow(data), replace = TRUE, prob = c(a, b))

#na binarne
data$Mezczyzna[data$Mezczyzna=="Kobieta"] <- 0 
data$Mezczyzna[data$Mezczyzna!=0] <- 1
data$Mezczyzna <- as.numeric(data$Mezczyzna)
data.train <- data[i==1,]
data.test <- data[i==2,]
# praca
data$nie_pracuje[data$nie_pracuje=="tak, pracuje"] <- 0 
data$nie_pracuje[data$nie_pracuje!=0] <- 1
data$nie_pracuje <- as.numeric(data$nie_pracuje)
data.train <- data[i==1,]
data.test <- data[i==2,]
#dwa kierunki
data$drugi_kierunek[data$drugi_kierunek=="nie, studiuje jeden kierunek"] <- 0 
data$drugi_kierunek[data$drugi_kierunek!=0] <- 1
data$drugi_kierunek <- as.numeric(data$drugi_kierunek)
data.train <- data[i==1,]
data.test <- data[i==2,]
#rodzaj uczelni
data$publiczna[data$publiczna=="Uczelnia prywatna"] <- 0 
data$publiczna[data$publiczna!=0] <- 1
data$publiczna <- as.numeric(data$publiczna)
data.train <- data[i==1,]
data.test <- data[i==2,]
#tryb studiowania
data$zaoczne[data$zaoczne=="dziennie"] <- 0 
data$zaoczne[data$zaoczne!=0] <- 1
data$zaoczne <- as.numeric(data$zaoczne)
data.train <- data[i==1,]
data.test <- data[i==2,]
#newsy
data$nowosci_nie[data$nowosci_nie=="tak"] <- 0 
data$nowosci_nie[data$nowosci_nie!=0] <- 1
data$nowosci_nie <- as.numeric(data$nowosci_nie)
data.train <- data[i==1,]
data.test <- data[i==2,]
#streaming
data$netflix_nie[data$netflix_nie=="tak, mam uzywam takich platform"] <- 0 
data$netflix_nie[data$netflix_nie!=0] <- 1
data$netflix_nie <- as.numeric(data$netflix_nie)
data.train <- data[i==1,]
data.test <- data[i==2,]


data[,24]<-as.factor(data[,24])
########
model3<-randomForest(formula = przedzialy_5 ~ .,data=data.train,ntree= n,mtry=m,do.trace=10, keep.forest =T)
print(model3)

plot(model3, main=NULL) #wykres bledu MSE
print("Wsp�lczynnik determinacji:", quote = F)
print(model3$rsq[n])
#wagi zmiennych
wz3<- round(importance(model3),2)
waga3<- wz3[order(wz3,decreasing = T),]
print("wagi poszczeg�lnych zmiennych", quote = F)
print(as.data.frame(waga3))

model3_p<-predict(model3, newdata=data.test)
mse_model3<-var(data.test$przedzialy_5-model3_p)
print("Blad MSE modelu 3 dla danych testowych:",quote = F)
print(mse_model3)
print("Blad MSE modelu 3 dla danych treningowych:",quote = F)
print(model3$mse[n])


#############
#Bledy rmse
rmse1<- sqrt(model1$mse[n])
rmse2<- sqrt(model2$mse[n])
rmse3<- sqrt(model3$mse[n])
rmse1
rmse2
rmse3


# number of trees with lowest MSE
lm1<-which.min(model1$mse)
lm2<-which.min(model2$mse)
lm3<-which.min(model3$mse)
lm1
lm2
lm3
#wszystkie 3 modele wspolczynniki determinacji
print(model1$rsq[n])

print(model2$rsq[n])

print(model3$rsq[n])


# Modele s� zasadniczo ok, ich jako�� nie jest powalaj�ca, bo dane s� wymagaj�ce/trudne do modelowania. 






