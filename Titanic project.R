---
  title: "PROJET TITANIC"
author: "TIMOTHE ET MAXENCE"
date: "22 avril 2021"
output: html_document
---
  
#QUESTION1
install.packages("tidyverse")
install.packages("dplyr")
install.packages("Hmisc")
install.packages("corrplot")
library(Hmisc)
library(dplyr)
library(ggplot2)
library(corrplot)

read.csv("titanic_train.Rdata")


#QUESTION 2
#A) 

dim(train)
#il y a 594 observations et 12 variables

#B) 

VALEUR = c(for(j in train) if(class(j) == "numeric") print("quantitatives number") else print("Qualitatives character"))


#C)

num_NA <- train %>% summarise(compte_NA = nchar( train[is.na(train)] ))
print(paste("le nombre de valeurs manquantes est de ", count(num_NA)))

colSums(is.na(train))


# Ils manquent au total 585 données dont 463 proviennent des Cabines et 121 de la variable Age et 1 valeur manquante de la variable Embarked.


#QUESTION 3
#   La variable S décrit si le passager a survécu ou pas. Il s'agit d'une variable continue discrète.
#   La variable Sx décrit le sexe du passager. Il s'agit du'une variable qualitative.
#   La variable P correspond à la classe du passager. Il s'agit d'une variable quantitative prenant des valeurs entre 1 et 3.
#   La variable A décrit l'age du passager. Il s'agit d'une variable quantitative.


#QUESTION 4
cAge = cut(train$Age, c(0,20,40,60,80), c("0-20ans","21-40ans","41-60ans","61-80ans"))
table(cAge)
# Cette variable range les âges des passager entre 0 et 80 ans par tranches de 20 années. On remarque qu'il y a 118 passsagés entre 0 et 20ans, 255 passagés entre 20 et 40ans, 89 passagés entre 40 et 60ans, et 11 passagés entre 60 et 80ans.


#QUESTION 5

#A)
table(train$Sex,train$Survived)
# Sur 594 passager on remarque que 152 femmes ont survécu contre 69 pour les hommes et que 49 femmes et 324 hommes sont morts.


#B)
table(train$Pclass,train$Survived)
# Nous remarquons qu'il y a beaucoup de morts en 3eme classe (257) tandis qu'en premiere beaucoup moins (48). En deuxieme classe cela reste mitigé.


#C)
ggplot(train[1:891,], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  xlab("Sex") +
  ylab("Nombre") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Relation entre le Sex et les survivant")
# Nous remarquons qu'il y a beaucoup plus d'homme qui n'ont pas survécu que les femmes


#D)
table(cAge, train$Survived)

#Il y a 160 personnes qui avaient entre 21 et 40ans qui n'ont pas survécu et seulement 1 personne de plus de 61 ans qui a survécu.

#correlation entre les variables
install.packages("corrplot")
library(corrplot)
mat= corrplot(train[,c("PassengerId","Survived","Pclass","Age","SibSp","Parch","Fare")], ,use="complete")
corrplot(mat)



#QUESTION 6

#résultats et hypothèses de variables Pclass et Survived

ggplot(train[1:891,], aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+
  xlab("Pclass") +
  facet_grid(.~Sex)+
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Pclass vs Sex vs Survived")

table(train$Pclass)
table(train$Survived)
table(train$Sex, train$Survived)
table(train$Survived,cAge, train$Sex, train$Pclass)

# Suivant la classe on constate que en 3éme les morts sont considérable avec 257 morts sur 331 passagés en 3éme class tandis que sur 139 passagés en 1ére classe 48 sont morts et 91 ont survécus. On constate que les passagés en 1ére classe ont étaient sauvés en priorités.

#La 2nd class est trés homogènes avec 124 passsagés dont 68 morts et 56 réscapés. En conclusion sur


#QUESTION 7

#La probabilité qu'une femme survive

TSVF = count(filter(train,Survived == 1, Sex == "female"))
print(TSV)
TFP = count(filter(train, Sex == "female"))
print(FSV)

P_F_S = TSVF / TFP
print(paste("La probabilité qu'une femme survive est : " ,P_F_S))

#La probabilités qu'un homme survive

THP = count(filter(train, Sex == "male"))
print(THP)
THSP = count(filter(train, Survived == 1, Sex == "male"))

P_H_S = THSP / THP 
print(paste("La probabilités qu'un homme survive est : ", P_H_S))

#La probabilité qu'une personne survive en 1ére classe

TC1 = count(filter(train, Pclass == 1))
print(TC1)

C1S= count(filter(train, Survived == 1 , Pclass == 1))
print(C1)

P_P_1 = C1S / TC1
print(paste( "La probabilité qu'une personne survive en 1ére classe est : ",  P_P_1))

#La probabilité qu'une personne survive en 2éme classe

TC2 = count(filter(train, Pclass == 2))
print(TC1)

C2S= count(filter(train, Survived == 1 , Pclass == 2))
print(C1)

P_P_2 = C2S / TC2
print(paste("La probabilité qu'une personne survive en 2éme classe est " , P_P_2))


#La probabilité qu'une personne survive en 3éme classe

TC3 = count(filter(train, Pclass == 3))
print(TC1)

C3S= count(filter(train, Survived == 1 , Pclass == 3))
print(C1)

P_P_3 = C3S / TC3
print(paste("La probabilité qu'une personne survive en 3éme classe est " , P_P_3))

#La probabilité qu'une personne de moins de 20ans survive
A2 = count(filter(train, Age <= 20))
print(A2)

A3 = count(filter(train, Survived == 1, Age <=20))
print(A3)

P_P_20 = A3 / A2
print(paste("La probabilité qu'une personne de moins de 20ans survive est : ", P_P_20))

#"La probabilité qu'une personne entre 20 et 40ans survive 

A4 = count(filter(train, Age >= 20 & Age <= 40))
print(A4)

A5 = count(filter(train, Survived == 1, Age >= 20 & Age <= 40))
print(A5)

P_P_20_40 = A5 / A4
print(paste("La probabilité qu'une personne entre 20 et 40ans survive est : ", P_P_20_40))


#La probabilité qu'une personne entre 20 et 40ans survive

A6 = count(filter(train, Age >= 40 & Age <= 60))
print(A6)

A7 = count(filter(train, Survived == 1, Age >= 40 & Age <= 60))
print(A7)

P_P_40_60 = A7 / A6
print(paste("La probabilité qu'une personne entre 40ans et 60ans survive est : ", P_P_40_60))


#La probabilité qu'une personne entre 60ans et 80ans survive 

A8 = count(filter(train, Age >= 60 & Age <= 80))
print(A8)

A9 = count(filter(train, Survived == 1, Age >=60 & Age <= 80))
print(A9)

P_P_60_80 = A9 / A8
print(paste("La probabilité qu'une personne entre 60ans et 80ans survive est : ", P_P_60_80))


#QUESTION 8 

(S_P <- prop.table(table(train$Pclass, train$Survived), margin=2))
rownames(S_P) <- c('1','2', '3')
colnames(S_P) <- c('0','1')
print(S_P)

(S_sx = prop.table(table(train$Sex, train$Survived), margin = 2))
rownames(S_sx) <- c('female','male')
colnames(S_sx) <- c('0','1')
print(S_sx)

(S_Ca = prop.table(table(cAge, train$Sex), margin = 2))
rownames(S_Ca) <- c('0-20','21-40', '41-60','61-80')
colnames(S_Ca) <- c('female','male')
print(S_Ca)

(S <- prop.table(table(train$Survived)))
print(S)

#QUESTION 9

prob_prediction = function(sex, pclass, cAge){
  P_sx = train %>% filter( Survived == 1 & Sex == sex) %>% summarise( nb = sum(Survived) )
  sx = train %>% filter(Sex == sex)
  sexe = summarise(train, nb_totale = count(sx))
  
  P_sx_s_1 = P_sx / sexe
  
  sum = 0
  
  for(i in 1){
    
    S_1 = train %>% filter( Survived == i ) %>% summarise( nb = sum(Survived) )
    sum = sum + S_1
    
  }
  
  if( cAge > 0 & 20 <= cAge){
    P_CA_S_1 = train %>% filter( Survived == 1,Age >  cAge & cAge <= Age ) %>% summarise( nb = sum(Survived) )
    S_1 = train%>% filter( Survived == 1 ) %>% summarise( nb = sum(Survived) )
    result = (P_sx_s_1 * S_P[pclass, '1'] * P_CA_S_1 * S_1) / sum
    
  }
  else if (cAge > 20 & 40 <= cAge) {
    P_CA_S_1 = train %>% filter( Survived == 1 & Age > cAge & cAge <= Age ) %>% summarise( nb = sum(Survived) )
    S_1 = train %>% filter( Survived == 1 ) %>% summarise( nb = sum(Survived) )
    result = (P_sx_s_1 * S_P[pclass, '1'] * P_CA_S_1 * S_1) / sum
  }
  else if (cAge > 40 & 60 <= cAge) {
    P_CA_S_1 = train %>% filter( Survived == 1 & Age > cAge & cAge <= Age ) %>% summarise( nb = sum(Survived) )
    S_1 = train %>% filter( Survived == 1 ) %>% summarise( nb = sum(Survived) )
    result = (P_sx_s_1 * S_P[pclass, '1'] * P_CA_S_1 * S_1) / sum
  }
  else if (cAge > 60 & 80 <= cAge) {
    P_CA_S_1 = train %>% filter( Survived == 1 & Age > cAge & cAge <= Age ) %>% summarise( nb = sum(Survived) )
    S_1 = train %>% filter( Survived == 1 ) %>% summarise( nb = sum(Survived) )
    result = (P_sx_s_1 * S_P[pclass, '1'] * P_CA_S_1 * S_1) / sum
  }
  else{
    result = "Error : "
  }
  
  return(result/100)
  
}