#Regréssion linéaire multiple
#*******************************************************************************
library(readxl)
Decathlon <- read_excel("C:/Users/hp/Desktop/Decathlon.xlsx")
View(Decathlon)
summary(Decathlon)
#*******************************************************************************
##le modèle de régression linéaire multiple incluant toute les variables explicatives
myregmult <- lm(Points~.,data=Decathlon)
summary(myregmult) #Résidu c'est l'erreur observé
#Intervalles de confiance des variables explicatives
confint(myregmult)
## Les intervalles de confiances des variables significatives ne contiennent pas le 0.
#*******************************************************************************
#Amélioration du modèle
step(myregmult)
myregmult1<-lm(Points~Longueur + Poids + `110m.haies` + Perche + Javelot + `1500m` + Disque, data = Decathlon)
summary(myregmult1)
#*******************************************************************************
## Validation de modèle par procédure step :
## test d'homoscédasticité
plot(predict(myregmult1),resid(myregmult1), xlab="valeur ajusté", ylab="résidu")
abline(h=c(600,0,600), lty=c(2,1,2), col=c(1,2,1))
## On remarque une abscence de structure conique, donc on accepte l'hypothèse nulle d'homoscédasticité des résidus.
## test de normalité (shapiro et ks)
## SHAPIRO
shapiro.test(resid(myregmult1)) ## p-value = 0.4448 > 0.05 Donc On ne rejette pas la normalité
## KS
ks.test(resid(myregmult1), pnorm) ## p-value = 5.251e-13 < 0.05 Donc on rejette la normalité
## recherche de valeurs aberrantes
sd=sqrt(deviance(myregmult1)/df.residual(myregmult1))  ## sd = 262.5128
x0=rep(1,55)
M=matrix(c(x0,Decathlon$Javelot,Decathlon$`1500m`,Decathlon$`110m.haies`,Decathlon$Poids,Decathlon$Perche,Decathlon$Disque,Decathlon$Longueur),55,8)
ul=matrix(0,55,1)
ll=matrix(0,55,1)
m=matrix(0,55,4)
j=1
for (i in 1:55) {
  ##  l'abscisse de la loi de Student à n-p-1 degré de libertés. Dans notre cas n=55 et p=7. Donc ddl = 7.
  # Le calcul donne : 2,0117 
  ul[i]=predict(myregmult1)[i]+2.0117*262.5128*sqrt(1+M[i,]%*%solve(t(M)%*%M)%*%M[i,])
  ll[i]=predict(myregmult1)[i]-2.0117*262.5128*sqrt(1+M[i,]%*%solve(t(M)%*%M)%*%M[i,])
  if (Decathlon$Points[i]>ul[i] | Decathlon$Points[i]<ll[i]) {
    m[j,]<-c(i,Decathlon$Points[i],ul[i],ll[i])
    j<-j+1 
  }                 
}
View(m)
seqx=seq(1,55,length=55)
sd=sqrt(deviance(myregmult1)/df.residual(myregmult1))
sd
abr=abs(Decathlon$Points-predict(myregmult1))/262.5128
plot(seqx,abr)
abline(h=2, lty=2,col=2)
#*******************************************************************************
#3A reg multiple
#Procédure de séléction Fisher
#*******************************************************************************
#Etape1:
Fish = rep(0,11)
for (i in  2:ncol(Decathlon) ) {
  mod1<-lm(Points~as.matrix(Decathlon[,i]), data=Decathlon)
  Fish[i]=var(predict(mod1))*(nrow(Decathlon)-1)/(deviance(mod1)/df.residual(mod1))
}
Fish  #on remarque que la plus grande valeur c'est 21.142459 qui correspond à 
#la variable "Longueur"
df2=nrow(Decathlon)-2
df2
1-pf(max(Fish),1,df2) #C'est égale à 2.683854e-05<max(Fish) donc il est significatif
## Introduction de la variable Longeur
summary(mod1)
#********************************************************************************
#Etape 2: -Inroduction- (on va introduire la variable Longueur)
Fish = rep(0,11)
SCR1<-deviance(lm(Points~Longueur, data=Decathlon))
for (i in 3:11) {
  mod<-lm(Points~Longueur+as.matrix(Decathlon[,i]),data=Decathlon)
  SCR2=deviance(mod)
  Fish[i]=(SCR1-SCR2)/(SCR2/(nrow(Decathlon)-3))
}
Fish #on remarque que la plus grande valeur c'est 17.00722446 qui correspond à 
#la variable "Disque"
df2=nrow(Decathlon)-3
df2
1-pf(max(Fish),1,df2) #c'est égale à 0.0001345579 < max(Fish) donc c'est significatif 
##Introduction de la variable Disque
summary(mod)
#*******************************************************************************
#Etape 2: -Retrait-
Fish = rep(0,2)
SCR2=deviance(lm(Points~Longueur+Disque, data=Decathlon))
mod<-lm(Points~Longueur, data=Decathlon)
SCR1<-deviance(mod)
Fish[1]=(SCR1-SCR2)/(SCR2/(nrow(Decathlon)-3))

mod<-lm(Points~Disque, data=Decathlon)
SCR1=deviance(mod)
Fish[2]=(SCR1-SCR2)/(SCR2/(nrow(Decathlon)-3))
Fish #on remarque que les 2 Fishers sont grands alors ils sont significatifs
df2=nrow(Decathlon)-3
df2
1-pf(min(Fish),1,df2) #C'est égale à 0.0001345579 donc il est significatif
## Aucune variable n'est retirée, les F sont significatifs
#*******************************************************************************
##### Etape 3 -Introduction- (on va introduire la variable Disque)
Fish = rep(0,11)
SCR2<-deviance(lm(Points~Longueur+Disque, data=Decathlon))
for (i in 3:10) {
  mod<-lm(Points~Longueur+Disque+as.matrix(Decathlon[,i]),data=Decathlon)
  SCR3=deviance(mod)
  Fish[i]=(SCR2-SCR3)/(SCR3/(nrow(Decathlon)-4))
}
Fish #on remarque que la plus grande valeur c'est 10.4508742 qui correspond à 
#la variable "Poids"
df2=nrow(Decathlon)-4
df2
1-pf(max(Fish),1,df2) #c'est égale à 0.002151312 < max(Fish) donc c'est significatif 
##Introduction de la variable Poids
summary(mod)
#*******************************************************************************
##### Etape 3: "Retrait"
SCR3<-deviance(lm(Points~Longueur+Poids+Disque, data=Decathlon))
Fish<-rep(0,3)
mod<-lm(Points~Longueur+Poids, data=Decathlon)
SCR2<-deviance(mod)
Fish[1]=(SCR2-SCR3)/(SCR3/(nrow(Decathlon)-4))

mod<-lm(Points~Longueur+Disque, data=Decathlon)
SCR2<-deviance(mod)
Fish[2]=(SCR2-SCR3)/(SCR3/(nrow(Decathlon)-4))

mod<-lm(Points~Poids+Disque, data=Decathlon)
SCR2<-deviance(mod)
Fish[3]=(SCR2-SCR3)/(SCR3/(nrow(Decathlon)-4))

Fish #on remarque que les 3 Fishers sont grands alors ils sont significatifs
df2=nrow(Decathlon)-4
df2
1-pf(min(Fish),1,df2) #C'est égale à 0.002151312 alors il est significatif
## Aucune variable n'est retitée
#*******************************************************************************
##### Etape 4 -Introduction- (on va introduire la variable Poids)
Fish<-rep(0,11)
SCR3<-deviance(lm(Points~Longueur+Poids+Disque, data=Decathlon))
mod<-lm(Points~Longueur+Poids+Disque+`100m`, data=Decathlon)
SCR4<-deviance(mod)
Fish[3]=(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Longueur+Poids+Disque+Hauteur, data=Decathlon)
SCR4<-deviance(mod)
Fish[5]=(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Longueur+Poids+Disque+`400m`, data=Decathlon)
SCR4<-deviance(mod)
Fish[6]=(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Longueur+Poids+Disque+`110m.haies`, data=Decathlon)
SCR4<-deviance(mod)
Fish[7]=(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Longueur+Poids+Disque+Perche, data=Decathlon)
SCR4<-deviance(mod)
Fish[8]=(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Longueur+Poids+Disque+Javelot, data=Decathlon)
SCR4<-deviance(mod)
Fish[9]=(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Longueur+Poids+Disque+`1500m`, data=Decathlon)
SCR4<-deviance(mod)
Fish[10]=(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

Fish #on remarque que la plus grande valeur c'est 9.820248599 qui correspond à 
#la variable "Perche"
df2=nrow(Decathlon)-5
df2
1-pf(max(Fish),1,df2) #c'est égale à 0.002885973 < max(Fish) donc c'est significatif 
##Introduction de la variables "Perche"
summary(mod)
#*******************************************************************************
#### Etape 4 -Retrait-
Fish = rep(0,4)
SCR4<-deviance(lm(Points~Longueur+Disque+Poids+Perche,data=Decathlon))
mod<-lm(Points~Longueur+Disque+Poids, data=Decathlon)
SCR3<-deviance(mod)
Fish[1]<-(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Longueur+Perche+Disque, data=Decathlon)
SCR3<-deviance(mod)
Fish[2]<-(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Longueur+Poids+Perche, data=Decathlon)
SCR3<-deviance(mod)
Fish[3]<-(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

mod<-lm(Points~Perche+Poids+Disque, data=Decathlon)
SCR3<-deviance(mod)
Fish[4]<-(SCR3-SCR4)/(SCR4/(nrow(Decathlon)-5))

Fish #on remarque que les 4 Fishers sont grands alors ils sont significatifs
df2=nrow(Decathlon)-5 
df2
1-pf(min(Fish),1,df2) #C'est égale à 0.002885973 alors il est significatif
## Aucune variable n'est retitée
#********************************************************************************
##### Etape 5 -Introduction- (On va introduire la variable Perche)
Fish = rep(0,11)
SCR4<-deviance(lm(Points~Longueur+Perche+Poids+Disque, data=Decathlon))
mod<-lm(Points~Longueur+Perche+Poids+Disque+`100m`, data=Decathlon)
SCR5=deviance(mod)
Fish[3]=(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Longueur+Perche+Poids+Disque+Hauteur, data=Decathlon)
SCR5=deviance(mod)
Fish[5]=(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`400m`, data=Decathlon)
SCR5=deviance(mod)
Fish[6]=(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`, data=Decathlon)
SCR5=deviance(mod)
Fish[7]=(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Longueur+Perche+Poids+Disque+Javelot, data=Decathlon)
SCR5=deviance(mod)
Fish[9]=(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`1500m`, data=Decathlon)
SCR5=deviance(mod)
Fish[10]=(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

Fish #on remarque que la plus grande valeur c'est 5.06510628 qui correspond à 
#la variable "110m.haies"
df2=nrow(Decathlon)-6
df2
1-pf(max(Fish),1,df2) #c'est égale à 0.02893396 < max(Fish) donc c'est significatif 
##Introduction de deux variables "110m.haies"
summary(mod)
#*******************************************************************************
###### Etape 5 -Retrait-
Fish<-rep(0,5)
SCR5<-deviance(lm(Points~Longueur+Disque+Poids+Perche+`110m.haies`,data=Decathlon))
mod<-lm(Points~Longueur+Perche+Poids+Disque, data=Decathlon)
SCR4<-deviance(mod)
Fish[1]<-(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Longueur+Perche+Disque+`110m.haies`, data=Decathlon)
SCR4<-deviance(mod)
Fish[2]<-(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Longueur+Poids+Disque+`110m.haies`, data=Decathlon)
SCR4<-deviance(mod)
Fish[3]<-(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Perche+Poids+Disque+`110m.haies`, data=Decathlon)
SCR4<-deviance(mod)
Fish[4]<-(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

mod<-lm(Points~Perche+Poids+Longueur+`110m.haies`, data=Decathlon)
SCR4<-deviance(mod)
Fish[5]<-(SCR4-SCR5)/(SCR5/(nrow(Decathlon)-6))

Fish #on remarque que les 5 Fishers sont grands alors ils sont significatifs
df2=nrow(Decathlon)-6 
df2
1-pf(min(Fish),1,df2) #C'est égale à 0.02893396 alors il est significatif
## Aucune variable n'est retitée
#*******************************************************************************
##### Etape 6 -Introduction- (110m.haies)
Fish = rep(0,11)
SCR5<-deviance(lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`, data=Decathlon))
mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+`100m`, data=Decathlon)
SCR6=deviance(mod)
Fish[3]=(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+Hauteur, data=Decathlon)
SCR6=deviance(mod)
Fish[5]=(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+`400m`, data=Decathlon)
SCR6=deviance(mod)
Fish[6]=(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+Javelot, data=Decathlon)
SCR6=deviance(mod)
Fish[9]=(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+`1500m`, data=Decathlon)
SCR6=deviance(mod)
Fish[10]=(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

Fish #on remarque que la plus grande valeur c'est 2.74780466 qui correspond à 
#la variable "1500m"
df2=nrow(Decathlon)-7
df2
1-pf(max(Fish),1,df2) #c'est égale à 0.1039105 < max(Fish) donc c'est significatif 
##Introduction de la variables "1500m"
summary(mod)
#*******************************************************************************
###### Etape 6 -Retrait-
Fish<-rep(0,6)
SCR6<-deviance(lm(Points~Longueur+Disque+Poids+Perche+`110m.haies`+`1500m`,data=Decathlon))
mod<-lm(Points~Longueur+Disque+Poids+Perche+`110m.haies`, data=Decathlon)
SCR5<-deviance(mod)
Fish[1]<-(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Longueur+Disque+Poids+Perche+`1500m`, data=Decathlon)
SCR5<-deviance(mod)
Fish[2]<-(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Disque+Poids+Perche+`110m.haies`+`1500m`, data=Decathlon)
SCR5<-deviance(mod)
Fish[3]<-(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Longueur+Disque+Perche+`110m.haies`+`1500m`, data=Decathlon)
SCR5<-deviance(mod)
Fish[4]<-(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Longueur+Poids+Perche+`110m.haies`+`1500m`, data=Decathlon)
SCR5<-deviance(mod)
Fish[5]<-(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

mod<-lm(Points~Disque+Longueur+Poids+`110m.haies`+`1500m`, data=Decathlon)
SCR5<-deviance(mod)
Fish[6]<-(SCR5-SCR6)/(SCR6/(nrow(Decathlon)-7))

Fish 
df2=nrow(Decathlon)-7 
df2
1-pf(min(Fish),1,df2) #C'est égale à 0.1039105> 0.10 
## On retire la variable "1500m"
#*******************************************************************************
##### Etape 7 -Introduction- 
Fish = rep(0,11)
SCR6<-deviance(lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`, data=Decathlon))
mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+`100m`, data=Decathlon)
SCR7=deviance(mod)
Fish[3]=(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+Hauteur, data=Decathlon)
SCR7=deviance(mod)
Fish[5]=(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+`400m`, data=Decathlon)
SCR7=deviance(mod)
Fish[6]=(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+Javelot, data=Decathlon)
SCR7=deviance(mod)
Fish[9]=(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

Fish #on remarque que la plus grande valeur c'est 1.449006165 qui correspond à 
#la variable "Javelot"
df2=nrow(Decathlon)-8
df2
1-pf(max(Fish),1,df2) #c'est égale à 0.2347153 < max(Fish) donc c'est significatif 
##Introduction de la variables "Javelot"
summary(mod)
#*******************************************************************************
###### Etape 7 -Retrait-
Fish<-rep(0,6)
SCR7<-deviance(lm(Points~Longueur+Disque+Poids+Perche+`110m.haies`+Javelot,data=Decathlon))
mod<-lm(Points~Longueur+Disque+Poids+Perche+`110m.haies`, data=Decathlon)
SCR6<-deviance(mod)
Fish[1]<-(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

mod<-lm(Points~Longueur+Disque+Poids+Perche+Javelot, data=Decathlon)
SCR6<-deviance(mod)
Fish[2]<-(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

mod<-lm(Points~Disque+Poids+Perche+`110m.haies`+Javelot, data=Decathlon)
SCR6<-deviance(mod)
Fish[3]<-(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

mod<-lm(Points~Longueur+Disque+Perche+`110m.haies`+Javelot, data=Decathlon)
SCR6<-deviance(mod)
Fish[4]<-(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

mod<-lm(Points~Longueur+Poids+Perche+`110m.haies`+Javelot, data=Decathlon)
SCR6<-deviance(mod)
Fish[5]<-(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

mod<-lm(Points~Disque+Longueur+Poids+`110m.haies`+Javelot, data=Decathlon)
SCR6<-deviance(mod)
Fish[6]<-(SCR6-SCR7)/(SCR7/(nrow(Decathlon)-8))

Fish 
df2=nrow(Decathlon)-8 
df2
1-pf(min(Fish),1,df2) #C'est égale à 0.2347153 > 0.10 
## On retire la variable "Javelot"
#*******************************************************************************
##### Etape 8 -Introduction- 
Fish = rep(0,11)
SCR7<-deviance(lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`, data=Decathlon))
mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+`100m`, data=Decathlon)
SCR8=deviance(mod)
Fish[3]=(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+Hauteur, data=Decathlon)
SCR8=deviance(mod)
Fish[5]=(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+`400m`, data=Decathlon)
SCR8=deviance(mod)
Fish[6]=(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))


Fish #on remarque que la plus grande valeur c'est 1.057337021 qui correspond à 
#la variable "100m"
df2=nrow(Decathlon)-9
df2
1-pf(max(Fish),1,df2) #c'est égale à 0.3092001<max(Fish) donc c'est significatif 
##Introduction de la variables "100m"
summary(mod)
#*******************************************************************************
###### Etape 8 -Retrait-
Fish<-rep(0,6)
SCR8<-deviance(lm(Points~Longueur+Disque+Poids+Perche+`110m.haies`+`100m`,data=Decathlon))
mod<-lm(Points~Longueur+Disque+Poids+Perche+`110m.haies`, data=Decathlon)
SCR7<-deviance(mod)
Fish[1]<-(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))

mod<-lm(Points~Longueur+Disque+Poids+Perche+`100m`, data=Decathlon)
SCR7<-deviance(mod)
Fish[2]<-(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))

mod<-lm(Points~Disque+Poids+Perche+`110m.haies`+`100m`, data=Decathlon)
SCR7<-deviance(mod)
Fish[3]<-(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))

mod<-lm(Points~Longueur+Disque+Perche+`110m.haies`+`100m`, data=Decathlon)
SCR7<-deviance(mod)
Fish[4]<-(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))

mod<-lm(Points~Longueur+Poids+Perche+`110m.haies`+`100m`, data=Decathlon)
SCR7<-deviance(mod)
Fish[5]<-(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))

mod<-lm(Points~Disque+Longueur+Poids+`110m.haies`+`100m`, data=Decathlon)
SCR7<-deviance(mod)
Fish[6]<-(SCR7-SCR8)/(SCR8/(nrow(Decathlon)-9))

Fish 
df2=nrow(Decathlon)-9 
df2
1-pf(min(Fish),1,df2) #C'est égale à 0.3092001 > 0.10 
## On retire la variable "100m"
#*******************************************************************************
##### Etape 9 -Introduction- 
Fish = rep(0,11)
SCR8<-deviance(lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`, data=Decathlon))
mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+Hauteur, data=Decathlon)
SCR9=deviance(mod)
Fish[5]=(SCR8-SCR9)/(SCR9/(nrow(Decathlon)-10))

mod<-lm(Points~Longueur+Perche+Poids+Disque+`110m.haies`+`400m`, data=Decathlon)
SCR9=deviance(mod)
Fish[6]=(SCR8-SCR9)/(SCR8/(nrow(Decathlon)-10))


Fish #on remarque que la plus grande valeur c'est 0.453655856 qui correspond à 
#la variable "100m"
df2=nrow(Decathlon)-10
df2
1-pf(max(Fish),1,df2) #c'est égale à 0.5040489 > max(Fish) donc ce n'est pas
#significatif 
##On ne doit pas introduire la variable
#******************************************************************************
myregmult2<-lm(Points~Longueur + Poids + `110m.haies` + Perche + Javelot + Disque, data = Decathlon)
summary(myregmult2)
## test d'homoscédasticité
plot(predict(myregmult2),resid(myregmult2), xlab="valeur ajusté", ylab="résidu")
abline(h=c(600,0,600), lty=c(2,1,2), col=c(1,2,1))
## On remarque une abscence de structure conique, donc on accepte l'hypothèse nulle d'homoscédasticité des résidus.
## test de normalité (shapiro et ks)
## SHAPIRO
shapiro.test(resid(myregmult2)) ## p-value = 0.5938 > 0.05 Donc On ne rejette pas la normalité
## KS
ks.test(resid(myregmult2), pnorm) ## p-value = 1.332e-15 < 0.05 Donc on rejette la normalité
## recherche de valeurs aberrantes
sd=sqrt(deviance(myregmult2)/df.residual(myregmult2))  ## sd = 272.9382
x0=rep(1,55)
M=matrix(c(x0,Decathlon$Javelot,Decathlon$`110m.haies`,Decathlon$Poids,Decathlon$Perche,Decathlon$Disque,Decathlon$Longueur),55,7)
ul=matrix(0,55,1)
ll=matrix(0,55,1)
m=matrix(0,55,4)
j=1
for (i in 1:55) {
  ##  l'abscisse de la loi de Student à n-p-1 degré de libertés. Dans notre cas n=55 et p=7. Donc ddl = 7.
  # Le calcul donne : 2,0117 
  ul[i]=predict(myregmult1)[i]+2.0117*272.9382*sqrt(1+M[i,]%*%solve(t(M)%*%M)%*%M[i,])
  ll[i]=predict(myregmult1)[i]-2.0117*272.9382*sqrt(1+M[i,]%*%solve(t(M)%*%M)%*%M[i,])
  if (Decathlon$Points[i]>ul[i] | Decathlon$Points[i]<ll[i]) {
    m[j,]<-c(i,Decathlon$Points[i],ul[i],ll[i])
    j<-j+1 
  }                 
}
View(m)
seqx=seq(1,55,length=55)
sd=sqrt(deviance(myregmult2)/df.residual(myregmult2))
sd
abr=abs(Decathlon$Points-predict(myregmult2))/272.9382
plot(seqx,abr)
abline(h=2, lty=2,col=2)
#*******************************************************************************
#Calcul d'AIC
step(myregmult2) ##AIC=623.2
step(myregmult) ##AIC=620.09
