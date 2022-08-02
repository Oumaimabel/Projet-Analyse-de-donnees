#####Classification tp 
#*******************************************************************************
library(readxl)
Decathlon <- read_excel("C:/Users/hp/Desktop/Decathlon.xlsx")
View(Decathlon)
nrow(Decathlon)
str(Decathlon)
#centrage réduction des données
Decathlon_cr<-scale(Decathlon,center=T,scale=T)
View(Decathlon_cr)
#*******************************************************************************
#évaluer la proportion d'inertie expliquée
N=41
inertie.expl <- rep(0,times=N)
for (k in 2:N){
  clus <- kmeans(Decathlon_cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}
#l'inertie explicative est nulle si k=1 (1 seule classe)
max(inertie.expl)
#N est le plus petit entier tel que max(inertie.expl)>0.95
#graphique
plot(1:N,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
#k-means avec les données centrées et réduites
#center = 41 - nombre de groupes demandés
#nstart = 5 - nombre d'essais avec différents individus de départ
groupes.kmeans <- kmeans(Decathlon_cr,centers=41,nstart=5)
#affichage des résultats
print(groupes.kmeans)
#*******************************************************************************
var(inertie.expl[17:N])*(N-17)*100/(var(inertie.expl)*(N-1))
#5.21719 > 5%
var(inertie.expl[18:N])*(N-18)*100/(var(inertie.expl)*(N-1))
#4.266346 < 5%
#le nombre de classe à retenir est 17
#k-means avec les données centrées et réduites
#center = 17 - nombre de groupes demandés
#nstart = 5 - nombre d'essais avec différents individus de départ
groupes.kmeans <- kmeans(Decathlon_cr,centers=17,nstart=5)
#affichage des résultats
print(groupes.kmeans)
#*******************************************************************************
library(FactoMineR)
#Les données sont centrées et réduites sur Excel
library(readxl)
Decathlon_CR <- read_excel("C:/Users/hp/Desktop/Decathlon_CR.xlsx")
View(Decathlon_CR)
#CAH
Res<-HCPC(Decathlon_CR,nb.clust=-1)
Res$desc.var
Res$data.clust
Res$desc.ind
str(Decathlon_CR)

##Calcul du taux d'inertie
I<-Res$call
I
#####gain d'inerite
Res$call$'t'$inert.gain
