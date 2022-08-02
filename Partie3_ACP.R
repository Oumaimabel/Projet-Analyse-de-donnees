#ACP
#*******************************************************************************
library(readxl)
Decathlon <- read_excel("C:/Users/hp/Desktop/Decathlon.xlsx")
View(Decathlon)
str(Decathlon) #voir quelles sont les variables qui existent dans cet objet
library(FactoMineR) #Les commandes de l'ACP existent dans ce package 'FactoMineR'
cor(Decathlon) #Calcul des corrélations entres les variables
#Test de sphéricité de Bartlett 
bartlett.test(Decathlon) #On a le p-value < 2.2e-16 (inférieur à 5%)
#Donc la matrice de corrélation est différente de l'identité
#Calcul de l'indice KMO et des MSAi
library(psych)
KMO(cor(Decathlon))
#L'application de l'ACP
res <- PCA(Decathlon,ncp=5,axes = c(1,2))
attributes(res) #??
#calcul des valeurs propres et la matrice de corrélation 
res$eig
#Graphique des valeurs propres
plot(1:11,res$eig[,1],type="b",ylab="Valeurs propres",xlab="Composantes",main="graphique des valeurs propres")
barplot(res$eig[,1],main="Eigenvalues", names.arg=paste("dim",1:nrow(res$eig)))
#Règle rapport des variances : 5 axes
var(res$eig[5:11,1])*6/(var(res$eig[,1])*10) #c'est égale à 0.07385619>0.05 
var(res$eig[6:11,1])*5/(var(res$eig[,1])*10) #C'est égale à 0.03907991<0.05
###Donc on va choisir un sous espace de dimension 5, en considérant un seuil de 5%
#Calcul de la somme des valeurs propres
sum(res$eig[,1]) #la somme des vps= le nombre de variables car là ce sont des vps
#de la matrice de corrélation qui n'a que des 1 dans la diagonale donc la trace 
#c'est le nombre des variables
#*********************************************************************************
#Nuages des variables:

#Calcul de : coord, cor, cos2, contrib
res$var

#Cumul des cos2
print(t(apply(res$var$cos2,1,cumsum)),digit=2)

#Contribution des variables au sous espace
cont<-res$var$contrib
write.table(cont,"Cont_Var.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
Don<-read.csv2("C:/Users/hp/Desktop/Cont_Var1.csv",row.names=1) 
View(Don)
Cat_var<-HCPC(Don,nb.clust=-1)
Cat_var$desc.var #L'interprétatation des 6 classes de variables 
Cat_var$data.clust
Don4<-Don[,1:4]
Cat_var4<-HCPC(Don4, nb.clust=-1)
Cat_var4$desc.var
Cat_var4$data.clust
#********************************************************************************
#Nuage des individus:

#Calcul de : coord, cor, cos2, contrib
res$ind

#Cumul des cos2
cum<-print(t(apply(res$ind$cos2,1,cumsum)),digit=2)
#kmeans (3 classes, 4 classes, 5 classes, 6 classes)
cat3<-kmeans(cum[,4],centers=3,nstart=5) #les individus qui sont bien, moyennement
#faiblement représenté (il faut faire un kmeans à 3)
cat3
cat4<-kmeans(cum[,4],centers=4,nstart=5)
cat4
cat5<-kmeans(cum[,4],centers=5,nstart=5)
cat5
cat6<-kmeans(cum[,4],centers=6,nstart=5)
cat6

#Contribution des individus au sous espace
cont<-res$ind$contrib[,1:4]
write.table(cont,"Cont_Ind.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
Don<-read.csv2("Cont_Ind1.csv",row.names=1)
View(Don)
Cat_Ind<-HCPC(Don, nb.clust =-1)
Cat_Ind$desc.var


