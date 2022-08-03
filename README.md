# Projet Analyse de données


### About Dataset
![Decathlon](https://user-images.githubusercontent.com/93741954/182478720-aea202d1-a56d-4f05-b787-b15a782fc1fc.jpg)
Notre jeu de données Décathlon contient les performances réalisées par des athlètes 
lors d’une compétition.
Dans ce jeu de données, il y a 55 lignes et 11 colonnes :
- Les colonnes 2 à 11 sont des variables quantitatives (les variables explicatives), 
correspondent aux performances des athlètes pour les dix épreuves du Décathlon 
(Longueur, 100m, Poids, Hauteur, 400m, 110m.haies, Perche, Javelot, 1500m et 
Disque).
- La 1ère colonne est une variable quantitative (variable dépendante) qui correspond 
au nombre de points obtenus par chaque athlète.

### Résumé

#### Partie 1:
On peut vouloir expliquer les points obtenus dans une compétition de jeux 
olympiques, en fonction des performances des athlètes. Nous allons faire une régression linéaire multiple sur notre jeu de données. 

#### Partie2: 
La classification permet d'identifier des groupes homogènes au sein de la population 
du point de vue des variables étudiées.
Nous allons faire une classification sur ce jeu de données afin d’identifier des groupes 
homogène, autrement dit afin de catégoriser les individus en classe d’invidus.
Dans cette partie, nous aurons besoin de deux fichier Excel, le premier c’est le fichier 
Excel « Decathlon » (notre jeu de donnée), et le 2 ème c’est « Decathlon_CR » c’est là où 
nous avons les données centrées et réduites manuellement sur Excel, qui vont être utilisée 
pour la CAH.

#### Comparer les classifications faites par kmeans et CAH
- Le kmeans nécessite une connaissance préalable du clusters (nombre de classes), alors 
que la CAH, on peut s’arrêter à n’importe quel nombre de groupes, on le trouve approprié en 
interprétant le dendrogramme.
- Les méthodes utilisées dans kmeans sont normalement moins gourmandes en calculs et 
sont adaptées à de très grands ensembles de données. Pour la CAH, les méthodes de division 
fonctionnent dans la direction opposées : en commençant par un cluster qui inclut tous les 
enregistrements, et les méthodes hiérarchiques sont particulièrement utiles lorsque l’objectif est 
d’organiser les clusters dans une hiérarchie naturelle.
- Coté avantages, pour le kmeans, y a toujours la convergence qui est garanti, et pour la 
CAH, elle facilite le traitement de toute forme de similitude ou de distance, par conséquent, elle 
est applicables à tous les types d’attributs.
- Coté désavantages, pour le kmeans, c’est parfois difficile de prévoir le nombre de classe, 
et pour la CAH, le clustering hiérarchique nécessite le calcul et le stockage d'une matrice de 
distance n × n. Pour les très grands ensembles de données, cela peut être coûteux et lent.

#### Partie3: 
L’objectif de l’ACP est de condenser l'information contenu dans le tableau de 
données par une analyse des corrélations linéaires entre les variables et une 
visualisation graphique des distances entre les individus. 
L'ACP nous a permis de :
- représenter les données en deux dimensions ;
- établir des profils des athlètes ;
- mettre au jour des corrélations entre des variables;
- dégager les liaisons entre variables et les ressemblances entre individus.
