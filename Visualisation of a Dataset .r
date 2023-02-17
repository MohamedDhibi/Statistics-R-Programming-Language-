#importation des donnees
Seeds=read.table(file=file.choose(),header=TRUE,sep=",",dec=".")
Seeds

#attacher les variables de poids a la memoire
attach(Seeds)

#afficher les types de variables avec ses noms
str(Seeds)
names(Seeds)

#dimmension de notre Table
dim(Seeds)


#### TACHE 2 : PRE-TRAITEMENT DES DONNEES
 
 #Données aberantes
 
#pour la colonne "A"
library("ggplot2")
ggplot(Seeds,aes(x=varietie,y=A,fill=varietie))+geom_boxplot()+xlab(label="Varietie")+ ylab(label="A")+ggtitle("Boites à moustache") 

#on re marque l'existence des valeurs aberantes telque varietie="kama wheat"

s=Seeds$A[Seeds$varietie=="Kama wheat"]
#View(s)
summary(s)
Q1=17.99
Q3=19.14
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
Vmin
#on remplace les valeurs aberantes par des Na pou "A" 
s[which(s>Vmax)]=NA
s[which(s<Vmin)]=NA
Seeds$A[Seeds$varietie=="Kama wheat"]=s
#pour la colonne "P"

library("ggplot2")
ggplot(Seeds,aes(x=varietie,y=P,fill=varietie))+geom_boxplot()+xlab(label="Varietie")+ ylab(label="P")+ggtitle("Boites à moustache")

#des valeurs aberantes(<vmin) telque varietie="kama wheat"
p=Seeds$P[Seeds$varietie=="Kama wheat"]
#View(p)
summary(p)
Q1= 15.86 
Q3=16.57 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#on remplace les valeurs aberantes par des Na pou "A" 
p[which(p>Vmax)]=NA
p[which(p<Vmin)]=NA
Seeds$P[Seeds$varietie=="Kama wheat"]=p

#pour la colonne "C"

library("ggplot2")
ggplot(Seeds,aes(x=varietie,y=C,fill=varietie))+geom_boxplot()+xlab(label="Varietie")+ ylab(label="C")+ggtitle("Boites à moustache")
#pas de valeurs aberrantes


#pour la colonne "LK"
library("ggplot2")
ggplot(Seeds,aes(x=varietie,y=Lk,fill=varietie))+geom_boxplot()+xlab(label="Varietie")+ ylab(label="Lk")+ggtitle("Boites à moustache")
##des valeurs aberantes(<vmin) telque varietie="Canadian wheat"
#et (<vmin) pour varietie="kama wheat"

#pour varietie=canadian wheat 
lk1=Seeds$Lk[Seeds$varietie=="Canadian wheat"]
#View(lk1)
summary(lk1)
Q1=5.357
Q3=5.658
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)#on remplace les valeurs aberantes par des Na pou "A" 
lk1[which(lk1>Vmax)]=NA
lk1[which(lk1<Vmin)]=NA
Seeds$Lk[Seeds$varietie=="canadian wheat"]=lk1

#pour varietie=kama wheat

lk2=Seeds$Lk[Seeds$varietie=="Kama wheat"]
#View(lk2)
summary(lk2)
Q1=5.370
Q3=5.684
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
lk2[which(lk2>Vmax)]=NA
lk2[which(lk2<Vmin)]=NA
Seeds$Lk[Seeds$varietie=="Kama wheat"]=lk2

#visualisation apres  les valeurs aberantes
library("ggplot2")
ggplot(Seeds,aes(x=varietie,y=Lk,fill=varietie))+geom_boxplot()



#pour la colonne "wk"

library("ggplot2")
ggplot(Seeds,aes(x=varietie,y=Wk,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Wk")
+ggtitle("Boites à moustache")
#pour le champs WK on a pas de données aberantes 


#pour la colonne "AC"

ggplot(Seeds,aes(x=varietie,y=Ac,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Ac")
+ggtitle("Boites à moustache")


ac1=Seeds$Ac[Seeds$varietie=="Canadian wheat"]
#View(ac1)
summary(ac1)
Q1=1.8355
Q3=3.1990
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#on remplace les valeurs manquantes avec Na 
ac1[which(ac1>Vmax)]=NA
ac1[which(ac1<Vmin)]=NA
Seeds$Ac[Seeds$varietie=="Canadian wheat"]=ac1

#maintenant Ac/Rosa wheat
ac2=Seeds$Ac[Seeds$varietie=="Rosa wheat"]
#View(ac2)
summary(ac2)
Q1=4.032
Q3=5.470
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#on remplace par na
ac2[which(ac2>Vmax)]=NA
ac2[which(ac2<Vmin)]=NA
Seeds$Ac[Seeds$varietie=="Rosa wheat"]=ac2

ggplot(Seeds,aes(x=varietie,y=Ac,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Ac")
+ggtitle("Boites à moustache")

#pour la colonne "Lkg"


ggplot(Seeds,aes(x=varietie,y=Lkg,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Ac")
+ggtitle("Boites à moustache")

#des valeurs aberantes(>vmin) telque varietie="Canadian wheat"
#des valeurs aberantes (<vmin)pour kama wheat

#pour Canadian wheat
lkg1=Seeds$Lkg[Seeds$varietie=="Canadian wheat"]
#View(lkg1)
summary(lkg1)
Q1=4.870 
Q3=5.219 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#on remplace les valeurs manquantes avec Na 
lkg1[which(lkg1>Vmax)]=NA
lkg1[which(lkg1<Vmin)]=NA
Seeds$Lkg[Seeds$varietie=="Canadian wheat"]=lkg1

#maintenant Lkg/Kama wheat
lkg2=Seeds$Lkg[Seeds$varietie=="Kama wheat"]
#View(lkg2)
summary(lkg2)
Q1= 5.879
Q3= 6.197
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#on remplace les valeurs manquantes avec Na 
lkg2[which(lkg2>Vmax)]=NA
lkg2[which(lkg2<Vmin)]=NA
Seeds$Lkg[Seeds$varietie=="kama wheat"]=lkg2

#####les valeurs manquantes
## Visualisation avant nettoyage des données manquantes
#install.packages("naniar")
library (naniar)
gg_miss_var(Seeds)


#on remplace les cellules vide avec NA
#on remplace les valeurs manquantes de C par leur formule 4*pi*Seeds["A"]/(Seeds["A"]^(2))
Seeds$A[is.na(Seeds$C)]
Seeds$C[is.na(Seeds$C)] <- 4*pi*Seeds$A[is.na(Seeds$C)]/(Seeds$P[is.na(Seeds$C)]^(2))
unique(Seeds$C)

#les valeurs manquantes
Seeds
na.fail(Seeds)
#plusieurs valeurs manquantes

summary(Seeds)
#11 pou "A",1 pour "p" , 27 pour LK, 4 pour "Ac",1 pour "LKG" 
#valeurs manquantes au totalité pour les données quantitatives
sum(Seeds$varietie=='')
#6 valeurs manquantes au totalité pour les données qualitatives
#on note que les varietes sont trie alphabetiquement

which(Seeds$varietie=='')
#34  35 =>Canadian wheat
#  83 109 =>Kama wheat
#150 153=>Rosa wheat
Seeds$varietie[34]="Canadian wheat"
Seeds$varietie[35]="Canadian wheat"
Seeds$varietie[83]="Kama wheat"
Seeds$varietie[109]="Kama wheat"
Seeds$varietie[150]="Rosa wheat"
Seeds$varietie[153]="Rosa wheat"
Seeds

#Seeds=Seeds[(Seeds$varietie!=""),]
#summary(Seeds)



#install.packages("VIM")
library(VIM)

# le taux des valeurs manquantes.
Taux=sum(is.na(Seeds))/prod(dim(Seeds))
Taux
#taux=0.02972973
# Taux < 5% 


canadian=Seeds[(Seeds$varietie=="Canadian wheat"),]
summary(canadian)
canadian1=kNN(canadian)[,1:8]
summary(canadian1)
na.fail(canadian1)
Seeds[(Seeds$varietie=="Canadian wheat"),]=canadian1

kama=Seeds[(Seeds$varietie=="Kama wheat"),]
summary(kama)
kama=kNN(kama)[,1:8]
summary(kama)
na.fail(kama)
Seeds[(Seeds$varietie=="Kama wheat"),]=kama

rosa=Seeds[(Seeds$varietie=="Rosa wheat"),]
summary(rosa)
library(VIM)
rosa=kNN(rosa)[,1:8]
summary(rosa)
na.fail(rosa)
Seeds[(Seeds$varietie=="Rosa wheat"),]=rosa
summary(Seeds)

#0 valeurs manquantes

## Visualisation après nettoyage des données manquantes

gg_miss_var(Seeds)






#### TACHE 3 : ANALYSE UNIVARIEE

### NORMALITE DES VARIABLES

# Shapiro test
# H0: on a la normalité
# H1: on n'a pas la normalité



#tester la normalite de chaqu’une des variables quantitatives

#visualisation
par(mfrow=c(1,7))
A <- density(Seeds$A) 
P <- density(Seeds$P) 
C <- density(Seeds$C) 
Lk <- density(Seeds$Lk) 
Wk <- density(Seeds$Wk) 
Ac <- density(Seeds$Ac)
Lkg <- density(Seeds$Lkg)
plot(A, xlab = "A")
plot(P, xlab = "P")
plot(C, xlab = "C")
plot(Lk, xlab = "Lk")
plot(Wk, xlab = "Wk")
plot(Ac, xlab = "Ac")
plot(Lkg, xlab = "Lkg")

#test de normalite
shapiro.test(Seeds$A)
#p-value = 2.907e-09 < 0.05 
# On accepte H1
# On n'a pas la normalité

shapiro.test(Seeds$P)
#p-value = 2.224e-08 <0.05
# On accepte H1
# On n'a pas la normalité

shapiro.test(Seeds$C)
#p-value = 0.001319 <0.05
# On accepte H1
# On n'a pas la normalité

shapiro.test(Seeds$Lk)
#p-value = 7.845e-09 <0.05
# On accepte H1
# On n'a pas la normalité 

shapiro.test(Seeds$Wk)
 p-value = 8.563e-06 <0.05
# On accepte H1
# On n'a pas la normalité

shapiro.test(Seeds$Ac)
p-value = 0.009433 <0.05
# On accepte H1
# On n'a pas la normalité

shapiro.test(Seeds$Lkg)
p-value = 3.374e-09 <0.05
# On accepte H1
# On n'a pas la normalité
#la normalite de chaqu’une des variables quantitatives n'est pas verifié

### MODALITE DES VARIABLES
str(Seeds)
#les modalites des chaqune des variables quantitatives.

#install.packages("diptest")
library(diptest)
##dip test for unimodality / multimodality

dip.test(Seeds$A)
#p-value = 0.03744
dip.test(Seeds$P)
#p-value = 0.364
dip.test(Seeds$C)
#p-value = 0.7648
dip.test(Seeds$Lk)
#p-value = 3.926e-06
dip.test(Seeds$Wk)
# p-value = 0.6316
dip.test(Seeds$Ac)
#p-value = 0.5673
dip.test(Seeds$Lkg)
#p-value = 0.2022

#A , A et LK sont  unimodal, les restes sont multimodal

# Histogramme des variables quantitatives
hist ( Seeds$A,col= " green ")
hist ( Seeds$P,col= " yellow ")
hist ( Seeds$C,col= " red ")
hist ( Seeds$Lk,col= " purple ")
hist ( Seeds$Wk,col= " orange ")
hist ( Seeds$Ac,col= " pink ")
hist ( Seeds$Lkg,col= " blue ")
Seeds[,"A"]            #Continue ; Elles peuvent prendre n'importe quelle valeur num�rique enti�re ou d�cimale.
Seeds[,"P"]               #Continue ; Elles peuvent prendre n'importe quelle valeur num�rique enti�re ou d�cimale.
Seeds[,"C"]               #Continue ; Elles peuvent prendre n'importe quelle valeur num�rique enti�re ou d�cimale.
Seeds[,"Lk"]         #Continue ; Elles peuvent prendre n'importe quelle valeur num�rique enti�re ou d�cimale.
Seeds[,"Wk"]     #Continue ; Elles peuvent prendre n'importe quelle valeur num�rique enti�re ou d�cimale.
Seeds[,"Ac"]         #Continue ; Elles peuvent prendre n'importe quelle valeur num�rique enti�re ou d�cimale.     
Seeds[,"Lkg"]        #Continue ; Elles ne peuvent prendre que des valeurs num�riques enti�res ou dicimale.    


#### TACHE 4 : ANALYSE BIVARIEE

## Relation entre une variable qualitative et une variable quantitative


install.packages("corrplot")
library(corrplot)
Seeds_cor=Seeds
summary(Seeds_cor)
Seeds_cor$varietie[Seeds_cor$varietie=="Canadian wheat"] <-"1"
Seeds_cor$varietie[Seeds_cor$varietie=="Kama wheat"] <-"2"
Seeds_cor$varietie[Seeds_cor$varietie=="Rosa wheat"] <-"3"
Seeds_cor
Seeds_cor$varietie<- as.numeric(Seeds_cor$varietie)
summary(Seeds_cor)
M<-cor(Seeds_cor)


# Matrice de corrélation
install.packages('corrplot')
library(corrplot)
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)


#tache 4 partie 2

# Aucune variable ne suit une loi normale
# On utilisera alors les méthodes kendall et spearman

#Methode spearman
# Coefficient de corrélation
Ro=cor(Seeds$A,Seeds$Lkg,method="spearman")
Ro
# Le coefficient de corrélation entre A  et Lkg est de  0.7039174
# On peut déduire qu'il existe une forte corrélation entre eux

#Test de corrélation : Test de significativité

#H0  ∶ absence de dépendance significative
#H1  ∶ existence de dépendance significative


res<-cor.test(Seeds$A,Seeds$Lkg, method="spearman")
res
# p-value < 2.2e-16 <0.05
# On accepte H1
# Les deux variables sont bien corrélées
plot(Seeds$A,Seeds$Lkg)
# forte liaison linéaire positive

#Methode Kendall
# Coefficient de corrélation
Ro=cor(Seeds$Wk,Seeds$A,method="kendall")
Ro
# Le coefficient de corr�lation entre A  et Wk est de 0.8577815
# On peut deduire qu'il existe une forte correlation entre eux
# Test de significativite
res<-cor.test(Seeds$Wk,Seeds$A, method="kendall")
res
# p-value  2.2e-16 <0.05
# On accepte H1
# Les deux variables sont bien correlees
plot(Seeds$Wk,Seeds$A)# forte liaison lin�aire positive
## Visualisation 
install.packages("ggplot2")
library(ggplot2)
ggplot(data=Seeds,aes(x=Wk,y=A,color=varietie))+geom_point()+geom_smooth(method="lm")
#ils sont bien corrélées meme si chaque variete pise toute seule

#### TACHE 5 : REGRESSION LINEAIRE

summary(Seeds)
any(is.na(Seeds))
na.fail(Seeds)
Seeds
unique(Seeds$varietie)
RM= lm(Lkg~ ., data=Seeds_cor)
RM
summary(RM)
#R^2= 0.93
# p_value de C =  0.939286 est la plus elevée
# On essaie encore une fois en enlevant la variable Whole_Weight et on compare
RM2= lm(Lkg~ A + varietie + P + Lk + Wk + Ac , data=Seeds_cor)
summary(RM2)
#R^2=0.93

# Le modele ne perd pas de qualite, on a toujours R2=0.93.
#On continue

# p_value de Ac = 0.15873 est la plus grande
# On essaie encore une fois en enlevant la variable Rings et on compare
RM3 = lm(Lkg ~ A + varietie + Lk + Wk + P , data=Seeds_cor)
RM3
summary(RM3)
# Le modele ne perd pas de qualite, on a toujours R2=0.93, alors le meme taux
# d'information expliquee de la variabilite de Diameter.

# p_value de A  =0.04   est la plus grande
# On essaie encore une fois en enlevant la variable Rings et on compare
RM4 = lm(Lkg ~ P + varietie + Lk + Wk , data=Seeds_cor)
RM4
summary(RM4)
# Le modele ne perd pas de qualite, on a toujours R2=0.93, alors le meme taux
# d'information expliquee de la variabilite de Diameter.


# p_value de Lk  =  0.000474 est la plus grande
# On essaie encore une fois en enlevant la variable Rings et on compare
RM5 = lm(Lkg ~ P + varietie + Wk , data=Seeds_cor)
RM5
summary(RM5)
# Le modele ne perd pas de qualite, on a toujours R2=0.93, alors le meme taux
# d'information expliquee de la variabilite de Diameter.



# p_value de Wk =4.69e-16 est la plus grande
# On essaie encore une fois en enlevant la variable Rings et on compare
RM6 = lm(Lkg ~ varietie + P , data=Seeds_cor)
RM6
summary(RM6)
# Le modele perd de qualite, on a  R2=0.92,
# d'information expliquee de la variabilite de LKg.
# Toutes les p_value sont égales
# On s'arrete ici

AIC(RM2)
AIC(RM)
AIC(RM3)
AIC(RM4)
AIC(RM5)

#AIC(RM2)< AIC(RM3)< AIC(RM)< AIC(RM4)< AIC(RM5)
#RM2 est le modele plus efficace
## Représentation des résidus
R=residuals(RM2)
plot(R)
#On remarque un comportement unimodale autour de 0, on peut conclure que les
#residus sont non corrolees et de moyenne nulle.
# On represente la droite de Henry qui represente la conformite des quantiles 
# pour la distribution cummelee des residus et la droite des quantiles 
# theoriques d'une normale.
qqnorm(R)
qqline(R)

####PCA
install.packages("FactoMineR")
install.packages("factoextra")
library("FactoMineR")
library("factoextra")
View(Seeds)
res.pca <- PCA(Seeds_cor[,2:8][,-2],scale.unit = TRUE, graph = FALSE)
res.pca
dimdesc(res.pca)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
#extraire les resultats
var <- get_pca_var(res.pca)
var
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # �vite le chevauchement de texte
)
#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
install.packages("dplyr")
library(dplyr)
pc <- prcomp(Seeds_cor[,2:8][,-2],center = TRUE,scale. = TRUE)
print(pc)
summary(pc)
# PC1 est fortement correl� avec Lkg
# PC2 est fortement correl� avec Varietie
summary(pc)
# PC5 explique X% de la variabilit� et PC2 explique X% de la variabilit�
# Les deux premi�res composantes capturent la majorit� de la variabilit�
# Regression lin�aire avec X et X
RM = lm(Lkg ~ P+Ac+varietie, data=Seeds)
RM
summary(RM)
# R^2 =  0.9
# 90% de la variabilitie de Lkg est explique par P , Ac et varietie
# C'est un bon modele

#### TACHE 6 : REGRESSION LINEAIRE GENERALISEE*******************************************************************************************************************

# On va appliquer la famille gamma car c'est le type le plus ad�quat � nos 
# données
# Guassian (donnees suivant la loi normale)
# Poisson (donnees discr�tes)
# Binomial (donnees binaires)
# Gamma (donnees continues non negatives)
summary(Seeds)
summary(Seeds_cor$Lkg)
# Application de la regression lineaire generalisee
model1<-glm(Seeds$Lkg  ~ Seeds$A + Seeds$P + Seeds$C + Seeds$Lk + Seeds$Wk + Seeds$Ac + Seeds$varietie , family=Gamma(link="inverse"))
model1
summary(model1)

# p_value de A = 0.86 est la plus grande
model2<-glm(Seeds$Lkg  ~  Seeds$P + Seeds$C + Seeds$Lk + Seeds$Wk + Seeds$Ac + Seeds$varietie , family=Gamma(link="inverse"))
model2
summary(model2)
AIC(model1,model2)
# l'AIC du modele 2 < l'AIC du modele 1 => On garde le modele 2

#appliquer test chi2
anova.res2=anova(model2,model1,test="Chisq")
1-pchisq( abs(anova.res2$Deviance[2]), abs(anova.res2$Df[2]))
# H0 : le modele 2 est meilleur
# H1: le modele 1 est meilleur
# p_value=0.99 > 0.05
# On accepte H0
# le modele 2 est meilleur






