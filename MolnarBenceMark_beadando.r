###################################################################################
#title:  Minav�telez�s beadand�                                                   #
#author: Moln�r Bence M�rk                                                        #
#Output: A f�l�v sor�n elsaj�t�tott mintav�teli elj�r�sok szimul�ci�ja            # 
###################################################################################

###########################
# Adathalmaz el��ll�t�sa  #
###########################

df <- data.frame(alsos_felsos=c(rep("f", 500), 
                                rep("a", 500)), 
                 oszt_azonos_i = c(rep(1:20, each=25),
                                   rep(21:45, each=20)), 
                 gyerek_azonos_j = c(rep(1:25, 20), 
                                     rep(1:20, 25)), 
                 magassag = c(rnorm(500, 160, 15), 
                              rnorm(500, 140, 10)), 
                 teszt =  c(rnorm(500, 150, 10), 
                            rnorm(500, 100, 10)))

#######################
# Le�r� statisztik�k  #
#######################

head(df)
tail(df)
summary(df)
colnames(df)
unique(df$alsos_felsos)
cor(newdf$magassag, newdf$teszt)  #0.5703985

#########################
# Sokas�gi param�terek  #
#########################

#a teljes populacio merete:
length(y)
#1000
#a popul�ci� �tlaga:
mean(df$magassag) 
#149.9011
#a popul�ci� �tlag�nak varianci�ja:
var(df$magassag)  
#254.8583

############################################################################################
############################################################################################

#######################
# V�letlen mintav�tel #
#######################

xbar <- apply(replicate(1000, sample(df$magassag, 50)), 2, FUN=mean)
#apply: oszloponk�nt ki�tlagoljuk (2) - �gy 1000 �tlagot kaptunk!
xbar
#minta�tlagok �tlaga:
mean(xbar)
#149.9251
#minta�tlagok varianci�ja:
var(xbar)
#4.647555
hist(xbar)
#124.5554
#confidencia intervallum: 
sort(xbar)[c(25,975)]
#145.5520 154.2379

############################################################################################
############################################################################################

#########################
# R�tegzett mintav�tel  #
#########################

#retegek letrehozasa (2 reteg: a = alsos, f = felsos):
reteg <- df$alsos_felsos
#az y-ba beleraktam a magassag �rt�keket:
y <- df$magassag
#r�tegek m�rete:
table(reteg)

#kiemelj�k y-b�l az als�s r�teg elemeit, majd ugyanezt megcsin�ljuk 
#a fels�s r�tegre is:
y1 <- y[reteg=='a']
print(y1)
length(y1)  #500
mean(y1)    #139.7756
var(y1)     #88.06683
y2 <- y[reteg == 'f']
print(y2)
length(y2)  #500
mean(y2)    #161.2424
var(y2)     #224.5927

#Mintaallok�ci�: Ha a minta ar�nyos lenne az elemsz�mmal:
table(reteg) / nrow(newdf)        #a:0.5 b:0,5
table(reteg) / nrow(newdf) * 50   #25;25

#R�tegeken bel�li SRS: 
xbarstrat <- NA
for (i in 1:1000) {
  xbarstrat[i] <- mean( c(df[sample(500, 25), "magassag"], df[sample(501:1000, 25), "magassag"]))
}
xbarstrat
mean(xbarstrat)
#150.5724
var(xbarstrat)
#2.973467
hist(xbarstrat)
sort(xbarstrat)[c(25,975)]

############################################################################################
############################################################################################

###########################
# T�bbl�pcs�s mintav�tel  #
###########################

#2 l�pcs�n haladunk v�gig. 
#El�sz�r kivessz�k az oszt�lyt (First Stage):
sample1 <- sample(unique(df$oszt_azonos_i), 10, replace = FALSE)
#9 elem� mint�t vett�nk a distinct oszt�lyazonos�t�k k�z�l.
sample1
unique(df$oszt_azonos_i)
#Ez egy vektor mely 9 elemb�l �ll. 
#Mi ezen 9 elemen/oszt�lyon bel�li magass�g �rt�kekre vagyunk k�v�ncsiak:
multidf <- df[df$oszt_azonos_i %in% sample1, ]
#Ebben a dataframe-be m�r csak az els� l�pcs� sor�n kivett 9 v�letlenszer�en
#kiv�lasztott oszt�ly elemei lesznek.
multidf
#A m�sodik l�p�sben (Second Stage) 50 elem� v�letlenmint�t veszek ki a 9 random 
#oszt�lyon bel�l:
sample2 <- apply(replicate(1000, sample(multidf$magassag, 50, replace=FALSE)), 2, mean)
mean(sample2)
#153.7821
var(sample2)
#4.070877
hist(sample2)
sort(sample2)[c(25,975)]
# 149.9417 157.6703

############################################################################################
############################################################################################

#######################
# Klaszter mintav�tel #
#######################

#El�sz�r random mintav�telez�nk 9 oszt�lyt a sokas�gb�l:
sampleosztaly <- sample(unique(df$oszt_azonos_i), 9, replace = FALSE)
sampleosztaly
#A kiv�lasztott oszt�lyok �sszes elm�t figyelembevessz�k
dfklaszter <- df[df$oszt_azonos_i %in% sampleosztaly, ]
mean(dfklaszter$magassag)
#148.2512
var(dfklaszter$magassag)
hist(dfklaszter$magassag)
     
     
############################################################################################
###########################
# �sszehasonl�t�s /       #
# Konkl�zi�               #
###########################

#centr�lis eloszl�s 1:
par(mfrow=c(1,3))
hist(xbar, freq=F)
lines(density(xbar), col="red")
hist(xbarstrat, freq=F)
lines(density(xbarstrat), col="green")
hist(sample2, freq=F)
lines(density(sample2), col="blue")

#centr�lis eloszl�s 2:
hist(xbar,col="blue",main="SRS vs. Stratified",
     xlab="estimate")
hist(xbarstrat,add=T,col="lightyellow")
hist(sample2, add=T, col="green")



