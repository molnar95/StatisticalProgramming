###################################################################################
#title:  Minavételezés beadandó                                                   #
#author: Molnár Bence Márk                                                        #
#Output: A félév során elsajátított mintavételi eljárások szimulációja            # 
###################################################################################

###########################
# Adathalmaz előállítása  #
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
# Leíró statisztikák  #
#######################

head(df)
tail(df)
summary(df)
colnames(df)
unique(df$alsos_felsos)
cor(newdf$magassag, newdf$teszt)  #0.5703985

#########################
# Sokasági paraméterek  #
#########################

#a teljes populacio merete:
length(y)
#1000
#a populáció átlaga:
mean(df$magassag) 
#149.9011
#a populáció átlagának varianciája:
var(df$magassag)  
#254.8583

############################################################################################
############################################################################################

#######################
# Véletlen mintavétel #
#######################

xbar <- apply(replicate(1000, sample(df$magassag, 50)), 2, FUN=mean)
#apply: oszloponként kiátlagoljuk (2) - így 1000 átlagot kaptunk!
xbar
#mintaátlagok átlaga:
mean(xbar)
#149.9251
#mintaátlagok varianciája:
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
# Rétegzett mintavétel  #
#########################

#retegek letrehozasa (2 reteg: a = alsos, f = felsos):
reteg <- df$alsos_felsos
#az y-ba beleraktam a magassag értékeket:
y <- df$magassag
#rétegek mérete:
table(reteg)

#kiemeljük y-ból az alsós réteg elemeit, majd ugyanezt megcsináljuk 
#a felsős rétegre is:
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

#Mintaallokáció: Ha a minta arányos lenne az elemszámmal:
table(reteg) / nrow(newdf)        #a:0.5 b:0,5
table(reteg) / nrow(newdf) * 50   #25;25

#Rétegeken belüli SRS: 
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
# Többlépcsős mintavétel  #
###########################

#2 lépcsőn haladunk végig. 
#Először kivesszük az osztályt (First Stage):
sample1 <- sample(unique(df$oszt_azonos_i), 10, replace = FALSE)
#9 elemű mintát vettünk a distinct osztályazonosítók közül.
sample1
unique(df$oszt_azonos_i)
#Ez egy vektor mely 9 elemből áll. 
#Mi ezen 9 elemen/osztályon belüli magasság értékekre vagyunk kíváncsiak:
multidf <- df[df$oszt_azonos_i %in% sample1, ]
#Ebben a dataframe-be már csak az első lépcső során kivett 9 véletlenszerűen
#kiválasztott osztály elemei lesznek.
multidf
#A második lépésben (Second Stage) 50 elemű véletlenmintát veszek ki a 9 random 
#osztályon belül:
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
# Klaszter mintavétel #
#######################

#Először random mintavételezünk 9 osztályt a sokaságból:
sampleosztaly <- sample(unique(df$oszt_azonos_i), 9, replace = FALSE)
sampleosztaly
#A kiválasztott osztályok összes elmét figyelembevesszük
dfklaszter <- df[df$oszt_azonos_i %in% sampleosztaly, ]
mean(dfklaszter$magassag)
#148.2512
var(dfklaszter$magassag)
hist(dfklaszter$magassag)
     
     
############################################################################################
###########################
# Összehasonlítás /       #
# Konklúzió               #
###########################

#centrális eloszlás 1:
par(mfrow=c(1,3))
hist(xbar, freq=F)
lines(density(xbar), col="red")
hist(xbarstrat, freq=F)
lines(density(xbarstrat), col="green")
hist(sample2, freq=F)
lines(density(sample2), col="blue")

#centrális eloszlás 2:
hist(xbar,col="blue",main="SRS vs. Stratified",
     xlab="estimate")
hist(xbarstrat,add=T,col="lightyellow")
hist(sample2, add=T, col="green")



