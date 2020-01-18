#####################################################################
# Beadand�: Eg�szs�g�gyi k�lts�gek becsl�se line�ris regresszi�val  #
# Adat: Kaggle.com                                                  #
#####################################################################

#########################
# csomagok import�l�sa  #
#########################

library(ggplot2)      # adatvizualiz�ci�
library(gridExtra)
library(corrplot)     # korrel�ci�s m�trix
library(fastDummies)  # dummy k�dol�s
library(lmtest)       # diagnosztika (Breusch-Pagan Test)
library(caret)
library(e1071)
library(car)          # diagnosztika (vif)
library(gvlma)        # diagnosztika

##################
# adatbeolvas�s #
#################

csv <- read.csv("C:/Users/molna/Desktop/Egyetem/KRE/T�bbv�ltoz�s statisztika/insurance.csv")
head(csv)

# Magyar�z� v�ltoz�k:
# age: kor (folytonos)
# sex: nem (binomi�lis)
# bmi: bmi index (folytonos)
# children: gyermeksz�m (folytonos)
# smoker: doh�nyz�s (binomi�lis)
# region: r�gi� (kategorikus)

# C�lv�ltoz�:
# charges: E�.-i k�lts�gek (folytonos v�ltoz�)


##################
# adatfelt�r�s  #
#################
str(csv)

# 1388 megfigyel�s, 7 dimenzi�

# van-e hi�nyz� �rt�k az adatban?
sapply(csv, function(x) sum(is.na(x)))


# c�lv�ltoz� hisztogramja:
hist(csv$charges, main = "Eg�szs�g�gyi k�lts�gek eloszl�sa", col = "lightblue", xlab = "K�lts�gek", ylab = "Gyakoris�g")
# jobbra ferde hisztogram: az adathalmazban kevesebb egyed van, akinek magasak az e� k�lts�gei.
# ez nem t�l optim�lis eloszl�s a line�ris regresszi�s modellnek.

# norm�lis eloszl�s el�rhet�-e transzform�ci�val?
hist(sqrt(csv$charges)) # n�gyzetgy�k
qqnorm(sqrt(csv$charges), pch = 1, frame = FALSE)
qqline(sqrt(csv$charges), col = "steelblue", lwd = 2)

hist(log(csv$charges)) # logaritmikus
qqnorm(log(csv$charges), pch = 1, frame = FALSE)
qqline(log(csv$charges), col = "steelblue", lwd = 2)


# magyar�z� v�ltoz�k adatvizualiz�ci�i:


# a doh�nyz�s �s a k�lts�gek kapcsolata:
x <- ggplot(csv, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()
print(x)
# a doh�nyos egyedekn�l a k�lts�gek magasabbak.

# a nem �s a k�lts�gek kapcsolata:
y <- ggplot(csv, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()
print(y)
# a nemek eset�n nincs k�l�nbs�g a k�lts�gekben.

# a gyermeksz�m �s a k�lts�gek kapcsolata:
z <- ggplot(csv, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()
print(z)
# �rdekes, hogy t�bb gyerek eset�n a k�lts�gek cs�kkenek.

# a kor �s a k�lts�gek kapcsolata:
g <- ggplot(csv, aes(age, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.5) +
  theme_light()
print(g)
# a kor n�veked�s�vel n�nek a k�lts�keg is.
# �rdekes, hogy a k�lts�gek s�vosan elv�lnak a kor ment�n. 
# Ez a doh�nyz�s kategorikus v�ltoz� miatt van.

# a bmi index �s a k�lts�gek kapcsolata:
j <- ggplot(csv, aes(bmi, charges)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  theme_light()
print(j)
# a bmi n�veked�s�vel a k�lts�gek is elkezdenek n�vekedni. 30-n�l van egy nagyobb ugr�s.

# a bmi index �s a kor kapcsolata:
r <- ggplot(csv, aes(age, bmi)) +
  geom_jitter(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  theme_light()
print(r)
# a kor �s a bmi index k�z�tt nincs szignifik�ns �sszef�gg�s.
# A magasabb kor nem jelent nagyobb k�lts�geket.

# kiugr� �rt�kek - box plot vizualiz�ci�k:
fun_mean <- function(x){
  return (round((data.frame(y=mean(x),label=mean(x,na.rm=T))),2))}

plot.sex <- ggplot(csv, aes(x = sex, y = charges, fill= sex)) +
            geom_boxplot(alpha = 0.5) +
            stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
            stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)

plot.smoker <- ggplot(csv, aes(x = smoker, y = charges, fill = smoker)) +
              geom_boxplot(alpha = 0.5) +
              stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
              stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)             

plot.child <- ggplot(csv, aes(x = as.factor(children), y = charges, fill = children)) +
              geom_boxplot(alpha = 1.5) +
              stat_summary(fun.y = mean, geom = "point", colour = "darkred", size = 3) +
              stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)

grid.arrange(plot.sex, plot.smoker, plot.child, ncol=2, nrow=2)
# a n�k �s a f�rfiak �tlagos k�lts�gei k�zel egyenl�ek.
# a doh�nyz�k �tlagos k�lts�gei 5x magasabbak, mint a nem doh�nyz�k�.
# a gyermeksz�m n�veked�s�vel az �tlagos k�lts�gek is n�nek.

colnames(csv)

# v�ltoz�k �br�zol�sa korrel�ci�s m�trixon:
data.cor <- cor(csv[ ,c(1,3,4,7)], use = "pairwise.complete.obs")
corrplot(data.cor)
# nincs multikollinearit�s a magyar�z� v�ltoz�k k�z�tt.


####################
# Adatel�k�sz�t�s #
###################

# kategorikus v�ltoz�k dummy-v� alak�t�sa:
csv <- dummy_cols(csv)

# sztring �rt�kek numerikuss� konvert�l�sa:
csv["region_num"] <- as.numeric(csv$region)
csv["smoker_num"] <- ifelse(csv$smoker == "yes", 1, 0)
csv["sex_num"] <- ifelse(csv$sex == "male", 1, 0)


# �j c�lv�ltoz� el��ll�t�sa:
csv["cost_per_year"] <- csv$charges / csv$age

colnames(csv)


####################
#   Modellez�s    #
###################

# nulladik modell fel�p�t�se:
model.0 <- lm(charges ~ age + sex_male + bmi + children + smoker_yes, data = csv)
summary(model.0)

# els� modell fel�p�t�se v�ltoz�szelekci�val: (d�nt�s a p-�rt�k alapj�n)
model.1 <- lm(charges ~ age + bmi + smoker_yes, data = csv)
summary(model.1)
par(mfrow=c(2,2))
plot(model.1)

# age: ha 1 �vvel n� a kor, akkor a k�lts�g 257.85$-al n�
# bmi: ha 1 egys�ggel n� a bmi index, akkor a k�lts�g 321.85$-al n�.
# children: ha egyel t�bb gyermek van, akkor a k�lts�g 473.5$-al n�.
# smoker_yes: ha valaki doh�nyzik, akkor az 23811.4$-al n�veli a k�lts�geket.

# Line�ris becsl�s k�plete:
# charges = -11676.83 + 259.55*age + 322.62*bmi + 23823.68*smoker_yes

###########################
#   model.1 ki�rt�kel�se  #
###########################

# a rezidu�lisok norm�lis eloszl�st k�vetnek:
hist(model.1$residuals)
# Olyan eloszl�s, amely kiss� norm�lis, szimmetrikus, harang alak� �s van farka "fat tails".
shapiro.test(model.1$residuals)
# Shapio-Wilk teszt: alpha = 0.05 > p = 2.2e-16 -> H0:elutas�t�s (normalit�s)

# a rezidu�lisok �tlaga 0-e?
mean(model.1$residuals)

# rezidumok eloszl�sa Normal Q-Q �br�n:
qqnorm(resid(model.1), main = "Normal Q-Q Plot, model.1", col = "darkgrey")
qqline(resid(model.1), col = "dodgerblue", lwd = 2)

# a megfigyel�sek f�ggetlenek-e egym�st�l? - Darwin - Watson teszt
dwtest(model.1)
# alpha = 0.05 < p = 0.9197 -> H0: elfogadva (f�ggetlens�g)

# diagnostic plot 
par(mfrow=c(2,2))
plot(model.1)

# a rezidu�lisok varianci�ja konstans-e: (homoszkedaszticit�s)
# Residuals vs Fitted plot - Van heteroszkedaszticit�s
bptest(model.1, studentize = TRUE)
# alpha = 0.05 -> p-value = 2.2e-16 < alpha -> H0:elutas�t�s (homoszkedaszticit�s!)

# vizualiz�lva:
# rezidu�lis �rt�kek vs val�s megfigyel�sek:
ggplot(,aes(x = fitted(model.1), y = csv$charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Becs�lt �rt�kek vs val�s megfigyel�sek")

# a magyar�z� v�ltoz�k f�ggetlenek-e egym�st�l (multikollinearit�s)
vif(model.1)
# az �sszes VIF �rt�k kisebb, mint 5, teh�t a v�ltoz�k f�ggetlenek egym�st�l a modellben.

###########################################
# �j modellek �p�t�se a model.1 alapj�n  #
##########################################

# A model.1-ben l�v� heteroszkedaszticit�st okozhatja a line�ris kapcsolat megl�t�nek hi�nya.
# A model.1 rezidu�lisainak nem norm�lis eloszl�s�t okozhatj�k a kiugr� �rt�kek.

# kiugr� �rt�kek kisz�r�se:
cooksd <- cooks.distance(model.1)
plot(cooksd, pch="*", cex=2, main="Kiugr� megfigyel�sek a Cooks t�vols�g alapj�n") 
abline(h = 5*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd > 5*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

kiugrok <- as.numeric(names(cooksd)[(cooksd > 5*mean(cooksd, na.rm=T))])  

# kiugr� �rt�kek sz�r�se az adathalmazb�l:
csv_no_out <- csv[-kiugrok, ]

# m�sodik modell fel�p�t�se, outlier sz�r�ssel:
model.2 <- lm(charges ~ age + bmi + children + smoker_yes, data = csv_no_out)
summary(model.2)

# A kiugr� �rt�kek kisz�r�s�vel a modell R^2-e 0.7469-r�l, 0.8027 ugrott.
# A modell magyar�z� ereje n�vekedett.


# Linearit�s vizsg�lata a folytonos magyar�z� v�ltoz�kn�l (bmi, age):
g <- ggplot(csv, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()
print(g)

g2 <- ggplot(csv, aes(age^2, charges)) +
  geom_jitter(color= "blue", alpha = 0.5) +
  theme_light()

grid.arrange(g, g2, ncol=2, nrow=1)
# A korn�l volt egy kis g�rbe a diagramon, teh�t felt�telezhetj�k, hogy nem teljesen line�ris a kapcsolat a kor
# �s a k�lts�gek k�zt.
# �rdemes a kor n�gyzetet haszn�lni.

model.3 <- lm(charges ~ age^2 + bmi + children + smoker_yes, data = csv_no_out)
summary(model.3)
par(mfrow=c(2,2))
plot(model.3)

# interakci�k felt�r�sa az adatokban:
ggplot(csv, aes(x = bmi, y = charges, col = smoker)) +
  geom_point()
# akik doh�nyoznak, nekik nagyobb a BMI index�k is, ez�ltal nagyobbak a k�lts�geik.

# interakci�s v�ltoz� beemel�se a modellbe:
model.4 <- lm(charges ~ age^2 + I(smoker_yes*bmi) + children, data = csv_no_out)
summary(model.4)
par(mfrow=c(2,2))
plot(model.4)
# az interakci�s v�ltoz� r�v�n a magyar�z� er� 0.8638-ra n�tt! 
# Az I() fv kiszedi az eredeti v�ltoz�kat, �gy nem lesz multikollinearit�s a modellbe.
# A heteroszkedaszticit�s m�rt�ke is cs�kkent.

# A rezidu�lisok eloszl�sa norm�lis, 0 �tlaggal:
hist(model.4$residuals)

shapiro.test(model.4$residuals)
# Shapio-Wilk teszt: alpha = 0.05 > p = 2.2e-16 -> H0:elutas�t�s (normalit�s)

# A heteroszkedaszticit�s m�rt�ke cs�kkent.
bptest(model.4)

# Multikollinearit�s a modellben - nincs
vif(model.4)

# Model.4 �ltal adott becsl�s �s a rezidu�lisok hozz�ad�sa a dataframe-hez:
csv_no_out['yhat'] <- predict(model.4)
csv_no_out['residuals'] <- csv_no_out$charges - csv_no_out$yhat
csv_no_out[ ,c("charges", "yhat", "residuals")]


#############################
# Modellek �sszehasonl�t�sa #
#############################

# ANOVA - illeszked�svizsg�latra.
aov(model.0) # Residual standard error: 6069
aov(model.1) # Residual standard error: 6092
aov(model.2) # Residual standard error: 5106
aov(model.3) # Residual standard error: 5106
aov(model.4) # Residual standard error: 4244

# A model.4 standard errorja a legkisebb ()


###########################
# Model.4 interpret�ci�ja #
###########################

# regresszi�s modell haszn�lata interpol�ci�ra:
x1 <- data.frame(age = 19,
                 bmi = 27.9,
                 children = 0,
                 smoker_yes = 1)
print(paste0("Az x1 egyed eg�szs�g�gyi k�lts�ge ($): ", round(predict(model.4, x1), 2)))

x2 <- data.frame(age = 56,
                 bmi = 36.7,
                 children = 2,
                 smoker_yes = 1)
print(paste0("Az x1 egyed eg�szs�g�gyi k�lts�ge ($): ", round(predict(model.4, x2), 2)))
