#####################################################################
# Beadandó: Egészségügyi költségek becslése lineáris regresszióval  #
# Adat: Kaggle.com                                                  #
#####################################################################

#########################
# csomagok importálása  #
#########################

library(ggplot2)      # adatvizualizáció
library(gridExtra)
library(corrplot)     # korrelációs mátrix
library(fastDummies)  # dummy kódolás
library(lmtest)       # diagnosztika (Breusch-Pagan Test)
library(caret)
library(e1071)
library(car)          # diagnosztika (vif)
library(gvlma)        # diagnosztika

##################
# adatbeolvasás #
#################

csv <- read.csv("C:/Users/molna/Desktop/Egyetem/KRE/Többváltozós statisztika/insurance.csv")
head(csv)

# Magyarázó változók:
# age: kor (folytonos)
# sex: nem (binomiális)
# bmi: bmi index (folytonos)
# children: gyermekszám (folytonos)
# smoker: dohányzás (binomiális)
# region: régió (kategorikus)

# Célváltozó:
# charges: Eü.-i költségek (folytonos változó)


##################
# adatfeltárás  #
#################
str(csv)

# 1388 megfigyelés, 7 dimenzió

# van-e hiányzó érték az adatban?
sapply(csv, function(x) sum(is.na(x)))


# célváltozó hisztogramja:
hist(csv$charges, main = "Egészségügyi költségek eloszlása", col = "lightblue", xlab = "Költségek", ylab = "Gyakoriság")
# jobbra ferde hisztogram: az adathalmazban kevesebb egyed van, akinek magasak az eü költségei.
# ez nem túl optimális eloszlás a lineáris regressziós modellnek.

# normális eloszlás elérhetõ-e transzformációval?
hist(sqrt(csv$charges)) # négyzetgyök
qqnorm(sqrt(csv$charges), pch = 1, frame = FALSE)
qqline(sqrt(csv$charges), col = "steelblue", lwd = 2)

hist(log(csv$charges)) # logaritmikus
qqnorm(log(csv$charges), pch = 1, frame = FALSE)
qqline(log(csv$charges), col = "steelblue", lwd = 2)


# magyarázó változók adatvizualizációi:


# a dohányzás és a költségek kapcsolata:
x <- ggplot(csv, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()
print(x)
# a dohányos egyedeknél a költségek magasabbak.

# a nem és a költségek kapcsolata:
y <- ggplot(csv, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()
print(y)
# a nemek esetén nincs különbség a költségekben.

# a gyermekszám és a költségek kapcsolata:
z <- ggplot(csv, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()
print(z)
# érdekes, hogy több gyerek esetén a költségek csökkenek.

# a kor és a költségek kapcsolata:
g <- ggplot(csv, aes(age, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.5) +
  theme_light()
print(g)
# a kor növekedésével nõnek a költsékeg is.
# érdekes, hogy a költségek sávosan elválnak a kor mentén. 
# Ez a dohányzás kategorikus változó miatt van.

# a bmi index és a költségek kapcsolata:
j <- ggplot(csv, aes(bmi, charges)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  theme_light()
print(j)
# a bmi növekedésével a költségek is elkezdenek növekedni. 30-nál van egy nagyobb ugrás.

# a bmi index és a kor kapcsolata:
r <- ggplot(csv, aes(age, bmi)) +
  geom_jitter(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  theme_light()
print(r)
# a kor és a bmi index között nincs szignifikáns összefüggés.
# A magasabb kor nem jelent nagyobb költségeket.

# kiugró értékek - box plot vizualizációk:
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
# a nõk és a férfiak átlagos költségei közel egyenlõek.
# a dohányzók átlagos költségei 5x magasabbak, mint a nem dohányzóké.
# a gyermekszám növekedésével az átlagos költségek is nõnek.

colnames(csv)

# változók ábrázolása korrelációs mátrixon:
data.cor <- cor(csv[ ,c(1,3,4,7)], use = "pairwise.complete.obs")
corrplot(data.cor)
# nincs multikollinearitás a magyarázó változók között.


####################
# Adatelõkészítés #
###################

# kategorikus változók dummy-vá alakítása:
csv <- dummy_cols(csv)

# sztring értékek numerikussá konvertálása:
csv["region_num"] <- as.numeric(csv$region)
csv["smoker_num"] <- ifelse(csv$smoker == "yes", 1, 0)
csv["sex_num"] <- ifelse(csv$sex == "male", 1, 0)


# új célváltozó elõállítása:
csv["cost_per_year"] <- csv$charges / csv$age

colnames(csv)


####################
#   Modellezés    #
###################

# nulladik modell felépítése:
model.0 <- lm(charges ~ age + sex_male + bmi + children + smoker_yes, data = csv)
summary(model.0)

# elsõ modell felépítése változószelekcióval: (döntés a p-érték alapján)
model.1 <- lm(charges ~ age + bmi + smoker_yes, data = csv)
summary(model.1)
par(mfrow=c(2,2))
plot(model.1)

# age: ha 1 évvel nõ a kor, akkor a költség 257.85$-al nõ
# bmi: ha 1 egységgel nõ a bmi index, akkor a költség 321.85$-al nõ.
# children: ha egyel több gyermek van, akkor a költség 473.5$-al nõ.
# smoker_yes: ha valaki dohányzik, akkor az 23811.4$-al növeli a költségeket.

# Lineáris becslés képlete:
# charges = -11676.83 + 259.55*age + 322.62*bmi + 23823.68*smoker_yes

###########################
#   model.1 kiértékelése  #
###########################

# a reziduálisok normális eloszlást követnek:
hist(model.1$residuals)
# Olyan eloszlás, amely kissé normális, szimmetrikus, harang alakú és van farka "fat tails".
shapiro.test(model.1$residuals)
# Shapio-Wilk teszt: alpha = 0.05 > p = 2.2e-16 -> H0:elutasítás (normalitás)

# a reziduálisok átlaga 0-e?
mean(model.1$residuals)

# rezidumok eloszlása Normal Q-Q ábrán:
qqnorm(resid(model.1), main = "Normal Q-Q Plot, model.1", col = "darkgrey")
qqline(resid(model.1), col = "dodgerblue", lwd = 2)

# a megfigyelések függetlenek-e egymástól? - Darwin - Watson teszt
dwtest(model.1)
# alpha = 0.05 < p = 0.9197 -> H0: elfogadva (függetlenség)

# diagnostic plot 
par(mfrow=c(2,2))
plot(model.1)

# a reziduálisok varianciája konstans-e: (homoszkedaszticitás)
# Residuals vs Fitted plot - Van heteroszkedaszticitás
bptest(model.1, studentize = TRUE)
# alpha = 0.05 -> p-value = 2.2e-16 < alpha -> H0:elutasítás (homoszkedaszticitás!)

# vizualizálva:
# reziduális értékek vs valós megfigyelések:
ggplot(,aes(x = fitted(model.1), y = csv$charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Becsült értékek vs valós megfigyelések")

# a magyarázó változók függetlenek-e egymástól (multikollinearitás)
vif(model.1)
# az összes VIF érték kisebb, mint 5, tehát a változók függetlenek egymástól a modellben.

###########################################
# Új modellek építése a model.1 alapján  #
##########################################

# A model.1-ben lévõ heteroszkedaszticitást okozhatja a lineáris kapcsolat meglétének hiánya.
# A model.1 reziduálisainak nem normális eloszlását okozhatják a kiugró értékek.

# kiugró értékek kiszûrése:
cooksd <- cooks.distance(model.1)
plot(cooksd, pch="*", cex=2, main="Kiugró megfigyelések a Cooks távolság alapján") 
abline(h = 5*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd > 5*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

kiugrok <- as.numeric(names(cooksd)[(cooksd > 5*mean(cooksd, na.rm=T))])  

# kiugró értékek szûrése az adathalmazból:
csv_no_out <- csv[-kiugrok, ]

# második modell felépítése, outlier szûréssel:
model.2 <- lm(charges ~ age + bmi + children + smoker_yes, data = csv_no_out)
summary(model.2)

# A kiugró értékek kiszûrésével a modell R^2-e 0.7469-rõl, 0.8027 ugrott.
# A modell magyarázó ereje növekedett.


# Linearitás vizsgálata a folytonos magyarázó változóknál (bmi, age):
g <- ggplot(csv, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()
print(g)

g2 <- ggplot(csv, aes(age^2, charges)) +
  geom_jitter(color= "blue", alpha = 0.5) +
  theme_light()

grid.arrange(g, g2, ncol=2, nrow=1)
# A kornál volt egy kis görbe a diagramon, tehát feltételezhetjük, hogy nem teljesen lineáris a kapcsolat a kor
# és a költségek közt.
# Érdemes a kor négyzetet használni.

model.3 <- lm(charges ~ age^2 + bmi + children + smoker_yes, data = csv_no_out)
summary(model.3)
par(mfrow=c(2,2))
plot(model.3)

# interakciók feltárása az adatokban:
ggplot(csv, aes(x = bmi, y = charges, col = smoker)) +
  geom_point()
# akik dohányoznak, nekik nagyobb a BMI indexük is, ezáltal nagyobbak a költségeik.

# interakciós változó beemelése a modellbe:
model.4 <- lm(charges ~ age^2 + I(smoker_yes*bmi) + children, data = csv_no_out)
summary(model.4)
par(mfrow=c(2,2))
plot(model.4)
# az interakciós változó révén a magyarázó erõ 0.8638-ra nõtt! 
# Az I() fv kiszedi az eredeti változókat, így nem lesz multikollinearitás a modellbe.
# A heteroszkedaszticitás mértéke is csökkent.

# A reziduálisok eloszlása normális, 0 átlaggal:
hist(model.4$residuals)

shapiro.test(model.4$residuals)
# Shapio-Wilk teszt: alpha = 0.05 > p = 2.2e-16 -> H0:elutasítás (normalitás)

# A heteroszkedaszticitás mértéke csökkent.
bptest(model.4)

# Multikollinearitás a modellben - nincs
vif(model.4)

# Model.4 által adott becslés és a reziduálisok hozzáadása a dataframe-hez:
csv_no_out['yhat'] <- predict(model.4)
csv_no_out['residuals'] <- csv_no_out$charges - csv_no_out$yhat
csv_no_out[ ,c("charges", "yhat", "residuals")]


#############################
# Modellek összehasonlítása #
#############################

# ANOVA - illeszkedésvizsgálatra.
aov(model.0) # Residual standard error: 6069
aov(model.1) # Residual standard error: 6092
aov(model.2) # Residual standard error: 5106
aov(model.3) # Residual standard error: 5106
aov(model.4) # Residual standard error: 4244

# A model.4 standard errorja a legkisebb ()


###########################
# Model.4 interpretációja #
###########################

# regressziós modell használata interpolációra:
x1 <- data.frame(age = 19,
                 bmi = 27.9,
                 children = 0,
                 smoker_yes = 1)
print(paste0("Az x1 egyed egészségügyi költsége ($): ", round(predict(model.4, x1), 2)))

x2 <- data.frame(age = 56,
                 bmi = 36.7,
                 children = 2,
                 smoker_yes = 1)
print(paste0("Az x1 egyed egészségügyi költsége ($): ", round(predict(model.4, x2), 2)))
