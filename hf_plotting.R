###############################
#11.óra: Adatvizualizáció     #
###############################

####################################Sima scatter plot######################################
#hasznos anyag:
#https://www.statmethods.net/advgraphs/axes.html
library(datasets)
data(iris)
colnames(iris)

szinek <- c("blue","green3","red")
plot(iris$Sepal.Length, iris$Petal.Length, 
     main="Edgar Anderson's Iris Data", 
     xlab = "Sepal.Length",
     ylab = "Petal.Length",
     col = szinek[iris$Species],
     col.axis = 'azure4',
     col.lab = 'azure4',
     col.ticks = "white",
     pch = 19,
     panel.first = grid(col = 'lightgray', lty = 7))
     box(col = "white")
     par(mar=c(4.5, 4.5, 4.5, 7.5), xpd = F)
     legend(8, 7, legend = unique(iris$Species),  col = szinek,
            pch = 19, bty = "n", xpd = T)

#https://benfradet.github.io/blog/2014/04/30/Display-legend-outside-plot-R
#par(mar=): margó beállítása
#panel.first: a vizualizációk layerekbõl épül fel így a sorrendet állthatjuk utólagosan.
#színezés másképp (unclass): c("blue","green3","red")[unclass(iris$Species)]
#pch: a pontok típusát lehet megadni.

########################################GGplot2###################################################
#http://bl.ocks.org/ramnathv/raw/10012123/example.html
#https://www.mailman.columbia.edu/sites/default/files/media/fdawg_ggplot2.html
library(ggplot2)
data("midwest", package = "ggplot2")
str(midwest)
unique(midwest$county)
nrow(midwest)

#objektumként elmenthetõ egy vizualizáció:
#alap (üres)
g <- ggplot(midwest, aes(x=area, y=poptotal))
g
str(g) #lista az objektum
#mûveletek is végezhetõek vele:
#pontok hozzáadása:
g2 <- g + geom_point()
g2
str(g2)
#lineáris regresszió hozzáadása:
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm", se = FALSE)  
#se = FALSE hibahatár leszedése
g
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="loess", se = FALSE)
g
?geom_smooth
#kiugró értékek kezelése (leszedni az axis alapján, átslálázás):
# Ebben az esetben TÖRLÖM A PONTOKAT:
g + xlim(c(0, 0.1)) + ylim(c(0, 1000000))
# Ebben az esetben zoomulunk: (nem dobjuk el az adatokat)
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")  # 
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in
plot(g1)
#cím, label hozzáadása:
g1 + labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
#szín, méret:
#színezés állam szerint: ehhez kell az aes fv, mely a df-en belüli oszlopra hivatkozik:
gg <-ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +   
  geom_smooth(method="lm", col="firebrick") +  	
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
gg + theme(legend.position="None") 

#formázás -> színvilág átállítása:
?theme
library(RColorBrewer)
gg + scale_colour_brewer(palette = "Dark2")

#labelek átállítása:
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11])

#tengelyek tükrözése:
gg + scale_x_reverse()

#sprintf: (C típusú függény)
sprintf("%s is %f feet tall\n", "Sven", 7.1)
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) + scale_y_continuous(breaks=seq(0, 1000000, 200000), 
                                                                                                                       labels = function(x){paste0(x/1000, 'K')})
#beépített téma meghívása:
?theme_set
theme_set(theme_dark())
gg
#apply parancs mégegyszer:
df <- data.frame(a=0:10, b=letters[1:11])
df
paste0(df$a, "%", df$b)
#ez egyenlõ:
apply(df, 1, function(x) paste0(x[1], "%", x[2]))
#ez egyenlõ:
apply(df, 1, function(x) sprintf("%0.1f%%%s", as.numeric(x[1]), x[2]))
      