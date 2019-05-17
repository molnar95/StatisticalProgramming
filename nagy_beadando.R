#############################
#Stat_prog_1 nagybeadandó   #
#                           #
#############################

#1. szükséges library-k beolvasása/installálása:
library(ggpubr)
library(ggplot2)      #adatvizualizáció
library(RColorBrewer) #ggplot színeinek kiegészíése
library(tidyverse)    #adatok leválogatása
library(gridExtra)    #grafikonok összefûzése
library(grid)         #grafikonok összefûzése

#data.frame beolvasása:
rm(list = ls())
setwd("C:/Users/molna/Desktop/Egyetem/KRE/R programozás/")
csv <- read.csv("master.csv", check.names=FALSE, header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
#header nevek regexp-je:
newhead <- enc2native(colnames(csv))
newhead <- gsub("<[^>]+>", "", newhead)
colnames(csv) <- newhead
#rendben van-e a country változó: (igen)
colnames(csv[1])
colnames(csv)

###########################################
#feltáró statisztika (adatok felfedezése) #
###########################################
#12 változónk van:
length(colnames(csv))
#van-e NA értékünk?:
which(is.na(csv) == TRUE)

#####################################################################################
#a. ábra: Idõsor (x tengely: évek és y tengely: suicides/100k pop) vonalak országok #
#####################################################################################

#vizualizációhoz szükséges adatok leválogatása egy külön data frame-be:
df1 <- aggregate(cbind(population, suicides_no) ~ country + year, data = csv, sum)
rate <- (df1$suicides_no/df1$population)*100000
df1 <- cbind(df1, rate)

#101 ország vizualizációja:
#legend-ek eltávolításra kerültek (túl sok volt)
#színezés az öngyilkosságok arányában:
a <- ggplot(df1, aes(x=year, y=rate, group=country)) +
        geom_line(aes(col=rate)) + 
        geom_point(aes(col=rate)) +
        ggtitle("Suicide rates grouped by country from 1985 to 2016") +
        labs(y="Suicides per 100k", x="Year") +
        theme(legend.position="none") +
        scale_color_gradient(low="lightblue", high="darkblue")
a

#tidyverse package kipróbálása: (globális trend!)
x_axis_labels <- min(csv$year):max(csv$year)
global_average <- (sum(as.numeric(csv$suicides_no)) / sum(as.numeric(csv$population))) * 100000
a3 <- csv %>%
        group_by(year) %>%
        summarize(population = sum(population), 
                  suicides = sum(suicides_no), 
                  suicides_per_100k = (suicides / population) * 100000) %>%
        ggplot(aes(x = year, y = suicides_per_100k)) + 
        geom_line(col = "deepskyblue3", size = 1) + 
        geom_point(col = "deepskyblue3", size = 2) + 
        geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
        labs(title = "Global Suicides trend from 1985 - 2015",
             subtitle = "Average line value: 13.15",
             x = "Year", 
             y = "Suicides per 100k") + 
        theme(axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
              axis.text.y = element_text(color = "black", face = "bold"),
              axis.title = element_text(face="bold")) +
        scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)
a3

#101 túl sok volt, ezért a top15-öt válogattam le:
top15_list <- tail(sort((tapply(df1$rate, df1$country, mean)), decreasing= FALSE), 15)
top15 <- unlist(dimnames(top15_list))
unique(top15)
df <- df1[df1$country %in% top15, ]
head(df)
unique(df$country)

#top 15 ország vizualizációja:
x_axis_labels <- min(df$year):max(df$year)
a2 <- ggplot(df, aes(x=year, y=rate, group=country)) +
        geom_line(aes(col=country), size = 0.72) + 
        geom_point(aes(col=country)) +
        ggtitle("Suicide rates grouped by top 25 country from 1985 to 2016") +
        labs(y="Suicides per 100k", x="Year",
             subtitle = "Average line value: 26.33") +
        theme(legend.title = element_blank()) +
        theme(legend.direction = "horizontal", legend.position = "bottom",
              axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
              axis.text.y = element_text(color = "black", face = "bold"),
              axis.title = element_text(face="bold")) +
      geom_hline(yintercept = mean(df$rate), linetype = 2, color = "grey35", size = 1) +
      scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
      scale_color_manual(values = 
                           c("Guyana"="chocolate1",
                             "Austria"="cornflowerblue",
                             "Belgium"="cyan4",
                             "Japan"="darkgoldenrod1",
                             "Finland"="darkgray",
                             "Ukraine"="darkolivegreen",
                             "Estonia"="darkorange",
                             "Slovenia"="darkorchid",
                             "Kazakhstan"="darkred",
                             "Latvia"="darkseagreen4",
                             "Hungary"="darkslategray3",
                             "Belarus"="chocolate4",
                             "Sri Lanka"="darkviolet",
                             "Russian Federation"="deeppink3",
                             "Lithuania"="deepskyblue4"))

a2

#######################################################################################################
#c. ábra: Idõsor (x tengely: évek és y tengely: suicides/100k pop) vonalak országok CSAK NÕK (top 11) #
#######################################################################################################

#vizualizációhoz szükséges adatok leválogatása egy külön data frame-be:
df2 <- aggregate(cbind(population, suicides_no) ~ country + year + sex, data = csv, sum)
#nõk leválogatása:
female_df <- df2[which(df2$sex=='female'),]
head(female_df)
#check:
unique(female_df$sex)
#öngyilkossági ráta kiszámítása nõkre:
rate_female <- (female_df$suicides_no/female_df$population)*100000
female_df <- cbind(female_df, rate_female)
head(female_df)
#top 11 ország a nõi öngyilkossági ráta alapján:
top11_f_list <- tail(sort((tapply(female_df$rate_female, female_df$country, mean)), decreasing= FALSE), 11)
top11_f <- unlist(dimnames(top11_f_list))
unique(top11_f)
female_df_11 <- female_df[female_df$country %in% top11_f, ]
head(female_df_11)
unique(female_df_11$country)

#top 11 ország a nõi öngyilkossági adatok vizualizációja:
x_axis_labels <- min(female_df_11$year):max(female_df_11$year)
c <- ggplot(female_df_11, aes(x=year, y=rate_female, group=country)) +
        geom_line(aes(col=country), size = 0.72) + 
        #geom_point(aes(col=country)) +
        ggtitle("Female suicide rates grouped by country from 1985 to 2016") +
        labs(y="Female suicides per 100k", x="Year") +
        theme(legend.title = element_blank()) +
        theme(legend.direction = "horizontal", legend.position = "bottom",
              axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
              axis.text.y = element_text(color = "black", face = "bold"),
              axis.title = element_text(face="bold")) +
        scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
        scale_colour_brewer(palette = "Spectral")
c

##########################################################################################################
#b. ábra: Idõsor (x tengely: évek és y tengely: suicides/100k pop) vonalak országok CSAK FÉRFIAK (top 11)#
##########################################################################################################

#vizualizációhoz szükséges adatok leválogatása, aggregálása egy külön data frame-be:
male_df <- df2[which(df2$sex=='male'), ]
#check:
unique(male_df$sex)
#öngyilkossági ráta kiszámítása férfiakra:
rate_male <- (male_df$suicides_no/male_df$population)*100000
#két dataframe összefûzése:
male_df <- cbind(male_df, rate_male)
#top 11 ország a férfi öngyilkossági ráta alapján:
top11_m_list <- tail(sort((tapply(male_df$rate_male, male_df$country, mean)), decreasing= FALSE), 11)
top11_m <- unlist(dimnames(top11_m_list))
unique(top11_m)
#top 11 ország leválogatása a férfi adatbázisból egy új data.frame-be:
male_df_11 <- male_df[male_df$country %in% top11_m, ]
unique(male_df_11$country)

#top 11 ország a férfi öngyilkossági adatok vizualizációja:
x_axis_labels <- min(male_df_11$year):max(male_df_11$year) #évek leválogatása
b <- ggplot(male_df_11, aes(x=year, y=rate_male, group=country)) +
        geom_line(aes(col=country), size = 0.72) + 
        geom_point(aes(col=country)) +
        ggtitle("Male suicide rates grouped by country from 1985 to 2016") +
        labs(y="Male suicides per 100k", x="Year") +
        theme(legend.title = element_blank()) +
        theme(legend.direction = "horizontal", legend.position = "bottom",
              axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
              axis.text.y = element_text(color = "black", face = "bold"),
              axis.title = element_text(face="bold")) +
        scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
        scale_colour_brewer(palette = "Spectral")
b

################################################################################################
#d. ábra: Idõsor (x tengely: évek és y tengely: suicides/100k pop) vonalak generációk (top11)  #
################################################################################################

#vizualizációhoz szükséges adatok leválogatása, aggregálása egy külön data frame-be:
df3 <- aggregate(cbind(population, suicides_no) ~ year + generation, data = csv, sum)
unique(df3$generation)
#generációkra vonatkozó öngyilkossági ráta kiszámítása:
rate_gen <- (df3$suicides_no/df3$population)*100000
#két dataframe összefûzése:
df3 <- cbind(df3, rate_gen)

#Öngyilkossági adatok vizualizációja generációnként:
x_axis_labels <- min(df3$year):max(df3$year) #évek leválogatása
d <- ggplot(df3, aes(x=year, y=rate_gen, group=generation)) +
        geom_line(aes(col=generation), size = 0.72) + 
        geom_point(aes(col=generation), size = 2) +
        ggtitle("Suicide rates grouped by generation from 1985 to 2016") +
        labs(y="Suicides per 100k", x="Year",
             subtitle = "Average line value: 13.97") +
        theme(legend.title = element_blank()) +
        theme(legend.direction = "horizontal", legend.position = "bottom",
              axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
              axis.text.y = element_text(color = "black", face = "bold"),
              axis.title = element_text(face="bold")) +
        scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
        scale_colour_brewer(palette = "Spectral") +
        geom_text(data = df3, label= round(df3$rate_gen, 1), size = 3, check_overlap = TRUE,
                  colour="black")+
        geom_hline(yintercept = mean(df3$rate_gen), linetype = 2, color = "grey35", size = 1)
d

#######################################################################################################################
#e. ábra: (x tengely: országok (csak a 6 legnagyobb öngyilk. arányú) - y tengely: suicides/100k pop - csoportok: évek #
#######################################################################################################################

#top 6 ország az öngyilkossági ráta alapján:
top6_list <- tail(sort((tapply(df1$rate, df1$country, mean)), decreasing= FALSE), 6)
top6 <- unlist(dimnames(top6_list))
unique(top6)
df1_6 <- df1[df1$country %in% top6, ]
head(df1_6)
unique(df1_6$country)


###############################################################################################################################
#f. ábra:  (x tengely: országok (csak a 6 legnagyobb öngyilk. arányú) - y tengely: suicides/100k pop - csoportok: generációk) #
###############################################################################################################################

#vizualizációhoz szükséges adatok leválogatása és aggregálása egy külön data frame-be:
df4 <- aggregate(cbind(population, suicides_no) ~ country  + generation, data = csv, sum)
#Öngyilkossági ráta kiszámítása a leválogatott adatokból:
ratedf4 <- (df4$suicides_no/df4$population)*100000
#vektor dataframe-hez fûzése:
df4 <- cbind(df4, ratedf4)

#a dataframe-bõl a top 6 ország leválogatása, az öngyilkossági ráta alapján:
top6_list2 <- tail(sort((tapply(df4$ratedf4, df4$country, mean)), decreasing=FALSE), 6)
top6_2 <- unlist(dimnames(top6_list2))
unique(top6_2)
#top 6 ország leválogatása az eredeti data.frame-bõl:
df2_6 <- df4[df4$country %in% top6_2, ]
unique(df2_6$country)

#generációk sorbaállítása:
df2_6$generation <- factor(df2_6$generation, 
                          ordered = T, 
                          levels = c("G.I. Generation", 
                                     "Silent",
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

#top 6 ország öngyilkosságának vizualizációja generációra bontva:
f <- ggplot(df2_6, aes(country, ratedf4)) +   
            geom_bar(aes(fill = generation),  position="dodge", stat="identity", width=0.8, colour="black") +
            labs(title = "Suicides in top 6 country by generations from 1985 to 2016",
                 y = "Suicides per 100k",
                 x = "Country",
                 subtitle = "Average line value: 31.18") +
            theme(legend.title = element_blank(),
                  legend.direction = "horizontal", legend.position = "bottom",
                  axis.text.x = element_text(angle = 15, vjust = 0.5, face = "bold", color = "black"),
                  axis.text.y = element_text(color = "black", face = "bold"),
                  axis.title = element_text(face="bold")) +
            scale_fill_brewer(palette = "Set1") +
            #geom_text(aes(label=round(ratedf4,1)), vjust=1.6, color="black", position = position_dodge(0.9), size=1)
            geom_hline(yintercept = mean(df2_6$ratedf4), linetype = 2, color = "grey35", size = 1)
f

###########################################################################################
##########################A létrehozott vizualizációk összefûzése##########################
###########################################################################################
w1 <- grid.arrange(a3, a2, d, f, ncol=2, top= textGrob("Analyze Global Suicide Trends",gp=gpar(fontsize=20,font=3)),
             bottom="Data Source:\nhttps://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016/ ")

w2 <- w1 <- grid.arrange(a, b, c, ncol=2, top= textGrob("Analyze Global Suicide Trends",gp=gpar(fontsize=20,font=3)),
                         bottom="Data Source:\nhttps://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016/ ")

