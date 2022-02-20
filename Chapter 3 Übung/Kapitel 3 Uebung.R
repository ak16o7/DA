
# Chapter 3 Exercise ------------------------------------------------------

options(scipen=999)
RNGkind(sample.kind="Rounding")


ggplot(data=BostonHousing, geom_histogram(binwidth=5))
install.packages("ggplot2")
library(ggplot2)

# Problem Set 1 -----------------------------------------------------------


#b)
#aes gibt an wie die Beschriftung des Graphen sein soll
qplot(BostonHousing$MEDV, geom="histogram")
ggplot(data=BostonHousing, aes(BostonHousing$MEDV)) + geom_histogram(binwidth = 1, fill="purple")
ggplot(data=BostonHousing, aes(MEDV)) + geom_histogram(binwidth = 5, fill="purple")
ggplot(data=BostonHousing, aes(MEDV)) + geom_histogram(binwidth = 10, fill="purple")
ggplot(data=BostonHousing, aes(MEDV)) + geom_histogram(binwidth = 200, fill="purple")
ggplot(data=BostonHousing, aes(MEDV)) + geom_histogram(binwidth = 1, fill="purple", alpha=0.5)
# alpha macht balken transparent

ggplot(data=BostonHousing, aes(MEDV)) + geom_histogram(binwidth = 1,alpha=0.4, fill="purple")

#c)Boxplot erstellen
#in data frame umwandeln
housing.df <- data.frame(BostonHousing)
#CHAS in kategoriale Variable (faktor) umwandeln, da sie nur 1 und 0 annehmen kann, R aber denkt dass dies eine kontinuierliche variable ist
housing.df$CHAS <- as.factor(housing.df$CHAS)


str(housing.df)

ggplot(data=housing.df, aes(y=MEDV, x=CHAS)) + geom_boxplot(fill="orange", alpha=0.6) + stat_boxplot(geom="errorbar", width=0.3)

#man sieht p25 und p75
#man sieht strich durch die box: Median 
#über der box: Outlier 
#man sieht das Hauspreise (MEDV) im median für Häuser höher liegen, die im Charles River sind


#nun barchart erstellen und Mittelwerte anzeigen

ggplot(housing.df, aes(x=CHAS, y=MEDV))+geom_bar(stat="summary", fun=mean, fill="purple", alpha=0.7)
#fun=mean = funktion mean

#d) colorcode scatterplot by CHAS, farblich hervorheben was am fluss liegt und was nicht 
ggplot(housing.df, aes(x=DIS, y=MEDV))+geom_point(color="purple",alpha=0.3) + theme_bw()

#e)  
ggplot(housing.df, aes(x=DIS, y=MEDV, color=CHAS))+geom_point(alpha=0.3) + theme_bw() #color raus, damit colorcoding funktioniert


#f) Lineare Regression einfügen in Boxplot
ggplot(housing.df, aes(x=LSTAT, y=MEDV))+geom_point(color="purple",alpha=0.3) + theme_bw() + geom_smooth(method='lm', formula=y~x+I(x^2),se=T)
#hier parabelförmige regression I(x^2), da wegen den Daten sinnvoll 
#mit I(x) wäre es ein simple linear regression model (SLR)
#wenn se=T, dann werden zusätzlich noch Konfidenzintervalle angezeigt. am ende mehr streuung, da weniger Daten verfügbar


# Problem 2 -----------------------------------------------------------

#a)
mean(housing.df$MEDV)
min(housing.df$MEDV)
max(housing.df$MEDV)
sd(housing.df$MEDV)
summary(housing.df$MEDV)

 
#b)
install.packages("psych")
library(psych)
describe(housing.df) 
#CHAS* , das sternchen heißt, dass es eine Faktorvariable (Binärvariable) ist

describeFast(housing.df)


#c)
#mehrere möglichkeiten, Spaltennahmen zu ändern.
#Namen von Zeile und Spalte ändern 
names(housing.df)
names(housing.df)[14] <- "CAT.MEDV"
#oder
housing.df$CAT.MEDV <- housing.df$CAT..MEDV
str(housing.df$CAT.MEDV)
housing.df$CAT.MEDV <- as.factor(housing.df$CAT.MEDV)
levels(housing.df$CAT.MEDV)
levels(housing.df$CHAS)

#Den levels nun Werte bzw. Namen zuweisen. Boolean Variable, die entweder 0 oder 1 ist. 
#Bei CAT.MEDV also 1 = cheap und 0 = expensive

str(housing.df$CAT.MEDV)
#man erkennt dass die Faktoren als 1 für 0 und 2 für 1 gespeichert sind
#dies liegt daran, dass der Faktor als Vektor (0,1) gespeichert ist. also erster Wert im Vektor =1, zweiter Wert =2
  

levels(housing.df$CAT.MEDV)[1] <- "cheap"
levels(housing.df$CAT.MEDV)[2] <- "expensive"
#nun steht statt 0 und 1, cheap und expensive

#nun für CHAS
levels(housing.df$CHAS)[1] <- "Nicht am Fluss"
levels(housing.df$CHAS)[2] <- "Am Fluss"

#d) table MEDV und CHAS
table(housing.df$CAT.MEDV, housing.df$CHAS)

