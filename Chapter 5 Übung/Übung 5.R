rm(list=ls())
options(scipen=999)
RNGkind(sample.king = "Rounding")

### Kapitel 5 Übung ###
#a) "interessieren" -> was würden die Kunden bezahlen?
#   y =^ Preis 

#c)
setwd("C:\\Users\\Kaan\\Desktop\\Data Analytics\\Chapter 2 Übung")
df <- read.csv("ToyotaCorolla.csv")
colnames(df) <- tolower(colnames(df)) #lower-case statt upper-case
df <- subset(df, select=c("price", "fuel_type", "km", "hp")) #wählt
#bestimmte Spalten aus und entfernt die restlichen
df$fuel_type = tolower(df$fuel_type) #die einzelnen Werte werden
#lower case


#d)
table(df$fuel_type) #Aufzählung der verschiedenen Ausprägungen der
#dummy Variable. Wie häufig kommt diesel vor, wie häufig petrol,..?

mean(df$price[df$fuel_type=="cng"]) 
mean(df$price[df$fuel_type=="petrol"])#Durchschnittlicher Preis für
#die Petrol Autos
mean(df$price[df$fuel_type=="diesel"])
# man erkennt: Durchschnittlicher Preis von Diesel am teuersten, 
#dann petrol, dann cng

#e) i)
library(tidyverse)
#Boxplot erstellen
ggplot(df, aes(y = price, x = fuel_type)) + geom_boxplot()
#größe der weißen Box ist die Varianz. Diesel zB viel höhere Varianz
#als cng und petrol. 
#Schiefe der Verteilung: weiße Box mit Länge der Linien vergleichen.
#Wenn Linien gleich lang sind oben und unten, dann ist Verteilung eher
#symmetrisch. Wenn z.B. untere Linie kurz und obere länger und man noch
#Ausreißer hat, dann ist diese Verteilung rechtsschief (diesel)
#Durchgezogene Linie in der weißen Box:= Median. cng und Diesel
#zB gleicher Median, aber unterschiedliche Durchschnittswerte (aufgrund der Schiefe von diesel)
#Varianz: relativ homogene Streuung bei cng. aber große Streuung
#bei Diesel. Hier scheinen wohl noch andere Variablen starken Einfluss
#Auf den Preis von Dieselfahrzeugen zu haben


median(df$price[df$fuel_type=="diesel"])
### Recap on Boxplot ###
#1. Boxplot shows the 25th, 50th and 75th percentile of the distribution
#unterer Rand der Box=25th percentile
#Strich durch die Box=50th percentile, oder Median
#Oberer Rand der Box= 75th percentile 
#25th percentile von cng müsste ungefähr 8000 liegen. überprüfen:
library(Hmisc)
describe(df$Price[df$Fuel_Type=="CNG"])

# ii)

#Ist die Varianz (grob) konstant?
#Nein, die Varianz ist unterschiedlich. dh., dass innerhalb der 
#Kraftstoffarten die Autos entweder homogen oder heterogen sind
#Also bei cng waren die Autos relativ homogen, während sich die
#Diesel Autos wahrscheinlich noch bei anderen Charakteristika unterscheiden
#zB haben Diesel Autos auch eine höhere Varianz in Bezug auf km, was
#ebenso die erhöhte Varianz des Preises von Diesel-Autos erklären kann 

# iii)

#warum kein Streudiagramm?
ggplot(df, aes(y = price, x = fuel_type)) + geom_point()
# -> deutlich weniger Informationen als beim Boxplot. Beim Boxplot kann
#man Median, interquartilsabstand,etc ablesen,
#und man sieht, ab wann Ausreißer überhaupt definiert sind(wenn sie 
#über dem weißen Kasten liegen)

# deswegen sollte Boxplot immer einem Scatterplot bevorzugt werden,
# wenn eine der Variablen kategorial ist. Bei nicht-kategorialen Variablen,
# können Scatterplots mehr Sinn machen 
# Visualization of data:
# numeric vs. numeric: scatterplot. e.g. price vs. km
ggplot(df, aes(y = price, x = km)) + geom_point() + geom_smooth(method='lm')

# numeric vs. categorical: boxplot. e.g. price vs. fuel type

#f) 
model1 <- lm(price ~ fuel_type, df)

#i)
summary(model1)
#aufgrund der multikollinearität wird hier eine Kraftstoffart (cng) 
#ausgelassen. wenn also diesel und petrol = 0, kann man davon ausgehen,
#dass cng=1 

#ii) Schätzmodell:
price = beta_0 + beta_1*diesel + beta_2*petrol + e

# Vorhersagemodell:
prîce = 9421.2 + 1873.4*diesel + 1258.1*petrol  #hier kein Fehlerterm

# Für cng-Autos zahlen Menschen durchschnittlich:
# 9421.2 + 1873.4*0 + 1258.1*0
#=9421.2

# Für diesel-Autos zahlen Menschen durchschnittlich:
# =11294.6 

## Für benzin-Autos zahlen Menschen durchschnittlich:
# =10679.3

### iii) Vergleich mit den Mean Werten weiter oben, man erkennt: ##
# die vorhergesagten Preise entsprechen den Durchschnittspreisen
#Dies ist immer dann so, wenn man nur eine Kategorialvariable im Modell hat

#g) i)

model2 <- lm(price ~ ., df) #punkt, um alle Variablen anzeigen zu lassen
summary(model2)

# Vergleich zum einfachen Modell:
prîce = 9421.2 + 1873.4*diesel + 1258.1*petrol

# erweitertes Modell:
prîce = 7212.95 + 4293.6*diesel - 1782.3*petrol - 0.06*km + 88.9*hp
#Diesel deutlich höher, wenn mehrere Variablen hinzukommen, und für Benzin deutlich geringer (sogar negativ)
#Dieser Unterschied bzw diese Veränderung kann dadurch kommen,
#dass die Variablen Diesel und Benzin mit anderen Variablen korrelieren.
#wenn man diese Variablen nicht mit ins Modell aufnimmt, fließen sie in den Fehlerterm
#dies führt zu verzerrten Koeffizienten

# Benziner: Aus dem einfachen Modell erkennt man, dass der Benzin Koeffizient deutlich zu hoch ist
#wenn der Wert also sinkt, scheint die Benzin Variable mit den positiven Eigenschaften eines Autos zu korrelieren 
#d.h. weniger km und/oder mehr hp. Im einfachen Modell fließen diese Variablen "verdeckt" mit hinein und beeinflussen die Koeffizienten.
#Da der Benzin Wert im einfachen Modell höher ist, scheint es so, als hätten Benziner tendenziell mehr PS und weniger negative Effekte wie "hohe KM"

#Diesel: Wert ist im einfach Modell zu niedrig
#dh Diesel korreliert mit negativen Eigenschaften
# dh mehr km und weniger hp. Im einfachen Modell wird der Koeffizient von
#Dieselfahrzeugen unterschätzt

#Daraus ergeben sich Hypothesen:
#y=km
summary(lm(km~fuel_type, df))
#Man erkennt, Benziner hat deutlich weniger km als ein cng auto
#Die hypothese, dass Dieselfahrzeuge tendenziell mehr km haben,
#muss jedoch verworfen werden, da negative Schätzung (-5888)

#nun das gleiche für PS:
summary(lm(hp~fuel_type, df))
# Hypothese war ja, dass Benziner im Datensatz mehr PS haben
# laut Regression stimmt dies aber nicht, da Benzin Koeffizient negativ ist und nur leicht signifikant
# Aber die Hypothese dass Diesel Autos tendenziell weniger PS haben, kann man annehmen (-31.465) und signifikant ***

#Dies erklärt, warum die Koeffizienten im einfachen und erweiterten Modell so unterschiedlich sind
#Benziner haben weniger km, deswegen korreliert Benziner mit den kilometern, dementsprechend macht es sinn dass der Benzin-Koeffizient im erweiterten Modell sinkt


#iii) Schätzmodell:
price = beta_0 + beta_1*diesel + beta_2*benzin + beta_3*km + beta_4*hp + e
#iv) Vorhersagemodell:
prîce = 7212 +4293*diesel -1782*petrol -0.06*km +88*hp
# Wie viel werden die Menschen nach dem Modell für cng Autos mit 100 PS und 10000 km laufleistung zahlen?
gegeben: cng = 1, hp=100, km=10000
-> prîce = 7212 -0.06*10000 + 88*100 = 15412??? wird vorausgesagt


# Zusatz zu Vorlesung 5, aus der Übung zu Vorlesung 5 ---------------------

setwd("C:/Users/Kaan/Desktop/Data Analytics/Chapter 2 Übung")
car.df <- read.csv("ToyotaCorolla.csv")
library(Hmisc)
describe(car.df$HP) #Mit describe Funktion erhält man ersten Überblick über die Variable

#Scatterplot from the lecture

ggplot(car.df, aes(y= Price, x=HP)) +   geom_point() +  expand_limits(x=0, y=0) + #Expand limits lässt Graph bei x=0 und y=0 anfangen
                                       stat_smooth(method = 'lm', se = F) #fügt Regressionsgerade hinzu

# in the following, we'll only use the first 1000 observations:
car.df <- car.df[1:1000,]

#select variables for regression:
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)

#partition data:
set.seed(1) # set seed for reproducing the partition
train.index <- sample((1:1000), 600)

train.df <- car.df[train.index, selected.var] #zufällig gewählte Zeilen (sample) werden in den train.df dataframe geschrieben, dabei werden nur bestimmte Variablen (3,4,7,..) übernommen.
valid.df <- car.df[-train.index, selected.var] #zieht die vorher partitionierte train.index aus dem Datensatz ab

#use lm() to run a linear regression of Price on the training set
#Zuerst, damit man Dezimalzahlen erhält statt e+02,.. 
options(digits=2)

car.lm.simple <- lm(Price ~ HP,data=train.df)
summary(car.lm.simple)
#ALso: im einfachen Modell lautet: wenn man HP um 1 erhöht, steigt der Preis um 86.57


#Prediction: Apply the regressed simple model onto the validation data  
Price.pred.simple <- predict(car.lm.simple, valid.df) #

data.frame(       'HP' = valid.df$HP,
                  'price_hat' = price_hat <- Price.pred.simple,
                  'price' = price <- valid.df$Price,
                  'error' = price - price_hat)[82:90,]

#Auto nummer 200 hatte 110 HP, der geschätzte Preis (einzig auf Basis der HP geschätzt) war 12540 und der echte Preis ist 11950.
#Also error von -590 (overestimation)

library(forecast)
accuracy(valid.df$Price, Price.pred.simple)


# Multiple Regression -----------------------------------------------------

car.lm <- lm(Price ~., data=train.df)

#use options() to ensure numbers are not displayed in scientific notations
#zB options(digits=2), oder options(scipen=999)
options(scipen=999)
summary(car.lm)
#Man erkennt, dass HP gesunken ist auf 37.258, d.h. im einfachen Modell hat man den Effekt von HP überschätzt 
#Dh im einfachen Modell hat der Koeffizient HP mit irgendwelchen anderen abhängigen Variablen korreliert, was den Effekt verfälscht hat. Dies wird im erweiterten Modell aufgehoben


#use predict() to make predictions on a new set
car.lm.pred <- predict(car.lm, valid.df) #here we use the validation dataset
options(scipen=999, digits = 0)

some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20] #also tatsächlicher Preis - vorhergesagter Preis, für 20 Beobachtungen
#Nun die tatsächlichen und vorhergesagten Werte und Residuen darstellen:
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)
#shows a sample of predicted prices for 20 cars in the validation set
options(scipen=999, digits=3)
#use accuracy() to compute common accuracy measures
library(forecast)
accuracy(Price.pred.simple, valid.df$Price) #für einfaches Modell
accuracy(car.lm.pred, valid.df$Price) #für erweitertes Modell
#those measures can be used to compare models. Man erkennt, dass im erweiterten Modell die Fehler (RMSE) deutlich geringer sind

#how are residuals distributed?
#look at the histogram of residuals
all.residuals <- valid.df$Price - car.lm.pred

hist(all.residuals, breaks =25, xlab="Residuals", main ="")
#wenn Residuen normalverteilt scheinen, ist das ein gutes Zeichen 


# Variable Selection: -----------------------------------------------------


# Exhaustive Search: Reducing the Number of Predictors -------------------------------------------------------
#Die Anzahl der Variablen so reduzieren, dass nur relevante Variablen im Modell bleiben
# Use regsubsets() in package leaps to run an exhaustive search
# Unlike with lm, categorical predictors must be turned into dummies manually

library(leaps)

#create dummies for fuel type
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=train.df))

#replace Fuel_Type column with 2 dummies #column bind
train.df <- cbind(train.df[,-4], Fuel_Type[,])

head(train.df)

search <- regsubsets(Price ~ ., data=train.df, nbest=1, nvmax=dim(train.df)[2], method="exhaustive")
#Die Anzahl der Variablen wurde auf 11 reduziert, Da eine Variable die abhängige Variable (y) ist, und die andere Variable die rausfliegt ist die dummy Variable für cng, um Kollinearität zu vermeiden 

sum <- summary(search)
#show models
sum$which #Im Modell mit einer erklärenden Variable (1) hat man nur das Alter drin
#Im Modell mit zwei erklärenden Variablen (2) hat man das Alter und Weight drin 

#Welches Modell ist das Beste?
sum$rsq #R2 - schlechtes Maß, da je mehr Variablen desto "besser" wird das R2 rechnerisch
sum$adjr2 #Adjustiertes R2 -> Modell mit 8 unabhängigen Variablen scheint das Beste zu sein
sum$cp #Mallow's CP -> Auch hier erkennt man, dass das Modell mit 7 oder 8 Prädiktoren am Besten zu sein scheint (je kleiner desto besser)



# Alternative Methode um Anzahl der Variablen zu reduzieren: --------------

# as we changed the train.df in the meantime, we have to define car.lm again
# using the train.df including those changes:
car.lm <- lm(Price ~., data=train.df) #here we use the training dataset

# Backward selection: Erstmal werden alle Prädiktoren aufgenommen und dann wird sukzessiv die am wenigsten nützlliche Variable raus bzw die am wenigsten zum adj.R² beiträgt
#use step() to run stepwise regression
#set directions = to either "backward", "forward", or "both"

car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)
#man erkennt wieder 8 Variablen, also selbes Ergebnis wie bei der Exhaustive Search
   
#wie sind die Korrelationen zwischen den Variablen?
selected.vars <- names(car.lm.step$model)
round(cor(train.df[, selected.vars]), 2)
# Man erkennt,z.B. Alter und Preis sind negativ korreliert: Wenn Alter des Auto steigt, sinkt der Preis
# Wenn Gewicht des Autos steigt, steigt auch der Preis
# Wenn km steigt, sinkt der Preis
# Wenn PS steigt, steigt der Preis, etc

#Welche Variablen wurden rausgeschmissen?
#Met_color, Automatic, CC und Doors
#
#
#
### Apply model to validation dataset
#   First, we have to generate a dummy for Fuel_type again:
# create dummies for fuel type in validation dataset: 
Fuel_Type_val <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=valid.df))

#replace Fuel_Type column with 2 dummies 
valid.df <- cbind(valid.df[,-4], Fuel_Type_val[,])
head(valid.df)

car.lm.step.pred.back <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred.back, valid.df$Price)


# Forward Selection -------------------------------------------------------


# create a model with no predictors
car.lm.null <- lm(Price ~ 1, data=train.df)
summary(car.lm.null)

#use step() and direction="forward" to run forward regression
car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction= "forward") #lower: es soll mindestens der y-achsenabschnitt drin sein (car.lm.null) und car.lm max. (wo alle Variablen enthalten sind)
summary(car.lm.step)

#which variables were added?
# all 11 variables are included (Fuel_TypePetrol is collinear)

car.lm.step.pred.for <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred.for, valid.df$Price)


# Stepwise regression -----------------------------------------------------

car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)

#which variables were dropped/added?
# -> Gleich geblieben, also so wie in backward elimination

car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
#selbe accuracy wie zuvor
