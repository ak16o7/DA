
# CHAPTER 2  --------------------------------------------------------------

##########################################################################
#
# Data Analytics ?bung zu Kapitel 2
#
#########################################################################



rm(list=ls()) #l?scht Environment komplett
options(scipen = 999) #regelt dezimalstellen
RNGkind(sample.kind = "Rounding") #specifies the random number generator, so that results for mac and windows users are comparable

View(housing_df)
housing.df <- housing_df
rm(housing_df)
View(housing.df)

#8) 
housing.df[1,1]
housing.df[1,3]
median(housing.df$TAX)
median(housing.df$TAX)
summary(housing.df$TAX)
summary(housing.df)
mean(housing.df$FLOORS)
median(housing.df$FIREPLACE)
dim(housing.df$FIREPLACE)
dim(housing.df) #wie viele Zeilen, Spalten
head(housing.df)
tail(housing.df)
# tibble:= data frame mit zus?tzlichen Attributen

row.names(housing.df)
rownames(housing.df)


vector <- c(1,2,3)
vector
?sample
zufall <- sample(c(1:100), 6)
sample(c(1:100), 6)
sample(c(1:100), 6) # new random draws generate new sequences of random numbers

set.seed(90) #
zufall <- sample(c(1:100), 6)
# sample funktion immer zsm mit set.seed ausf?hren
?set.seed
sample(c(1:100), 6)
sample(c(1:100), 6)


# element eines Vektors ein Gewicht zuweisen:
set.seed(1)
sample(c(1:6, 4000), 3, prob=c(100,300,100,1000,700,500,50))
#die Zahl 4 hat hier zb die h?chste Wkt gezogen zu werden (1000)

setdiff(c(1:10), c(9,10,5,7)) #gibt an welche Elemente nicht vorkommen sollen, f?r test und validierungs sample wichtig

union(c(1:5), c(7:9, 100)) #verbindet elemente
##### Seite 2

#10)

# Normalisieren -----------------------------------------------------------


age <- c(25,56, 65, 32, 41, 49)
income <- c(49000, 156000, 99000, 192000, 39000, 57000)

library(scales)
#rescale:= Umskalieren auf das Intervall x bis y, z. B. 0 bis 1 f?r Normalisieren, ausgehend von dem Intervall, das die Variable aktuell hat


# Min-Max Normalisierung auf Werte zwischen 0 und 1 -----------------------


age_norm <-rescale(age, to=c(0,1), from=range(age, na.rm=TRUE)) #Skalierung f?ngt ab dem niedrigsten Wert in age an und endet mit dem h?chsten 
income_norm <- rescale(income, to = c(0,1), from=range(income, na.rm=TRUE))

age
age_norm #der niedrigste Wert 25, kriegt den Wert 0, der h?chste Wert 65 kriegt den Wert 1
#Alternativ:
age_norm2 <- (age-min(age))/(max(age)-min(age))
age_norm==age_norm2


# Standardisieren ---------------------------------------------------------
#Es wird so umskaliert, dass Mean = 0 und SD=1

age_std <- scale(age, center=TRUE, scale=TRUE)

income_std <- scale(income, center = TRUE, scale = TRUE)
#bei scale() wird so umskaliert, dass sich eine Verteilung ergibt, die den Mittelwert 0 und die Standardabweichung 1 hat.


### Alternativ:
age_std2 <- (age-mean(age))/sd(age) 

age_std2 == age_std


########### 11) #########

#a) load the data set ToyotaCorolla.csv

toyota.df <- read.csv("ToyotaCorolla.csv")

# b) Dummy Variablen f?r Diesel/Benzin einf?gen, da R sonst nicht damit arbeiten kann

library(dummies)
toyota.df.test <- dummy(toyota.df$Fuel_Type) #wandelt die variable Fuel_Type in dummy variable um
toyota.df <- ToyotaCorolla
toyota.df.test2 <- dummy(toyota.df$Color)

# c) Prepare dataset for data mining techniques of supervised learning by ...
# Partitionieren: 50% Trainingsdatensatz, 30% Validierungsdatensatz, 20% Testdatensatz

set.seed(1)

#50% Trainingsdatensatz: Funktion, welche Zeilen des Datensatzes sollen die jenigen Zeilen sein, die in den Trainingsdatensatz einflie?en?

train.rows <- sample(row.names(toyota.df), dim(toyota.df)[1]*0.5)
#row.names , Die Zeilennummern des Datensatzes. Hier also eine Liste mit 1436 Zeilen
#dim(toyota.df) zweiter Teil der sample Funktion gibt an, wie viele Elemente man samplen m?chte. 0.5= Die H?lfte der Zahlen soll train.rows zugewiesen werden
#-> hierdurch hat man zuf?llige Auswahl an Zeilennummern (aus denen der Trainingsdatensatz gebildet wird)

valid.rows <- sample(setdiff(row.names(toyota.df), train.rows), dim(toyota.df)[1]*0.3)

#setdiff(row.names(toyota.df), train.rows) = vom gesamten Datensatz toyota.df werden die Testdatenzeilen test.rows abgezogen, es bleiben 50% ?brig.

test.rows <- setdiff(row.names(toyota.df), union(train.rows, valid.rows))
#Aus dem Datensatz werden Zeilennumern rausgenommen, die bereits schon im Train.rows oder valid.rows stehen)

#Nun mit den zuf?llig gew?hlten Zeilennumern die Trainings-,Valid und Test Dataframes bauen:
train.data <- toyota.df[train.rows,]
valid.data <- toyota.df[valid.rows,]                    
test.data <- toyota.df[test.rows,]



# CHAPTER 3 ---------------------------------------------------------------


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
#?ber der box: Outlier 
#man sieht das Hauspreise (MEDV) im median f?r H?user h?her liegen, die im Charles River sind


#nun barchart erstellen und Mittelwerte anzeigen

ggplot(housing.df, aes(x=CHAS, y=MEDV))+geom_bar(stat="summary", fun=mean, fill="purple", alpha=0.7)
#fun=mean = funktion mean

#d) colorcode scatterplot by CHAS, farblich hervorheben was am fluss liegt und was nicht 
ggplot(housing.df, aes(x=DIS, y=MEDV))+geom_point(color="purple",alpha=0.3) + theme_bw()

#e)  
ggplot(housing.df, aes(x=DIS, y=MEDV, color=CHAS))+geom_point(alpha=0.3) + theme_bw() #color raus, damit colorcoding funktioniert


#f) Lineare Regression einf?gen in Boxplot
ggplot(housing.df, aes(x=LSTAT, y=MEDV))+geom_point(color="purple",alpha=0.3) + theme_bw() + geom_smooth(method='lm', formula=y~x+I(x^2),se=T)
#hier parabelf?rmige regression I(x^2), da wegen den Daten sinnvoll 
#mit I(x) w?re es ein simple linear regression model (SLR)
#wenn se=T, dann werden zus?tzlich noch Konfidenzintervalle angezeigt. am ende mehr streuung, da weniger Daten verf?gbar


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
#CHAS* , das sternchen hei?t, dass es eine Faktorvariable (Bin?rvariable) ist

describeFast(housing.df)


#c)
#mehrere m?glichkeiten, Spaltennahmen zu ?ndern.
#Namen von Zeile und Spalte ?ndern 
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
#man erkennt dass die Faktoren als 1 f?r 0 und 2 f?r 1 gespeichert sind
#dies liegt daran, dass der Faktor als Vektor (0,1) gespeichert ist. also erster Wert im Vektor =1, zweiter Wert =2


levels(housing.df$CAT.MEDV)[1] <- "cheap"
levels(housing.df$CAT.MEDV)[2] <- "expensive"
#nun steht statt 0 und 1, cheap und expensive

#nun f?r CHAS
levels(housing.df$CHAS)[1] <- "Nicht am Fluss"
levels(housing.df$CHAS)[2] <- "Am Fluss"

#d) table MEDV und CHAS
table(housing.df$CAT.MEDV, housing.df$CHAS)


# CHAPTER 4 ---------------------------------------------------------------


# Kapitel 4 ?bungsblatt ---------------------------------------------------

### Aufgabe 1 ###

#a) 

setwd("C:/Users/Kaan/Desktop/Data Analytics/Data Analytics Chapter 4 ?bung")
owner.df <- read.csv("ownerExample.csv")


#Unter Class ist der wahre Wert, ob die Person Eigent?mer ist oder nicht
#Unter Probability sind die Vorhersagen des Modells

#24 Beobachtungen
#2 Variablen

#b) Confusion Matrix mit den Cutoffs 0.25, 0.5, 0.75
library(caret)

confusionMatrix(table(ifelse(ownerExample$Probability>0.5, "owner", "nonowner"), ownerExample$Class), positive = "owner")  #positive = "owner" muss enthalten sein, damit R wei?, auf welcher Klassifizierung die Performance gemessen werden soll.sonst ist auch oft 
# unter positive= legt man die Klasse fest
# ifelse(Hier steht die Bedingung die gepr?ft werden muss, hier wenn zu trifft, hier wenn nicht zutrifft also else)
# ifelse funktion sagt also aus, wenn der Probability Wert gr??er 0.5, dann wird diese Beobachtung als Owner deklariert
# table -> tabelle wird eingef?gt, zuerst die Probability und dann die echten Werte


#Aussagen der Confusion Matrix:
#           nonowner owner
#nonowner       10     1
#owner           2    11
#Die zwei diagonalen Zellen geben die Anzahl der richtigen Klassifizierung an
#Zellen au?erhalb der Diagonalen sind die Fehlklassifizierungen 
#1 Eigent?mer ist f?lschlicherweise als Nicht-Eigent?mer klassifiziert worden
#2 Nicht-Eigent?mer sind f?lschlicherweise als Eigent?mer klassifiziert worden 
#In den Spalten sind also die wahren Werte und in den Zeilen die Probabilities


### Confusion Matrix Allgemein ###
#                  |<Actual Negative>  |  <Actual Positive>#
#Predicted Negative| True Negative     |  False Negative#
#Predicted Positive| False Positive    | True Positive#
#also
#                       10                    1
#                       2                     11


### Sensitivity ###
# = True Positive Rate: TP / (TP+FN)
#hier: 11/(11+1) = 0.9167

### Specifity ###
# = True Negative Rate: TN / (TN+FP)
#hier: 10/(10+2) = 0.833

### Accuracy ###
#(10+11)/24 = 0.875

###Misclassification rate = 1 - 0.875. Also der Anteil der Beobachtungen an den gesamten Beobachtungen, 
#die falsch klassifiziert worden sind
#(1+2)/24


# Verschiedene Cut-Off levels ---------------------------------------------

#0.25
confusionMatrix(table(ifelse(owner.df$Probability>0.25, "owner", "nonowner"), owner.df$Class), positive = "owner") 
#Nun hat man nat?rlicherweise mehr False und True Positives, daf?r sinkt die Accuracy (und somit Misclassification Rate) und die Specifity

#0.75, also Cut-Off erh?hen
confusionMatrix(table(ifelse(owner.df$Probability>0.75, "owner", "nonowner"), owner.df$Class), positive = "owner") 
#Hier ist die Accuracy sogar noch niedriger, d.h. zu hoch oder zu tief angesetzte Cut Off values k?nnen das Ergebnis stark beeinflussen
#also hier noch h?here Misclassification rate


#k?rzere Schreibweise: Schleifen in R:Funktion wird 3x ausgef?hrt, f?r 
for (i in c(0.25, 0.5, 0.75, 0.8, 0.95)){
  print(c("Cut-off Value: ", i))
  print(confusionMatrix(table(ifelse(ownerExample$Probability>i, "owner", "nonowner"), ownerExample$Class), positive = "owner"))
}


#c) Was w?rde mit der Accuracy, Sensitity und Specifity passieren, wenn man "non-owner" also positive Klasse festlegt?

#Bisher: Positive Klasse: "Owner":
confusionMatrix(table(ifelse(owner.df$Probability>0.5, "owner", "nonowner"), owner.df$Class), positive="owner")
#Accuracy: 0.875
#Sensitivity: 0.9167
#Specifity: 0.8333

#Nun: Positive Klasse: "nonowner":
confusionMatrix(table(ifelse(owner.df$Probability>0.5, "owner", "nonowner"), owner.df$Class), positive = "nonowner") 
#Accuracy bleibt gleich: 0.875
#Sensitivity und Specificity haben sich gedreht, da sich die True Positives und die True Negatives umgekehrt haben


#d) Plotten Sie die ROC-Kurve und berechnen Sie die AUC.
# Laden Sie das Paket pROC, verwenden Sie die Befehle: roc(), auc(), plot.roc()

install.packages("pROC")
library(pROC)

#F?r Plotten zuerst Character in Factor umwandeln:
ownerExample$Class <- as.factor(ownerExample$Class)

r <- roc(relevel(ownerExample$Class, ref="owner"), ownerExample$Probability)
#Mit relevel ordnet man die Faktoren. Also erste/positive Klasse benennen. Referenzklasse ist hier: "owner".
#Als zweites muss man die Variable mit den Wahrscheinlichkeiten noch angeben

#Nun ROC plotten
plot.roc(r)
#Man erkennt wieder den Tradeoff zwischen sensitivity und specifity, der durch die Festlegung des Cut-off values entsteht
#Je weiter die ROC Curve links oben ist, desto besser performen Modelle


# Area under the curve (AUC) ----------------------------------------------

auc(r) #Area under the curve: 0.9375
#Je n?her an 1, desto besser

#e) Beurteilen Sie die Leistung des Modells.
#Vergleich des Modells mit einem M?nzwurf und mit der naive Rule
#Verwenden der Option direction = ">" des Befehls roc(), um die
#AUCs f?r randomisierte Werte korrekt zu bewerten.

#erst neues Objekt erstellen um M?nzwurf abzubilden
#Simuliere 1000 zuf?llige Assignments um dies zu ?berpr?fen
coin.toss.auc <- c() #Geneiert leeren Vektor

for (i in 1:1000){
  r <- roc(relevel(ownerExample$Class, ref="owner"), runif(24), direction = ">") #direction = > damit es keinen Richtungswechsel im Vergleich gibt
  coin.toss.auc[i] <- auc(r) #Anh?ngen des auc an den Vektor
}

mean(coin.toss.auc)
#die funktion direction = ">" f?hrt dazu, dass die Zufallsziehung im Mittel n?her an 0.5 rankommt
#F?hrt dazu, dass es keinen Richtungswechsel im Vergleich gibt 



# Vergleich mit Naive Rules -----------------------------------------------

r <- roc(relevel(ownerExample$Class,ref="owner"), replicate(24,1), direction =">") #hier ist die Naive Rule dass man sagt dass jeder eine 1 hat
auc(r)

r <- roc(relevel(ownerExample$Class,ref="owner"), replicate(24,0.5), direction =">") #hier ist die Naive Rule dass man sagt dass jeder eine 0.5 hat
auc(r)

r <- roc(relevel(ownerExample$Class, ref="owner"), replicate(24,0), direction =">") #hier ist die Naive Rule dass man sagt dass jeder eine 0 hat
auc(r) 

plot.roc(r)


#Naive Rule diskriminiert nicht zwischen den Beobachtungen. 
#Naive Rule:= Zufallswert bei 0.5, Modell sollte immer besser als das performen.
# => Wenn ein Modell keine Diskriminierungsf?higkeit hat zwischen den einzelnen Beobachtungen, dann ist die AUC = 0.5
# => Wenn ein Modell diskriminieren kann zwischen den einzelnen Beobachtungen, sollte es eine AUC von > 0.5 haben 


# Aufgabe 2 ---------------------------------------------------------------

#a) 
install.packages("caret")
library(caret)
?lift

lift.df <- read.csv("liftExample.csv")
#24 Beobachtungen,
#Prob:= Vorhersage
#Actual:= (0,1)


#b) Erstellen Sie einen Lift-Chart

lift.df$actual <- as.factor(lift.df$actual)

lift.example <- lift(relevel(as.factor(actual), ref="1") ~ prob, data = lift.df) # Tilde und dann die Variable in der die errechnete Wahrscheinlichkeit ist
#Wieder Element ("1") zuweisen, actual soll als Faktorvariable interpretiert werden, die Reference Group ist "1"

xyplot(lift.example, plot="gain") #gains immer gleich

#c): Interpretation: Mithilfe der ersten 20% mit der h?chsten Wahrscheinlichkeit, identifiziert man ~35% der K?ufer, um 80% der K?ufer zu identifizieren muss man die ersten 50% (mit der h?chsten Wkt) der Kunden kontaktieren
# Modell performt also besser als Naive Rule (45 Grad Linie von 0,0 bis 100,100), dh das Modell diskriminiert besser als man es per Zufall mit Naive Rule k?nnte 
#Lift curve k?nnte etwas steiler sein, also optimierungsbedarf 

#d) Erstellen Sie ein dezile-wise Lift-Chart mit vier Gruppen
# Verwenden Sie den Befehl barplot() zum Plotten und den Befehl gains() aus
# dem Paket "gains", um die Zahlen zu berechnen. 

install.packages("gains")
library(gains)


lift.df$actual <- as.numeric(lift.df$actual)

gain <- gains(lift.df$actual, lift.df$prob, groups = 4)

barplot(gain$mean.resp/mean(lift.df$actual), #Hier wird die H?he der Balken berechnet: Mean Response (durchschnittliche Quote an 1ern innerhalb des Balkens) geteilt durch durchschnittliche Rate an 1ern im gesamten Sample
        names.arg = gain$depth,
        ylab = "Main Response",
        xlab = "Depth of File",
        main = "Decile-wise lift chart")
# names.arg weist dem Graphen die verschiedenen Gruppen zu (25, 60, 75, 100)
#mit 10 Gruppen sieht man, dass in den ersten F?llen nicht viel Diskriminierung passiert, da man nur 24 Beobachtungen hat. Man kann keine Dezile bilden

#decile-wise lift Chart: Gibt an, um wie viel fach h?her die Response rate
#innerhalb der festgelegten Gruppe ist, verglichen mit einem zuf?lligen Sample
#aus dem Datensatz

#e) Wenn man an die 25% der wahrscheinlichsten Personen eine Angebots-Email schickt
#dann ist die zu erwartende Response Rate 2x so hoch als wenn
#man ein eine zuf?llige Mail an eine 25%ige Zufallsstichprobe aus dem Datensatz schicken w?rde


# CHAPTER 5 ---------------------------------------------------------------

rm(list=ls())
options(scipen=999)
RNGkind(sample.king = "Rounding")

### Kapitel 5 ?bung ###
#a) "interessieren" -> was w?rden die Kunden bezahlen?
#   y =^ Preis 

#c)
setwd("C:\\Users\\Kaan\\Desktop\\Data Analytics\\Chapter 2 ?bung")
df <- read.csv("ToyotaCorolla.csv")
colnames(df) <- tolower(colnames(df)) #lower-case statt upper-case
df <- subset(df, select=c("price", "fuel_type", "km", "hp")) #w?hlt
#bestimmte Spalten aus und entfernt die restlichen
df$fuel_type = tolower(df$fuel_type) #die einzelnen Werte werden
#lower case


#d)
table(df$fuel_type) #Aufz?hlung der verschiedenen Auspr?gungen der
#dummy Variable. Wie h?ufig kommt diesel vor, wie h?ufig petrol,..?

mean(df$price[df$fuel_type=="cng"]) 
mean(df$price[df$fuel_type=="petrol"])#Durchschnittlicher Preis f?r
#die Petrol Autos
mean(df$price[df$fuel_type=="diesel"])
# man erkennt: Durchschnittlicher Preis von Diesel am teuersten, 
#dann petrol, dann cng

#e) i)
library(tidyverse)
#Boxplot erstellen
ggplot(df, aes(y = price, x = fuel_type)) + geom_boxplot()
#gr??e der wei?en Box ist die Varianz. Diesel zB viel h?here Varianz
#als cng und petrol. 
#Schiefe der Verteilung: wei?e Box mit L?nge der Linien vergleichen.
#Wenn Linien gleich lang sind oben und unten, dann ist Verteilung eher
#symmetrisch. Wenn z.B. untere Linie kurz und obere l?nger und man noch
#Ausrei?er hat, dann ist diese Verteilung rechtsschief (diesel)
#Durchgezogene Linie in der wei?en Box:= Median. cng und Diesel
#zB gleicher Median, aber unterschiedliche Durchschnittswerte (aufgrund der Schiefe von diesel)
#Varianz: relativ homogene Streuung bei cng. aber gro?e Streuung
#bei Diesel. Hier scheinen wohl noch andere Variablen starken Einfluss
#Auf den Preis von Dieselfahrzeugen zu haben


median(df$price[df$fuel_type=="diesel"])
### Recap on Boxplot ###
#1. Boxplot shows the 25th, 50th and 75th percentile of the distribution
#unterer Rand der Box=25th percentile
#Strich durch die Box=50th percentile, oder Median
#Oberer Rand der Box= 75th percentile 
#25th percentile von cng m?sste ungef?hr 8000 liegen. ?berpr?fen:
library(Hmisc)
describe(df$Price[df$Fuel_Type=="CNG"])

# ii)

#Ist die Varianz (grob) konstant?
#Nein, die Varianz ist unterschiedlich. dh., dass innerhalb der 
#Kraftstoffarten die Autos entweder homogen oder heterogen sind
#Also bei cng waren die Autos relativ homogen, w?hrend sich die
#Diesel Autos wahrscheinlich noch bei anderen Charakteristika unterscheiden
#zB haben Diesel Autos auch eine h?here Varianz in Bezug auf km, was
#ebenso die erh?hte Varianz des Preises von Diesel-Autos erkl?ren kann 

# iii)

#warum kein Streudiagramm?
ggplot(df, aes(y = price, x = fuel_type)) + geom_point()
# -> deutlich weniger Informationen als beim Boxplot. Beim Boxplot kann
#man Median, interquartilsabstand,etc ablesen,
#und man sieht, ab wann Ausrei?er ?berhaupt definiert sind(wenn sie 
#?ber dem wei?en Kasten liegen)

# deswegen sollte Boxplot immer einem Scatterplot bevorzugt werden,
# wenn eine der Variablen kategorial ist. Bei nicht-kategorialen Variablen,
# k?nnen Scatterplots mehr Sinn machen 
# Visualization of data:
# numeric vs. numeric: scatterplot. e.g. price vs. km
ggplot(df, aes(y = price, x = km)) + geom_point() + geom_smooth(method='lm')

# numeric vs. categorical: boxplot. e.g. price vs. fuel type

#f) 
model1 <- lm(price ~ fuel_type, df)

#i)
summary(model1)
#aufgrund der multikollinearit?t wird hier eine Kraftstoffart (cng) 
#ausgelassen. wenn also diesel und petrol = 0, kann man davon ausgehen,
#dass cng=1 

#ii) Sch?tzmodell:
price = beta_0 + beta_1*diesel + beta_2*petrol + e

# Vorhersagemodell:
pr?ce = 9421.2 + 1873.4*diesel + 1258.1*petrol  #hier kein Fehlerterm

# F?r cng-Autos zahlen Menschen durchschnittlich:
# 9421.2 + 1873.4*0 + 1258.1*0
#=9421.2

# F?r diesel-Autos zahlen Menschen durchschnittlich:
# =11294.6 

## F?r benzin-Autos zahlen Menschen durchschnittlich:
# =10679.3

### iii) Vergleich mit den Mean Werten weiter oben, man erkennt: ##
# die vorhergesagten Preise entsprechen den Durchschnittspreisen
#Dies ist immer dann so, wenn man nur eine Kategorialvariable im Modell hat

#g) i)

model2 <- lm(price ~ ., df) #punkt, um alle Variablen anzeigen zu lassen
summary(model2)

# Vergleich zum einfachen Modell:
pr?ce = 9421.2 + 1873.4*diesel + 1258.1*petrol

# erweitertes Modell:
pr?ce = 7212.95 + 4293.6*diesel - 1782.3*petrol - 0.06*km + 88.9*hp
#Diesel deutlich h?her, wenn mehrere Variablen hinzukommen, und f?r Benzin deutlich geringer (sogar negativ)
#Dieser Unterschied bzw diese Ver?nderung kann dadurch kommen,
#dass die Variablen Diesel und Benzin mit anderen Variablen korrelieren.
#wenn man diese Variablen nicht mit ins Modell aufnimmt, flie?en sie in den Fehlerterm
#dies f?hrt zu verzerrten Koeffizienten

# Benziner: Aus dem einfachen Modell erkennt man, dass der Benzin Koeffizient deutlich zu hoch ist
#wenn der Wert also sinkt, scheint die Benzin Variable mit den positiven Eigenschaften eines Autos zu korrelieren 
#d.h. weniger km und/oder mehr hp. Im einfachen Modell flie?en diese Variablen "verdeckt" mit hinein und beeinflussen die Koeffizienten.
#Da der Benzin Wert im einfachen Modell h?her ist, scheint es so, als h?tten Benziner tendenziell mehr PS und weniger negative Effekte wie "hohe KM"

#Diesel: Wert ist im einfach Modell zu niedrig
#dh Diesel korreliert mit negativen Eigenschaften
# dh mehr km und weniger hp. Im einfachen Modell wird der Koeffizient von
#Dieselfahrzeugen untersch?tzt

#Daraus ergeben sich Hypothesen:
#y=km
summary(lm(km~fuel_type, df))
#Man erkennt, Benziner hat deutlich weniger km als ein cng auto
#Die hypothese, dass Dieselfahrzeuge tendenziell mehr km haben,
#muss jedoch verworfen werden, da negative Sch?tzung (-5888)

#nun das gleiche f?r PS:
summary(lm(hp~fuel_type, df))
# Hypothese war ja, dass Benziner im Datensatz mehr PS haben
# laut Regression stimmt dies aber nicht, da Benzin Koeffizient negativ ist und nur leicht signifikant
# Aber die Hypothese dass Diesel Autos tendenziell weniger PS haben, kann man annehmen (-31.465) und signifikant ***

#Dies erkl?rt, warum die Koeffizienten im einfachen und erweiterten Modell so unterschiedlich sind
#Benziner haben weniger km, deswegen korreliert Benziner mit den kilometern, dementsprechend macht es sinn dass der Benzin-Koeffizient im erweiterten Modell sinkt


#iii) Sch?tzmodell:
price = beta_0 + beta_1*diesel + beta_2*benzin + beta_3*km + beta_4*hp + e
#iv) Vorhersagemodell:
pr?ce = 7212 +4293*diesel -1782*petrol -0.06*km +88*hp
# Wie viel werden die Menschen nach dem Modell f?r cng Autos mit 100 PS und 10000 km laufleistung zahlen?
gegeben: cng = 1, hp=100, km=10000
-> pr?ce = 7212 -0.06*10000 + 88*100 = 15412??? wird vorausgesagt


# Zusatz zu Vorlesung 5, aus der ?bung zu Vorlesung 5 ---------------------

setwd("C:/Users/Kaan/Desktop/Data Analytics/Chapter 2 ?bung")
car.df <- read.csv("ToyotaCorolla.csv")
library(Hmisc)
describe(car.df$HP) #Mit describe Funktion erh?lt man ersten ?berblick ?ber die Variable

#Scatterplot from the lecture

ggplot(car.df, aes(y= Price, x=HP)) +   geom_point() +  expand_limits(x=0, y=0) + #Expand limits l?sst Graph bei x=0 und y=0 anfangen
  stat_smooth(method = 'lm', se = F) #f?gt Regressionsgerade hinzu

# in the following, we'll only use the first 1000 observations:
car.df <- car.df[1:1000,]

#select variables for regression:
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)

#partition data:
set.seed(1) # set seed for reproducing the partition
train.index <- sample((1:1000), 600)

train.df <- car.df[train.index, selected.var] #zuf?llig gew?hlte Zeilen (sample) werden in den train.df dataframe geschrieben, dabei werden nur bestimmte Variablen (3,4,7,..) ?bernommen.
valid.df <- car.df[-train.index, selected.var] #zieht die vorher partitionierte train.index aus dem Datensatz ab

#use lm() to run a linear regression of Price on the training set
#Zuerst, damit man Dezimalzahlen erh?lt statt e+02,.. 
options(digits=2)

car.lm.simple <- lm(Price ~ HP,data=train.df)
summary(car.lm.simple)
#ALso: im einfachen Modell lautet: wenn man HP um 1 erh?ht, steigt der Preis um 86.57


#Prediction: Apply the regressed simple model onto the validation data  
Price.pred.simple <- predict(car.lm.simple, valid.df) #

data.frame(       'HP' = valid.df$HP,
                  'price_hat' = price_hat <- Price.pred.simple,
                  'price' = price <- valid.df$Price,
                  'error' = price - price_hat)[82:90,]

#Auto nummer 200 hatte 110 HP, der gesch?tzte Preis (einzig auf Basis der HP gesch?tzt) war 12540 und der echte Preis ist 11950.
#Also error von -590 (overestimation)

library(forecast)
accuracy(valid.df$Price, Price.pred.simple)


# Multiple Regression -----------------------------------------------------

car.lm <- lm(Price ~., data=train.df)

#use options() to ensure numbers are not displayed in scientific notations
#zB options(digits=2), oder options(scipen=999)
options(scipen=999)
summary(car.lm)
#Man erkennt, dass HP gesunken ist auf 37.258, d.h. im einfachen Modell hat man den Effekt von HP ?bersch?tzt 
#Dh im einfachen Modell hat der Koeffizient HP mit irgendwelchen anderen abh?ngigen Variablen korreliert, was den Effekt verf?lscht hat. Dies wird im erweiterten Modell aufgehoben


#use predict() to make predictions on a new set
car.lm.pred <- predict(car.lm, valid.df) #here we use the validation dataset
options(scipen=999, digits = 0)

some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20] #also tats?chlicher Preis - vorhergesagter Preis, f?r 20 Beobachtungen
#Nun die tats?chlichen und vorhergesagten Werte und Residuen darstellen:
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)
#shows a sample of predicted prices for 20 cars in the validation set
options(scipen=999, digits=3)
#use accuracy() to compute common accuracy measures
library(forecast)
accuracy(Price.pred.simple, valid.df$Price) #f?r einfaches Modell
accuracy(car.lm.pred, valid.df$Price) #f?r erweitertes Modell
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
#Die Anzahl der Variablen wurde auf 11 reduziert, Da eine Variable die abh?ngige Variable (y) ist, und die andere Variable die rausfliegt ist die dummy Variable f?r cng, um Kollinearit?t zu vermeiden 

sum <- summary(search)
#show models
sum$which #Im Modell mit einer erkl?renden Variable (1) hat man nur das Alter drin
#Im Modell mit zwei erkl?renden Variablen (2) hat man das Alter und Weight drin 

#Welches Modell ist das Beste?
sum$rsq #R2 - schlechtes Ma?, da je mehr Variablen desto "besser" wird das R2 rechnerisch
sum$adjr2 #Adjustiertes R2 -> Modell mit 8 unabh?ngigen Variablen scheint das Beste zu sein
sum$cp #Mallow's CP -> Auch hier erkennt man, dass das Modell mit 7 oder 8 Pr?diktoren am Besten zu sein scheint (je kleiner desto besser)



# Alternative Methode um Anzahl der Variablen zu reduzieren: --------------

# as we changed the train.df in the meantime, we have to define car.lm again
# using the train.df including those changes:
car.lm <- lm(Price ~., data=train.df) #here we use the training dataset

# Backward selection: Erstmal werden alle Pr?diktoren aufgenommen und dann wird sukzessiv die am wenigsten n?tzlliche Variable raus bzw die am wenigsten zum adj.R? beitr?gt
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


# CHAPTER 6 VORLESUNG -----------------------------------------------------


rm(list=ls())
options(scipen = 999)
RNGkind(sample.kind = "Rounding")
setwd("C:\\Users\\Kaan\\Downloads")

df <- read.csv("UniversalBank.csv")
colnames(df) <- tolower(colnames(df)) #lower case variables
df <- subset(df, select = -c(id, zip.code)) #the minus-sign drops columns id and zip

#Education is coded as integer, we want to recode it as factor
#Treat education as categorical (R will create Dummy variable)
str(df)
df$education <- factor(df$education, levels = c(1,2,3), labels = c("_undergrad", "_graduate", "advanced"))
table(df$education)
library(Hmisc)
describe(df)
#Man sieht zB dass Alter symmetrisch verteilt ist, da Median und Mean ungef?hr gleich sind

#partition data
set.seed(2)
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.6)
train.df <- df[train.index,]                      
valid.df <- df[-train.index,]

#run logistic regression
#use glm() (general linear model) with family = "binomial" to fit a logistic regression

logit.simple <- glm(personal.loan ~ income, data = train.df, family ="binomial")
summary(logit.simple)

#create probability line (loan of acceptence measured on income)
#Mit diesen 3 Codezeilen wird b0 und b1 gespeichert
str(logit.simple$coefficients)
(b0 <- logit.simple$coefficients[1])
(b1 <- logit.simple$coefficients[2])

#Logistische Regression Funktion (Wahrscheinlichkeit)   
p <- function(x) exp(b0 + b1*x) / (1 + exp(b0 + b1*x))

#Log funktion zeichnen:
ggplot(train.df, aes(y=personal.loan, x=income)) + geom_point() + stat_function(fun=p) + xlim(0,250) #stat_function zeichnet die Kurve der vorhergesagten Wahrscheinlichkeiten aus dem Modell ein

#Nun anhand des Modells Vorhersagen treffen auf dem Validierungsdatensatz bzw. Klassifikationen bestimmen
logit.simple.pred <- predict(logit.simple, valid.df, type ="response")

library(caret)
library(e1071)

classifications <- as.factor(ifelse(logit.simple.pred > 0.5, 1, 0))
confusionMatrix(classifications, as.factor(valid.df$personal.loan), positive = "1")


#Nun die Regression mit allen Variablen, erweitertes Modell
logit.reg <- glm(personal.loan ~ ., data = train.df, family = "binomial")

summary(logit.reg)
#Koeffizient f?r Income ist gestiegen mit steigender Anzahl von Variablen, das hei?t, man hat im einfachen Modell den Effekt von Income untersch?tzt

table(df$education)


### Nun Performance des erweiterten Modells bewerten ###
logit.reg.pred <- predict(logit.reg, valid.df, type ="response")

#Ersten 5 Beobachtungen anzeigen lassen:
data.frame(actual = valid.df$personal.loan[1:5], predicted = logit.reg.pred[1:5])
#Bei cut-off value von 0.5 w?rde ersten drei Beobachtungen korrekt als 0 klassifiziert werden
#vierte Beobachtung w?rde ebenfalls korrekt als 1 klassifiziert werden
#F?nfte Beobachtung w?rde jedoch f?lschlicherweise als 1 klassifiziert werden 

#Confusion Matrix f?r das erweiterte Modell:
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid.df$personal.loan), positive = "1")

#Man sieht, Accuracy ist im erweiterten Modell gestiegen, scheint also besser zu funktionieren als simples Modell wo nur income gemessen wurde


#nun cut-off von 0.3
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.3,1,0)), as.factor(valid.df$personal.loan), positive = "1")
#accuracy ist gesunken, dh cutoff von 0.5 performt besser


# Lift-chart --------------------------------------------------------------


library(gains)
gain <- gains(valid.df$personal.loan, logit.reg.pred, groups=10)

#plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$personal.loan))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type ="l", axes = TRUE)
#Naive Rule einzeichnen
lines(c(0, sum(valid.df$personal.loan))~c(0, dim(valid.df)[1]), lty=2)

#X-achse n?her beschriften
axis(side = 1, at = seq(from = 250, to = 2000, by = 250))
#Wenn man also 500 Leute zuf?llig pickt, hat man ~50 Annehmende, 
#aber wenn man das Modell benutzt, erh?lt man ~150 Annehmende


# Decile-wise chart -------------------------------------------------------


#compute decile and plot decile-wise chart
heights <- gain$mean.resp / mean(valid.df$personal.loan)
midpoints <- barplot(heights, names.arg= gain$depth, ylim= c(0,9),
                     xlab= "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# Wenn man also die oberen 10% der Kunden, die durch das erweiterte Modell als am wahrscheinlichsten den Kredit anzunehmen klassifiziert wurde, nimmt, erh?lt man 8-mal so viele Responder (Annehmende) im Vergleich zu einer zuf?lligen Ziehung von 10% des Datensatzes
# Wenn man die oberen 20% der durch das Modell am wahrscheinlichsten Bestimmten Kunden ausw?hlt, erh?lt man immer noch 1.5 mal so viele R?ckmeldungen wie durch zuf?llige Auswahl


# CHAPTER 6 UEBUNG --------------------------------------------------------

rm(list=ls())

### Chapter 6 Tut ###

#a)

# Predictive Model --------------------------------------------------------

setwd("C:\\Users\\Kaan\\Downloads")
df <- read.csv("UniversalBank.csv")
colnames(df) <- tolower(colnames(df))
df <- subset(df, select = c(income, family, mortgage)) #only select (keep) the necessary variables

#create dummy for mortgage y/n
df$has.mortgage <- ifelse(df$mortgage > 0, 1, 0) #IF the mortgage variable exceeds 0,then the has.mortgage variable should take in the value 1, ELSE zero 
describe(df)
#check has.mortgage variable -> on average ~30% of the people have a mortgage

df <- subset(df, select = -c(mortgage)) #remove mortgage variable since we only need has.mortgage

# what does the command subset do?
# -> selects AND reorders variables according to the order of the variables in the command

#b) see word doc
#c) 
set.seed(1)
train.index <- sample(1:dim(df)[1], dim(df)*0.6)
train.df <- df[train.index,]
valid.df <- df[-train.index, ]

#i. now estimate a logit model with all predictors on training data

glm(has.mortgage ~ ., data=train.df, family="binomial") ## use family binomial to tell R to not make a regression but to estimate a logit model 

#store the model for evaluation reasons
logit <- glm(has.mortgage ~ ., data=train.df, family="binomial")
summary(logit)

#ii. beta_hat_income =  -0.0000894 < 0 relation but insignificant (no ***)
#iii. beta_hat_family = 0.0298413 > 0 also insignificant
#beta_hat_family makes sense because the bigger the family, the larger the need for more houses and possibly a mortgage

#d) now, after generating the estimation model, generate a vector of predictions based on the validation data
# first, predict probabilities, and then set a threshold to make a prediction about the outcome variable. For example s=0.5
#
# 
predict(logit, valid.df, type = "response") #creates a prediction vector
#Apply the logit model to the validation data. Type = response gives out a response variable

#store in the logit predicted values
logit.pred <- predict(logit, valid.df, type = "response") #creates a prediction vector
logit.pred[1:5]
describe(logit.pred)
#very little variation in the predictions. only range from 0.3 (lowest) to 0.33 (highest)
#which could mean that the model is not doing a great job because its not discriminating well between people who need a mortgage and people who dont

#now, s has to lie between 0.3065270 and 0.3297601

#
