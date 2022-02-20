##########################################################################
#
# Data Analytics Übung zu Kapitel 2
#
#########################################################################



rm(list=ls()) #löscht Environment komplett
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
# tibble:= data frame mit zusätzlichen Attributen

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
# sample funktion immer zsm mit set.seed ausführen
?set.seed
sample(c(1:100), 6)
sample(c(1:100), 6)


# element eines Vektors ein Gewicht zuweisen:
set.seed(1)
sample(c(1:6, 4000), 3, prob=c(100,300,100,1000,700,500,50))
#die Zahl 4 hat hier zb die höchste Wkt gezogen zu werden (1000)

setdiff(c(1:10), c(9,10,5,7)) #gibt an welche Elemente nicht vorkommen sollen, für test und validierungs sample wichtig

union(c(1:5), c(7:9, 100)) #verbindet elemente
##### Seite 2

#10)

# Normalisieren -----------------------------------------------------------


age <- c(25,56, 65, 32, 41, 49)
income <- c(49000, 156000, 99000, 192000, 39000, 57000)

library(scales)
#rescale:= Umskalieren auf das Intervall x bis y, z. B. 0 bis 1 für Normalisieren, ausgehend von dem Intervall, das die Variable aktuell hat


# Min-Max Normalisierung auf Werte zwischen 0 und 1 -----------------------


age_norm <-rescale(age, to=c(0,1), from=range(age, na.rm=TRUE)) #Skalierung fängt ab dem niedrigsten Wert in age an und endet mit dem höchsten 
income_norm <- rescale(income, to = c(0,1), from=range(income, na.rm=TRUE))

age
age_norm #der niedrigste Wert 25, kriegt den Wert 0, der höchste Wert 65 kriegt den Wert 1
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

# b) Dummy Variablen für Diesel/Benzin einfügen, da R sonst nicht damit arbeiten kann

library(dummies)
toyota.df.test <- dummy(toyota.df$Fuel_Type) #wandelt die variable Fuel_Type in dummy variable um
toyota.df <- ToyotaCorolla
toyota.df.test2 <- dummy(toyota.df$Color)

# c) Prepare dataset for data mining techniques of supervised learning by ...
# Partitionieren: 50% Trainingsdatensatz, 30% Validierungsdatensatz, 20% Testdatensatz

set.seed(1)

#50% Trainingsdatensatz: Funktion, welche Zeilen des Datensatzes sollen die jenigen Zeilen sein, die in den Trainingsdatensatz einfließen?

train.rows <- sample(row.names(toyota.df), dim(toyota.df)[1]*0.5)
#row.names , Die Zeilennummern des Datensatzes. Hier also eine Liste mit 1436 Zeilen
#dim(toyota.df) zweiter Teil der sample Funktion gibt an, wie viele Elemente man samplen möchte. 0.5= Die Hälfte der Zahlen soll train.rows zugewiesen werden
#-> hierdurch hat man zufällige Auswahl an Zeilennummern (aus denen der Trainingsdatensatz gebildet wird)

valid.rows <- sample(setdiff(row.names(toyota.df), train.rows), dim(toyota.df)[1]*0.3)

#setdiff(row.names(toyota.df), train.rows) = vom gesamten Datensatz toyota.df werden die Testdatenzeilen test.rows abgezogen, es bleiben 50% übrig.

test.rows <- setdiff(row.names(toyota.df), union(train.rows, valid.rows))
#Aus dem Datensatz werden Zeilennumern rausgenommen, die bereits schon im Train.rows oder valid.rows stehen)

#Nun mit den zufällig gewählten Zeilennumern die Trainings-,Valid und Test Dataframes bauen:
train.data <- toyota.df[train.rows,]
valid.data <- toyota.df[valid.rows,]                    
test.data <- toyota.df[test.rows,]

