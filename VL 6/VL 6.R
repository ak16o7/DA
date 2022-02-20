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
#Man sieht zB dass Alter symmetrisch verteilt ist, da Median und Mean ungefähr gleich sind

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
#Koeffizient für Income ist gestiegen mit steigender Anzahl von Variablen, das heißt, man hat im einfachen Modell den Effekt von Income unterschätzt

table(df$education)


### Nun Performance des erweiterten Modells bewerten ###
logit.reg.pred <- predict(logit.reg, valid.df, type ="response")

#Ersten 5 Beobachtungen anzeigen lassen:
data.frame(actual = valid.df$personal.loan[1:5], predicted = logit.reg.pred[1:5])
#Bei cut-off value von 0.5 würde ersten drei Beobachtungen korrekt als 0 klassifiziert werden
#vierte Beobachtung würde ebenfalls korrekt als 1 klassifiziert werden
#Fünfte Beobachtung würde jedoch fälschlicherweise als 1 klassifiziert werden 

#Confusion Matrix für das erweiterte Modell:
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

#X-achse näher beschriften
axis(side = 1, at = seq(from = 250, to = 2000, by = 250))
#Wenn man also 500 Leute zufällig pickt, hat man ~50 Annehmende, 
#aber wenn man das Modell benutzt, erhält man ~150 Annehmende


# Decile-wise chart -------------------------------------------------------


#compute decile and plot decile-wise chart
heights <- gain$mean.resp / mean(valid.df$personal.loan)
midpoints <- barplot(heights, names.arg= gain$depth, ylim= c(0,9),
                      xlab= "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# Wenn man also die oberen 10% der Kunden, die durch das erweiterte Modell als am wahrscheinlichsten den Kredit anzunehmen klassifiziert wurde, nimmt, erhält man 8-mal so viele Responder (Annehmende) im Vergleich zu einer zufälligen Ziehung von 10% des Datensatzes
# Wenn man die oberen 20% der durch das Modell am wahrscheinlichsten Bestimmten Kunden auswählt, erhält man immer noch 1.5 mal so viele Rückmeldungen wie durch zufällige Auswahl