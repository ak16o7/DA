
# Kapitel 4 Übungsblatt ---------------------------------------------------

### Aufgabe 1 ###

#a) 

setwd("C:/Users/Kaan/Desktop/Data Analytics/Data Analytics Chapter 4 Übung")
owner.df <- read.csv("ownerExample.csv")


#Unter Class ist der wahre Wert, ob die Person Eigentümer ist oder nicht
#Unter Probability sind die Vorhersagen des Modells

#24 Beobachtungen
#2 Variablen

#b) Confusion Matrix mit den Cutoffs 0.25, 0.5, 0.75
library(caret)

confusionMatrix(table(ifelse(ownerExample$Probability>0.5, "owner", "nonowner"), ownerExample$Class), positive = "owner")  #positive = "owner" muss enthalten sein, damit R weiß, auf welcher Klassifizierung die Performance gemessen werden soll.sonst ist auch oft 
# unter positive= legt man die Klasse fest
# ifelse(Hier steht die Bedingung die geprüft werden muss, hier wenn zu trifft, hier wenn nicht zutrifft also else)
# ifelse funktion sagt also aus, wenn der Probability Wert größer 0.5, dann wird diese Beobachtung als Owner deklariert
# table -> tabelle wird eingefügt, zuerst die Probability und dann die echten Werte


#Aussagen der Confusion Matrix:
#           nonowner owner
#nonowner       10     1
#owner           2    11
#Die zwei diagonalen Zellen geben die Anzahl der richtigen Klassifizierung an
#Zellen außerhalb der Diagonalen sind die Fehlklassifizierungen 
#1 Eigentümer ist fälschlicherweise als Nicht-Eigentümer klassifiziert worden
#2 Nicht-Eigentümer sind fälschlicherweise als Eigentümer klassifiziert worden 
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
#Nun hat man natürlicherweise mehr False und True Positives, dafür sinkt die Accuracy (und somit Misclassification Rate) und die Specifity

#0.75, also Cut-Off erhöhen
confusionMatrix(table(ifelse(owner.df$Probability>0.75, "owner", "nonowner"), owner.df$Class), positive = "owner") 
#Hier ist die Accuracy sogar noch niedriger, d.h. zu hoch oder zu tief angesetzte Cut Off values können das Ergebnis stark beeinflussen
#also hier noch höhere Misclassification rate
 

#kürzere Schreibweise: Schleifen in R:Funktion wird 3x ausgeführt, für 
for (i in c(0.25, 0.5, 0.75, 0.8, 0.95)){
  print(c("Cut-off Value: ", i))
  print(confusionMatrix(table(ifelse(ownerExample$Probability>i, "owner", "nonowner"), ownerExample$Class), positive = "owner"))
}


#c) Was würde mit der Accuracy, Sensitity und Specifity passieren, wenn man "non-owner" also positive Klasse festlegt?

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

#Für Plotten zuerst Character in Factor umwandeln:
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
#Je näher an 1, desto besser

#e) Beurteilen Sie die Leistung des Modells.
#Vergleich des Modells mit einem Münzwurf und mit der naive Rule
#Verwenden der Option direction = ">" des Befehls roc(), um die
#AUCs für randomisierte Werte korrekt zu bewerten.

#erst neues Objekt erstellen um Münzwurf abzubilden
#Simuliere 1000 zufällige Assignments um dies zu überprüfen
coin.toss.auc <- c() #Geneiert leeren Vektor

for (i in 1:1000){
  r <- roc(relevel(ownerExample$Class, ref="owner"), runif(24), direction = ">") #direction = > damit es keinen Richtungswechsel im Vergleich gibt
  coin.toss.auc[i] <- auc(r) #Anhängen des auc an den Vektor
}

mean(coin.toss.auc)
#die funktion direction = ">" führt dazu, dass die Zufallsziehung im Mittel näher an 0.5 rankommt
#Führt dazu, dass es keinen Richtungswechsel im Vergleich gibt 



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
# => Wenn ein Modell keine Diskriminierungsfähigkeit hat zwischen den einzelnen Beobachtungen, dann ist die AUC = 0.5
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

#c): Interpretation: Mithilfe der ersten 20% mit der höchsten Wahrscheinlichkeit, identifiziert man ~35% der Käufer, um 80% der Käufer zu identifizieren muss man die ersten 50% (mit der höchsten Wkt) der Kunden kontaktieren
# Modell performt also besser als Naive Rule (45 Grad Linie von 0,0 bis 100,100), dh das Modell diskriminiert besser als man es per Zufall mit Naive Rule könnte 
#Lift curve könnte etwas steiler sein, also optimierungsbedarf 

#d) Erstellen Sie ein dezile-wise Lift-Chart mit vier Gruppen
# Verwenden Sie den Befehl barplot() zum Plotten und den Befehl gains() aus
# dem Paket "gains", um die Zahlen zu berechnen. 

install.packages("gains")
library(gains)


lift.df$actual <- as.numeric(lift.df$actual)

gain <- gains(lift.df$actual, lift.df$prob, groups = 4)

barplot(gain$mean.resp/mean(lift.df$actual), #Hier wird die Höhe der Balken berechnet: Mean Response (durchschnittliche Quote an 1ern innerhalb des Balkens) geteilt durch durchschnittliche Rate an 1ern im gesamten Sample
        names.arg = gain$depth,
        ylab = "Main Response",
        xlab = "Depth of File",
        main = "Decile-wise lift chart")
# names.arg weist dem Graphen die verschiedenen Gruppen zu (25, 60, 75, 100)
#mit 10 Gruppen sieht man, dass in den ersten Fällen nicht viel Diskriminierung passiert, da man nur 24 Beobachtungen hat. Man kann keine Dezile bilden

#decile-wise lift Chart: Gibt an, um wie viel fach höher die Response rate
#innerhalb der festgelegten Gruppe ist, verglichen mit einem zufälligen Sample
#aus dem Datensatz

#e) Wenn man an die 25% der wahrscheinlichsten Personen eine Angebots-Email schickt
#dann ist die zu erwartende Response Rate 2x so hoch als wenn
#man ein eine zufällige Mail an eine 25%ige Zufallsstichprobe aus dem Datensatz schicken würde