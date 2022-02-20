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