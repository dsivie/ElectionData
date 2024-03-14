#Random Forest - Accuracy vs. Winning Margin

library(tidyverse)
library(caret)
library(caTools)
library(skimr)
library(rpart)
library(ROCR)
library(gmodels)
library(pROC)
library(cvms)
library(rsvg)
library(ggnewscale)
library(gridExtra)
library(knitr)
library(partykit)
library(randomForest)


options(scipen = 999)

######################################Random Forest Model for District Data

rm(list = ls())

load("Election_Data_District_Census.RData")

data <- as.data.frame(df_prop_census)

#Split data

rnd_seed <- 3245

set.seed(rnd_seed)

split = sample.split(data$rep_won, SplitRatio = 0.7)

#Select Target and Input columns

data <- data %>% select(rep_won, under_29_prop:ncol(.))

#### Parameter Tuning: mtry ####

m<-round(seq(from=2, to=28, length.out = 5))
m<-seq(2,7)
fscore.seq<-numeric()

for(i in 1:length(m)){ 
  set.seed(1234)
  rf <- randomForest(rep_won ~., data=data[split,],
                     ntree=500, mtry=m[i])
  
  rf.class<- predict(rf, newdata=data[!split,], type="response")
  fscore.seq[i]<-confusionMatrix(rf.class,data$rep_won[!split],
                                 positive = "1")$byClass["F1"]
}

plot(m, fscore.seq, pch=19 , col="blue", type="b",
     ylab="F-score",xlab="Number of Predictors considered at each split")

m.best<- m[which.max(fscore.seq)] 
# assign m.best to mtry in the above RF 
# and then pick appropriate ntree

RF <- randomForest(
  rep_won ~ .,
  data = data[split, ],
  ntree = 500,
  mtry = m.best,
  importance = TRUE
)

print(RF)
plot(RF)

data_test <- df_prop_census[!split, ]

val.prob <- predict(RF, data[!split, ], type = "prob")[, 2]

val.class <-  predict(RF, data[!split, ], type = "response")

data_test$Pred_Winner <- val.class

data_test$Pred_Prob <- val.prob


data_test

data_test <- data_test %>% relocate(Pred_Winner, .after=rep_won)
data_test <- data_test %>% relocate(Pred_Prob, .after=Pred_Winner)
data_test$delta <- abs(data_test$rep_prop - data_test$dem_prop)
data_test <- data_test %>% relocate(delta, .after=rep_prop)
data_test$error <- data_test$rep_won != data_test$Pred_Winner
data_test <- data_test %>% relocate(error, .after=Pred_Winner)


data_test <- data_test %>% mutate(delta_bin = cut(delta, breaks=as.vector(seq(0, 1, by = 0.10)) ))

data_test <- data_test %>% relocate(delta_bin, .after=Pred_Prob)

z <- data_test %>% group_by(delta_bin) %>% summarise(correct_rate = 1 - mean(error))

plot(x = z$delta_bin, y=z$correct_rate, xlab="Winning Margin", ylab="Accuracy")

x <- data_test %>% group_by(delta_bin) %>% summarise(n = n()) %>%
  mutate(freq = n / sum(n))



