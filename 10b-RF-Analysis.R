#Random Forest analysis of 10 different runs

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

######################################Random Forest Model for County Internally Balanced

rm(list = ls())

load("Election_Data_County_Census.RData")

data <- as.data.frame(df_prop_census)

#Split data

rnd_seed <- 3245

eval_df_county_B <- data.frame()

for(j in 1:10) {
  split = sample.split(data$rep_won, SplitRatio = 0.7)
  
  #Select Target and Input columns
  
  data <- data %>% select(rep_won, under_29_prop:ncol(.))
  
  minor <- unname(summary(data$rep_won[split])[1])
  
  #### Parameter Tuning: mtry ####
  
  m <- round(seq(
    from = 2,
    to = 28,
    length.out = 5
  ))
  m <- seq(2, 7)
  fscore.seq <- numeric()
  
  for (i in 1:length(m)) {
    set.seed(1234)
    rf <- randomForest(
      rep_won ~ .,
      data = data[split, ],
      strata = data$rep_won[split],
      sampsize = c(minor, minor),
      ntree = 1000,
      mtry = m[i]
    )
    
    rf.class <-
      predict(rf, newdata = data[!split, ], type = "response")
    
    fscore.seq[i] <-
      confusionMatrix(rf.class,
                      data$rep_won[!split],
                      positive = "1")$byClass["F1"]
  }
  
  
  m.best <- m[which.max(fscore.seq)]
  
  
  RF <- randomForest(
    rep_won ~ .,
    data = data[split, ],
    strata = data$rep_won[split],
    sampsize = c(minor, minor),
    ntree = 500,
    mtry = m.best,
    importance = TRUE
  )
  
  val.prob <- predict(RF, data[!split,], type = "prob")[, 2]
  
  val.class <-  predict(RF, data[!split,], type = "response")
  
  eval <-
    cvms::evaluate(
      tibble(
        "target" = as.character(data$rep_won[!split]),
        "prediction" = as.character(val.class)
      ),
      target_col = "target",
      prediction_cols = "prediction",
      type = "binomial"
    )
  
  eval <-
    eval %>% mutate(`Misclassification Rate` = 1 - Accuracy,
                    Precision = `Pos Pred Value`)
  
  eval_df <-
    t(eval[, c(
      "Accuracy",
      "Misclassification Rate",
      "F1",
      "Sensitivity",
      "Specificity",
      "Precision",
      "AUC"
    )])
  
  if (nrow(eval_df_county_B) == 0) {
    temp_df <-
      eval_df %>% as.data.frame %>% rename(County_B = V1) %>% mutate(Run = j)
    temp_df$Parameter <- rownames(temp_df)
    eval_df_county_B <- temp_df
  }
  else{
    temp_df <-
      eval_df %>% as.data.frame %>% rename(County_B = V1) %>% mutate(Run = j)
    temp_df$Parameter <- rownames(temp_df)
    eval_df_county_B <- rbind(eval_df_county_B,
                              temp_df)
  }
  
  
  
}

#eval_df_county_B$Parameter <- rownames(eval_df_county_B)
#eval_df_county_B$Run <- 1


#########################################

rownames(eval_df_county_B) <- NULL

eval_df_county_B <- eval_df_county_B %>% rename(Value = County_B)

ggplot(eval_df_county_B, aes(x = Run, y = Value)) +
  geom_line(aes(color = Parameter, linetype = Parameter)) +
  ggtitle("Random Forest Stats") +
  scale_x_discrete(name = "Run", limits = 1:10)

