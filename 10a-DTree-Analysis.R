#Decision Trees analysis of 10 different runs

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

options(scipen = 999)

######################################Decision Tree Model for County Data - Balanced

rm(list = ls())

load("Election_Data_County_Census.RData")

data <- as.data.frame(df_prop_census)

#Split data

rnd_seed <- 3245

eval_df_county_B <- data.frame()

for(j in 1:10) {
  split = sample.split(data$rep_won, SplitRatio = 0.7)
  
  downSampledTrain <-
    downSample(
      x = data[split, ] %>% select(-rep_won),
      y = data$rep_won[split],
      ## keep the class variable name the same:
      yname = "rep_won"
    )
  
  data.down <-
    data %>% filter(fipscode %in% downSampledTrain$fipscode | !split)
  
  split.down <-
    ifelse(data.down$fipscode %in% downSampledTrain$fipscode,
           TRUE,
           FALSE)
  
  
  
  data.down <- data.down %>% select(rep_won, under_29_prop:ncol(.))
  
  # Build Model
  
  tree <-
    rpart(rep_won ~ ., data = data.down[split.down, ], control = rpart.control(cp = 0.0))
  

  # Model pruning
  cp.seq = tree$cptable[, 1]
  misc <- numeric()
  
  for (i in 1:length(cp.seq)) {
    tree.predict <-
      predict(prune(tree, cp = cp.seq[i]), data.down[!split.down, ], type = "class")
    cm <- table(data.down$rep_won[!split.down], tree.predict)
    misc[i] = (cm[1, 2] + cm[2, 1]) / sum(cm)
  }
  
  tree.final = prune(tree, cp = cp.seq[misc == min(misc)][[1]])
  #plot(as.party(tree.final))
  
  ## Model Evaluation: Lift Graph ##
  
  val.prob <-
    predict(tree.final, data.down[!split.down, ], type = "prob")[, 2]
  train.prob <-
    predict(tree.final, data.down[split.down, ], type = "prob")[, 2]
  
  val.class <- ifelse(val.prob  > 0.5, 1, 0)
  train.class <- ifelse(train.prob  > 0.5, 1, 0)
  
  pred.val <- prediction(val.prob, data.down$rep_won[!split.down])
  pred.train <- prediction(train.prob, data.down$rep_won[split.down])
  
  perf.val <- performance(pred.val, "lift", "rpp")
  perf.train <- performance(pred.train, "lift", "rpp")
  
  ### Confusion Matrix ###
  
  eval <-
    cvms::evaluate(
      tibble(
        "target" = as.character(data.down$rep_won[!split.down]),
        "prediction" = as.character(val.class)
      ),
      target_col = "target",
      prediction_cols = "prediction",
      type = "binomial"
    )
  
  conf_mat <- eval$`Confusion Matrix`[[1]]
  
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

#########################################

rownames(eval_df_county_B) <- NULL

eval_df_county_B <- eval_df_county_B %>% rename(Value = County_B)

ggplot(eval_df_county_B, aes(x = Run, y = Value)) + 
  geom_line(aes(color = Parameter, linetype = Parameter)) + ggtitle("Decision Tree Stats") +
  scale_x_discrete(name = "Run", limits = 1:10)
