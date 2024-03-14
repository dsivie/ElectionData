###################################### Random Forest Model for District Data

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

# Delete all objects in environment from prior scripts

if (dev.cur() != 1)
  dev.off()
rm(list = ls())

######### Parameters Specific to this script

prop_train_data <- 0.70
rnd_seed <- 3245
datafile_name <- "Election_Data_District_Census.RData"

model_type <- "Random Forest"
model_level <- "District"

m <- seq(2, 10)

target_var <- "rep_won"
input_vars <-
  c(
    "under_29_prop",
    "over_65_prop",
    "female_prop",
    "median_age",
    "white_prop",
    "black_prop",
    "hispanic_prop",
    "foreign_born_prop",
    "med_HH_inc",
    "unemp_prop",
    "no_hs_prop",
    "no_bach_prop",
    "grad_plus_prop",
    "wht_no_hs_prop",
    "wht_no_bach_prop",
    "wht_bach_plus_prop",
    "gini_idx",
    "rural_prop",
    "density"
  )

all_vars_needed <- c(target_var, input_vars)

###########################################

# Load the data and select target and input variables

load(datafile_name)

data <- as.data.frame(df_prop_census)

data <- data %>% select(all_of(all_vars_needed))

# Set Random Seed for train/test split and do split

set.seed(rnd_seed)

split = sample.split(data$rep_won, SplitRatio = prop_train_data)

# Build Model

#### Parameter Tuning: mtry ####

fscore.seq <- numeric()

for(i in 1:length(m)) {
  set.seed(1234)
  rf <- randomForest(rep_won ~ .,
                     data = data[split,],
                     ntree = 500,
                     mtry = m[i])
  
  rf.class <-
    predict(rf, newdata = data[!split,], type = "response")
  fscore.seq[i] <- confusionMatrix(rf.class, data$rep_won[!split],
                                   positive = "1")$byClass["F1"]
}


plot(
  m,
  fscore.seq,
  pch = 19 ,
  col = "blue",
  type = "b",
  ylab = "F-score",
  xlab = "Number of Predictors considered at each split"
)

m.best<- m[which.max(fscore.seq)] 

RF <- randomForest(
  rep_won ~ .,
  data = data[split, ],
  ntree = 500,
  mtry = m.best,
  importance = TRUE
)

# Model Evaluation: Lift Graph

val.prob <- predict(RF, data[!split, ], type = "prob")[, 2]
train.prob <- predict(RF, data[split, ], type = "prob")[, 2]

val.class <- ifelse(val.prob > 0.5, 1, 0)
train.class <- ifelse(train.prob > 0.5, 1, 0)

pred.val <- prediction(val.prob, data$rep_won[!split])
pred.train <- prediction(train.prob, data$rep_won[split])

perf.val <- performance(pred.val, "lift", "rpp")
perf.train <- performance(pred.train, "lift", "rpp")

plot(
  perf.train,
  col = 'blue',
  type = "b",
  main = paste("Lift Curve -", model_type, "-", model_level)
)
plot(perf.val,
     col = 'red',
     type = "b",
     add = TRUE)
legend(
  'topright',
  legend = c('train', 'validation'),
  col = c("blue", "red"),
  lty = c(1, 1)
)

# Confusion Matrix

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

conf_mat <- eval$`Confusion Matrix`[[1]]

plot_confusion_matrix(conf_mat,
                      add_sums = TRUE,
                      class_order = c("1", "0")) + ggtitle(paste("Confusion Matrix -", model_type, "-", model_level))

# Store Performance Metrics

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

eval_df <- eval_df %>% as.data.frame %>% rename(!!model_level := V1)
eval_df <- eval_df %>% round(3)

eval_df["Best mtry", model_level] <- m.best

rocCurve <- roc(
  response =  data$rep_won[!split],
  predictor = as.vector(val.prob),
  levels = levels(data$rep_won[!split])
)

if (!exists("RF_summary_df")) {
  RF_summary_df <- eval_df
} else {
  RF_summary_df[model_level] <- eval_df[[model_level]]
}

if (!exists("RF_ROC_lst")) {
  RF_ROC_lst <- list()
  RF_ROC_lst[[model_level]] <- rocCurve
} else {
  RF_ROC_lst[[model_level]] <- rocCurve
}