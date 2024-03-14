###################################### Decision Tree Model for County Balanced Data

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

# Delete all objects in environment from prior scripts

if (dev.cur() != 1)
  dev.off()
#rm(list = ls())

######### Parameters Specific to this script

prop_train_data <- 0.70
rnd_seed <- 3245
datafile_name <- "Election_Data_County_Census.RData"

model_type <- "Decision Tree"
model_level <- "County_Balanced"

cp <- 0.0

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

data <- data %>% select(all_of(all_vars_needed), fipscode)

# Set Random Seed for train/test split and do split
# Special handling here for down-sampling

set.seed(rnd_seed)

split = sample.split(data$rep_won, SplitRatio = prop_train_data)

downSampledTrain <-
  downSample(
    x = data[split, ] %>% select(-rep_won),
    y = data$rep_won[split],
    yname = "rep_won"
  )

data.down <-
  data %>% filter(fipscode %in% downSampledTrain$fipscode | !split)

split <-
  ifelse(data.down$fipscode %in% downSampledTrain$fipscode, TRUE, FALSE)

data <- data.down %>% select(all_of(all_vars_needed))

data %>% filter(split) %>% skim
data %>% filter(!split) %>% skim

# Build Model

tree <-
  rpart(rep_won ~ ., data = data[split, ], control = rpart.control(cp = cp))

summary(tree)
print(tree$cptable)
printcp(tree)

# Model visualization

plot(as.party(tree))
print(tree)

# Model pruning

cp.seq = tree$cptable[, 1]
misc <- numeric()

for (i in 1:length(cp.seq)) {
  tree.predict <-
    predict(prune(tree, cp = cp.seq[i]), data[!split,], type = "class")
  cm <- table(data$rep_won[!split], tree.predict)
  misc[i] = (cm[1, 2] + cm[2, 1]) / sum(cm)
}

# Plot Misc. Rate vs Leaves

plot(
  tree$cptable[, 'nsplit'] + 1,
  misc,
  type = "o",
  xlab = "Number of Leaves",
  ylab = "Misclassification Rate",
  main = paste("Misclassification Rate vs Leaves -", model_type)
)

# Final model Selection

tree.final = prune(tree, cp = cp.seq[misc == min(misc)][[1]])

plot(as.party(tree.final),
     main = paste("Best Decision Tree -", model_level))

paste("Lowest misclassification rate:",
      min(misc),
      "with cp =",
      cp.seq[misc == min(misc)][[1]])

leaves <- tail(tree.final$cptable[, 'nsplit'], n = 1) + 1
paste("Best tree has :", tail(tree.final$cptable[, 'nsplit'], n = 1) + 1, "leaves.")

tree.final$variable.importance
names(tree.final$variable.importance)

# Model Evaluation: Lift Graph

val.prob <- predict(tree.final, data[!split, ], type = "prob")[, 2]
train.prob <- predict(tree.final, data[split, ], type = "prob")[, 2]

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
eval_df["Alt Threshold", model_level] <- NA
eval_df["Leaves", model_level] <- leaves

rocCurve <- roc(
  response =  data$rep_won[!split],
  predictor = as.vector(val.prob),
  levels = levels(data$rep_won[!split])
)

if (!exists("DTree_summary_df")) {
  DTree_summary_df <- eval_df
} else {
  DTree_summary_df[model_level] <- eval_df[[model_level]]
}

if (!exists("DTree_ROC_lst")) {
  DTree_ROC_lst <- list()
  DTree_ROC_lst[[model_level]] <- rocCurve
} else {
  DTree_ROC_lst[[model_level]] <- rocCurve
}