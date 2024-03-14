###################################### Decision Tree Model for County - Alternative Threshold

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
model_level <- "County_Alt_Thresh"

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

data <- data %>% select(all_of(all_vars_needed))

# Set Random Seed for train/test split and do split

set.seed(rnd_seed)

split = sample.split(data$rep_won, SplitRatio = prop_train_data)

# Build Model

tree <-
  rpart(rep_won ~ ., data = data[split,], control = rpart.control(cp = cp))

summary(tree)
print(tree$cptable)
printcp(tree)

# Model visualization

plot(as.party(tree))
print(tree)

# Model pruning

cp.seq <- tree$cptable[, 1]
fscore <- numeric()
fscore[1] <- 0

for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(tree, cp = cp.seq[i]), data[!split,], type =
                        "prob")[, 2]
  rocCurve.tree <-
    roc(data$rep_won[!split], tree.prob, quiet = TRUE)
  treeThresh <-
    coords(rocCurve.tree, x = "best", best.method = "closest.topleft")
  tree.class <-
    as.factor(ifelse(tree.prob >= treeThresh$threshold, 1, 0))
  fscore[i] <-
    confusionMatrix(tree.class, data$rep_won[!split], positive = "1")$byClass["F1"]
}

plot(
  tree$cptable[, 'nsplit'] + 1,
  fscore,
  type = "o",
  xlab = "Number of Leaves",
  ylab = "F-score",
  main = paste("F-score vs Leaves -", model_level)
)

tree.final = prune(tree, cp = cp.seq[fscore == max(fscore)][[1]])

tree.prob = predict(tree.final, data[!split,], type = "prob")[, 2]

rocCurve.tree <-
  roc(data$rep_won[!split], tree.prob, quiet = TRUE)
treeThresh <-
  coords(rocCurve.tree, x = "best", best.method = "closest.topleft")

paste("Highest F-Score:",
      max(fscore),
      "with cp =",
      cp.seq[fscore == max(fscore)][[1]])

leaves <- tail(tree.final$cptable[, 'nsplit'], n = 1) + 1
paste("Best tree has :", tail(tree.final$cptable[, 'nsplit'], n = 1) + 1, "leaves.")

plot(as.party(tree.final),
     main = paste("Best Decision Tree -", model_level))

# Model Evaluation: Lift Graph

val.prob <- predict(tree.final, data[!split,], type = "prob")[, 2]
train.prob <- predict(tree.final, data[split,], type = "prob")[, 2]

val.class <- ifelse(val.prob >= treeThresh$threshold, 1, 0)
train.class <- ifelse(train.prob >= treeThresh$threshold, 1, 0)

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
eval_df["Alt Threshold", model_level] <- treeThresh$threshold
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