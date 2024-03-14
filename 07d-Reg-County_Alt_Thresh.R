###################################### Regression Model for County Unbalanced Data

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

model_type <- "Regression"
model_level <- "County_Alt_Thresh"

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

# Perform BoxCox Transformations on needed variables

TransformParams <- data  %>%
  select(all_of(input_vars)) %>%
  preProcess(method = c("BoxCox"))

TransformParams$bc

data.mdl <- data  %>%
  predict(TransformParams, .) %>%
  as_tibble

levels(data.mdl$rep_won) # check primary outcome

# Build Model

# Build full model
full <-
  glm(rep_won ~ ., family = binomial, data = data.mdl[split,])

# Set up null model
null <-
  glm(rep_won ~ 1, family = binomial, data = data.mdl[split,])
n <- sum(split) # training size (for BIC)

model_lst <- list()
F1_lst <- list()
thresh_lst <- list()

# Stepwise selection

reg.step <-
  step(null,
       scope = formula(full),
       direction = "both",
       k = log(n))
summary(reg.step)
reg.step.prob <-
  predict(reg.step, data.mdl[!split,], type = "response")
rocCurve.reg <-
  roc(data.mdl$rep_won[!split], reg.step.prob, quiet = TRUE)
regThresh <-
  coords(rocCurve.reg, x = "best", best.method = "closest.topleft")
reg.class <-
  as.factor(ifelse(reg.step.prob >= regThresh$threshold, 1, 0))
reg.step.fscore <-
  confusionMatrix(reg.class, data.mdl$rep_won[!split],
                  positive = "1")$byClass["F1"]

model_lst[["Stepwise_Reg"]] <- reg.step
F1_lst[["Stepwise_Reg"]] <- reg.step.fscore
thresh_lst[["Stepwise_Reg"]] <- regThresh$threshold

# Forward

reg.fwd <-
  step(
    null,
    scope = formula(full),
    direction = "forward",
    k = log(n),
    trace = FALSE
  )
summary(reg.fwd)
reg.fwd.prob <-
  predict(reg.fwd, data.mdl[!split,], type = "response")
rocCurve.reg <-
  roc(data.mdl$rep_won[!split], reg.fwd.prob, quiet = TRUE)
regThresh <-
  coords(rocCurve.reg, x = "best", best.method = "closest.topleft")
reg.class <-
  as.factor(ifelse(reg.fwd.prob >= regThresh$threshold, 1, 0))
reg.fwd.fscore <-
  confusionMatrix(reg.class, data.mdl$rep_won[!split],
                  positive = "1")$byClass["F1"]

model_lst[["Forward_Reg"]] <- reg.fwd
F1_lst[["Forward_Reg"]] <- reg.fwd.fscore
thresh_lst[["Forward_Reg"]] <- regThresh$threshold

# Backward

reg.bwd <- step(full,
                direction = "backward",
                k = log(n),
                trace = FALSE)
summary(reg.bwd)
reg.bwd.prob <-
  predict(reg.bwd, data.mdl[!split,], type = "response")
rocCurve.reg <-
  roc(data.mdl$rep_won[!split], reg.bwd.prob, quiet = TRUE)
regThresh <-
  coords(rocCurve.reg, x = "best", best.method = "closest.topleft")
reg.class <-
  as.factor(ifelse(reg.bwd.prob >= regThresh$threshold, 1, 0))
reg.bwd.fscore <-
  confusionMatrix(reg.class, data.mdl$rep_won[!split],
                  positive = "1")$byClass["F1"]

model_lst[["Backward_Reg"]] <- reg.bwd
F1_lst[["Backward_Reg"]] <- reg.bwd.fscore
thresh_lst[["Backward_Reg"]] <- regThresh$threshold

best_mdl_idx <- which.max(F1_lst)
best_mdl_name <- names(best_mdl_idx)
best_F1 <- F1_lst[[best_mdl_idx]]
best_mdl <- model_lst[[best_mdl_idx]]
regThresh <- thresh_lst[[best_mdl_idx]]

summary(best_mdl)

# odds ratio estimate #
exp(coef(best_mdl))

# variable importance #
varImp(best_mdl) %>% arrange((desc(Overall)))

## Model Evaluation: Lift Graph ##

val.prob <- predict(best_mdl, data.mdl[!split, ], type = "response")
train.prob <- predict(best_mdl, data.mdl[split, ], type = "response")

val.class <- ifelse(val.prob > regThresh, 1, 0)
train.class <- ifelse(train.prob  > regThresh, 1, 0)

pred.val <- prediction(val.prob, data.mdl$rep_won[!split])
pred.train <- prediction(train.prob, data.mdl$rep_won[split])

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

eval_df["Regression Type", model_level] <- best_mdl_name
eval_df["Alt Threshold", model_level] <- round(regThresh, 3)

rocCurve <- roc(
  response =  data$rep_won[!split],
  predictor = as.vector(val.prob),
  levels = levels(data$rep_won[!split])
)

if (!exists("Reg_summary_df")) {
  Reg_summary_df <- eval_df
} else {
  Reg_summary_df[model_level] <- eval_df[[model_level]]
}

if (!exists("Reg_ROC_lst")) {
  Reg_ROC_lst <- list()
  Reg_ROC_lst[[model_level]] <- rocCurve
} else {
  Reg_ROC_lst[[model_level]] <- rocCurve
}
