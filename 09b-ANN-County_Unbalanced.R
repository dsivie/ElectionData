###################################### ANN Model for County Unbalanced Data

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

model_type <- "ANN"
model_level <- "County_Unbalanced"

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

# Use logistic regression with backwards selection to choose variables

# Build full model
full <- glm(rep_won ~ ., family = binomial, data = data.mdl[split, ])

# Set up null model
null <- glm(rep_won ~ 1, family = binomial, data = data.mdl[split, ])
n <- sum(split) # training size (for BIC)
reg.bwd <- step(full,
                direction = "backward",
                k = log(n),
                trace = FALSE)

vars.ann <-
  attr(terms(reg.bwd), "term.labels") # extract variable names from bwd model

#####################
## ANN Preparation ##
#####################
data.ann <- data.mdl

vars.ann <-
  attr(terms(reg.bwd), "term.labels") # extract variable names from bwd model

ScaleParams <-
  preProcess(data.ann[split, vars.ann], method = c("center", "scale"))
data.ann <- data.ann %>% predict(ScaleParams, .)

## Dummy Encoding: nominal inputs ##
dummy <-
  dummyVars(~ . , data = data.ann[split, c(vars.ann, "rep_won")], fullRank = TRUE)
data.ann.encode <- data.ann %>% predict(dummy, .)

## Prepare train/validation sets as matrices ##
x.train <- data.ann.encode[split,-ncol(data.ann.encode)]
y.train <- data.ann.encode[split, "rep_won.1"]
x.valid <- data.ann.encode[!split,-ncol(data.ann.encode)]
y.valid <- data.ann.encode[!split, "rep_won.1"]

####################
### ANN Building ###
####################

#install.packages("tensorflow")
#library(reticulate)
#virtualenv_create("r-reticulate")

#library(tensorflow)
#install_tensorflow(envname = "r-reticulate")

#install.packages("keras")
#library(keras)
#install_keras(envname = "r-reticulate")

#library(tensorflow)
#use_virtualenv("r-reticulate")
#tf$constant("Hello Tensorflow!")

library(tensorflow)
library(keras)

set.seed(1234)

ann <- keras_model_sequential() %>% 
  layer_dense(units = 4, activation = "tanh", input_shape = c(length(vars.ann))) %>% 
  layer_dense(units = 1, activation = "sigmoid")

ann %>% compile(loss = "binary_crossentropy",
                optimizer = "adam",
                metrics = "accuracy")

callbacks.list = list(
  callback_model_checkpoint(
    filepath = "my_ann.h5",
    monitor = "val_accuracy",
    save_best_only = TRUE
  )
)

history <- ann %>% fit(
  x = x.train,
  y = y.train,
  epochs = 40,
  validation_data = list(x.valid, y.valid),
  verbose = 1,
  callbacks = callbacks.list
)

ann.select <- load_model_hdf5("my_ann.h5") 
summary(ann.select)

results.valid <- keras::evaluate(ann.select, x.valid,y.valid)
results.valid

## Prediction ##
val.prob <- predict(ann.select, x.valid)

val.class <- ifelse(val.prob > 0.5, 1, 0)

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

eval <-
  eval %>% mutate(`Misclassification Rate` = 1 - Accuracy,
                  Precision = `Pos Pred Value`)

plot_confusion_matrix(conf_mat,
                      add_sums = TRUE,
                      class_order = c("1", "0")) + ggtitle(paste("Confusion Matrix -", model_type, "-", model_level))

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

rocCurve <- roc(
  response =  data$rep_won[!split],
  predictor = as.vector(val.prob),
  levels = levels(data$rep_won[!split])
)

if (!exists("ANN_summary_df")) {
  ANN_summary_df <- eval_df
} else {
  ANN_summary_df[model_level] <- eval_df[[model_level]]
}

if (!exists("ANN_ROC_lst")) {
  ANN_ROC_lst <- list()
  ANN_ROC_lst[[model_level]] <- rocCurve
} else {
  ANN_ROC_lst[[model_level]] <- rocCurve
}