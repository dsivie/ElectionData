# District Analysis

library(tidyverse)
library(skimr)
library(rattle)
library(fBasics)
library(caret)
library(corrplot)

options(scipen = 999)

# Delete all objects in environment from prior scripts

if (dev.cur() != 1)
  dev.off()
rm(list = ls())

######### Parameters Specific to this script
datafile_name <- "Election_Data_County_Census.RData"

model_level <- "County"

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

###########################################
#rattle()
# Run saved rattle code for plot generation

source("Rattle_Analysis.R")

#############
str(data)

data %>% skim

## Binary Target ##

# numeric input - t stat
p_vals_df <- data %>%
  summarise(across(all_of(input_vars), ~ t.test(. ~ rep_won)$p.value)) %>%
  t %>% as.data.frame %>% rename("p-value" = V1) %>% arrange(`p-value`)

# numeric input - area under ROC curve for predicting target
roc_df <- data %>% as_tibble %>% 
  filterVarImp(.$rep_won) %>% 
  arrange(desc(X1)) %>% as.data.frame %>% select(-X1) %>% rename(AUC = X0)

# Stats before transformation

b_stats <- data %>% select(all_of(input_vars)) %>% basicStats

t(b_stats[c("Skewness"),]) %>% as.data.frame %>% arrange(desc(Skewness))
t(b_stats[c("Kurtosis"),]) %>% as.data.frame %>% arrange(desc(Kurtosis))

# Transformation

TransformParams <- data %>%
  select(all_of(input_vars)) %>%
  preProcess(method = c("BoxCox"))

TransformParams$bc

data <- data  %>%
  predict(TransformParams, .) %>%
  as_tibble

# Stats after transformation

b_stats <- data %>% select(all_of(input_vars)) %>% basicStats

t(b_stats[c("Skewness"),]) %>% as.data.frame %>% arrange(desc(Skewness))
t(b_stats[c("Kurtosis"),]) %>% as.data.frame %>% arrange(desc(Kurtosis))