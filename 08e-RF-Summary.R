###################################### Random Forest Summary Stats

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

model_type <- "Random Forest"

# Plot ROC Curves

if (exists("RF_ROC_lst")) {
  legend <- names(RF_ROC_lst)
  col = c("green", "red", "blue", "yellow")
  
  for (i in 1:length(RF_ROC_lst)) {
    if (i == 1) {
      plot.roc(RF_ROC_lst[[i]],
               legacy.axes = TRUE, main = paste("ROC Curve -", model_type),
               col = col[i])
    }
    else{
      plot.roc(
        RF_ROC_lst[[i]],
        add = TRUE,
        legacy.axes = TRUE, 
        col = col[i]
      )
    }
  }
  
  legend('topleft',
         legend = legend,
         col = col,
         lty = 1)
}

# Plot summary table

if (exists("RF_summary_df")) {
  if (dev.cur() != 1)
    dev.off()
  grid.table(RF_summary_df)
} 