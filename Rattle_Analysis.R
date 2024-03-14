#=======================================================================

# Rattle is Copyright (c) 2006-2021 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2022-12-19 18:10:41 x86_64-w64-mingw32 

# Rattle version 5.5.1 user 'DavidSivieri'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2022-12-19 18:10:56 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- df_prop_census

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:10:56 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=412 train=288 validate=62 test=62

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("state", "district", "dem_prop", "rep_prop",
                   "other_prop", "rep_won", "total_pop",
                   "cit_vote_pop", "under_29_prop", "over_65_prop",
                   "female_prop", "median_age", "white_prop",
                   "black_prop", "hispanic_prop",
                   "foreign_born_prop", "med_HH_inc", "unemp_prop",
                   "no_hs_prop", "no_bach_prop", "grad_plus_prop",
                   "wht_no_hs_prop", "wht_no_bach_prop",
                   "wht_bach_plus_prop", "gini_idx", "rural_prop",
                   "density")

crs$numeric   <- c("dem_prop", "rep_prop", "other_prop",
                   "total_pop", "cit_vote_pop", "under_29_prop",
                   "over_65_prop", "female_prop", "median_age",
                   "white_prop", "black_prop", "hispanic_prop",
                   "foreign_born_prop", "med_HH_inc", "unemp_prop",
                   "no_hs_prop", "no_bach_prop", "grad_plus_prop",
                   "wht_no_hs_prop", "wht_no_bach_prop",
                   "wht_bach_plus_prop", "gini_idx", "rural_prop",
                   "density")

crs$categoric <- c("state", "district", "rep_won")

crs$target    <- "party_winner"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:22 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("total_pop", "cit_vote_pop", "under_29_prop",
                   "over_65_prop", "female_prop", "median_age",
                   "white_prop", "black_prop", "hispanic_prop",
                   "foreign_born_prop", "med_HH_inc", "unemp_prop",
                   "no_hs_prop", "no_bach_prop", "grad_plus_prop",
                   "wht_no_hs_prop", "wht_no_bach_prop",
                   "wht_bach_plus_prop", "gini_idx", "rural_prop",
                   "density")

crs$numeric   <- c("total_pop", "cit_vote_pop", "under_29_prop",
                   "over_65_prop", "female_prop", "median_age",
                   "white_prop", "black_prop", "hispanic_prop",
                   "foreign_born_prop", "med_HH_inc", "unemp_prop",
                   "no_hs_prop", "no_bach_prop", "grad_plus_prop",
                   "wht_no_hs_prop", "wht_no_bach_prop",
                   "wht_bach_plus_prop", "gini_idx", "rural_prop",
                   "density")

crs$categoric <- NULL

crs$target    <- "rep_won"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("state", "district", "dem_prop", "rep_prop", "other_prop", "party_winner")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:26 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation df_prop_census using Pearson",
    sub=)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:43 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for wht_no_hs_prop

# Generate a box plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  ggplot2::ggplot(ggplot2::aes(y=wht_no_hs_prop)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=rep_won, fill=rep_won), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=rep_won), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("rep_won\n\n") +
  ggplot2::ggtitle("Distribution of wht_no_hs_prop\nby rep_won") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:43 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for wht_no_hs_prop

# Generate the plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  dplyr::select(wht_no_hs_prop, rep_won) %>%
  ggplot2::ggplot(ggplot2::aes(x=wht_no_hs_prop)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=rep_won, colour=rep_won), alpha=0.55) +
  ggplot2::xlab("wht_no_hs_prop\n\n") +
  ggplot2::ggtitle("Distribution of wht_no_hs_prop\nby rep_won") +
  ggplot2::labs(fill="rep_won", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:47 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for wht_no_bach_prop

# Generate a box plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  ggplot2::ggplot(ggplot2::aes(y=wht_no_bach_prop)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=rep_won, fill=rep_won), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=rep_won), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("rep_won\n\n") +
  ggplot2::ggtitle("Distribution of wht_no_bach_prop\nby rep_won") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:47 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for wht_no_bach_prop

# Generate the plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  dplyr::select(wht_no_bach_prop, rep_won) %>%
  ggplot2::ggplot(ggplot2::aes(x=wht_no_bach_prop)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=rep_won, colour=rep_won), alpha=0.55) +
  ggplot2::xlab("wht_no_bach_prop\n\n") +
  ggplot2::ggtitle("Distribution of wht_no_bach_prop\nby rep_won") +
  ggplot2::labs(fill="rep_won", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:51 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for wht_bach_plus_prop

# Generate a box plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  ggplot2::ggplot(ggplot2::aes(y=wht_bach_plus_prop)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=rep_won, fill=rep_won), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=rep_won), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("rep_won\n\n") +
  ggplot2::ggtitle("Distribution of wht_bach_plus_prop\nby rep_won") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:51 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for wht_bach_plus_prop

# Generate the plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  dplyr::select(wht_bach_plus_prop, rep_won) %>%
  ggplot2::ggplot(ggplot2::aes(x=wht_bach_plus_prop)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=rep_won, colour=rep_won), alpha=0.55) +
  ggplot2::xlab("wht_bach_plus_prop\n\n") +
  ggplot2::ggtitle("Distribution of wht_bach_plus_prop\nby rep_won") +
  ggplot2::labs(fill="rep_won", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:58 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for white_prop

# Generate a box plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  ggplot2::ggplot(ggplot2::aes(y=white_prop)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=rep_won, fill=rep_won), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=rep_won), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("rep_won\n\n") +
  ggplot2::ggtitle("Distribution of white_prop\nby rep_won") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:11:58 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for white_prop

# Generate the plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  dplyr::select(white_prop, rep_won) %>%
  ggplot2::ggplot(ggplot2::aes(x=white_prop)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=rep_won, colour=rep_won), alpha=0.55) +
  ggplot2::xlab("white_prop\n\n") +
  ggplot2::ggtitle("Distribution of white_prop\nby rep_won") +
  ggplot2::labs(fill="rep_won", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:12:01 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for black_prop

# Generate a box plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  ggplot2::ggplot(ggplot2::aes(y=black_prop)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=rep_won, fill=rep_won), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=rep_won), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("rep_won\n\n") +
  ggplot2::ggtitle("Distribution of black_prop\nby rep_won") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:12:01 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for black_prop

# Generate the plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  dplyr::select(black_prop, rep_won) %>%
  ggplot2::ggplot(ggplot2::aes(x=black_prop)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=rep_won, colour=rep_won), alpha=0.55) +
  ggplot2::xlab("black_prop\n\n") +
  ggplot2::ggtitle("Distribution of black_prop\nby rep_won") +
  ggplot2::labs(fill="rep_won", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:12:11 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for rural_prop

# Generate a box plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  ggplot2::ggplot(ggplot2::aes(y=rural_prop)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=rep_won, fill=rep_won), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=rep_won), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("rep_won\n\n") +
  ggplot2::ggtitle("Distribution of rural_prop\nby rep_won") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2022-12-19 18:12:11 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for rural_prop

# Generate the plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(rep_won=as.factor(rep_won)) %>%
  dplyr::select(rural_prop, rep_won) %>%
  ggplot2::ggplot(ggplot2::aes(x=rural_prop)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=rep_won, colour=rep_won), alpha=0.55) +
  ggplot2::xlab("rural_prop\n\n") +
  ggplot2::ggtitle("Distribution of rural_prop\nby rep_won") +
  ggplot2::labs(fill="rep_won", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)
