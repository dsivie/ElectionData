#This is the SECOND script that should be run
#It aggregates data to district level and does some data cleaning

library(tidyverse)
library(skimr)
library(Hmisc)

options(scipen = 999)

rm(list = ls())

load("Election_Data_Cleaned.RData")

str(df_cleaned)

sum_group_df <- function(df) {
  df$party_winner <-
    colnames(df %>% select(dem_prop:other_prop))[max.col(df %>% select(dem_prop:other_prop),
                                                         ties.method =
                                                           "first")]
  df <- df %>%
    mutate(across(party_winner, as.factor))
  
  levels(df$party_winner) <- list(Democrat = "dem_prop", Republican = "rep_prop", Other = "other_prop")
  
  df %>% mutate(rep_won = factor(ifelse(party_winner == "Republican", 1, 0)))
}

#############Handle District Level Data

df_district_prop <- df_cleaned %>% group_by(state, district) %>%
  summarise(across(dem:other, ~ sum(.) / sum(total.votes)),
            .groups = 'drop') %>% rename(dem_prop = dem,
                                         rep_prop = rep,
                                         other_prop = other)

df_district_prop <- sum_group_df(df_district_prop)

df_district_prop <-
  df_district_prop %>% filter(party_winner %in% c("Republican", "Democrat"))

df_district_prop$party_winner <- factor(df_district_prop$party_winner)

levels(df_district_prop$party_winner)

save(df_district_prop, file = "Election_Data_District.RData")

#############Handle County Level Data

df_county_prop <- df_cleaned %>% group_by(state, fipscode) %>%
  summarise(across(dem:other, ~ sum(.) / sum(total.votes)),
            .groups = 'drop') %>% rename(dem_prop = dem,
                                         rep_prop = rep,
                                         other_prop = other)

df_county_prop <- sum_group_df(df_county_prop)

df_county_prop <-
  df_county_prop %>% filter(party_winner %in% c("Republican", "Democrat"))

df_county_prop$party_winner <- factor(df_county_prop$party_winner)

levels(df_county_prop$party_winner)

save(df_county_prop, file = "Election_Data_County.RData")

#rm(df_district_prop, df_county_prop, df_cleaned)
