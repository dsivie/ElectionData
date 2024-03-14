#This is the FIRST script that should be run
#It inputs the county level election data and does some cleaning and NA replacement

library(tidyverse)
library(skimr)

options(scipen = 999)

rm(list = ls())

#Use the datatype information provided in "./Harvard_County-Level_2018_House/2018-elections-official-master/field_details.md"
col_types = list(
  state = col_character(),
  county = col_character(),
  fipscode = col_character(),
  fipscode2 = col_character(),
  office = col_character(),
  district = col_character(),
  total.votes = col_integer(),
  dem = col_integer(),
  rep = col_integer(),
  other = col_integer()
)

#Read in Election Data

df_raw <-
  read_csv(
    "./Harvard_County-Level_2018_House/us-house-wide.csv",
    col_names = TRUE,
    col_types = col_types,
    na = c(".", "NA", "", "?")
  )

#Fix Massachusetts district 4 error

index <- df_raw$state == "MA" & df_raw$district == "4"
df_raw <-
  within(df_raw, {
    dem[index] <- other[index]
    other[index] <- NA
  })

#Check for number of rows the have NA values

df_raw %>% select(state:district) %>% filter(rowSums(is.na(.)) > 0) %>% nrow
df_raw %>% select(total.votes:rep) %>% filter(rowSums(is.na(.)) > 0) %>% nrow

#NA values for rep and dem indicate unopposed candidates.  Remove them as outliers
#Replace NA other votes with 0
#Filter out no vote rows

#df_cleaned <- df_raw %>% filter(!is.na(dem) & !is.na(rep)) %>%
#  mutate(other = replace_na(other, 0)) %>% filter(total.votes > 0)

df_cleaned <- df_raw %>% 
  mutate(other = replace_na(other, 0), dem = replace_na(dem, 0), rep = replace_na(rep, 0)) %>% filter(total.votes > 0)

c(
  "state",
  "county",
  "fipscode" ,
  "fipscode2",
  "office",
  "district",
  "total.votes",
  "dem",
  "rep",
  "other"
)

df_cleaned %>% skim

rm(df_raw)

save(df_cleaned, file = "Election_Data_Cleaned.RData")

#rm(df_cleaned)
