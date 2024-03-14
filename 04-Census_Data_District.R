#This is the FOURTH script that should be run
#It uses tidycensus's API to add census data to the district data.

library(tidycensus)
library(tidyverse)
library(tigris)
library(units)

options(scipen = 999)

rm(list = ls())

###DON'T RUN WITHOUT GETTING API KEY
#census_api_key("")
###DON'T RUN WITHOUT GETTING API KEY

#######
geography = "congressional district"
year_acs = 2018
year_dec = 2010
state = NULL
cache_table = TRUE
#######

var_lst = list(
  total_pop = list(summary = "one", vars = c("B01001_001")),
  cit_vote_pop = list(
    summary = "one",
    vars = c("B05003_009", "B05003_011", "B05003_020", "B05003_022")
  ),
  under_29_prop = list(
    summary = "total_pop",
    vars = paste("B01001_", str_pad(c(3:11, 27:35), 3, pad = "0"), sep = "")
  ),
  over_65_prop = list(
    summary = "total_pop",
    vars = paste("B01001_", str_pad(c(20:25, 44:49), 3, pad = "0"), sep = "")
  ),
  #male_prop = list(summary = "total_pop", vars = c("B01001_002")),
  female_prop = list(summary = "total_pop", vars = c("B01001_026")),
  median_age = list(summary = "one", vars = c("B01002_001")),
  white_prop = list(summary = "total_pop", vars = c("B01001H_001")),
  black_prop = list(summary = "total_pop", vars = c("B02001_003")),
  hispanic_prop = list(summary = "total_pop", vars = c("B01001I_001")),
  foreign_born_prop = list(summary = "total_pop", vars = c("B05002_013")),
  med_HH_inc = list(summary = "one", vars = c("B19013_001")),
  tot_civ_lab_force = list(summary = "one", vars = c("B23025_003")),
  unemp_prop = list(summary = "tot_civ_lab_force", vars = c("B23025_005")),
  pop_25_up = list(summary = "one", vars = c("B15003_001")),
  no_hs_prop = list(
    summary = "pop_25_up",
    vars = paste("B15003_", str_pad(2:16, 3, pad = "0"), sep = "")
  ),
  no_bach_prop = list(
    summary = "pop_25_up",
    vars = paste("B15003_", str_pad(2:21, 3, pad = "0"), sep = "")
  ),
  grad_plus_prop = list(
    summary = "pop_25_up",
    vars = paste("B15003_", str_pad(23:25, 3, pad = "0"), sep = "")
  ),
  wht_pop_25_up = list(summary = "one", vars = c("C15002H_001")),
  wht_no_hs_prop = list(
    summary = "pop_25_up",
    vars = paste("C15002H_", str_pad(c(3, 8), 3, pad = "0"), sep = "")
  ),
  wht_no_bach_prop = list(
    summary = "pop_25_up",
    vars = paste("C15002H_", str_pad(c(3:5, 8:10), 3, pad = "0"), sep = "")
  ),
  wht_bach_plus_prop = list(
    summary = "pop_25_up",
    vars = paste("C15002H_", str_pad(c(6, 11), 3, pad = "0"), sep = "")
  ),
  gini_idx = list(summary = "one", vars = c("B19083_001"))
)

df_acs <- get_acs(
  geography = geography,
  cache_table = cache_table,
  state = state,
  variables = var_lst[[1]][["vars"]],
  geometry = FALSE,
  year = year_acs,
  output = "wide"
)

df_acs$one = 1

est_vars <- paste(var_lst[[1]][["vars"]], "E", sep = "")
mar_vars <- paste(var_lst[[1]][["vars"]], "M", sep = "")
sum_var <- var_lst[[1]][["summary"]]
var_name <- names(var_lst[1])

df_acs[var_name] <- rowSums(df_acs[est_vars]) / df_acs[sum_var]

df_acs <- df_acs %>% select(-est_vars,-mar_vars)

str(df_acs)

for (i in names(var_lst[-1])) {
  temp_acs <- get_acs(
    geography = geography,
    cache_table = cache_table,
    state = state,
    variables = var_lst[[i]][["vars"]],
    geometry = FALSE,
    year = year_acs,
    output = "wide"
  )
  
  df_acs <- df_acs %>% full_join(temp_acs, by = c("GEOID", "NAME"))
  
  est_vars <- paste(var_lst[[i]][["vars"]], "E", sep = "")
  mar_vars <- paste(var_lst[[i]][["vars"]], "M", sep = "")
  sum_var <- var_lst[[i]][["summary"]]
  
  df_acs[i] <- rowSums(df_acs[est_vars]) / df_acs[sum_var]
  
  df_acs <- df_acs %>% select(-est_vars,-mar_vars)
}

df_acs$nonwhite_prop <- 1 - df_acs$white_prop

#Get one copy of the dataframe with geometry data
temp_acs <- get_acs(
  geography = geography,
  cache_table = cache_table,
  state = state,
  variables = "B01001_001",
  geometry = TRUE,
  keep_geo_vars = TRUE,
  year = year_acs,
  output = "wide"
)

#Populate land area for calculating density
df_acs <-
  df_acs %>% inner_join(as_tibble(temp_acs)[, c("GEOID", "ALAND")], by = c("GEOID"))

#Convert to square miles
df_acs$ALAND <- set_units(set_units(df_acs$ALAND, m^2), mi^2)

#############

temp_dec <- get_decennial(
  geography = geography,
  cache_table = cache_table,
  state = state,
  variables = c("P002001", "P002005"),
  geometry = FALSE,
  year = year_dec,
  output = "wide"
)

temp_dec <- temp_dec %>% mutate(rural_prop = P002005 / P002001)
temp_dec <- temp_dec %>% select(-c(P002001, P002005, NAME))

df_acs <- df_acs %>% inner_join(temp_dec, by = c("GEOID"))

###############
df_acs$density <- as.numeric(df_acs$total_pop / df_acs$ALAND)

df_acs <- df_acs %>% select(-c(one, tot_civ_lab_force, pop_25_up, wht_pop_25_up, ALAND)) %>% arrange(GEOID)

rm(temp_acs, temp_dec)
################

load("Election_Data_District.RData")

df_district_prop <- df_district_prop %>% inner_join(unique(fips_codes[, c("state", "state_code")]), by = c("state"="state"))

df_district_prop$district[df_district_prop$state %in% c("AK", "DE", "VT", "ND", "SD", "VT", "WY", "NE")] <- 0

df_district_prop$district <- as.integer(df_district_prop$district)

df_district_prop$district <- str_pad(df_district_prop$district, 2, pad = "0")

df_district_prop$GEOID <- paste(df_district_prop$state_code, df_district_prop$district, sep = "") 

df_district_prop <- df_district_prop %>% select(-state_code)

df_district_prop <- df_district_prop %>% full_join(df_acs, by = c("GEOID"))
##########

df_district_prop <- df_district_prop %>% select(-NAME, -GEOID)

df_district_prop %>% filter(rowSums(is.na(.)) > 0)

df_district_prop <- df_district_prop %>% filter(rowSums(is.na(.)) == 0)

df_district_prop <- df_district_prop %>% relocate(nonwhite_prop, .after=white_prop)

df_district_prop <- df_district_prop %>% select(-nonwhite_prop)

df_prop_census <- df_district_prop

save(df_prop_census, file = "Election_Data_District_Census.RData")