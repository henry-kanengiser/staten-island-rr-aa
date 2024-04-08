# National Transit Database

# The purpose of this script is to programmatically pull data from the US DOT's
#  National Transportation Database about the Staten Island Railroad

# 0. Packages -----------------------------------------------------------------
library(tidyverse)
library(janitor)


# 1. Read in data -------------------------------------------------------------

fund_22 <- read_csv("https://data.transportation.gov/api/views/yuaq-zdvc/rows.csv?date=20231102&accessType=DOWNLOAD&bom=true&format=true") %>%
  clean_names() %>%
  filter(str_detect(agency, "Staten Island"))
