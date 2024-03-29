# Staten Island demographic information

# The purpose of this program is to download demographic information about the 
# borough of Staten Island

# 0. Packages -----------------------------------------------------------------
library(tidyverse)
library(sf)
library(tidycensus)
library(janitor)

# census_api_key("b79cffc22be2625e69c44505ffb55c60498f5796", install=TRUE)


# 1. Read in data -------------------------------------------------------------

ct <- st_read("nyct2020_24a/nyct2020.shp") %>%
  clean_names() %>%
  filter(boro_code == 5) %>%
  select(geoid, nta_name, cdtaname, geometry) %>%
  mutate(area_sqmi = st_area(.) / 2.788e+7) #units are feet so needs to be adjusted

# 2. Get Census Data ----------------------------------------------------------

# acsvar <- load_variables(2022, "acs5")

# Population
totpopvar <- c(totpop = "B01001_001")

# Population density: calculated based on land area of the district, 
## use census tracts from https://www.nyc.gov/site/planning/data-maps/open-data/census-download-metadata.page
# Race/ethnicity: 
racevars <- c(hispa = "B03002_012",
              white = "B03002_003",
              black = "B03002_004",
              asian = "B03002_006",
              nhisp = "B03002_002")

# Income
incvars <- c(mhhi = "B19013_001")

# Employment rate
empvars <- c(labforce   = "B23025_003",
             employed   = "B23025_004")

# English Language Proficiency
engvars <- c(hhpop     = "S1602_C01_001",
             limeng    = "S1602_C03_001",
             limengsp  = "S1602_C03_002",
             limengie  = "S1602_C03_003",
             limengas  = "S1602_C03_004",
             limengot  = "S1602_C03_005",
             limengpct = "S1602_C04_001")

# Vehicles Available
vehvars <- c(vehtot = "B08141_001",
             veh0   = "B08141_002",
             veh1   = "B08141_003",
             veh2   = "B08141_004",
             veh3m  = "B08141_005")

# Transport to Work
tranvars <- c(comtot = "B08006_001",
              comdri = "B08006_003",
              combus = "B08006_009",
              comtrn = "B08006_010",
              comcom = "B08006_011",
              comfer = "B08006_013",
              comwlk = "B08006_015",
              combik = "B08006_014",
              comwfh = "B08006_017")

acsvars <- c(totpopvar, racevars, incvars, empvars, engvars, vehvars, tranvars)

## Download data from ACS ----

### TRACT LEVEL
acs <- get_acs(geography = "tract", 
               variables = c(acsvars),
               state = "NY",
               county = "Richmond",
               year = 2022,
               survey = "acs5")

### COUNTY LEVEL
acs_county <- get_acs(geography = "county", 
               variables = c(acsvars),
               state = "NY",
               county = "Richmond",
               year = 2022,
               survey = "acs5")

## Pivot data wider ----
acs_wide <- acs %>%
  clean_names() %>%
  select(-moe) %>%
  pivot_wider(id_cols = c(geoid, name),
              names_from = variable,
              values_from = estimate) %>%
  # create Other race category based on existing categories
  mutate(re_other = nhisp - (asian + black + white),
         pwhite = white/totpop,
         pbipoc = 1 - pwhite,
         pveh0 = veh0/vehtot,
         pdri = comdri/comtot,
         ptrn = comtrn/comtot,
         pbus = combus/comtot,
         pfer = comfer/comtot,
         pwfh = comwfh/comtot)

acs_county_wide <- acs_county %>%
  clean_names() %>%
  select(-moe) %>%
  pivot_wider(id_cols = c(geoid, name),
              names_from = variable,
              values_from = estimate) %>%
  # create Other race category based on existing categories
  mutate(re_other = nhisp - (asian + black + white),
         pwhite = white/totpop,
         pbipoc = 1 - pwhite,
         pveh0 = veh0/vehtot,
         pdri = comdri/comtot,
         ptrn = comtrn/comtot,
         pbus = combus/comtot,
         pfer = comfer/comtot,
         pwfh = comwfh/comtot)


## Join to census tract to calculate the population density ----
acs_wide2 <- acs_wide %>%
  full_join(ct, by = "geoid") %>%
  mutate(dens_sqmi = totpop/area_sqmi)


# 3. Save permanent datasets --------------------------------------------------

# check that var names aren't too long for the shapefile
names(acs_wide2) %>%
  as.data.frame() %>%
  mutate(nchar = nchar(.)) %>%
  arrange(desc(nchar))

acs_wide2 %>%
  st_drop_geometry() %>%
  write_csv("si_dat/si_acs_ct.csv")

acs_county_wide %>%
  write_csv("si_dat/si_acs_county.csv")

acs_wide2 %>%
  st_write("si_dat/si_acs_ct.shp", delete_dsn = TRUE)

