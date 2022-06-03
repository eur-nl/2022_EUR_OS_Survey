
# RNG ---------------------------------------------------------------------

seed_proj <- 100
set.seed(seed_proj)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)

# load custom function with recoding scheme
source(here("code", "functions", "recoding.R"))

# Data --------------------------------------------------------

OS_data_clean <-
  readRDS(here("data", "preprocessed", "rds", "CLEAN_OS_Survey_responses.rds"))

cluster1 <-
  OS_data_clean %>%
  filter(
    Finished == TRUE & # only completed surveys
    cluster %in% c(0, 1) & # keep questions of relevant cluster + cluster0
    question != "Department" # omit department info
  ) %>%
  droplevels() %>% # drop unused levels
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  pivot_longer( # convert to long format
    "value_1":tail(names(.), 1),
    names_to = "value",
    values_to = "item"
  ) %>%
  filter(!is.na(item)) %>%
  mutate(
    item = gsub("Other_", "", item), # delete "Other_" from free text responses
    item = fct_recode(item, !!!recode_all) # recode items for better readability
  )

# save as .rds (to keep formatting in R)
saveRDS(
  cluster1,
  file = here("data", "preprocessed", "rds", "cluster_1.rds"),
  compress = TRUE
)

# save as .csv (for use with other software)
write_csv(
  cluster1,
  file = here("data", "preprocessed", "csv", "cluster_1.csv")
)

# END ----------------------------------------------------------------
