
# RNG ---------------------------------------------------------------------

seed_proj <- 100
set.seed(seed_proj)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)

# Data --------------------------------------------------------

cluster0 <- 
  readRDS(here("data", "preprocessed", "rds", "CLEAN_OS_Survey_responses.rds")) %>% 
  select(participant, Finished, School, Department, Position) %>% # select only relevant columns
  distinct() %>% # keep only rows with unique combinations of values
  filter(!is.na(participant)) # delete participant as NA

# save as .rds (to keep formatting in R)
saveRDS(
  cluster0,
  file = here("data", "preprocessed", "rds", "cluster_0.rds"),
  compress = TRUE
)

# save as .csv (for use with other software)
write_csv(
  cluster0,
  file = here("data", "preprocessed", "csv", "cluster_0.csv")
)

# END ----------------------------------------------------------------
