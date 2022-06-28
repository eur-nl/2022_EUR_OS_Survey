
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

cluster4 <-
  readRDS(here("data", "preprocessed", "rds", "CLEAN_OS_Survey_responses.rds")) %>%
  filter(
    Finished == TRUE & # only completed surveys
    cluster == 4 # keep questions of relevant cluster
  ) %>%
  select(-c(Finished, Department, cluster)) %>% # omit irrelevant columns
  count(School, Position, question, response)

# save as .rds (to keep formatting in R)
saveRDS(
  cluster4,
  file = here("data", "preprocessed", "rds", "cluster_4.rds"),
  compress = TRUE
)

# save as .csv (for use with other software)
write_csv(
  cluster4,
  file = here("data", "preprocessed", "csv", "cluster_4.csv")
)

# END ----------------------------------------------------------------
