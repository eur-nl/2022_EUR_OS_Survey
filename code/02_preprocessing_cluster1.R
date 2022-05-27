
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

OS_data_clean <- 
  readRDS(here("data", "preprocessed", "rds", "CLEAN_OS_Survey_responses.rds"))

cluster1 <- 
  OS_data_clean %>%
  filter(cluster %in% c(0, 1) # keep questions of relevant cluster + cluster0
      # & !question %in% c("Department", "Position") # only keep school info
  ) %>% 
  droplevels() %>% # drop unused levels
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  pivot_longer( # convert to long format
    "value_1":tail(names(.), 1),
    names_to = "value",
    values_to = "item"
  ) %>%
  mutate(
    item = gsub("Other_", "", item), # delete "Other_" from free text responses
    item = recode(
      factor(item),
      # recode school
      "Erasmus School of History, Culture and Communication (ESHCC)" = "ESHCC",
      "Erasmus School of Health Policy & Management (ESHPM)" = "ESHPM",
      "Erasmus School of Philosophy (ESPHIL)" = "ESPhil",
      "Erasmus School of Social and Behavioural Sciences (ESSB)" = "ESSB",
      "Erasmus School of Law (ESL)" = "ESL",
      "International Institute of Social Studies (ISS)" = "ISS",
      "EUC" = "Other", # too few responses
      "ESSB and ESHPM" = "Other", # double affiliation coded as "Other"
      "I would rather not say" =  "Other", # non-disclosure coded as "Other"
      # recode position
      "Senior lecturer" = "Lecturer",
      "lecturer" = "Lecturer", 
      "Research assistant" = "Other",
      "Employee" = "Other",
      "Support" = "Other",
      "Support Staff" = "Other",
      "Educational advisor" = "Other",
      "Not sure why this matters" = "Other",
      # recode department
      "Erasmus SYNC Lab" = "Other",
      "Strategy group: Communication and behavior change" = "Other",
      "Strategy Group" = "Other",
      "Strategy" = "Other",
      "strategy" = "Other",
      "Public Administration,Sociology" = "Other", # double affiliation coded as "Other"
    
      
      
      
      
      
      
      
      )
    
    
    




# END ----------------------------------------------------------------
