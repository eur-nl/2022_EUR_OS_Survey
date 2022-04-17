
# RNG ---------------------------------------------------------------------

seed_proj <- 52118 # A1Z26 cipher (https://www.boxentriq.com/code-breaking/letters-to-numbers)
set.seed(seed_proj)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)

# load custom function to split variable into multiple columns
source(here("code", "functions", "split_into_multiple.R"))

# Setup ----------------------------------------------------------------

# separate questions into clusters:
# 0 = demographics
# 1 = open access
# 2 = open data, materials, and/or code
# 3 = preregistration
# 4 = open educational resources
# 5 = public engagement
# 6 = EUR support
# 7 = recognition and reward
# 8 = other

# assign a cluster number to each question
levels_question <- 
  c(
    "0" = "What is your school affiliation? - Selected Choice",
    "0" = "What is your school affiliation? - Other - Text", 
    "0" = "Which department are you affiliated to? - Selected Choice",
    "0" = "Which department are you affiliated to? - Other - Text",
    "0" = "Which department are you affiliated to?",
    "0" = "What is your position? - Selected Choice",
    "0" = "What is your position? - Other - Text",
    "1" = "In your opinion, how important is Open Access for your work?",
    "1" = "What is your experience with Open Access?",
    "1" = "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with? Select all suitable options. - Selected Choice",
    "1" = "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with? Select all suitable options. - Other - Text",
    "1" = "Is there anything you want to share with us regarding your experiences with Open Access?", # ESL-only question
    "2" = "In your opinion, how important are open data, materials, and/or code for your work?",
    "2" = "What is your experience with using open data, materials, and/or code developed by others?",
    "2" = "What is your experience with openly sharing data, materials, and/or code that you developed?",
    "2" = "Are you familiar with the FAIR principles for data and code?",
    "2" = "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with? Select all suitable options. - Selected Choice",
    "2" = "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with? Select all suitable options. - Other - Text",
    "3" = "In your opinion, how important is preregistration for your work?",
    "3" ="What is your experience with study preregistration?",
    "3" = "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with? Select all suitable options. - Selected Choice",
    "3" = "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with? Select all suitable options. - Other - Text",
    "4" = "In your opinion, how important are open educational resources for your work?",
    "4" = "What is your experience with using open educational resources developed by others?",
    "4" = "What is your experience with openly sharing educational resources that you developed?",
    "4" = "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with? Select all suitable options. - Selected Choice",
    "4" = "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with? Select all suitable options. - Other - Text",
    "5" = "In your opinion, how important is to have an open dialogue with society in your work?",
    "5" = "What is your experience engaging with society?",
    "5" = "The following are possible concerns that researchers could have about engaging with society.Which of these concerns would apply to you? Select all suitable options - Selected Choice",
    "5" = "The following are possible concerns that researchers could have about engaging with society.Which of these concerns would apply to you? Select all suitable options - Other - Text",
    "6" = "Do you expect EUR to support you in learning open science practices?",
    "6" = "Which of the following open science practices would you like EUR to provide information or support for? Select all suitable options. - Selected Choice",
    "6" = "Which of the following open science practices would you like EUR to provide information or support for? Select all suitable options. - Other - Text",
    "6" = "What support services provided at EUR have you used to make your data FAIR? Select all suitable options.",
    "7" = "Do you feel recognized and rewarded by EUR (e.g. in the R&O cycle or appraisal conversation) for the Open Science activities you undertake?",
    "7" = "In what way were you recognized and rewarded? Select all suitable options. - Selected Choice",
    "7" = "In what way were you recognized and rewarded? Select all suitable options. - Other - Text",
    "7" = "In what way do you expect to be recognized and rewarded? Select all suitable options. - Selected Choice",
    "7" = "In what way do you expect to be recognized and rewarded? Select all suitable options. - Other - Text",
    "8" = "Is there anything else you would like to mention about Open Science practices?"
  )

# Load data ----------------------------------------------------------------

# EUR OS Survey, pseudonymized data (retrieved on April 14th 2022)
EUR_OS_data <-
  read_csv(
    here("data", "20220414_EUR_OS_Survey_responses.csv"),
    col_names = TRUE, # other option: as_vector(EUR_OS_data[2, ])
    show_col_types = FALSE
  )

# ELS OS Survey (additional question), pseudonymized data (retrieved on April 14th 2022)
ESL_OS_data <-
  read_csv(
    here("data", "20220414_ESL_OS_Survey_responses.csv"),
    col_names = TRUE, # other option: as_vector(ESL_OS_data[2, ])
    show_col_types = FALSE
  )

# Clean data --------------------------------------------------------

OS_data_clean <- 
  full_join(EUR_OS_data, ESL_OS_data) %>% # merge EUR and ESL data
  relocate(Q5.1, .after = "Q5_7_TEXT") # move ESL-only question on open access close to other open access questions
  filter(Finished == "True") # only keep completed surveys
  
  
  
  
  
  

# 
# 
# # merge datasets
# OS_data <- 
#   bind_rows(EUR_OS_data, ESL_OS_data) %>% 
#   slice(3:n()) %>% # discard first 2 rows with redundant and useless information
# # 
# # # change column names
# # names(OS_data) <- as_vector(OS_data[2, ])



  
  
  





  # select(-c(11, 12, 17, 22, 26, 29, 40, 42, 45, 47, 50)) %>%  # discard columns with free text
  # rowid_to_column(var = "participant") %>%  # assign ID to each participant
  # convert to long format
  pivot_longer(
    3:tail(names(.), n = 1),
    names_to = "question",
    values_to = "value"
  ) %>% 
  # Multiple options can be selected for some questions
  # We need to separate answers into different columns
  bind_cols(
    split_into_multiple(
      .$value,
      pattern = ";",
      into_prefix = "value"
    )
  ) %>% 
  # delete column with redundant information
  select(-value) %>% 
  # convert all columns to factors
  mutate(
    across(
      .cols = everything(),
      .fns = ~ as_factor(.)
    )
  ) %>% 
  # add column with cluster
  mutate(
    cluster = fct_recode(question, !!!levels_question),
    .after = "Finished"
  )

ERIM_OS_clean

# save as .csv
write_csv(
  ERIM_OS_clean,
  here("data", "preproc", "CLEAN_20210608_ERIM_OS_Survey.csv")
)

# Cluster 0 ----------------------------------------------------------------

num_cluster <- 0

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  drop_na() %>% # drop rows with missing values
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "Which faculty are you from?",
      "Which department are you affiliated to? [RSM]",
      "Which department are you affiliated to? [ESE]",
      "What is your position?",
      "Are you member of any research institute affiliated with RSM or ESE?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 1 ----------------------------------------------------------------

num_cluster <- 1

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "What is your experience with open science practices?",
      "Are you sharing your knowledge about open science practices with others?",
      "During March 2021 ERIM launched an ORCID campaign. Did you participate in it and got your own ORCID iD?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 2 ----------------------------------------------------------------

num_cluster <- 2

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important for your field is it that researchers preregister their studies?",
      "What is your experience with study preregistration?",
      "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would apply to you?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 3 ----------------------------------------------------------------

num_cluster <- 3

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  dplyr::select(-c(Finished, cluster)) %>% # drop unused columns
  dplyr::select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important for your field is it that materials and/or code are openly available?",
      "What is your experience with using open materials and/or code?",
      "What is your experience with sharing open materials and/or code?",
      "The following are possible concerns that researchers could have about making their materials and/or code openly available. Which of these concerns would apply to you?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 4 ----------------------------------------------------------------

num_cluster <- 4

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important for your field is it that data from published research are openly available?",
      "What is your experience with using open data?",
      "What is your experience with sharing open data?",
      "The following are possible concerns that researchers could have about making their data openly available. Which of these concerns would apply to you?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 5 ----------------------------------------------------------------

num_cluster <- 5

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important is pre-publication archiving for your field?",
      "What is your experience with pre-publication archiving?",
      "The following are possible concerns that researchers could have about uploading a manuscript to a pre-publication archive before submitting it for peer review. Which of these concerns would apply to you?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 6 ----------------------------------------------------------------

num_cluster <- 6

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "Approximately what proportion of your publications from the last 5 years are open access?",
      "Many open access journals charge a fee for processing the article for publication. How have you managed payment of these fees?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 7 ----------------------------------------------------------------

num_cluster <- 7

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(question)) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 8 ----------------------------------------------------------------

num_cluster <- 8

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  filter(!is.na(value_1)) %>% # keep rows where response to value_1 is not NAs
  rename("item" = "value_1") %>%
  mutate(question = recode_factor(
    question,
    "Please indicate your awareness of each of the open science resources listed below [Open Science Framework]" = "Open Science Framework",    
    "Please indicate your awareness of each of the open science resources listed below [GitHub]" = "GitHub",      
    "Please indicate your awareness of each of the open science resources listed below [EUR data repository/Figshare]" = "EUR data repository/Figshare",
    "Please indicate your awareness of each of the open science resources listed below [4TU Center for Research Data]" = "4TU Center for Research Data",
    "Please indicate your awareness of each of the open science resources listed below [EUR SurfDrive]" = "EUR SurfDrive",
    "Please indicate your awareness of each of the open science resources listed below [EUR Dropbox (not personal)]" = "EUR Dropbox (not personal)",
    "Please indicate your awareness of each of the open science resources listed below [FAIR data principles]" = "FAIR data principles",
    "Please indicate your awareness of each of the open science resources listed below [EUR RePub]" = "EUR RePub",
    "Please indicate your awareness of each of the open science resources listed below [Zenodo]" = "Zenodo",                 
    "Please indicate your awareness of each of the open science resources listed below [Other 1]" = "Other 1",
    "Please indicate your awareness of each of the open science resources listed below [Other 2]" = "Other 2"
  )
  ) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 9 ----------------------------------------------------------------

num_cluster <- 9

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  filter(!is.na(value_1)) %>% # keep rows where response to value_1 is not NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "Do you expect that ERIM supports you in learning open science practices?",
      "Which of the following open science practices would you like ERIM to provide information or support for?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# END ----------------------------------------------------------------
