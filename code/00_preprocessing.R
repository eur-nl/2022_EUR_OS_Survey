
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

# Setup --------------------------------------------------------

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
    "0" = "School",
    "0" = "Department",
    "0" = "Position",
    "1" = "In your opinion, how important is Open Access for your work?",
    "1" = "What is your experience with Open Access?",
    "1" = "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?",
    "1" = "Is there anything you want to share with us regarding your experiences with Open Access?", # ESL-only question
    "2" = "In your opinion, how important are open data, materials, and/or code for your work?",
    "2" = "What is your experience with using open data, materials, and/or code developed by others?",
    "2" = "What is your experience with openly sharing data, materials, and/or code that you developed?",
    "2" = "Are you familiar with the FAIR principles for data and code?",
    "2" = "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?",
    "3" = "In your opinion, how important is preregistration for your work?",
    "3" = "What is your experience with study preregistration?",
    "3" = "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with?",
    "4" = "In your opinion, how important are open educational resources for your work?",
    "4" = "What is your experience with using open educational resources developed by others?",
    "4" = "What is your experience with openly sharing educational resources that you developed?",
    "4" = "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with?",
    "5" = "In your opinion, how important is to have an open dialogue with society in your work?",
    "5" = "What is your experience engaging with society?",
    "5" = "The following are possible concerns that researchers could have about engaging with society. Which of these concerns would apply to you?",
    "6" = "Do you expect EUR to support you in learning open science practices?", 
    "6" = "Which of the following open science practices would you like EUR to provide information or support for?",
    "6" = "What support services provided at EUR have you used to make your data FAIR?",
    "7" = "Do you feel recognized and rewarded by EUR (e.g. in the R&O cycle or appraisal conversation) for the Open Science activities you undertake?",
    "7" = "In what way were you recognized and rewarded?",
    "7" = "In what way do you expect to be recognized and rewarded?",
    "8" = "Is there anything else you would like to mention about Open Science practices?"
  )

# Load data ----------------------------------------------------------------

# EUR OS Survey, pseudonymized data (retrieved on April 14th 2022)
EUR_OS_data <-
  read_csv(
    here("data", "20220414_EUR_OS_Survey_responses.csv"),
    col_names = TRUE,
    show_col_types = FALSE
  )

# ELS OS Survey (additional question), pseudonymized data (retrieved on April 14th 2022)
ESL_OS_data <-
  read_csv(
    here("data", "20220414_ESL_OS_Survey_responses.csv"),
    col_names = TRUE, 
    show_col_types = FALSE
  )

# merge EUR and ESL data
OS_data <- 
  full_join(EUR_OS_data, ESL_OS_data) %>% 
  relocate(Q5.1, .after = "Q5_7_TEXT") # move ESL-only question on open access close to other open access questions

# save as .csv
write_csv(
  OS_data,
  here("data", "preproc", "merged_OS_Survey_responses.csv")
)

# NOTE: the file 'merged_OS_Survey_responses.csv' is modified in Excel prior to loading in R:
# multiple choices are manually separated with ";" instead of the default ","
# to be able to split them into separate columns (see below).
# The resulting file is saved as 'manual_merged_OS_Survey_responses.csv'

# Clean data --------------------------------------------------------

OS_data_clean <- 
  read_csv(
    here("data", "preproc", "manual_merged_OS_Survey_responses.csv"),
    col_names = TRUE,
    show_col_types = FALSE
  ) %>% 
  `names<-`(as_vector(OS_data[1, ])) %>% # use questions as column names
  unite( # merge columns with school affiliation
    "School", 
    c(
      "What is your school affiliation? - Selected Choice",
      "What is your school affiliation? - Other - Text",
      ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
    ) %>% 
  unite( # merge columns with department affiliation
    "Department", 
    c(
      "Which department are you affiliated to? - Selected Choice",
      "Which department are you affiliated to? - Other - Text",
      "Which department are you affiliated to? - Selected Choice",
      "Which department are you affiliated to? - Other - Text",
      "Which department are you affiliated to?"  
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>% 
  unite( # merge columns with position
    "Position", 
    c(
      "What is your position? - Selected Choice",
      "What is your position? - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>% 
  unite( # merge (multiple choice & free text) columns on concerns on open access 
    "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?", 
    c(
      "The following are possible concerns that\nresearchers could have about Open Access publishing. \nWhich of these concerns would you agree with? Select all suitable options. - Selected Choice",
      "The following are possible concerns that\nresearchers could have about Open Access publishing. \nWhich of these concerns would you agree with? Select all suitable options. - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>% 
  unite( # merge (multiple choice & free text) columns on concerns on open data/materials/code 
    "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?", 
    c(
      "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. \n\nWhich of these concerns would you agree with? Select all suitable options. - Selected Choice",
      "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. \n\nWhich of these concerns would you agree with? Select all suitable options. - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>% 
  unite( # merge (multiple choice & free text) columns on concerns on preregistration 
    "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with?", 
    c(
      "The following are possible concerns that\nresearchers could have about preregistering their studies. Which of these concerns would you agree with? Select all suitable options. - Selected Choice",
      "The following are possible concerns that\nresearchers could have about preregistering their studies. Which of these concerns would you agree with? Select all suitable options. - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>% 
  unite( # merge (multiple choice & free text) columns on concerns on open educational resources 
    "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with?", 
    c(
      "The following are possible concerns that researchers could have about making educational resources developed by them openly available. \n Which of these concerns would you agree with? Select all suitable options. - Selected Choice",
      "The following are possible concerns that researchers could have about making educational resources developed by them openly available. \n Which of these concerns would you agree with? Select all suitable options. - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>% 
  unite( # merge (multiple choice & free text) columns on concerns on societal engagement 
    "The following are possible concerns that researchers could have about engaging with society. Which of these concerns would apply to you?", 
    c(
      "The following are possible concerns that\nresearchers could have about engaging with society.Which of these concerns would apply to you? Select all suitable options - Selected Choice",
      "The following are possible concerns that\nresearchers could have about engaging with society.Which of these concerns would apply to you? Select all suitable options - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>%
  unite( # merge (multiple choice & free text) columns on EUR support 
    "Which of the following open science practices would you like EUR to provide information or support for?", 
    c(
      "Which of the following open science practices would you like EUR to provide information or support for?Select all suitable options. - Selected Choice",
      "Which of the following open science practices would you like EUR to provide information or support for?Select all suitable options. - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>%
  unite( # merge (multiple choice & free text) columns on current EUR recognition and rewards 
    "In what way were you recognized and rewarded?", 
    c(
      "In what way were you recognized and rewarded? \nSelect all suitable options. - Selected Choice",
      "In what way were you recognized and rewarded? \nSelect all suitable options. - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>%
  unite( # merge (multiple choice & free text) columns on expected EUR recognition and rewards 
    "In what way do you expect to be recognized and rewarded?", 
    c(
      "In what way do you expect to be recognized and rewarded?\nSelect all suitable options. - Selected Choice",
      "In what way do you expect to be recognized and rewarded?\nSelect all suitable options. - Other - Text",
    ), 
    sep = "_", 
    remove = TRUE, 
    na.rm = TRUE
  ) %>%
  rename( # rename column (for better readability)
    "What support services provided at EUR have you used to make your data FAIR?" = "What support services provided at EUR have you used to make your data FAIR?\nSelect all suitable options."
  ) %>% 
  slice(-c(1:2)) %>% # delete rows with redundant or unnecessary data
  filter(Finished == "True") %>% # only keep completed surveys
  rowid_to_column(var = "Participant") %>%  # assign number to each participant
  select(-c("Start Date", "End Date", "Response Type", "Progress", "Duration (in seconds)", # discard unnecessary columns
            "Finished", "Recorded Date", "Response ID", "Distribution Channel", "User Language")) %>% 
  # convert to long format
  pivot_longer(
    "School":"Is there anything else you would like to mention about Open Science practices?", # keep participant as separate column
    names_to = "question",
    values_to = "value"
  ) %>%
  # multiple options can be selected for some questions;
  # we need to separate answers into different columns
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
    .before = "question"
  )

# save as .csv
write_csv(
  OS_data_clean,
  here("data", "preproc", "CLEAN_OS_Survey_responses.csv")
)

# Cluster 0 ----------------------------------------------------------------

num_cluster <- 0

cluster <-
  OS_data_clean %>%
  filter(cluster == num_cluster) %>% # keep only questions of relevant cluster
  droplevels() %>% # drop unused levels
  select(-cluster) %>% # drop unused column
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>% # rename column (for better readability)
  mutate(
    question = factor( # assign order questions
    question,
    levels = c(
      "School",
      "Position",
      "Department"
    ),
    ordered = TRUE
  ),
  item = gsub("Other_.*", "Other", item) # group all other responses in "Other"
  ) %>%
  group_by(question, item) %>%
  summarize( # count number of responses per question and item
    number_responses = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses, na.rm = FALSE) * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster_", num_cluster, ".csv"))
)

# Cluster 1 ----------------------------------------------------------------

num_cluster <- 1

cluster <-
  OS_data_clean %>%
  filter(cluster == num_cluster) %>% # keep only questions of relevant cluster
  droplevels() %>% # drop unused levels
  select(-cluster) %>% # drop unused column
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>% # rename column (for better readability)
  mutate(
    question = factor( # assign order questions
      question,
      levels = c(
        "In your opinion, how important is Open Access for your work?",
        "What is your experience with Open Access?",
        "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?",
        "Is there anything you want to share with us regarding your experiences with Open Access?" # ESL-only question
      ),
      ordered = TRUE
    ),
    item = gsub("Other_", "", item) # delete "Other_" from free text responses
  ) %>%
  group_by(question, item) %>%
  summarize( # count number of responses per question and item
    number_responses = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses, na.rm = FALSE) * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster_", num_cluster, ".csv"))
)

# Cluster 2 ----------------------------------------------------------------

num_cluster <- 2

cluster <-
  OS_data_clean %>%
  filter(cluster == num_cluster) %>% # keep only questions of relevant cluster
  droplevels() %>% # drop unused levels
  select(-cluster) %>% # drop unused column
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>% # rename column (for better readability)
  mutate(
    question = factor( # assign order questions
      question,
      levels = c(
        "In your opinion, how important are open data, materials, and/or code for your work?",
        "What is your experience with using open data, materials, and/or code developed by others?",
        "What is your experience with openly sharing data, materials, and/or code that you developed?",
        "Are you familiar with the FAIR principles for data and code?",
        "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?"
      ),
      ordered = TRUE
    ),
    item = gsub("Other_", "", item) # delete "Other_" from free text responses
  ) %>%
  group_by(question, item) %>%
  summarize( # count number of responses per question and item
    number_responses = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses, na.rm = FALSE) * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster_", num_cluster, ".csv"))
)

# Cluster 3 ----------------------------------------------------------------

num_cluster <- 3

cluster <-
  OS_data_clean %>%
  filter(cluster == num_cluster) %>% # keep only questions of relevant cluster
  droplevels() %>% # drop unused levels
  select(-cluster) %>% # drop unused column
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>% # rename column (for better readability)
  mutate(
    question = factor( # assign order questions
      question,
      levels = c(
        "In your opinion, how important is preregistration for your work?",
        "What is your experience with study preregistration?",
        "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with?"
      ),
      ordered = TRUE
    ),
    item = gsub("Other_", "", item) # delete "Other_" from free text responses
  ) %>%
  group_by(question, item) %>%
  summarize( # count number of responses per question and item
    number_responses = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses, na.rm = FALSE) * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster_", num_cluster, ".csv"))
)

# Cluster 4 ----------------------------------------------------------------

num_cluster <- 4

cluster <-
  OS_data_clean %>%
  filter(cluster == num_cluster) %>% # keep only questions of relevant cluster
  droplevels() %>% # drop unused levels
  select(-cluster) %>% # drop unused column
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>% # rename column (for better readability)
  mutate(
    question = factor( # assign order questions
      question,
      levels = c(
        "In your opinion, how important are open educational resources for your work?",
        "What is your experience with using open educational resources developed by others?",
        "What is your experience with openly sharing educational resources that you developed?",
        "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with?"
      ),
      ordered = TRUE
    ),
    item = gsub("Other_", "", item) # delete "Other_" from free text responses
  ) %>%
  group_by(question, item) %>%
  summarize( # count number of responses per question and item
    number_responses = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses, na.rm = FALSE) * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster_", num_cluster, ".csv"))
)

# Cluster 5 ----------------------------------------------------------------

num_cluster <- 5

cluster <-
  OS_data_clean %>%
  filter(cluster == num_cluster) %>% # keep only questions of relevant cluster
  droplevels() %>% # drop unused levels
  select(-cluster) %>% # drop unused column
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>% # rename column (for better readability)
  mutate(
    question = factor( # assign order questions
      question,
      levels = c(
        "In your opinion, how important is to have an open dialogue with society in your work?",
        "What is your experience engaging with society?",
        "The following are possible concerns that researchers could have about engaging with society. Which of these concerns would apply to you?"
      ),
      ordered = TRUE
    ),
    item = gsub("Other_", "", item) # delete "Other_" from free text responses
  ) %>%
  group_by(question, item) %>%
  summarize( # count number of responses per question and item
    number_responses = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses, na.rm = FALSE) * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster_", num_cluster, ".csv"))
)

# Cluster 6 ----------------------------------------------------------------

num_cluster <- 6

cluster <-
  OS_data_clean %>%
  filter(cluster == num_cluster) %>% # keep only questions of relevant cluster
  droplevels() %>% # drop unused levels
  select(-cluster) %>% # drop unused column
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>% # rename column (for better readability)
  mutate(
    question = factor( # assign order questions
      question,
      levels = c(
        "Do you expect EUR to support you in learning open science practices?",
        "Which of the following open science practices would you like EUR to provide information or support for?",
        "What support services provided at EUR have you used to make your data FAIR?"
      ),
      ordered = TRUE
    ),
    item = gsub("Other_", "", item) # delete "Other_" from free text responses
  ) %>%
  group_by(question, item) %>%
  summarize( # count number of responses per question and item
    number_responses = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses, na.rm = FALSE) * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster_", num_cluster, ".csv"))
)

# Cluster 7 ----------------------------------------------------------------

num_cluster <- 7

cluster <-
  OS_data_clean %>%
  filter(cluster == num_cluster) %>% # keep only questions of relevant cluster
  droplevels() %>% # drop unused levels
  select(-cluster) %>% # drop unused column
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>% # rename column (for better readability)
  mutate(
    question = factor( # assign order questions
      question,
      levels = c(
        "Do you feel recognized and rewarded by EUR (e.g. in the R&O cycle or appraisal conversation) for the Open Science activities you undertake?",
        "In what way were you recognized and rewarded?",
        "In what way do you expect to be recognized and rewarded?"
      ),
      ordered = TRUE
    ),
    item = gsub("Other_", "", item) # delete "Other_" from free text responses
  ) %>%
  group_by(question, item) %>%
  summarize( # count number of responses per question and item
    number_responses = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses, na.rm = FALSE) * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster_", num_cluster, ".csv"))
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

# END ----------------------------------------------------------------
